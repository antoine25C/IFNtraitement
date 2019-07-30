#' rassemble l'ensemble des data.frames contenant des données arbres en un seul data.frame
#'
#' @param IFN_data Base de données IFN issue du téléchargement par la fonction import_IFN(...)
#'
#' @return renvoie un data.frame issue de la fusion de tous les dataframes initialment présent dans l'objet IFN_dat$IFNarbres
#' @export
#'
#' @examples
concatenation_BD_arbres <- function(IFN_data){
  IFNarbres <- data.frame()
  for (i in 1:length(IFN_data$IFNarbres)){
    IFNarbres <- rbind(IFNarbres, IFN_data$IFNarbres[[i]])
  }
  return(IFNarbres)
}

#' rassemble l'ensemble des data.frames contenant des données placettes en un seul data.frame
#'
#' @param IFN_data Base de données IFN issue du téléchargement par la fonction import_IFN(...)
#'
#' @return renvoie un data.frame issue de la fusion de tous les dataframes initialment présent dans l'objet IFN_dat$IFNplacette
#' @export
#'
#' @examples
concatenation_BD_placette <- function(IFN_data){
  IFNplacette <- data.frame()
  for (i in 1:length(IFN_data$IFNplacette)){
    IFNplacette <- rbind(IFNplacette, IFN_data$IFNplacette[[i]])
  }
  return(IFNplacette)
}

#' fonctions permettant la fusion de raster
#'
#' initialement cette fonction à permit la fusion de BD_alti de l'IGN sur plusieurs département
#'
#' @param list_dir_raster liste comportant le chemin sur le disque dur de chaque raster à fussionner
#' @param nom_sortie nom du raster qui sera enregistrer en sortie
#' @param chemin_sortie chemin vers le dossier où sera enregistrer le raster en sortie de la fonction
#' @param ext extension selon laquelle le raster en sortie sera enregistré (par défaut ".tif")
#' @param crs système de coordonnées géographique de référence, par défaut : Lambert 93 (code EPSG = 2154)
#' Actuellement la fonction ne peut fonctionner qu'avec des raster en Lambert 93
#'
#' @return renvoie le raster une fois fussionné et l'enregistre dans les dossier et avec le nom spécifié en entrée de la fonction
#' @export
#'
#' @import raster
fusion_BD_alti<- function(list_dir_raster, nom_sortie, chemin_sortie = getwd(), ext = ".tif", crs =2154){
  if(crs != 2154){
    warning("cette fonction nécessite que le système de coordonnées des raster d'entrée soit le Lambert 93 code epsg = 2154")
    return(NULL)
  }
  Lamb93 <- '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
  for (i in 1:length(list_dir_raster)){
    assign(x = as.character(paste0("bdalti",i)),value = raster(list_dir_raster[[i]]))
  }
  bdalti_merge <- get(paste0("bdalti",1))
  crs(bdalti_merge) <- Lamb93
  if (length(list_dir_raster)>1){
    for (i in 2:length(list_dir_raster)){
      bdalti <- get(paste0("bdalti",i))
      crs(bdalti) <- Lamb93
      bdalti_merge <- raster::merge(bdalti_merge, bdalti)
    }
  }
  writeRaster(bdalti_merge,paste0(paste(chemin_sortie,nom_sortie, sep = "/"),ext))
  return(bdalti_merge)
}


#' fonction permettant le calcul de l'erreur quadratique moyenne entre données prédites et données de référence
#'
#'les paramètres ref et predict doivent être de même longueur et bien ordonnée
#'(i.e. la nième valeur prédite doit correspondre à la nième valeur de référence)
#'
#' @param ref valeure ou vecteur des valeures de référence
#' @param predict valeure ou vecteur des valeures prédites
#'
#' @return renvoie l'ecart quadratique moyen entre valeures prédites et valeures de référence
#' @export
#'
#' @import dplyr
RMSE = function(ref, predict){
  df = data.frame(ref = ref,predict =predict)
  df <- df %>%
    filter(!is.na(ref),
           !is.na(predict))
  return(sqrt(mean((df$predict - df$ref)^2)))
}
