#' Fonction qui à partir des coordonnées en X et Y floutées des données IFN crée un fichier de points de type shapefile
#' où chaque point correspond à une placette et leur affecte une altitude extraite d'un mnt spécifié en entée de la fonction
#'
#' @param IFN_data Base de données IFN au même format qu'à la sortie de la fonction import_IFN(...)
#' @param mnt_L93 Modèle Numérique de Terrain (MNT / DEM), raster supposé être projeté en lambert 93
#' @param crs système de coordonnées géographiques de référence, par défaut : Lambert 93 code EPSG = 2154
#' actuellement seul ce system de coordonée fonctionne pour cette fonction
#'
#' @return retourne la base de données initial avec le rajout d'un élement à la liste de donnée correspondant
#' au spatialDataFrame des placettes auxquelles une altitude approximative à été affectée
#' @export
#'
#' @import sf raster
extract_alt_plct_IFN<- function(IFN_data, mnt_L93, crs = 2154){
  if(crs != 2154){
    warning("cette fonction nécessite que le système de coordonnées des raster d'entrée soit le Lambert 93 code epsg = 2154")
    return(NULL)
  }
  IFNplacettes <- IFN_data$IFNplacettes[[1]]
  if(length(names(IFN_data$IFNplacettes))>1){
    for (i in 2:length(names(IFN_data$IFNplacettes))){
      IFNplacettes <- rbind(IFNplacettes, IFN_data$IFNplacettes[[i]])
    }
  }

  shp_plct_IFN <- st_as_sf(IFNplacettes, coords = c('xl93','yl93'))

  Lamb93 <- '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
  crs(mnt_L93) <- Lamb93

  alt <- raster::extract(mnt_L93, shp_plct_IFN)
  shp_plct_IFN$alt <- alt
  st_crs(shp_plct_IFN) <- crs

  nb_NA <- length(shp_plct_IFN$alt[is.na(shp_plct_IFN$alt)])
  if (nb_NA != 0){
    print(cat(paste0("Attention : ",nb_NA,"placettes / ",length(shp_plct_IFN$alt)," placettes\nn'ont pas eu d'altitude affectée par cette fonction\nPensez à vérifier que le MNT en entrée couvre bien l'ensemble des placettes IFN traitées" )))
  }
  IFN_data$shp_plct_IFN <- shp_plct_IFN
  return(IFN_data)
}


#' A partir d'un shapefile des placettes IFN auquelles des données géologiques ont été attribuées, cette fonction affecte
#' à chaque placette de la base de données IFN un type de regroupement géologique.
#'
#' @param IFN_data base de donnnées IFN (du même type que celle issue de la fonction import_IFN(...))
#' @param shp_geol shapefile des placettes IFN (avec positionnemeent en X et Y floutée) pour lequel sont spécifié:
#' l'identifiant de chaque placette (idp), un code de niveau de fertilite (niv_fertilite),
#' un classement entre acide/basique (basique_acide) et un classement entre roche dure/tendre (dure_tendre)
#'
#' @return renvoir la base de données IFN pour laquelle on a joint par placette et pour chaque arbre
#' les données géologiques : niveau de fertilité, acide/basique, dure/tendre
#' @export
#'
#' @import dplyr
affect_geol_plct_IFN <- function(IFN_data, shp_geol){
  plct_geol <- as.data.frame(shp_geol)[,c('idp', 'niv_fertilite', 'basique_acide', 'dure_tendre')]

  for (i in 1:length(names(IFN_data$IFNplacettes))){
    IFN_data$IFNplacettes[[i]] <- IFN_data$IFNplacettes[[i]] %>%
      left_join(plct_geol, by = 'idp')

    IFN_data$IFNarbres[[i]] <- IFN_data$IFNarbres[[i]] %>%
      left_join(plct_geol, by = 'idp')
  }
  return(IFN_data)
}

#' Affecte à chaque placette et chaque arbres de la base de données IFN une valeur de radiation issue d'un raster de rayonnement
#'
#' @param IFN_data base de donnnées IFN (du même type que celle issue de la fonction import_IFN(...))
#' @param rad cartographie du rayonnement solaire reçue au format raster et nécessairement projeté en lambert 93 (code EPSG : 2154)
#'
#' @return renvoie la base de données IFN avec un ajout de la donnée de rayonnement perçu sur une placette (rad).
#' Donnée ajoutée aux tableux de données par placette et par arbre.
#' @export
#'
#' @import sf raster dplyr
rayonnement <- function(IFN_data, rad){
  for (i in 1:length(names(IFN_data$IFNplacettes))){
    plct <- sf::st_as_sf(IFN_data$IFNplacettes[[i]], coords = c('xl93','yl93'))[,c('idp','geometry')]
    plct$rad <- raster::extract(rad, plct)
    IFN_data$IFNplacettes[[i]] <- IFN_data$IFNplacettes[[i]] %>%
      left_join(as.data.frame(plct)[colnames(plct) %in% c('idp', 'rad')] , by = 'idp')
    IFN_data$IFNarbres[[i]] <- IFN_data$IFNarbres[[i]] %>%
      left_join(as.data.frame(plct)[colnames(plct) %in% c('idp', 'rad')] , by = 'idp')
  }
  return(IFN_data)
}


#' Affecte à chaque placette et chaque arbres de la base de données IFN une valeur de précipitation issue d'un raster de précipitation
#'
#' @param IFN_data base de donnnées IFN (du même type que celle issue de la fonction import_IFN(...))
#' @param prec cartographie des précipitations au format raster et nécessairement projeté en lambert 93 (code EPSG : 2154)
#'
#' @return renvoie la base de données IFN avec un ajout de la donnée de précipitation sur une placette (prec).
#' Donnée ajoutée aux tableux de données par placette et par arbre.
#' @export
#'
#' @import sf raster dplyr
precipitation <- function(IFN_data, prec){
  for (i in 1:length(names(IFN_data$IFNplacettes))){
    plct <- sf::st_as_sf(IFN_data$IFNplacettes[[i]], coords = c('xl93','yl93'))[,c('idp','geometry')]
    plct$prec <- raster::extract(prec, plct)
    IFN_data$IFNplacettes[[i]] <- IFN_data$IFNplacettes[[i]] %>%
      left_join(as.data.frame(plct)[colnames(plct) %in% c('idp', 'prec')] , by = 'idp')
    IFN_data$IFNarbres[[i]] <- IFN_data$IFNarbres[[i]] %>%
      left_join(as.data.frame(plct)[colnames(plct) %in% c('idp', 'prec')] , by = 'idp')
  }
  return(IFN_data)
}
