#' A partir des données brutes de l'IFN cette fonction calcul un ensemble de données dendrométriques à l'échelle de l'arbre et de la placette
#' (accroissement individuel d'un arbre, ... , surface terière totale de la placette, résineuse, ...)
#'
#' @param IFN_data données brutes de l'IFN téléchargées (cf. fonction import_IFN)
#' @param precomptable diamètre de précomptabilité pour effectuer les calculs dendrométriques. (Par défaut 17,5 cm)
#' @param code_ess tableau de correspondance entre les codes essences IFN (espar) et les codes essences ONF + regroupement par famille (R : résineux et F : feuillus )
#'
#' @return renvoie la base de données IFN avec l'ajout des calculs dendrométriques à l'échelle de l'arbre (cf. fonction calc_dendro_arbre)
#' et l'ajout sur la base de donnée placettes d'un ensemble de données dendrométriques à l'échelle placette :
#' densité de tige, surface terrière, accroissement en surface terrière, diamère quadratique moyen, surface terrière et accroissement
#' en surface terrière feuillus & résineuse, surface terrière de gros bois, porportion de feuillus, résineux et gros bois et
#' type de mélange entre feuillus et résieneux du peuplement)
#' @export
#'
#' @examples
calc_dendro <- function(IFN_data, precomptable = 17.5, code_ess = IFNtraitement::code_ess){
  for (i in 1:length(names(IFN_data$IFNarbres))){
    IFN_data$IFNarbres[[i]] <- IFN_data$IFNarbres[[i]]%>%
      filter(c13 > (precomptable*pi))
  }
  IFN_data <- select_plct_completes(IFN_data)
  IFN_data <- classif_cat_Diam(IFN_data)
  IFN_data <- calc_dendro_arbre(IFN_data, precomptable = 17.5, code_ess)
  for (i in 1:length(names(IFN_data$IFNplacettes))){
    #calcul de donnéees par placette toutes essences confondues
    Tot <- IFN_data$IFNarbres[[i]] %>%
      group_by(idp)%>%
      summarise(Htot =mean(htot),
                Nha = sum(w/100),
                Gha = sum(g*(w/100)),
                AccG = sum((w/100)*acc_g),
                Dg = sqrt(sum((c13/pi)^2)/n()))

    #calcul des données par placette en différenciant feuillus et résineux

    feu <-IFN_data$IFNarbres[[i]] %>%
      filter(fam == 'F') %>%
      group_by(idp)%>%
      summarise(G_feu = sum(g*(w/100)),
                AccG_feu = sum((w/100)*acc_g))

    res <-IFN_data$IFNarbres[[i]] %>%
      filter(fam == 'R') %>%
      group_by(idp)%>%
      summarise(G_res = sum(g*(w/100)),
                AccG_res = sum((w/100)*acc_g))

    #calcul G/ha de gros bois par placette
    GB <- IFN_data$IFNarbres[[i]] %>%
      filter(grd_catD %in% c("GB","TGB")) %>%
      group_by(idp)%>%
      summarise(G_GB = sum(g*(w/100)))

    #Concaténation de l'ensemble des données calculées par placettes
    IFN_data$IFNplacettes[[i]] <- IFN_data$IFNplacettes[[i]] %>%
      left_join(Tot, by = 'idp')%>%
      left_join(feu, by ='idp')%>%
      left_join(res, by = 'idp')%>%
      left_join(GB, by ='idp')%>%
      filter(!is.na(AccG))

    for (j in c('AccG_feu','AccG_res','G_feu','G_res', 'G_GB')){
      IFN_data$IFNplacettes[[i]][,j][is.na(IFN_data$IFNplacettes[[i]][,j])] <- 0
    }

    IFN_data$IFNplacettes[[i]] <- IFN_data$IFNplacettes[[i]] %>%
      mutate(p100_G_feu = (G_feu/Gha)*100,
             p100_G_res = (G_res/Gha)*100,
             p100_G_GB = (G_GB/Gha)*100,
             type_melange = NA)

    IFN_data$IFNplacettes[[i]]$type_melange <- NA
    IFN_data$IFNplacettes[[i]]$type_melange[IFN_data$IFNplacettes[[i]]$p100_G_res >= 80] <- 'res'
    IFN_data$IFNplacettes[[i]]$type_melange[IFN_data$IFNplacettes[[i]]$p100_G_res >= 60 & IFN_data$IFNplacettes[[i]]$p100_G_res < 80] <- 'mixte_res'
    IFN_data$IFNplacettes[[i]]$type_melange[IFN_data$IFNplacettes[[i]]$p100_G_res >= 40 & IFN_data$IFNplacettes[[i]]$p100_G_res < 60] <- 'mixte'
    IFN_data$IFNplacettes[[i]]$type_melange[IFN_data$IFNplacettes[[i]]$p100_G_res >= 20 & IFN_data$IFNplacettes[[i]]$p100_G_res < 40] <- 'mixte_feu'
    IFN_data$IFNplacettes[[i]]$type_melange[IFN_data$IFNplacettes[[i]]$p100_G_res < 20] <- 'feu'

    IFN_data$IFNplacettes[[i]] <- IFN_data$IFNplacettes[[i]] %>%
      left_join(IFN_data$IFNecologie[[i]][,c('idp','roche')], by = 'idp')

  }


  return(IFN_data)
}

#' A partir des données brutes de l'IFN, cette fonction sélectionne les placettes pour lesquelless on possède
#'l'accroissement individuel de l'ensemble des arbres de la placettes
#'
#' @param IFN_data données brutes de l'IFN téléchargées (cf. fonction import_IFN)
#'
#' @return renvoie la base de données filtrée sous le même format que la base de données spécifiée en entrée
#'
#' @examples
select_plct_completes<- function(IFN_data){
  for (i in 1:length(names(IFN_data$IFNarbres))){
    ir_manquant <- unique(IFN_data$IFNarbres[[i]]$idp[is.na(IFN_data$IFNarbres[[i]]$ir5)])
    IFN_data$IFNplacettes[[i]] <- IFN_data$IFNplacettes[[i]] %>%
      filter((idp %in% ir_manquant)==FALSE)
    IFN_data$IFNarbres[[i]] <- IFN_data$IFNarbres[[i]]%>%
      filter((idp %in% ir_manquant)==FALSE)
    IFN_data$IFNecologie[[i]] <- IFN_data$IFNecologie[[i]] %>%
      filter(idp %in% IFN_data$IFNplacettes[[i]]$idp)
  }
  return(IFN_data)
}

#' Détermine pour chaque arbre de la base de données de l'IFN sa catégorie de diamètre en classe de 5cm à 5cm
#' et en grandes classes (perches, PB, BM, GB, TGB)
#'
#'
#' @param IFN_data données brutes de l'IFN téléchargées (cf. fonction import_IFN)
#'
#' @return renvoie la base de données IFN spécifiée en entrée avec deux champs supplémentaires dans la base de données arbres
#' (catégorie de diamètre de 5cm en 5cm et grande catégorie de diamètre)
#' @export
#'
#' @examples
classif_cat_Diam <- function(IFN_data){
  listeD <- seq(2.5,147.5, by = 5)
  list_Cat <-append(rep("régé",2),c(rep("Perche",2),rep("PB",2),rep("BM",3), rep("GB",4),
                                    rep("TGB",(length(listeD)-13))))
  corres_cat <- data.table(listeD,list_Cat)

  for (j in 1:length(names(IFN_data$IFNarbres))){
    IFN_data$IFNarbres[[j]] <- IFN_data$IFNarbres[[j]]%>%
      mutate(catD = NA,
             grd_catD = NA)
    for (i in corres_cat$listeD){
      IFN_data$IFNarbres[[j]]$catD[IFN_data$IFNarbres[[j]]$c13<(i*pi) & IFN_data$IFNarbres[[j]]$c13>((i-5)*pi)] <- round(i-2.5, 0)
      IFN_data$IFNarbres[[j]]$grd_catD[IFN_data$IFNarbres[[j]]$c13<(i*pi) & IFN_data$IFNarbres[[j]]$c13>((i-5)*pi)] <- corres_cat$list_Cat[corres_cat$listeD == i]
    }
  }
  return(IFN_data)
}

#' Cette fonction calcule pour chaque arbre de l'inventaire foretier sa surface terrière individuelle
#' ainsi que son accroissement en diamètre et surface terrière
#'
#' @param IFN_data données brutes de l'IFN téléchargées (cf. fonction import_IFN)
#' @param precomptable diamètre de précomptabilité pour effectuer les calculs dendrométriques. (Par défaut 17,5 cm)
#' @param code_ess tableau de correspondance entre les codes essences IFN (espar) et les codes essences ONF + regroupement par famille (R : résineux et F : feuillus )
#'
#' @return renvoie la base de données IFN spécifiée en entrée avec l'ajout dans la base de données arbres des données :
#' surface terrière individuelle (g), accroissement individuel sur le diamètre (acc_d) et en surface terrière (acc_g)
#' @export
#'
#' @examples
calc_dendro_arbre <- function(IFN_data, precomptable = 17.5, code_ess = IFNtraitement::code_ess){
  IFNarbres <- IFN_data$IFNarbres
  for (i in 1:length(names(IFNarbres))){
    IFNarbres[[i]] <- IFNarbres[[i]]%>%
      filter(c13 > (precomptable*pi)) %>%
      mutate(g = (c13*10^(-2))^2/(4*pi),
             acc_d = (ir5/5)*(2/10),
             acc_g = (10^(-4)/(4*pi)) * (c13^2-(((c13/pi)-acc_d)*pi)^2))%>%
      left_join(codes_ess, by = "espar")
  }
  IFN_data$IFNarbres <-IFNarbres
  return(IFN_data)
}

