#' Création d'une base de données par placette IFN contenant à la fois des variables dendrométriques et environementales
#'
#' Cette fonction renvoie une base de donnée par placette IFN comportant l'accroissement courant en surface terrière sur la placette
#' et un ensemble de données dendrométriques et environnementales.
#' Info : initialement ceci à été fait dans le but de calibrer un modèle d'accroissement en surface terrière.
#'
#' @param IFN_data Base de données IFN issue du téléchargement par la fonction import_IFN(...) puis
#' traitée par l'ensemble des fonctions calc_dendro(...), extract_alt_plct_IFN(...), affect_geol_plct_IFN(...),
#' rayonnement(...) et precipitation(...),
#' @param code_ser tableau de correspondance entre le code de la sylvo-éco-région de l'IFN (codeser) et un code numérique qui permettra ensuite
#'l'application des modèles de prédictions à des données aux formats raster.
#'
#' @return renvoie un data_frame avec des données par placettes IFN comprenant des données "brute", issues des calculs dendrométriques,
#' de données ajoutées à partir de données cartographiées extérieur + ajout du code sylvo-écorégion, et d'un regroupement
#' en grandes classee de l'altitude, lapente et l'exposition.
#' @export
#'
#' @import dplyr rlist
bd_calibration <- function (IFN_data, code_ser = IFNtraitement::code_ser_num){

  list_ser <- list()
  BD_calib <- list()
  BD_calib_tt_ser <- data.frame()
  don_placettes <- data.frame()
  don_ecologie <- data.frame()
  code_ser <- code_ser %>%
    mutate(codeser = as.character(codeser))
  for (i in 1:length(names(IFN_data$IFNplacettes))){
    don_placettes <- IFN_data$IFNplacettes[[i]]%>%
      mutate(ser = as.character(ser),
             tx_AccG = AccG/Gha,
             tx_AccG_feu = AccG_feu/G_feu,
             tx_AccG_res = AccG_res/G_res)%>%
      left_join(code_ser, by = c('ser' = 'codeser'))%>%
      left_join(as.data.frame(IFN_data$shp_plct_IFN)[,c('idp', 'alt')], by = 'idp')%>%
      dplyr::select(idp, tx_AccG, tx_AccG_feu, tx_AccG_res, AccG, AccG_feu, AccG_res, Nha, Dg, Gha, G_feu, G_res, G_GB, p100_G_feu, p100_G_res, type_melange, niv_fertilite, basique_acide, dure_tendre , ser, codeser_num, alt, rad, prec)


    don_ecologie <- IFN_data$IFNecologie[[i]]%>%
      dplyr::select(idp,topo, pent2, expo)%>%
      mutate(topo = as.factor(topo))

    bd_calib <- don_placettes %>%
      left_join(don_ecologie, by = c('idp'))%>%
      filter(#!is.na(niv_fertilite),
        #!is.na(basique_acide),
        #!is.na(dure_tendre),
        !is.na(alt),
        !is.na(topo),
        !is.na(pent2),
        !is.na(expo),
        !is.na(rad),
        !is.na(prec),
        !is.na(tx_AccG),
        !is.na(tx_AccG_feu),
        !is.na(tx_AccG_res))

    BD_calib_tt_ser <- rbind(BD_calib_tt_ser, bd_calib)
    BD_calib <- list.append(BD_calib, bd_calib)
    list_ser <- list.append(list_ser, unique(IFN_data$IFNplacettes[[i]]$ser)[[1]])
  }
  BD_calib <- list.append(BD_calib,BD_calib_tt_ser)
  names(BD_calib) <- c(paste("BD_calib", list_ser, sep="_"),"BD_calib_tt_ser")

  pente_class <- seq(0, 120, by = 20)
  BD_calib$BD_calib_tt_ser$pente_inter <- cut(BD_calib$BD_calib_tt_ser$pent2, pente_class)
  pente_inter <- levels(BD_calib$BD_calib_tt_ser$pente_inter)
  pente_corresp <- data.frame(pente_inter,pente_class = as.character(pente_class[-1])) %>%
    mutate(pente_inter = as.character(pente_inter))

  alt_class <- c(0,900,1200,1500,1800,2500)
  BD_calib$BD_calib_tt_ser$alt_inter <- cut(BD_calib$BD_calib_tt_ser$alt, alt_class)
  alt_inter <- levels(BD_calib$BD_calib_tt_ser$alt_inter)
  alt_corresp <- data.frame(alt_inter,alt_class = as.character(alt_class[-1]))%>%
    mutate(alt_inter = as.character(alt_inter))

  expo_class <- c(0,50,150,250,350,400)
  BD_calib$BD_calib_tt_ser$expo_inter <- cut(BD_calib$BD_calib_tt_ser$expo, expo_class)
  expo_inter <- levels(BD_calib$BD_calib_tt_ser$expo_inter)
  expo_corresp <- data.frame(expo_inter ,expo_class = as.character(expo_class[-1]))%>%
    mutate(expo_inter = as.character(expo_inter))


  type_melange <- as.character(c("feu","mixte","mixte_feu","mixte_res","res"))
  type_melange_corresp <- data.frame(type_melange,type_melange_code = as.character(c(5,3,4,2,1)))%>%
    mutate(type_melange = as.character(type_melange))


  for(i in 1:length(BD_calib)){
    BD_calib[[i]] <- BD_calib[[i]] %>%
      mutate(pente_inter = as.character(cut(pent2, pente_class)),
             alt_inter = as.character(cut(alt, alt_class)),
             expo_inter = as.character(cut(expo, expo_class)),
             type_melange = as.character(type_melange))%>%
      left_join(pente_corresp, by ='pente_inter')%>%
      left_join(alt_corresp, by='alt_inter')%>%
      left_join(expo_corresp, by = 'expo_inter')%>%
      left_join(type_melange_corresp, by = 'type_melange')%>%
      dplyr::select(-pente_inter, -alt_inter, -expo_inter#', - pent2, -expo
      )%>%
      filter(!is.na(pente_class),
             !is.na(expo_class),
             !is.na(alt_class)) %>%
      mutate(pente_class = as.factor(pente_class),
             alt_class = as.factor(alt_class),
             expo_class = as.factor(expo_class),
             type_melange_code = as.factor(type_melange_code),
             type_melange = as.factor(type_melange),
             ser = as.factor(ser),
             niv_fertilite = as.factor(niv_fertilite),
             basique_acide = as.factor(basique_acide),
             dure_tendre = as.factor(dure_tendre))
    BD_calib[[i]]$expo_class[BD_calib[[i]]$expo_class == '400']<-'50'
  }

  return(BD_calib)
}
