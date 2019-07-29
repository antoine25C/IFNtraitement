if (!require("data.table")) install.packages("data.table")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("sf")) install.packages("sf")
if (!require("raster")) install.packages("raster")
if (!require("rlist")) install.packages("rlist")
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages("caret")
if (!require("stringr")) install.packages("stringr")
if (!require("tcltk2")) install.packages("tcltk2")

#Librairies à charger ------------
library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(raster)
library(rlist)
library(randomForest)
library(caret)
library(stringr)
library(tcltk2)

# import des donnéees -------

#'
#'@choix_dept numéro du département ou liste des numéros de départements sur lequel /lesquels on souhaite obtenir les données
#'brutes de l'IFN. Par défaut vaut NULL et donc on récupère les données brutes de tous les départements métropolitain
#'@choix_dept code de la sylvo-écorégion (format character/texte) ou liste des codes de la sylvo-écorégion sur laquel/lesquelles
#'on souhaite obtenir les données brutes de l'IFN. Par défaut vaut NULL et donc on récupère les données brutes de tous les ser
#'@annees année (format numeric) ou liste d'années pour laquel/lesquels on souhaite télécharger les données brutes IFN
#'les données étant téléchargeable que depuis 2005, il n'est pas possible d'indiquer une année antérieure à 2005
#'@split prend la valeur TRUE ou FALSE. Si la valeur est TRUE alors les données brutes IFN seront divisé en autant de data.frame 
#'qu'il y a de sylvo-écorégion sur la zone où les données ont été téléchargé. Par défaut split vaut TRUE. 
#'
#'
#'@Return la fonction retourne une liste avec trois sous listes correspondant chacune aux données placettes, arbres et écologies des données brutes de l'IFN
#'selon la valeur de split en entrée de la fonction chaque sous liste comportera 1 'si (split == FALSE) ou n dataframe correspondant chacun aux données restreindre aux sylvo-écorégion de la zone d'étude
#'
import_IFN <- function(choix_dept = NULL, choix_ser = NULL, annees = NULL, split = T){
  if (is.null(annees)){
    warning("Attention vous n'avez pas spécifié d'année ou plage d'année sur
            lesquelles vous souhaitiez téléchargé les données IFN, 
            auncune données n'ont donc été téléchargées")
    return(NULL)
  } 
  else if((annees < 2005) || (min(annees)<2005)){
    warning("Attention l'année que vous avez spécifié ou la plus petite année de la plage 
            d'annéee spécifiée est antérieur à 2005 (première année pour lesquelles les données 
            IFN sont disponibles en libre accès sur internet)
            Aucune donnée n'a donc été téléchargée")
    return(NULL)
  } 
  else if (!class(annees)%in% c("integer","numeric")){
    warning("l'année ou plage d'année spécifié n'est pas composé de nombre entier,
            Aucune donnée n'a donc été téléchargée")
    return(NULL)
  } 
  else {
    rep <- "https://inventaire-forestier.ign.fr/IMG/zip/" #lien permettant le téléchargemnt via R des données brutes de l'IFN
    IFNarbres    <- data.table()
    IFNplacettes <- data.table()
    IFNecologie <- data.table()
    options(warn = -1) #permet de supprimer les messages d'alerte
    for (i in annees){
      print(paste("les données de l'année",i,"sont en cours de téléchargement", sep = " "))
      tempRep    <- tempdir()
      temp       <- tempfile()
      liste <- character()
      j <- 15 #à fixer à une valeur d'au moins 10 pour avoir les dernières données à jour de l'IFN (au 08/03/2019)
      test <- 1
      while (length(liste) == 0 & j >= -1){ #boucle qui test le téléchargement à l'adresse par exempl pour 2010"https://inventaire-forestier.ign.fr/IMG/zip/2010-'valeur de j dans la boucle'.zip"
        if (j > 0){
          repTour    <- paste0(rep,i,"-",j,".zip")
          test<- try(download.file(repTour, temp),silent = T)
          liste      <- unzip(temp, exdir=tempRep)
        } else if (j== 0){
          repTour    <- paste0(rep,i,".zip")
          test<- try(download.file(repTour, temp),silent = T)
          liste      <- unzip(temp, exdir=tempRep)
        } else {
          print(cat(paste("les données de l'année",i,"n'ont pas été téléchargées.\nCause probable : données absentes de la base de données IFN\ndisponible sur le site de l'inventaire forestier https://inventaire-forestier.ign.fr", sep = " ")))
        }
        j <- (j-1) #j diminue après chaque boucle pour trouver pas à pas le dernier fichier le plus à jour de la base de donnée
      }
      if (class(test) != "try-error"){
        tabArbres     <- read_csv2(liste[grepl("arbres_foret", liste)])
        tabPlacettes  <- read_csv2(liste[grepl("placettes_foret_2", liste)])
        tabEcologie <- read.csv2(liste[grepl("ecologie_2", liste)])
        tabPlacettes$Annee <- i
        tabArbres$Annee <- i
        tabEcologie$Annee <- i
        IFNarbres <- rbindlist(list(IFNarbres, tabArbres), use.names=TRUE, fill=TRUE) #data.table
        IFNplacettes <- rbindlist(list(IFNplacettes, tabPlacettes), use.names=TRUE, fill=TRUE)
        IFNecologie <- rbindlist(list(IFNecologie, tabEcologie), use.names=TRUE, fill=TRUE)
        unlink(temp); unlink(tempRep)
      }
    }
    options(warn = 0)
    IFN_data <- list()
    IFN_data$IFNplacettes <- IFNplacettes
    IFN_data$IFNarbres <- IFNarbres
    IFN_data$IFNecologie <- IFNecologie
    
  }
  if (dim(IFN_data$IFNplacettes)[1] != 0){
    IFN_data <-select_don_IFN(IFN_data, choix_dept = choix_dept, choix_ser=choix_ser, split = split)
  }
  if(!dir.exists("rdata")){
    dir.create("rdata")
  }
  if (!dir.exists("rdata/don_IFN")){
    dir.create("rdata/don_IFN")
  }
  if (!dir.exists(paste0("rdata/don_IFN/",paste(choix_dept, collapse = "_"),"_",
                         paste(choix_ser, collapse = "_"),"_periode_de_", min(annees),"_a_",max(annees)))){
    dir.create(paste0("rdata/don_IFN/",paste(choix_dept, collapse = "_"),"_",
                      paste(choix_ser, collapse = "_"),"_periode_de_",min(annees),"_a_",max(annees)))
  }
  saveRDS(IFN_data, file=paste0("rdata/don_IFN/",paste(choix_dept, collapse = "_"),"_",
                                paste(choix_ser, collapse = "_"),"_periode_de_", min(annees),"_a_",max(annees),"/IFN_data.rds"))
  return(IFN_data)
}


#'Fonction utilisé en intern de la fonction import_IFN précédente
#'
#'@IFN_data liste de de 3 sous listes comportant les données brutes IFN des fichiers placettes, arbres et écologie.
#'chacune de ces sous liste comporte un data.frame par année des données brutes IFN pour l'ensemble de la France métropolitaine.
#'@choix_dept numéro du département ou liste des numéros de départements sur lequel /lesquels on souhaite obtenir les données
#'brutes de l'IFN. Par défaut vaut NULL et donc on récupère les données brutes de tous les départements métropolitain
#'@choix_dept code de la sylvo-écorégion (format character/texte) ou liste des codes de la sylvo-écorégion sur laquel/lesquelles
#'on souhaite obtenir les données brutes de l'IFN. Par défaut vaut NULL et donc on récupère les données brutes de tous les ser
#'@split prend la valeur TRUE ou FALSE. Si la valeur est TRUE alors les données brutes IFN seront divisé en autant de data.frame 
#'qu'il y a de sylvo-écorégion sur la zone où les données ont été téléchargé. Par défaut split vaut TRUE. 
#'
#'
#'@Return la fonction retourne une liste avec trois sous listes correspondant chacune aux données placettes, arbres et écologies des données brutes de l'IFN
#'selon la valeur de split en entrée de la fonction chaque sous liste comportera 1 'si (split == FALSE) ou n dataframe correspondant chacun aux données restreindre aux sylvo-écorégion de la zone d'étude
#'
select_don_IFN <- function(IFN_data, choix_dept = NULL, choix_ser=NULL, split = T){
  ListPlacettes <- list()
  ListArbres <- list()
  ListEcologie <- list()
  
  
  if (!is.null(choix_dept)){
    IFN_data$IFNplacettes <- IFN_data$IFNplacettes %>%
      dplyr::select(idp,xl93,yl93,dep,ser,csa,dc,dist,Annee)%>%
      mutate(dep = as.numeric(dep))%>%
      filter(dep %in% choix_dept)
    IFN_data$IFNecologie <- IFN_data$IFNecologie %>%
      left_join(IFN_data$IFNplacettes[,c('idp', 'dep')], by = 'idp')%>%
      filter(dep %in% choix_dept) %>%
      dplyr::select(idp, topo, pent2, expo, roche)
    IFN_data$IFNarbres <- IFN_data$IFNarbres %>%
      left_join(IFN_data$IFNplacettes, by = c('idp','Annee'))%>%
      left_join(IFN_data$IFNecologie, by = 'idp')%>%
      dplyr::select(idp,a,simplif,espar,acci,ori,mortb,c13,ir5,age,htot,hdec,v,w,dep,ser,Annee,topo,pent2,expo,roche)%>%
      mutate(ir5 = as.numeric(ir5),
             v = as.numeric(v))%>%
      filter(dep %in% choix_dept)
  }
  if(!is.null(choix_ser) & is.null(choix_dept)){
    IFN_data$IFNplacettes <- IFN_data$IFNplacettes %>%
      dplyr::select(idp,xl93,yl93,dep,ser,csa,dc,dist,Annee)%>%
      mutate(dep = as.numeric(dep))%>%
      filter(ser %in% choix_ser)
    IFN_data$IFNecologie <- IFN_data$IFNecologie %>%
      left_join(IFN_data$IFNplacettes[,c('idp', 'ser')], by = 'idp')%>%
      filter(ser %in% choix_ser) %>%
      dplyr::select(idp, topo, pent2, expo, roche)
    IFN_data$IFNarbres <- IFN_data$IFNarbres %>%
      left_join(IFN_data$IFNplacettes, by = c('idp','Annee'))%>%
      left_join(IFN_data$IFNecologie, by = 'idp')%>%
      dplyr::select(idp,a,simplif,espar,acci,ori,mortb,c13,ir5,age,htot,hdec,v,w,dep,ser,Annee,topo,pent2,expo,roche)%>%
      mutate(ir5 = as.numeric(ir5),
             v = as.numeric(v))%>%
      filter(ser %in% choix_ser)
  }
  if(!is.null(choix_ser) & !is.null(choix_dept)){
    IFN_data$IFNplacettes <- IFN_data$IFNplacettes %>%
      filter(ser %in% choix_ser)
    IFN_data$IFNecologie <- IFN_data$IFNecologie %>%
      left_join(IFN_data$IFNplacettes[,c('idp', 'ser')], by = 'idp')%>%
      filter(ser %in% choix_ser) 
    IFN_data$IFNarbres <- IFN_data$IFNarbres %>%
      filter(ser %in% choix_ser)
  }
  if (is.null(choix_ser) & is.null(choix_dept)){
    IFN_data$IFNplacettes <- IFN_data$IFNplacettes %>%
      dplyr::select(idp,xl93,yl93,dep,ser,csa,dc,dist,Annee)
    IFN_data$IFNecologie <- IFN_data$IFNecologie %>% 
      dplyr::select(idp, topo, pent2, expo, roche)
    IFN_data$IFNarbres <- IFN_data$IFNarbres %>%
      left_join(IFN_data$IFNplacettes, by = c('idp','Annee'))%>%
      left_join(IFN_data$IFNecologie, by = 'idp')%>%
      dplyr::select(idp,a,simplif,espar,acci,ori,mortb,c13,ir5,age,htot,hdec,v,w,dep,ser,Annee,topo,pent2,expo,roche)%>%
      mutate(ir5 = as.numeric(ir5),
             v = as.numeric(v))
  }
  
  if (split == T){
    list_ser <- unique(IFN_data$IFNplacettes$ser)
    for (Ser in list_ser){
      IFNplacettes <- IFN_data$IFNplacettes %>%
        filter(ser == Ser)
      IFNarbres <- IFN_data$IFNarbres %>%
        filter(ser == Ser)
      IFNecologie <- IFN_data$IFNecologie %>%
        filter(idp %in% unique(IFNplacettes$idp))
      ListPlacettes <- list.append(ListPlacettes,IFNplacettes)
      ListArbres <- list.append(ListArbres,IFNarbres)
      ListEcologie <- list.append(ListEcologie,IFNecologie)
    }
    names(ListPlacettes) <-paste("IFNplacettes", list_ser, sep = "_")
    names(ListArbres) <-paste("IFNarbres", list_ser, sep = "_")
    names(ListEcologie) <-paste("IFNEcologie", list_ser, sep = "_")
  }
  else {
    ListPlacettes <- list.append(ListPlacettes,IFN_data$IFNplacettes)
    ListArbres <- list.append(ListArbres,IFN_data$IFNarbres)
    ListEcologie <- list.append(ListEcologie,IFN_data$IFNecologie)
    names(ListPlacettes) <-paste("IFNplacettes_tot")
    names(ListArbres) <-paste("IFNarbres_tot")
    names(ListEcologie) <-paste("IFNEcologie_tot")
  }
  
  
  IFN_data <- list()
  IFN_data$IFNplacettes <- ListPlacettes
  IFN_data$IFNarbres <- ListArbres
  IFN_data$IFNecologie <- ListEcologie
  return(IFN_data)
}

#'fonction permettant à partir des données IFN @IFN_data téléchargées de les sauvegardé au format .rds 
#'tout en récupérant le nom de l'ensemble des départements @dep pour lesquels les données ont été chargé
save_IFN_RData <- function(IFN_data){
  departement <- vector()
  ser <- vector()
  for (i in 1:length(names(IFN_data$IFNplacettes))){
    departement <-append(departement, unique(IFN_data$IFNplacettes[[i]]$dep))
    ser <- append(ser, unique(IFN_data$IFNplacettes[[i]]$ser))
  }
  departement <- unique(departement)
  ser <- unique(ser)
  
  dep<- departement[1]
  if (length(departement > 1)){
    for (i in departement[-1]){
      dep <- paste(dep,i,sep = "&")
    }
  }
  # saveRDS(IFN_data, file=paste0("IFN_data_",paste(choix_dep, collapse = "&"),"_",paste(choix_ser, collapse = "&","_de_", min(annees),"_a_",max(annees),".rds"))
  return(dep)
}
# ---------------traitement-----------------


#'fonction qui à partir des données brutes téléchargées @IFN_data sélectionne les placettes pour lesquels on possède
#'l'accroissement sur l'ensemble des arbres de la placettes
#'@return renvoie cette base de données nettoyée sous le même format que la base de donnéeen entrée
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

#' fonction qui à partir de la base de données brutes de l'IFN téléchargées @IFN_data détermine deux nouveaux champs
#' dans les données à l'échelle de l'arbre correspondant à la classe de diamètre par catégorie de diamètre de 5 en 5 cm @catD
#' et par grandes catégories de diamètre @grd_catD (perches, PB, BM, GB, TGB)
#' @return renvoie la base de données initiales avec ces deux champs supplémentaires 
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

#' fonctions effectuant des calculs dendrométriques à l'échelle de l'arbre 
#' 
#' @IFN_data données brutes de l'IFN téléchargées (cf. fonction import_IFN)
#' @precomptable diamètre de précomptabilité pour effectuer les calculs dendrométriques. (Par défaut 17,5 cm)
#' @code_ess tableau de correspondance entre les codes essences IFN (espar) et les codes essences ONF + regroupement par famille (R : résineux et F : feuillus )
#' 
#' @return  renvoie la base de données IFN avec la base de données arbres restreindre aux arbres précomptables et pour lesquesls
#' la surface terrière de l'arbre, son accroissement en diamètre et en surface terrière ont été calculés. le code essence ONF et 
#' la famille d'essence à laquel l'arbre appartient est également ajouté
#' 
calc_dendro_arbre <- function(IFN_data, precomptable = 17.5, code_ess){
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

#' fonctions effectuant des calculs dendrométriques à l'échelle de l'arbre 
#' 
#' @IFN_data données brutes de l'IFN téléchargées (cf. fonction import_IFN)
#' @precomptable diamètre de précomptabilité pour effectuer les calculs dendrométriques. (Par défaut 17,5 cm)
#' @code_ess tableau de correspondance entre les codes essences IFN (espar) et les codes essences ONF + regroupement par famille (R : résineux et F : feuillus )
#' 
#' @autres_fonctions fait appels à trois autres fonctions effectuant des actions sur la base de données brute de l'IFN,
#' select_plct_completes, classif_cat_Diam, calc_dendro_arbre
#' 
#' @return  renvoie la base de données IFN avec l'ajout des calculs dendrométriques à l'échelle de l'arbre (cf. fonction calc_dendro_arbre)
#' et l'ajout sur la base de onnée placettes d'un ensemble de données dendrométriques à l'échelle placette :
#' densité de tige, surface terrière, accroissement en surface terrière, diamère quadratique moyen, surface terrière et accroissement
#' en surface terrière feuillus & résineuse, surface terrière de gros bois, porportion de feuillus, résineux et gros bois et 
#' type de mélange entre feuillus et résieneux du peuplement)
#' 
calc_dendro <- function(IFN_data, precomptable = 17.5, code_ess){
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


# -----------------------------------------------Modélisations
#-------------------------------------------Préparation des données
##---Affectation de l'altitude à toutes les placettes IFN du secteur analysé----------------------

#'fonctions permettant la fusion de raster (initialement les BD_alti de l'IGN sur pluseurs département)
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

# list_dir_raster <- list("//ssig092/N_SIG_092/Commun/exo/ign/bdalti/dep73/v2012/BDALTI-73/mnt_73/w001001.adf",
#                         "//ssig092/N_SIG_092/Commun/exo/ign/bdalti/dep74/v2012/BDALTI-74/mnt_74/w001001.adf",
#                         "//ssig092/N_SIG_092/Commun/exo/ign/bdalti/dep01/v2012/BDALTI-01/mnt_01/w001001.adf")
# chemin_sortie  <- "F:/1_Stage_EAM_Lidar_Acct/05_IFN/espace_travail_IFN/carto"
# nom_sortie <- "bd_alti_fusionne"
# 
# bd_alti <- fusion_BD_alti(list_dir_raster, nom_sortie, chemin_sortie = chemin_sortie, ext = ".tif", crs =2154)



#' fonction qui à partir des coordonnées en lambert 93 floutées des placettes IFN et d'un MNT (@mnt_L93 : en Lambert 93)
#'  attribue une altitude auxplacettes de la base de données IFN @IFN_data 
#'  
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

##---Affectation de le géologie ------------------

# shp_geol <- st_read("F:/1_Stage_EAM_Lidar_Acct/04_SIG/brgm/plct_IFN_BRGM.shp")
# excel_geol <- readxl::read_xlsx("F:/1_Stage_EAM_Lidar_Acct/05_IFN/espace_travail_IFN/geol_regpt_v2.xlsx", sheet = "Sheet1")
# excel_geol <- excel_geol %>% 
#   mutate(id = paste(TYPE_GEOL, GEOCHIMIE, LITHOLOGIE, sep = "_")) %>%
#   dplyr::select(id, basique_acide, hydro, sols, dure_tendre, type, niv_fertilite)
# 
# shp_geol <- shp_geol%>% 
#   mutate(id = paste(TYPE_GEOL, GEOCHIMIE, LITHOLOGIE, sep = "_")) %>%
#   left_join(excel_geol) %>% 
#   dplyr::select(idp, dep, ser, TYPE_GEOL, GEOCHIMIE, LITHOLOGIE, id, basique_acide, hydro, sols, dure_tendre, type, niv_fertilite, geometry)
# sf::st_write(shp_geol,"carto/shp_plct_geol.shp")
# data <- readRDS("rdata/data.rds")
# data$shp_geol <- shp_geol
# saveRDS(data, "rdata/data.rds")

#' fonction qui à partir d'un shapefile des placettes IFN auquelles des données géologiques ont été attribuées @shp_geol
#' et des de la base de données IFN @IFN_data attribue à chaque placette un type de regroupement géologique.
#' 
#' 
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


##---Affectation des données climatiques -----

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

##---Preparation base de donnees pour modelisation de l'accroissement sans modèle IF parallèle----------------------
#'
#'@IFN_data données issues du téléchargement des données brutes de l'IFN et pour lesquels l'ensemble des données dendrométriques
#'ont été calculées et toutes les données nécessaires ont été jointes (altitude, géologie, ...)
#'@code_ser tableau de correspondance entre le code de la sylvo-éco-région de l'IFN (codeser) et un code numérique qui permettra ensuite
#'l'application des modèles de prédictions à des données aux formats raster.
#'
#'@return renvoie un data_frame avec les données pour chaque placettes IFN de la zone d'étude avec des données dendrométriques
#'+ code sylvo-écorégion, altitude, pente et exposition regroupé en grandes classes
#'
#'
bd_calibration <- function (IFN_data, code_ser){
  
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


#fonction permettant le calcul de l'erreur quadratique moyenne entre données prédites et données de référence;
RMSE = function(ref, predict){
  df = data.frame(ref = ref,predict =predict)
  df <- df %>% 
    filter(!is.na(ref),
           !is.na(predict))
  return(sqrt(mean((df$predict - df$ref)^2)))
}
