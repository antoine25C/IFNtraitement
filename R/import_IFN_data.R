#' Téléchargement en ligne de la base de données brutes de l'IFN
#'
#' Cette fonction télécharge en ligne les dernières données misent à jour de l'inventaire forestier national en pouvant remonter
#' jusqu'aux inventaires de l'année 2015 et en général va jusqu'à l'année actuelle - 2 ans. Il est possible de préciser une année ou
#' plage d'année sur laquelle on souhaite télécharger les données. Il est aussi possible de préciser les départements
#' (numéro des départements) et/ou des sylvo-éco-région (code des SER) pour lesquels on veut restraindre les données. les données pauvent même
#' être éclatées par sylvo-écorégion
#'
#' @param choix_dept numéro du département ou liste des numéros de départements sur lequel /lesquels on souhaite obtenir les données
#'brutes de l'IFN. Par défaut vaut NULL et donc on récupère les données brutes de tous les départements métropolitain
#' @param choix_ser code de la sylvo-écorégion (format character/texte) ou liste des codes de la sylvo-écorégion sur laquel/lesquelles
#'on souhaite obtenir les données brutes de l'IFN. Par défaut vaut NULL et donc on récupère les données brutes de tous les ser
#' @param annees année (format numeric) ou liste d'années pour laquel/lesquels on souhaite télécharger les données brutes IFN
#'les données étant téléchargeable que depuis 2005, il n'est pas possible d'indiquer une année antérieure à 2005
#' @param split prend la valeur TRUE ou FALSE. Si la valeur est TRUE alors les données brutes IFN seront divisé en autant de data.frame
#'qu'il y a de sylvo-écorégion sur la zone où les données ont été téléchargé. Par défaut split vaut TRUE.
#' @param interactif vaut TRUE ou FALSE (FALSE par défaut), si vaut TRUE la fonction interragie avec l'utilisateur pour spécifier les paramètres
#' choix_dept, choix_ser et annees
#' @param save_dsn chemin complet pour l'enregistrement au format RDS des données IFN en sortie de la fonction, par défaut vaut "" et rien n'est enregistrer.
#' Si le chemein spécifié n'est pas correct le fichier n'est pas enregistrer (ex save_dsn = "C:/dos1/ssdos2/donnees_IFN" enregistrera
#' l'archive nommée donnees_IFN dans le sous dossier 2 du dossier 1 dans le répertoire C)
#'
#' @return la fonction retourne une liste avec trois sous listes correspondant chacune aux données placettes, arbres et écologies des données brutes de l'IFN
#'selon la valeur de split en entrée de la fonction chaque sous liste comportera 1 'si (split == FALSE) ou n dataframe correspondant chacun aux données restreintes
#'aux sylvo-éco-régions de la zone d'étude
#' @export
#'
#' @examples
import_IFN <- function(choix_dept = NULL, choix_ser = NULL, annees = NULL, split = T, interactif = F, save_dsn = ""){
  dep <- IFNtraitement::dep
  dep_choix <- tk_select.list(as.vector(dep[,2]), preselect = NULL, multiple = T,
                              title = "Selectionner les départements dont vous souhaiter extraire les données IFN")
  dep_choix <-left_join(data.frame(NomDep = dep_choix), dep[,c('NumDep','NomDep')], by = 'NomDep')[,'NumDep']
  ser <- IFNtraitement::ser
  ser_choix <- tk_select.list(as.vector(ser[,2]), preselect = NULL, multiple = T,
                              title = "Selectionner les sylvo-éco-régions sur lesquelles vous souhaitez restreindre les données IFN")
  ser_choix <-as.character(left_join(data.frame(NomSER = ser_choix), ser, by = 'NomSER')[,'codeser'])
  an_debut <- tk_select.list(2005:2017, preselect = NULL, multiple = F,
                             title = "choix de la première année dont les données IFN seront considérées")
  an_fin <- tk_select.list(an_debut:2017, preselect = NULL, multiple = F,
                           title = "choix de la dernière année dont les données IFN seront considérées")
  annees <- an_debut:an_fin
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
  if (dim(IFN_data$IFNplacettes)[1] != 0){
    test <- try(saveRDS(IFN_data, file= save_dsn))
    if (class(test) == "try-error" & save_dsn != ""){
      warning("Les données IFN n'ont pas pu être enregistrées,
              Cause probable : chemin vers le dossier de sauvegarde incorrect")
    }
  }
  return(IFN_data)
}



#' Fonction interne à la fonction IFNtraitement::import_IFN(...) qui effectue le filtrage des données
#' selon les départements et sylvo-éco-régions spécifiées
#'
#' @param choix_dept numéro du département ou liste des numéros de départements sur lequel /lesquels on souhaite obtenir les données
#'brutes de l'IFN. Par défaut vaut NULL et donc on récupère les données brutes de tous les départements métropolitain
#' @param choix_ser code de la sylvo-écorégion (format character/texte) ou liste des codes de la sylvo-écorégion sur laquel/lesquelles
#'on souhaite obtenir les données brutes de l'IFN. Par défaut vaut NULL et donc on récupère les données brutes de tous les ser
#' @param annees année (format numeric) ou liste d'années pour laquel/lesquels on souhaite télécharger les données brutes IFN
#'les données étant téléchargeable que depuis 2005, il n'est pas possible d'indiquer une année antérieure à 2005
#' @param split prend la valeur TRUE ou FALSE. Si la valeur est TRUE alors les données brutes IFN seront divisé en autant de data.frame
#'qu'il y a de sylvo-écorégion sur la zone où les données ont été téléchargé. Par défaut split vaut TRUE.
#'
#' @return la fonction retourne une liste avec trois sous listes correspondant chacune aux données placettes, arbres et écologies des données brutes de l'IFN
#'selon la valeur de split en entrée de la fonction chaque sous liste comportera 1 'si (split == FALSE) ou n dataframe correspondant chacun aux données restreindre aux sylvo-écorégion de la zone d'étude
#'
#' @examples
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
      dplyr::select(idp,a,simplif,espar,acci,ori,mortb,c13,ir5,age,htot,hdec,v,w,dep,ser,Annee,topo,pent2,expo,roche,decoupe)%>%
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
      dplyr::select(idp,a,simplif,espar,acci,ori,mortb,c13,ir5,age,htot,hdec,v,w,dep,ser,Annee,topo,pent2,expo,roche,decoupe)%>%
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
      dplyr::select(idp,a,simplif,espar,acci,ori,mortb,c13,ir5,age,htot,hdec,v,w,dep,ser,Annee,topo,pent2,expo,roche,decoupe)%>%
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
