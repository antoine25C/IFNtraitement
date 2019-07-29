#fixe l'environnement de travaille -----

dir <-choose.dir(caption = "Choissisez le dossier contenant les scripts R de traitement des données IFN et l'archive data.rds")#direction du dossier de travail où se trouve les scripts
setwd(dir)
#######################################import fonctions#######################################################
source("scripts_R_v2/fonctions.R", encoding = "UTF-8")
#######################################test de modélisation linéaire #######################################################
#--------------------------------------------Chargement RData-------
data<- readRDS('rdata/data.rds')
codes_ess <- data$codes_ess
codeser <- data$codeser
mnt_L93 <- raster("carto/bd_alti_fusionne.tif")
shp_geol <- data$shp_geol
Nom_ser <- data$Nom_Ser
dep <- data$dep
rad <- raster("carto/rad_estival_v1.tif")
prec <- raster("carto/prec_estival_v1.tif")

#----------------------------------------------------Appel des fonctions-----
  #--------------import des données IFN depuis Internet---

dep <- dep %>% 
  mutate(NumDep = as.character(NumDep),
         NomDep = as.character(paste(NomDep, NumDep,"    ")))
dep_choix <- tk_select.list(as.vector(dep[,2]), preselect = NULL, multiple = T,
                            title = "Selectionner les départements dont vous souhaiter extraire les données IFN")
dep_choix <- data.frame(NomDep = dep_choix)
dep_choix <- mutate(dep_choix, NomDep = as.character(NomDep))
dep_choix <-left_join(dep_choix, dep[,c('NumDep','NomDep')], by = 'NomDep')[,'NumDep']
Nom_ser <- Nom_ser %>% 
  mutate(NomSER = paste(NomSER, codeser,"    "))
ser_choix <- tk_select.list(as.vector(Nom_ser[,2]), preselect = NULL, multiple = T,
                            title = "Selectionner les sylvo-éco-régions sur lesquelles vous souhaitez restreindre les données IFN")
ser_choix <- data.frame(NomSER = ser_choix)
ser_choix <- mutate(ser_choix, NomSER = as.character(NomSER))
ser_choix <-as.character(left_join(data.frame(NomSER = ser_choix), Nom_ser, by = 'NomSER')[,'codeser'])
an_debut <- tk_select.list(2005:2017, preselect = NULL, multiple = F,
                           title = "choix de la première année dont les données IFN seront considérées")
an_fin <- tk_select.list(an_debut:2017, preselect = NULL, multiple = F,
                         title = "choix de la dernière année dont les données IFN seront considérées")
if (file.exists(paste0("rdata/don_IFN/",paste(dep_choix, collapse = "_"),"_",
                       paste(ser_choix, collapse = "_"),"_periode_de_", an_debut,"_a_",an_fin,"/IFN_data.rds"))){
  file_info <- as.Date(str_split(file.info(paste0("rdata/don_IFN/",paste(dep_choix, collapse = "_"),"_",
                                                  paste(ser_choix, collapse = "_"),"_periode_de_", an_debut,"_a_",an_fin,"/IFN_data.rds"))$mtime, pattern = " ")[[1]][1])
  if(as.Date(Sys.time())-30 < file_info){
    choix <-  tk_messageBox(type = "yesno", message = "Des donnes brutes IFN ont déjà été chargés il y a moins de 30 jours\nVoulez vous tout de même recharger ces données ?")
    if(choix == "yes"){
      IFN <-import_IFN(choix_dept = dep_choix, choix_ser = ser_choix, annees = an_debut:an_fin)
      #dep <- save_IFN_RData(IFN)
    } else {
      IFN <- readRDS(paste0("rdata/don_IFN/",paste(dep_choix, collapse = "_"),"_",
                            paste(ser_choix, collapse = "_"),"_periode_de_", an_debut,"_a_",an_fin,"/IFN_data.rds"))
    }
  }
} else {
  IFN <-import_IFN(choix_dept = dep_choix, choix_ser = ser_choix, annees = an_debut:an_fin)
  #dep <- save_IFN_RData(IFN)
}


  #---------------Calculs données dendrométriques par arbres et placettes------

IFN <- calc_dendro(IFN, 17.5, code_ess = codes_ess )
saveRDS(IFN, file =paste0("rdata/don_IFN/",paste(dep_choix, collapse = "_"),"_",
                          paste(ser_choix, collapse = "_"),"_periode_de_", an_debut,"_a_",an_fin,"/IFN_traite.rds"))

  #----------------ajout des données d'altitude aux placette depuis la BDalti-----

#bdalti73_74 <- fusion_BD_alti(list("mnt_73.tif","mnt_74.tif"), nom_sortie, crs =2154)
#raster::writeRaster(bdalti73_74, "bd_alti73&74.tif", format = "GTiff")

IFN <- extract_alt_plct_IFN (IFN, mnt_L93, crs = 2154)

  #----------------ajout des données geol avec données pré-traités depuis cousche de base BRGM-----
IFN <- affect_geol_plct_IFN(IFN, shp_geol)
IFN <- rayonnement(IFN, rad)
IFN <- precipitation(IFN, prec)

  #----------------preparation BD modèle d'accroissemen-------------------------------
BD_calib <- bd_calibration(IFN, codeser)


#----------test avec jeu de validation---------


##-----------résineux---------------
test_res <- BD_calib$BD_calib_tt_ser %>%
  dplyr::select(idp, AccG_res, Nha, Dg, Gha, G_feu, G_res, G_GB,
                type_melange_code, 
                codeser_num, #topo,
                pent2 , alt, expo, rad, prec #?niv_fertilite,
                #basique_acide, dure_tendre
                ) %>%
  mutate(type_melange_code=as.factor(type_melange_code),
         #niv_fertilite = as.factor(niv_fertilite),
         #basique_acide = as.factor(basique_acide),
         #dure_tendre = as.factor(dure_tendre),
         codeser_num = as.factor(as.integer(codeser_num))
         #,topo = as.factor(topo),
         #pente_class = as.factor(pente_class),
         #alt_class = as.factor(alt_class),
         #expo_class= as.factor(expo_class)
         )


test_res_train <-sample_frac(test_res, 0.75)
test_res_valid <- anti_join(test_res, test_res_train)
lm_G_res <- step(lm(AccG_res ~codeser_num +Nha + I(Nha^2) +
                      Dg + I(Dg^2) +
                      Gha + I(Gha^2) +
                      G_feu + I(G_feu^2) +
                      G_res + I(G_res^2) +
                      G_GB +
                      #p100_G_feu +
                      #p100_G_res +
                      type_melange_code +
                      #niv_fertilite +
                      #basique_acide +
                      #dure_tendre +
                      #topo +
                      alt +
                      expo +
                      pent2 +
                      rad + 
                      prec, data = test_res_train[!(colnames(test_res_train) %in% c('idp'))]),
                 direction = "both",
                 scope =lm(AccG_res~1, data = test_res_train[!(colnames(test_res_train) %in% c('idp'))]))
rf_res <- randomForest(AccG_res~., data = test_res_train[!(colnames(test_res_train) %in% c('idp'))], na.action = na.omit)
caret_res <- caret::train(AccG_res~., data = test_res_train[!(colnames(test_res_train) %in% c('idp'))], method = "rf")
print(rf_res)
print(caret_res)

# test_res_valid <- test_res_valid %>% 
#   left_join(BD_calib$BD_calib_tt_ser[colnames(BD_calib$BD_calib_tt_ser) %in% c('idp','Gha','G_res','G_feu','AccG','AccG_res','AccG_feu')], by = 'idp')
# test_res_train <- test_res_train %>% 
#   left_join(BD_calib$BD_calib_tt_ser[colnames(BD_calib$BD_calib_tt_ser) %in% c('idp','Gha','G_res','G_feu','AccG','AccG_res','AccG_feu')], by = 'idp')

test_res_valid$AccG_res_pred <-predict(rf_res, test_res_valid)#*test_res_valid$G_res
test_res_train$AccG_res_pred <-predict(rf_res, test_res_train)#*test_res_train$G_res
# test_res_valid$AccG_res_pred<-predict(caret_res, test_res_valid)#*test_res_valid$G_res
# test_res_train$AccG_res_pred <-predict(caret_res, test_res_train)#*test_res_train$G_res
# test_res_valid$AccG_res_lm <-predict(lm_G_res, test_res_valid)#*test_res_valid$G_res
# test_res_train$AccG_res_lm <-predict(lm_G_res, test_res_train)#*test_res_train$G_res


# 
# rectif_res_lm <- lm(AccG_res ~ AccG_res_pred, data = test_res_train)
# summary(rectif_res_lm)
# test_res_valid$AccG_res_pred<-predict(rectif_res_lm, test_res_valid)
# test_res_train$AccG_res_pred <-predict(rectif_res_lm, test_res_train)
# 


# 
# test_res_valid$err_pred <- test_res_valid$AccG_res_pred - test_res_valid$AccG_res
# test_res_valid$err_pred_caret <- test_res_valid$AccG_res_pred_caret - test_res_valid$AccG_res
# test_res_valid$err_pred_rectif <- test_res_valid$AccG_res_pred_caret_rectif - test_res_valid$AccG_res



##-----------feuillus--------------

test_feu <- BD_calib$BD_calib_tt_ser %>%
  dplyr::select(idp, AccG_feu, Nha, Dg,
                Gha, G_feu, G_res, G_GB,
                type_melange_code,
                codeser_num, #topo,
                pent2 , alt, expo,rad, prec#, niv_fertilite,
                #basique_acide, dure_tendre
                ) %>%
  mutate(type_melange_code=as.factor(type_melange_code),
         #niv_fertilite = as.factor(niv_fertilite),
         #basique_acide = as.factor(basique_acide),
         #dure_tendre = as.factor(dure_tendre),
         codeser_num = as.factor(as.integer(codeser_num))
         #topo = as.factor(topo),
         #pente_class = as.factor(pente_class),
         #alt_class = as.factor(alt_class),
         #expo_class= as.factor(expo_class)
  )


test_feu_train <-sample_frac(test_feu, 0.75)
test_feu_valid <- anti_join(test_feu, test_feu_train)

lm_G_feu <- step(lm(AccG_feu ~codeser_num +Nha + I(Nha^2) +
                      Dg + I(Dg^2) +
                      Gha + I(Gha^2) +
                      G_feu + I(G_feu^2) +
                      G_res + I(G_res^2) +
                      G_GB +
                      #p100_G_feu +
                      #p100_G_res +
                      type_melange_code +
                      #niv_fertilite +
                      #basique_acide +
                      #dure_tendre +
                      #alt +
                      #Itopo +
                      alt +
                      expo+
                      pent2 +
                      rad +
                      prec, data = test_feu_train[!(colnames(test_feu_train) %in% c('idp'))]),
                 direction = "both",
                 scope =lm(AccG_feu~1, data = test_feu_train[!(colnames(test_feu_train) %in% c('idp'))]))
rf_feu <- randomForest(AccG_feu~., data = test_feu_train[!(colnames(test_feu_train) %in% c('idp'))], na.action = na.omit)
caret_feu <- caret::train(AccG_feu~., data = test_feu_train[!(colnames(test_feu_train) %in% c('idp'))], method = "rf", na.action = na.omit)
print(rf_feu)
print(caret_feu)

# test_feu_valid <- test_feu_valid %>% 
#   left_join(BD_calib$BD_calib_tt_ser[colnames(BD_calib$BD_calib_tt_ser) %in% c('idp','Gha','G_res','G_feu','AccG','AccG_res','AccG_feu')], by = 'idp')
# test_feu_train <- test_feu_train %>% 
#   left_join(BD_calib$BD_calib_tt_ser[colnames(BD_calib$BD_calib_tt_ser) %in% c('idp','Gha','G_res','G_feu','AccG','AccG_res','AccG_feu')], by = 'idp')


test_feu_valid$AccG_feu_pred <-predict(rf_feu, test_feu_valid)#*test_feu_valid$G_feu
test_feu_train$AccG_feu_pred <-predict(rf_feu, test_feu_train)#*test_feu_train$G_feu
# test_feu_valid$AccG_feu_pred <-predict(caret_feu, test_feu_valid)#*test_feu_valid$G_feu
# test_feu_train$AccG_feu_pred <-predict(caret_feu, test_feu_train)#*test_feu_train$G_feu
# test_feu_valid$AccG_feu_lm <-predict(lm_G_feu, test_feu_valid)#*test_feu_valid$G_feu
# test_feu_train$AccG_feu_lm <-predict(lm_G_feu, test_feu_train)#*test_feu_train$G_feu


# rectif_feu_lm <- lm(AccG_feu ~ AccG_feu_pred, data = test_feu_train)
# summary(rectif_feu_lm)
# test_feu_valid$AccG_feu_pred <-predict(rectif_feu_lm, test_feu_valid)
# test_feu_train$AccG_feu_pred<-predict(rectif_feu_lm, test_feu_train)


# test_feu_valid$err_pred <- test_feu_valid$AccG_feu_pred - test_feu_valid$AccG_feu
# test_feu_valid$err_pred_caret <- test_feu_valid$AccG_feu_pred_caret - test_feu_valid$AccG_feu
# test_feu_valid$err_pred_rectif <- test_feu_valid$AccG_feu_pred_caret_rectif - test_feu_valid$AccG_feu
# test_feu_valid$err_pred_lm <- test_feu_valid$AccG_feu_lm - test_feu_valid$AccG_feu



#graphs -------------



rmse = round(RMSE(test_res_valid$AccG_res, test_res_valid$AccG_res_pred),2)
precis_mod_rf_res_don_valid <- ggplot(test_res_valid, aes(x=AccG_res, y =AccG_res_pred))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", colour = 'blue')+
  xlab("Accroissements résineux\nissues des données d'inventaire") +
  ylab("Accroissements résineux\nprédits par le modèle" )+
  ggtitle("Modèle résineux : précision des prédictions", subtitle = str_c("sur le jeu de données de validation      RMSE = ", rmse,"m²/ha",sep = " "))
  
  
rmse = round(RMSE(test_res_train$AccG_res, test_res_train$AccG_res_pred),2)
precis_mod_rf_res_don_calib <- ggplot(test_res_train, aes(x=AccG_res, y =AccG_res_pred))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", colour = 'blue')+
  xlab("Accroissements résineux\nissues des données d'inventaire") +
  ylab("Accroissements résineux\nprédits par le modèle" )+
  ggtitle("Modèle résineux : précision des prédictions", subtitle = str_c("sur le jeu de données de calibration      RMSE = ", rmse,"m²/ha",sep = " "))

rmse = round(RMSE(test_feu_valid$AccG_feu, test_feu_valid$AccG_feu_pred),2)
precis_mod_rf_feu_don_valid <- ggplot(test_feu_valid, aes(x=AccG_feu, y =AccG_feu_pred))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", colour = 'blue')+
  xlab("Accroissements feuillus\nissues des données d'inventaire") +
  ylab("Accroissements feuillus\nprédits par le modèle" )+
  ggtitle("Modèle feuillus : précision des prédictions", subtitle = str_c("sur le jeu de données de validation      RMSE =", rmse,"m²/ha",sep = " "))

rmse = round(RMSE(test_feu_train$AccG_feu, test_feu_train$AccG_feu_pred),2)
precis_mod_rf_feu_don_calib <- ggplot(test_feu_train, aes(x=AccG_feu, y =AccG_feu_pred))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", colour = 'blue')+
  xlab("Accroissements feuillus\nissues des données d'inventaire") +
  ylab("Accroissements feuillus\nprédits par le modèle" )+
  ggtitle("Modèle feuillus : précision des prédictions", subtitle = str_c("sur le jeu de données de calibration      RMSE = ", rmse,"m²/ha",sep = " "))

ggsave("graphs_results/precis_mod_rf_res_don_valid.png", precis_mod_rf_res_don_valid)
ggsave("graphs_results/precis_mod_rf_res_don_calib.png", precis_mod_rf_res_don_calib)
ggsave("graphs_results/precis_mod_rf_feu_don_valid.png", precis_mod_rf_feu_don_valid)
ggsave("graphs_results/precis_mod_rf_feu_don_calib.png", precis_mod_rf_feu_don_calib)
#sauvegarde modèles-----------------------
modeles <- list(rf_res, rf_feu)
names(modeles) <- c("rf_AccG_res", "rf_AccG_feu")
if(!dir.exists("rdata")){
  dir.create("rdata")
}
if (!dir.exists("rdata/modeles")){
  dir.create("rdata/modeles")
}
if (!dir.exists(paste0("rdata/modeles/",paste(dep_choix, collapse = "_"),"_",
                       paste(ser_choix, collapse = "_"),"_periode_de_", an_debut,"_a_",an_fin))){
  dir.create(paste0("rdata/modeles/",paste(dep_choix, collapse = "_"),"_",
                    paste(ser_choix, collapse = "_"),"_periode_de_", an_debut,"_a_",an_fin))
}
saveRDS(modeles, paste0("rdata/modeles/",paste(dep_choix, collapse = "_"),"_",
                        paste(ser_choix, collapse = "_"),"_periode_de_", an_debut,"_a_",an_fin,"/Modeles_Acc.rds"))


####################################en cours ########################



#------------test-------------
library(nlme)
library(easyreg)
library(RColorBrewer)

test <- test_res_train
nb_class <- 5
quantiles = data.frame()
for (i in 0:round(40/5,0)){
  if (i == 0){
    tmp <- test %>% 
      filter(G_res < 2.5)
  } else{
    tmp <- test %>% 
      filter(G_res >= (i*5-2.5),
             G_res < (i*5+2.5))
  }
  if(dim(tmp)[1] > (2*(nb_class-1))){
    quantiles <-  rbind(quantiles,cbind(G_res = i*5,t(data.frame(quantile(tmp$AccG_res, seq(0,1,1/nb_class))[-c(1,nb_class+1)]))))
  }
}
tmp <- test %>% 
  filter(G_res >= (round(40/5,0)*5-2.5))
if(dim(tmp)[1] > (2*(nb_class-1))){
  quantiles <-  rbind(quantiles,cbind(G_res = 60,t(data.frame(quantile(tmp$AccG_res, seq(0,1,1/nb_class))[-c(1,nb_class+1)]))))
}
row.names(quantiles) <- 1:dim(quantiles)[1]
# ggplot(quantiles, aes(x=G_res))+
#   geom_point(aes( y = `20%`, color = 'red'))+
#   geom_point(aes( y = `40%`, color = 'orange'))+
#   geom_point(aes( y = `60%`, color = 'yellow'))+
#   geom_point(aes( y = `80%`, color = 'green'))
# ggplot(test, aes(x = G_res, y = AccG_res), add = T) + 
#   geom_point()
# 
# fm_lm <- lm(log(quantiles[,2]) ~ quantiles[,1])
# st <- list(a = exp(coef(fm_lm)[1]), b = coef(fm_lm)[2])
# regplot(quantiles, model = 6, start = c(a=st[[1]], b= st[[2]]))

# start_value <- data.frame(a = numeric(), b = numeric(), c= numeric())
# for (i in 2:dim(quantiles)[2]){
#   a = max(quantiles[,i])
#   b = ((quantiles[dim(quantiles)[1],i]-quantiles[dim(quantiles)[1]-1,i])/
#          (quantiles[dim(quantiles)[1],1]-quantiles[dim(quantiles)[1]-1,1]))/a
#   c = -log(quantiles[1,i]/a)
#   start_value <- rbind(start_value, data.frame(a=a, b=b, c=c))
# }

list_gnls_res <- list()
for (i in 2:nb_class){
  data <- quantiles[,c(1,i)]
  colnames(data) <- c('G_res','AccG_res')
  gnls <- nlme::gnls(AccG_res ~ SSlogis(G_res, Asym, xmid, scal),data)
  list_gnls_res <- list.append(list_gnls_res, gnls)
  names(list_gnls_res)[length(list_gnls_res)] <- paste0("Q",i-1)
  quantiles[[paste0("Q",i-1,"_pred")]] <- predict(gnls,data)
  ggplot(quantiles, aes(x = G_res))+
    geom_point(aes(y = paste0("Q",i-1), color = 'green')) + 
    geom_point(aes(y = paste0("Q",i-1,"_pred"), color = 'red'))
}

for (i in 1:length(list_gnls_res)){
  test[[paste0("Q",i)]] <- raster::predict(list_gnls_res[[paste0("Q",i)]], test)
}
for (i in 1:length(list_gnls_res)){
  test[[paste0("Q",i)]] <- abs(test[[paste0("Q",i)]] - test$AccG_res)
}
test$cat <- NA
for (i in 1:dim(test)[1]){
  test$cat[i] = min(test[i,(dim(test)[2]-length(list_gnls_res)):(dim(test)[2]-1)])
  for (j in (dim(test)[2]-length(list_gnls_res)):(dim(test)[2]-1)){
    if (test$cat[i] == test[i,j]){
      test$cat[i] = names(test[j])
    }
  }
}

ggplot(test, aes(x = G_res, y = AccG_res, color = cat)) + geom_point()

test <- test %>%
  dplyr::select(idp,Nha,Dg,type_melange_code,codeser_num,pent2,alt,expo,rad,prec,basique_acide,dure_tendre,cat) %>% 
  mutate(cat = as.factor(cat))

y_a_expliquer <- as.factor(test$cat)
x_explicatives <- test[,c("Nha","Dg","type_melange_code","codeser_num","pent2","alt","expo",
                        "rad","prec","basique_acide","dure_tendre")]

rf_classification <- randomForest(x = x_explicatives, y = y_a_expliquer, importance = T)
print(rf_classification)

rf_classification$confusion

test$cat_pred <- predict(rf_classification, test[,c("Nha","Dg","type_melange_code","codeser_num","pent2","alt","expo",
                                                   "rad","prec","basique_acide","dure_tendre")])

calib <- test_res_train
calib$cat_pred <- predict(rf_classification, calib, na.action = na.omit)

valid <- test_res_valid
valid$cat_pred <- predict(rf_classification, valid)

test <- test %>% 
  left_join(test_res_train[,c("idp", "G_res", "AccG_res")], by = "idp")

ggplot(valid, aes(x = G_res, y = AccG_res, color = cat_pred)) + geom_point()

cpt <- 0
for (i in 1:dim(test)[1]){
  if (test$cat[i] == test$cat_pred[i]){
    cpt <-  cpt + 1
  }
}
(cpt/dim(test)[1])*100

test$cat_pred <- as.character(test$cat_pred)
test$AccG_res_pred2 <- NA
for (i in 1:dim(test)[1]){
  Quartile <- as.numeric(str_remove(test$cat_pred[i], patter = "Q"))
  test$AccG_res_pred2[i] <- predict(list_gnls_res[[Quartile]], test[i,])
}

test$err_pred<- test$AccG_res_pred2 - test$AccG_res


rmse = round(RMSE(test$AccG_res, test$AccG_res_pred2),2)
ggplot(test, aes(x=AccG_res, y =AccG_res_pred2))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm", colour = 'blue')+
  xlab("Accroissements résineux\nissues des données d'inventaire") +
  ylab("Accroissements résineux\nprédits par le modèle" )+
  ggtitle("Modèle résineux : précision des prédictions", subtitle = str_c("sur le jeu de données de calibration      RMSE = ", rmse,"m²/ha",sep = " "))

# test <- quantiles[,c(1,2,6)]
# colnames(test) <- c("G_res","AccG", "AccG_pred")
# tmp <-quantiles[,c(1,3,7)]
# colnames(tmp) <- colnames(test)
# test <- rbind(test, tmp)
# tmp <-quantiles[,c(1,4,8)]
# colnames(tmp) <- colnames(test)
# test <- rbind(test, tmp)
# tmp <-quantiles[,c(1,5,9)]
# colnames(tmp) <- colnames(test)
# test <- rbind(test, tmp)

ggplot(test, aes(x= G_res)) + 
  geom_point(aes(y = AccG), color = 'green') + 
  geom_point(aes(y = AccG_pred), color = 'red')


## extrait spacialise  donnant idées d'améliorations de la modélisations----------

# else if (family=="sdm"){
#   # package smd: variable binaire 0/1
#   
#   train=spTransform(values$placettes_extract,crs(values$bandes[[1]]))
#   colnames(train@data)[1]="bin"
#   train@data=train@data[,colnames(train@data)%in%c("bin",colnames(don))]
#   train@data=train@data[,!str_detect(colnames(train@data),"MG2")]
#   
#   # test un modele random forest pour recuperer les 10 variables les + importantes
#   # !!!!!!!!!!!!!!!!  AMELIORER LA SELCTION DE VARIABLES !!!!!!!!!!!!!!!!!!!!!!!!!
#   m=randomForest(bin~.,data=train)
#   imp=m$importance[order(m$importance,decreasing = T)]
#   names(imp)=attr(m$importance,"dimnames")[[1]][order(m$importance,decreasing = T)]
#   imp=imp[!str_detect(names(imp),"coords.x")]
#   cor=imp[1:10]
#   cor
#   train@data=train@data[,colnames(train@data)%in%c("bin",names(cor))]
#   d <- sdmData(bin~., train=train)
#   d
#   # 4 methodes sont utilisees, avec cross-validation
#   m=sdm(bin~.,data=d,methods=c('rf','brt','svm','rpart'),replication='cv')
#   m
#   values$modele=m }
# 
