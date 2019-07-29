# --------------- Librairies --------
library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(sf)
library(fasterize)
library(elevatr) #permet d'acceder aux bases de donnÃ©es MNH
library(raster)
library(gstat)
library(randomForest)

# --------------- Chargement données -----------------------------------------------
#rm(list = objects())
#departement <- "74_73_C52_E10_H10_H21_H22_periode_de_2008_a_2017"
departement <- "1_C51_C52_E10_E20_periode_de_2008_a_2017"
wd <- "F:/1_Stage_EAM_Lidar_Acct/05_IFN/espace_travail_IFN"
setwd(wd)

##---Chargement des modèles----
modeles <- readRDS(paste0("rdata/modeles/",departement, "/Modeles_Acc_1.1.rds"))

##---Chargement de données de codification
codeser <- readRDS("rdata/data.rds")$codeser

##---définit l'environnement SIG----
sig_wd <-"F:/1_Stage_EAM_Lidar_Acct/04_SIG"
zone <- "01"
sig_dendro_dir <- paste0("dendro/dendro_zone", zone)
raster_dir <-"/raster"
shp_dir <-"/shape"
dir_raster_par_zone <- "raster_par_zones/raster_zone_"
setwd(sig_wd)
##---Chargement des rasters----

#alt <- raster("mnt/938_6508.tif") #altitude issue du MNT
#pente <- raster("p100_pente.tif")  #pente en degré
N_ha <- raster(paste0(sig_dendro_dir,raster_dir,'/','N_pred.tiff'))
G_ha <- raster(paste0(sig_dendro_dir,raster_dir,'/','G_pred.tiff'))
G_res<- raster(paste0(sig_dendro_dir,raster_dir,'/','GR_pred.tiff'))
G_feu<- raster(paste0(sig_dendro_dir,raster_dir,'/','GF_pred.tiff'))

#G_GB<- raster(paste0(sig_dendro_dir,raster_dir,'/','GGB_pred.tif'))
Dg <- raster(paste0(sig_dendro_dir,raster_dir,'/','Dg_pred.tiff'))
#p100G_res <- raster(paste0(sig_dendro_dir,raster_dir,'/','p100GR_pred.tif'))
#p100G_feu <- raster(paste0(sig_dendro_dir,raster_dir,'/','p100GF_pred.tif'))
p100G_res <- (G_res/G_ha)*100
p100G_res[G_ha == 0] <- 0
p100G_res[p100G_res >= 100] <- 100
p100G_feu <- (G_feu/G_ha)*100
p100G_feu[G_feu == 0] <- 0
p100G_feu[p100G_feu >= 100] <- 100



##---Chargement des shapefiles----
ser <- st_read("ser_l93/ser_l93.shp") #shapefile des sylvo-éco-régions à appliquer à un raster de mêmme résolution, emprise et même calage que la données MNT, pente, donnees dendro
ser <- st_transform(ser, crs = 2154)

#---------------- Calculs des donnees nécésaires à l'application du modéle AccG ----
if (!"mnt_resample.tif" %in% list.files(paste0(dir_raster_par_zone,zone,"/resample"))){
  alt <- raster(paste0(dir_raster_par_zone,zone,"/MNT_Zone_",zone,"_2016_1m.tif"))
  system.time(alt <- resample(alt, G_ha, method = "bilinear"))
  raster::writeRaster(alt, paste0(dir_raster_par_zone,zone,"/resample/mnt_resample.tif"), overwrite = TRUE)
  #rm(alt)
}

if (!"pente_pc_resample.tif" %in% list.files(paste0(dir_raster_par_zone,zone,"/resample"))){
  pente <- raster("//ssig092/N_SIG_092/Commun/exo/lidar/dep73/RGD_IGN_C_2016/raster/raster_unique_allege/pente_pc_c.tif")
  #pente <- raster(paste0(dir_raster_par_zone,zone,"/pente_pcent.tif"))
  system.time(pente <- resample(pente, G_ha, method = "bilinear"))
  raster::writeRaster(pente, paste0(dir_raster_par_zone,zone,"/resample/pente_pc_resample.tif"), overwrite = TRUE)
  # rm(pente)
}

if (!"expo_resample.tif" %in% list.files(paste0(dir_raster_par_zone,zone,"/resample"))){
  expo<- raster("//ssig092/N_SIG_092/Commun/exo/lidar/dep73/RGD_IGN_C_2016/raster/raster_unique/exposition_c.tif")
  #expo<- raster(paste0(dir_raster_par_zone,zone,"/exposition.tif"))
  system.time(expo <- resample(expo, G_ha, method = "bilinear"))
  raster::writeRaster(expo, paste0(dir_raster_par_zone,zone,"/resample/expo_resample.tif"), overwrite = TRUE)
}

if (!"rayonnement_resample.tif" %in% list.files(paste0(dir_raster_par_zone,zone,"/resample"))){
  rad<- raster("raster_par_zones/raster_zone_B/rad_estival_v1.tif")
  system.time(rad <- resample(rad, G_ha, method = "bilinear"))
  raster::writeRaster(rad, paste0(dir_raster_par_zone,zone,"/resample/rayonnement_resample.tif"), overwrite = TRUE)
}

if (!"precipitation_resample.tif" %in% list.files(paste0(dir_raster_par_zone,zone,"/resample"))){
  prec<- raster(paste0(dir_raster_par_zone,zone,"/prec_estival_v1.tif"))
  system.time(prec <- resample(prec, G_ha, method = "bilinear"))
  raster::writeRaster(prec, paste0(dir_raster_par_zone,zone,"/resample/precipitation_resample.tif"), overwrite = TRUE)
}


#alt_class

alt <- raster(paste0(dir_raster_par_zone,zone,"/resample/mnt_resample.tif"))
# classe_altitude <- c(900,1200,1500,1800,2500)
# inf <- 0
# sup <- NA
# for (i in classe_altitude){
#   sup <- i
#   alt[(alt > inf) & (alt <= sup)]<-i
#   inf <-i
# }
# alt <- asFactor(alt)

#ser
ser <- ser %>%
  left_join(codeser, by = 'codeser')
ser_raster <- fasterize(ser, G_ha, field ="codeser_num")
ser_raster <- as.factor(ser_raster)

#pente_class


pente <- raster(paste0(dir_raster_par_zone,zone,"/resample/pente_pc_resample.tif"))
# classe_pente <- c(20,40,60,80,100,120)
# inf <- -1
# sup <- NA
# for (i in classe_pente){
#   sup <- i
#   pente[(pente > inf) & (pente <= sup)]<-i
#   inf <-i
# }
# pente[(pente > 120)]<-120
# pente <- asFactor(pente)


expo <- raster(paste0(dir_raster_par_zone,zone,"/resample/expo_resample.tif"))
# expo[] <- expo[]*(200/180)
# classe_expo<- c(50,150,250,350,400)
# inf <- -2
# sup <- NA
# for (i in classe_expo){
#   sup <- i
#   expo[(expo > inf) & (expo <= sup)]<-i
#   inf <-i
# }
# expo[expo == 400] <- 50
# expo <- asFactor(expo)

#rayonnement

rad <- raster(paste0(dir_raster_par_zone,zone,"/resample/rayonnement_resample.tif"))

#précipitation

prec <- raster(paste0(dir_raster_par_zone,zone,"/resample/precipitation_resample.tif"))

#type_melange

type_melange_code <- p100G_res

type_melange_code[type_melange_code >= 80] <- 101
type_melange_code[type_melange_code  >= 60 & type_melange_code < 80] <- 102
type_melange_code[type_melange_code >= 40 & type_melange_code < 60] <- 103
type_melange_code[type_melange_code  >= 20 & type_melange_code < 40] <- 104
type_melange_code[type_melange_code  < 20] <- 105

type_melange_code[] <-type_melange_code[]-100

type_melange_code <- as.factor(type_melange_code)
#resultat test

#type_melange_code[1000,500]#47,90999
#type_melange_code[700,500]#75,90823

#----------------Application du modèle AccG-------------------------------------------
N_ha <- mask(N_ha,G_ha)
G_res<- mask(G_res,G_ha)
G_feu[is.na(G_feu) & !is.na(G_ha)] <- 0
G_feu<- mask(G_feu,G_ha)
#G_GB<- mask(G_GB,G_ha)
Dg <- mask(Dg,G_ha)
# p100G_res <- mask(p100G_res,G_ha)
# p100G_feu <- mask(p100G_feu,G_ha)
type_melange_code <- mask(type_melange_code,G_ha)
alt <- mask(alt,G_ha)
pente <- mask(pente,G_ha)
expo <-mask(expo,G_ha)
ser_raster <- mask(ser_raster,G_ha)
rad <- mask(rad,G_ha)
prec <- mask(prec,G_ha)

BD <- brick(N_ha, G_ha, G_res, G_feu,# G_GB,
            Dg, type_melange_code, alt, pente, expo, ser_raster, rad, prec)
names(BD) <- c("Nha", "Gha", "G_res", "G_feu", #"G_GB",
               "Dg", "type_melange_code", "alt", "pent2", "expo", "codeser_num", "rad", "prec")
AccG_res <- raster::predict(object = BD, model = modeles$rf_AccG_res) #liste_modeles[2]
#AccG_res <- G_res * AccG_res
AccG_res[AccG_res < 0] <- 0
AccG_feu <- raster::predict(BD, modeles$rf_AccG_feu)
#AccG_feu <- G_feu * AccG_feu
AccG_feu[AccG_feu < 0] <- 0
AccG_stack <- brick(AccG_res, AccG_feu, BD$Gha)
names(AccG_stack) <- c("AccG_res", "AccG_feu", "Gha")
AccG_stack$AccG_res[AccG_stack$Gha == 0] <- 0
AccG_stack$AccG_feu[AccG_stack$Gha == 0] <- 0
AccG_res <- AccG_stack$AccG_res
AccG_feu <- AccG_stack$AccG_feu
AccG <-AccG_res + AccG_feu


AccG_res <- AccG_res*100
AccG_feu <- AccG_feu*100
AccG <-AccG_res + AccG_feu

writeRaster(AccG_res, filename = paste0(dir_raster_par_zone,zone,"/res_AccG/AccG_res_pred_v1.2.tif"), format = "GTiff",overwrite=TRUE)
writeRaster(AccG_feu, filename = paste0(dir_raster_par_zone,zone,"/res_AccG/AccG_feu_pred_v1.2.tif"), format = "GTiff",overwrite=TRUE)
writeRaster(AccG, filename = paste0(dir_raster_par_zone,zone,"/res_AccG/AccG_pred_v1.2.tif"), format = "GTiff",overwrite=TRUE)
#plot(AccG)
