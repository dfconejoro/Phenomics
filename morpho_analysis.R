library(Momocs)
library(colordistance)
library(tidyverse)
library(readxl)

esquisse::esquisser()


ruta_p <- "D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/mask/mani/leaflet"
setwd(ruta_p) 


lf <- list.files(ruta_p, full.names=TRUE)
## importar imagenes usando Momocs ##
coo <- import_jpg(lf)


leaf_dbmani <- read_excel("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/mask/mani/db_leafpeanutR.xlsx")

dd <- leaf_dbmani

tibble(dd)

names(dd)


leaf_peanut <-Out(coo, fac = dplyr::data_frame(ACC=as.factor(dd$Accesion), LEFLET=as.factor(dd$`Leaflet number`),
                                               LEAF=as.factor(dd$Leaf), SPECIE=as.factor(dd$species)),
                  ldk = list())


b1 <- leaf_peanut%>%
  coo_center %>% coo_scale %>%
  coo_alignxax() %>% coo_slidedirection("up") %T>%
  print()

stack(b1)


bot.mani <- efourier(b1, nb.h=10)
bot.mani

bot.mani$coe

bot.mani$fac

as_df(bot.mani)

hcontrib(bot.mani,harm.r = 1:5,mp.r = c(0,1,2,5,10))

bot.mani %>%  PCA() %T>%                   
  plot_PCA(~SPECIE,labelgroups =T, legend = F,
           morphospace_position = "range_axes",palette = col_sari,
  ) 

bot.manip <- PCA(bot.mani)

#plot(bot.beanp, "SPECIE", pos.shp="range_axes", stars=TRUE, #     chull.filled=F, palette=col_spring#plot(bot.beanp, "SPECIE", chull=TRUE, pos.shp = "full_axes", abbreviate.labelsgroups = TRUE, points=FALSE, labelspoints = TRUE

plot(bot.manip,labelsgroups = T, "SPECIE", ellipses=F, 
     pos.shp="range_axes", pch=c(4, 5),palette = col_sari,
     chull=TRUE,  points=T,legend = T )

gg <-PCcontrib(bot.manip,nax = 1:4,sd.r = c(-2.5,-1,0,1,2.5))

gg$gg + geom_polygon(fill="slategrey", col="black")

#### shapemean #### 
bot.mani %>% MSHAPES("SPECIE") %>% plot_MSHAPES(size = 0.9,
                                                palette=pal_manual(c("darkgreen", "orange")))

#### MANOVA #### 

MANOVA(bot.manip, "SPECIE") ## Hotelling ## 

manova <- MANOVA_PW(bot.manip,"SPECIE")

manova$stars.tab
#### KMEANS #### 
KMEANS(bot.manip,centers = 4)
##### LDA #####

lda <- bot.mani %>%  PCA() %T>%                   # Principal Component Analysis
  plot_PCA(~SPECIE,labelgroups =T, legend = T) %>%           # A PC1:2 plot
  LDA(~SPECIE) 

# plot the cross-validation table
plot_CV(lda)  # tabular version
plot_CV(lda, freq=TRUE) # frequency table
plot_CV2(lda) # arrays version

##### Extraccion de morfometria tradicionale ### 





####### extraccion de datos  PC #######

pca_fourier_mani <- bot.manip %>% as_df()


pca_data <- pca_fourier_mani %>% group_by(ACC,LEAF) %>% 
  summarise(PC1LM = mean(PC1),
            PC2LM = mean(PC2),
            PC3LM = mean(PC3),
            PC4LM = mean(PC4),
            PC5LM = mean(PC5),
            PC6LM = mean(PC6),
            PC7LM = mean(PC7),
            PC8LM = mean(PC8),
            PC9LM = mean(PC9),
            PC10LM = mean(PC10))



write.csv(pca_data, file = "pca_fourier_mani.csv")


getwd()


### POD ##### 

ruta2 <- "D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/mask/mani/pod"
setwd(ruta2) 


lf_pod <- list.files(ruta2, full.names=TRUE)
## importar imagenes usando Momocs ##
coo_pod<- import_jpg(lf_pod)


## subir base de datos de factores ##
pod_dbmani <- read_excel("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/mask/mani/db_podpeanut.xlsx")

ee <- pod_dbmani

names(ee)

## Asignar clase Out + factores de variables ##

pod_peanut <-Out(coo_pod, fac = dplyr::data_frame(ACC=as.factor(ee$Accesion),
                                                    POD=as.factor(ee$pod_number),
                                                    SPECIE=as.factor(ee$specie)),
                  ldk = list())


b2 <- pod_peanut %>%
  coo_center %>% coo_scale %>%
  coo_alignxax() %>% coo_slidedirection("up") %T>%
  print()

stack(b2)

### Eliptics de fourier ###

bot.mani.pod <- efourier(b2, nb.h=10)
bot.mani.pod

bot.mani.pod$coe

bot.mani.pod$fac

as_df(bot.mani.pod)

hcontrib(bot.mani.pod,harm.r = 1:5,mp.r = c(0,1,2,5,10))

### pca ####
bot.mani.pod %>%  PCA() %T>%                   
  plot_PCA(~SPECIE,labelgroups =T, legend = F,
           morphospace_position = "range_axes",palette = col_sari,
  ) 

bot.manipod <- PCA(bot.mani.pod)

plot(bot.manipod,labelsgroups = T, "SPECIE", ellipses=F, 
     pos.shp="range_axes", pch=c(4, 5),palette = col_sari,
     chull=TRUE,  points=T,legend = T )

gg <-PCcontrib(bot.manipod,nax = 1:4,sd.r = c(-2.5,-1,0,1,2.5))

gg$gg + geom_polygon(fill="slategrey", col="black")

#### shapemean #### 
bot.mani.pod %>% MSHAPES("SPECIE") %>% plot_MSHAPES(size = 0.9,
                                                palette=pal_manual(c("darkgreen", "orange")))

#### MANOVA #### 

MANOVA(bot.manipod, "SPECIE") ## Hotelling ## 

manova <- MANOVA_PW(bot.manipod,"SPECIE")

manova$stars.tab
#### KMEANS #### 
KMEANS(bot.manipod,centers = 4)
##### LDA #####

lda <- bot.mani.pod %>%  PCA() %T>%                   # Principal Component Analysis
  plot_PCA(~SPECIE,labelgroups =T, legend = T) %>%           # A PC1:2 plot
  LDA(~SPECIE) 

# plot the cross-validation table
plot_CV(lda)  # tabular version
plot_CV(lda, freq=TRUE) # frequency table
plot_CV2(lda) # arrays version


pca_fourier_mani <- bot.manipod %>% as_df()

pca_data_pod <- pca_fourier_mani

getwd()

write.csv(pca_data_pod, file = "pca_fourier_mani_pod.csv")


### SEED ##### 

ruta3 <- "D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/mask/mani/seeds"
setwd(ruta3) 


lf_seed <- list.files(ruta3, full.names=TRUE)
## importar imagenes usando Momocs ##
coo_seed<- import_jpg(lf_seed)


## subir base de datos de factores ##
seed_dbmani <- read_excel("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/mask/mani/db_seedpeanut.xlsx")

aa <- seed_dbmani

names(aa)

## Asignar clase Out + factores de variables ##

seed_peanut <-Out(coo_seed, fac = dplyr::data_frame(ACC=as.factor(aa$Accesion),
                                                  SEED=as.factor(aa$seed_number),
                                                  SPECIE=as.factor(aa$specie)),
                 ldk = list())


b3 <- seed_peanut%>%
  coo_center %>% coo_scale %>%
  coo_alignxax() %>% coo_slidedirection("up") %T>%
  print()

stack(b3)

### Eliptics de fourier ###

bot.mani.seed <- efourier(b3, nb.h=10)
bot.mani.seed

bot.mani.seed$coe

bot.mani.seed$fac

as_df(bot.mani.seed)

hcontrib(bot.mani.seed,harm.r = 1:5,mp.r = c(0,1,2,5,10))


### pca ####
bot.mani.seed %>%  PCA() %T>%                   
  plot_PCA(~SPECIE,labelgroups =T, legend = F,
           morphospace_position = "range_axes",palette = col_sari,
  ) 

bot.maniseed <- PCA(bot.mani.seed)

#plot(bot.beanp, "SPECIE", pos.shp="range_axes", stars=TRUE, #     chull.filled=F, palette=col_spring#plot(bot.beanp, "SPECIE", chull=TRUE, pos.shp = "full_axes", abbreviate.labelsgroups = TRUE, points=FALSE, labelspoints = TRUE

plot(bot.maniseed,labelsgroups = T, "SPECIE", ellipses=F, 
     pos.shp="range_axes", pch=c(4, 5),palette = col_sari,
     chull=TRUE,  points=T,legend = T )

gg <-PCcontrib(bot.maniseed,nax = 1:4,sd.r = c(-2.5,-1,0,1,2.5))

gg$gg + geom_polygon(fill="slategrey", col="black")

#### shapemean #### 
bot.mani.seed %>% MSHAPES("SPECIE") %>% plot_MSHAPES(size = 0.9,
                                                palette=pal_manual(c("darkgreen", "orange")))
#### MANOVA #### 

MANOVA(bot.maniseed, "SPECIE") ## Hotelling ## 

manova <- MANOVA_PW(bot.maniseed,"SPECIE")

manova$stars.tab
#### KMEANS #### 
KMEANS(bot.maniseed,centers = 4)
##### LDA #####

lda <- bot.mani.seed %>%  PCA() %T>%                   # Principal Component Analysis
  plot_PCA(~SPECIE,labelgroups =T, legend = T) %>%           # A PC1:2 plot
  LDA(~SPECIE) 

# plot the cross-validation table
plot_CV(lda)  # tabular version
plot_CV(lda, freq=TRUE) # frequency table
plot_CV2(lda) # arrays version


pca_fourier_mani <- bot.maniseed %>% as_df()

pca_data_seed <- pca_fourier_mani

getwd()

write.csv(pca_data_seed, file = "pca_fourier_mani_seed.csv")
