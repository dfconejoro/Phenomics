library(colordistance)
library(tidyverse)
library(factoextra)


##### Flores ##### 

#ruta_p <-"D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/mani/flower/"                                 
#setwd(paste(ruta_p))

ruta_b <- "D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/frijol/flower"
setwd(paste(ruta_b))

images_flores <- list.files(ruta_b, full.names=TRUE)

lower <- lower <- c(0.99, 0.99, 0.99)
upper <- c(1, 1, 1)


kmeans01 <- colordistance::getKMeanColors(images_flores[[129]], lower = lower, upper = upper, n = 10)


#histList_flores <- colordistance::getHistList(images_flores, lower=lower, upper=upper, bins=rep(3,3 ), 
#                                              plotting=F, pausing=FALSE)

#CDM_flores <- colordistance::getColorDistanceMatrix(histList_flores, method="emd", plotting=F)
#print(CDM_flores)

#colordistance::heatmapColorDistance(CDM_flores)

#lab_kmeans_flores <- colordistance::getKMeanColors(images_flores, n = 10, sample.size = 90000,
#                                            lower = rep(0.8, 3), upper = rep(1, 3), 
#                                            color.space = "lab", ref.white = "D65")

kmeans_f <- colordistance::getKMeansList(images_flores, bins = 10, lower = lower, upper = upper, 
                                         plotting = F)

kmeansClusters <- colordistance::extractClusters(kmeans_f, ordering = T)

head(kmeansClusters)


names <- as.data.frame(names(kmeansClusters))

names(kmeansClusters)

bb <- as.data.frame(do.call(rbind, kmeansClusters)) 

names <- as.data.frame(names(bb))

tibble(bb)

getwd()

#write.csv(bb, "color_flower_peanut.csv")
write.csv(bb, "color_flower_bean.csv")


#ruta0 <- "D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/color_mani/data"
#setwd(paste(ruta0))


color_flower_peanut <- read_csv("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/mani/color_flower_peanut.csv")
color_flower_bean <- read_csv("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/frijol/color_flower_beanR.csv")


names(color_flower_bean)

color_flower_peanut$Accesion <- as.factor(color_flower_peanut$Accesion)


db_color <- color_flower_bean %>% select(Accesions, Cluster,Flower, R, G, B) %>% 
  pivot_wider(names_from = c(Cluster), 
              values_from = c(R,G,B))

db_color

getwd()

write.csv(db_color, file = "color_flowerBR.csv")


#levels(db_color$Accessions)

############## POD #####################################

ruta1 <-"D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/mani/pod/"                                 
setwd(paste(ruta1))

images_pod <- list.files(ruta1, full.names=TRUE)

lower <- lower <- c(0.99, 0.99, 0.99)
upper <- c(1, 1, 1)

#histList_pod <- colordistance::getHistList(images_pod, lower=lower, upper=upper, bins=rep(3,3 ), 
#                                              plotting=F, pausing=FALSE)

#CDM_pod <- colordistance::getColorDistanceMatrix(histList_pod, method="emd", plotting=F)
#print(CDM_pod)

#colordistance::heatmapColorDistance(CDM_pod)

#lab_kmeans_pod <- colordistance::getKMeanColors(images_pod, n = 10, sample.size = 90000,
#                                                  lower = rep(0.8, 3), upper = rep(1, 3), 
#                                                   color.space = "lab", ref.white = "D65")

kmeans_p <- colordistance::getKMeansList(images_pod, bins = 10, lower = lower, upper = upper, 
                                         plotting = F)

kmeansClusters <- colordistance::extractClusters(kmeans_p, ordering = T)

head(kmeansClusters)

names <- as.data.frame(names(kmeansClusters))

names

bb <- as.data.frame(do.call(rbind, kmeansClusters)) 

tibble(bb)

getwd()

write.csv(bb, file = "color_pod.csv")

#ruta0 <- "D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/dataR/R/mani"
#setwd(paste(ruta0))


pod_color <- read_csv("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/mani/color_pod.csv")


db_color <- pod_color %>% select(Accesions, Cluster,Pod, R, G, B) %>% 
  pivot_wider(names_from = c(Cluster), 
              values_from = c(R,G,B))


db_color

levels(db_color$Accessions)



write.csv(db_color, file = "pod_colorR.csv")
###### SEEEDS #######


ruta2 <-"D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/mani/seeds/"                                 
setwd(paste(ruta2))


ruta3 <- "D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/frijol/seeds"
setwd(paste(ruta3))


images_seed <- list.files(ruta3, full.names=TRUE)

lower <- lower <- c(0.99, 0.99, 0.99)
upper <- c(1, 1, 1)

#histList_seed <- colordistance::getHistList(images_seed, lower=lower, upper=upper, bins=rep(3,3 ), 
#                                           plotting=F, pausing=FALSE)

#CDM_seed <- colordistance::getColorDistanceMatrix(histList_seed, method="emd", plotting=F)
#print(CDM_seed)

#colordistance::heatmapColorDistance(CDM_seed)

#lab_kmeans_seed <- colordistance::getKMeanColors(images_seed, n = 10, sample.size = 90000,
#                                                lower = rep(0.8, 3), upper = rep(1, 3), 
#                                                color.space = "lab", ref.white = "D65")

kmeans_s <- colordistance::getKMeansList(images_seed, bins = 10, lower = lower, upper = upper, 
                                         plotting = F)

kmeans01 <- colordistance::getKMeanColors(images_seed[[138]], 
                                          lower = lower, upper = upper, n = 10)


kmeansClusters <- colordistance::extractClusters(kmeans_s, ordering = T)

head(kmeansClusters)

names <- as.data.frame(names(kmeansClusters))

names

bb <- as.data.frame(do.call(rbind, kmeansClusters)) 

tibble(bb)

getwd()

write.csv(bb, file = "color_seed_bean.csv")


########################################################################################

ruta0 <-"D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/frijol"
setwd(paste(ruta0))

library(tidyr)


color_seed <- read_csv("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/frijol/color_seed_bean.csv")

color_flower <- read_excel("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/mani/color_flower.xlsx")

color_pod <- read_csv("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/mani/color_pod.csv")

color_flower_beanE <- read_excel("color_flower_beanE.xlsx")



color_flower_bean$Cluster <- as.factor(color_flower_bean$Cluster)

str(color_flower_bean)

names(color_flower_bean)




levels(color_flower_peanut$Cluster)

color_flower_peanut <- read_csv("D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/dataR/R/mani/color_flower_peanut.csv")

color_flower_peanut$Cluster <- as.factor(color_flower_peanut$Cluster)
color_flower_peanut$Accesion <- as.factor(color_flower_peanut$Accesion)


color_flower_peanut %>% select(Accesion, Cluster,Flower, R, G, B) %>% 
  group_by(Cluster) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = c(Cluster), values_from = c(R,G,B))


db_color <- color_seed %>% select(Accesions, Cluster,Seeds, R, G, B) %>%
  pivot_wider(names_from = c(Cluster), 
              values_from = c(R,G,B))


db_color


color_flower_peanut %>% select(Accesion, Cluster,Flower, R, G, B) %>%
  tidyr::pivot_wider(names_from = c(Cluster)
                     , values_from = c(R,G,B))


tibble(db_color)



tibble(db_color1)
getwd()

write.csv(db_color, file = "color_seed_beanR.csv")

db_color_seed <- color_seed_bean %>% select(Accesions, Cluster,Seed, R, G, B) %>% 
  pivot_wider(names_from = c(Cluster), 
              values_from = c(R,G,B))

names(color_pod)
dim(color_pod)


db_color_pod <- color_pod %>% select(Accessions, Cluster,Pod, R, G, B) %>% 
  pivot_wider(names_from = Cluster, 
              values_from = c(R,G,B))



db_color_flower <- color_flower_peanut %>% select(Accessions, Cluster,Flower, R, G, B) %>% 
  pivot_wider(names_from = Cluster, 
              values_from = c(R,G,B))

DT::datatable(db_color_seed)

db_color$Accesions <- as.factor(db_color$Accesions)

levels(db_color$Accesions)

tibble(db_color_seed)


ruta4 <- "D:/OneDrive - CGIAR/Conejo_Ing/Documentacion2020B/Caracterizacion/imagenesR/RGBph/mani"
setwd(paste(ruta4))

getwd()


write.csv(db_color, file = "color_florR.csv")

write.csv(db_color_pod, file = "color_podR.csv")

write.csv(db_color_seed, file = "color_seedR.csv")


