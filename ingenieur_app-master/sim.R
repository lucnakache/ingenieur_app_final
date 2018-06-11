rm(list=ls())
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# IMPORTATION DES LIBRAIRIES
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
library(raster)
library(sp)
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-




# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *- 
#              POUR DOWNNLOAD DES SHAPES
# L'output est un fichier dit Spatial polygon dataframe
# avec level t'és différentes découpes
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
france<-getData('GADM', country='FRA', level=1)
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-




# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *- 
# Simulation des données (OPTION 1, classic mode)
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# Rectangle d'où tirer les positions
tl = c(40,-3)
br = c(50,3)

# Drawing des positions
position_n = 1000
position_lat = runif(n = position_n,
                     min = min(c(tl[1],br[1])),
                     max = max(c(tl[1],br[1])))
position_long = runif(n = position_n,
                      min = min(c(tl[2],br[2])),
                      max = max(c(tl[2],br[2])))
position_df = data.frame("x" = position_long,
                         "y" = position_lat)
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# CONVERTIR DATAFRAME/MATRIX EN SPATIALPOINTSDATAFRAME
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# Step 1 / matrix to spatial points
temp = SpatialPoints(coords = position_df)
# Step 2 / spatial points to spatial points dataframe
temp = SpatialPointsDataFrame(coords = temp,
                              data = data.frame(id=1:length(temp)))
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-




position_n = 100
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
#     TIRER DES POINTS A L INTERIEUR D UN SHAPEFILE
# l'output est un fichier dit spatial points
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
p <- spsample(france, n=position_n, type="random")
# CONVERTIR DES SPATIAL POINTS EN SPATIAL POINTS DATAFRAME
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
p <- SpatialPointsDataFrame(p, data.frame(id=1:position_n))
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-







# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# COMPTABILISER NOMBRE DE POINTS A LINTERIEUR DUN SHAPEFILE
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
proj4string(temp) <- france@proj4string
proj4string(temp) == proj4string(france)
res = sp::over(temp,france)

# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# ENRICHIR UN SHAPEFILE AVEC UNE NOUVELLE COLONNE
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# france$population = as.vector(table(res$NAME_1))
freq_polynome_df = data.frame(table(res$NAME_1))
names(freq_polynome_df) = c("polygone","occurence")
polygon_temp_df = merge(x = france@data,
                    y = freq_polynome_df,
                    by.x = "NAME_1",
                    by.y = "polygone",
                    all.x = TRUE)
polygon_temp_df = polygon_temp_df[,c(names(france@data),"occurence")]
polygon_temp_df = polygon_temp_df[order(polygon_temp_df$ID_1),]
idx = is.na(polygon_temp_df)
polygon_temp_df[idx] = NA
polygon_temp_df = polygon_temp_df[order(polygon_temp_df$ID_1),]
france@data = polygon_temp_df
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-

# pure = france
# sale = france
# sale@data = polygon_temp_df
# sale@data$TESTVAR = 0
# polygon_propre_df = france@data
# lapply(pure@data,class)
# lapply(sale@data,class)

# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-
# CONSTRUIRE UNE CHLOROPETH MAP, INPUT UN SHAPEFILE
# tu peux ajouter ou enlever le addtiles ou leprovidertiles
# selon que tu veux les informations leaflet ou juste tes shapes
# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-



# EXEMPLE DE palette à créer, colornumeric est une fonction de leaflet r package
pal_2 <- colorNumeric("viridis",
                    NULL)


france$occurence
# Construction de la map
library(leaflet)
leaflet(france) %>%
  addPolygons(color = "grey",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 1,
              fillColor = ~pal_2(occurence),
              label = ~NAME_1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = TRUE))

# * - *- * - *- * - *- * - *- * - *- * - *- * - *- * - *-


