# test de leaflet mini charts
# bigds dataset est issu de build_df.R


# install.packages("leaflet.minicharts")
library(leaflet.minicharts)
library(leaflet)
library(geosphere)



# Ajout des centroids
dataset_promotion = bigds[bigds$annee == 1801,]
centroids_df = as.data.frame(centroid(dataset_promotion))
names(centroids_df) = c("long","lat")
dataset_promotion@data = cbind(dataset_promotion@data,centroids_df)


xx = dataset_promotion@data


# Essai simple sans dimension temporelle
a_map = leaflet(dataset_promotion) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
              color = "grey",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 0.5,
              fillOpacity =0,
              label = ~paste0(NAME_1,":",f_b),
              highlightOptions = highlightOptions(color = "red",
                                                  weight = 2,
                                                  bringToFront = TRUE)) %>%
  addMinicharts(lng = dataset_promotion@data$long,
                lat = dataset_promotion@data$lat,
                chartdata = dataset_promotion@data[,c("f_a","f_b","f_c")],
                type = "pie",
                colorPalette = c("red","green","blue"),
                width = 40,
                height = 40,
                layerId = paste0("l",seq(1,nrow(dataset_promotion@data))))


a_map



dataset_promotion_annee_2 = bigds[bigds$annee == 1802,]

a_map %>%
  updateMinicharts(layerId = paste0("l",seq(1,nrow(dataset_promotion@data))),
                   chartdata = dataset_promotion_annee_2@data[,c("f_a","f_b","f_c")],
                   type = "pie",
                   colorPalette = c("red","green","blue"),
                   width = 40,
                   height = 40,
                   transitionTime = 5000)
