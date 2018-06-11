# global

# load relevant rds
foldername = "C:/Serge/lucurgent/apptest/"
filename = "bigds.rds"
pathfile = paste0(foldername,filename)
bigds = readRDS(file = pathfile)
filename = "france_region_polygone.rds"
pathfile = paste0(foldername,filename)
france_region_polygone = readRDS(file = pathfile)

# Calcul des barycentres des shapes
centroids_df = as.data.frame(centroid(france_region_polygone))
names(centroids_df) = c("long","lat")
france_region_polygone@data = cbind(france_region_polygone@data,centroids_df)
# france_region_polygone@data le premier dataframe de la premiere annee


# la fonction
df_to_html = function(popup_data){
  f2 = as.vector(t(popup_data[1,]))
  f1 = colnames(popup_data)
  res = paste0("<br/>",paste(paste(f1,f2,sep = ":"),collapse = "<br/>"))
  return (res)
}


# basemap
basemap = leaflet(france_region_polygone) %>%
  addPolygons(
    color = "white",
    weight = 2,
    smoothFactor = 0.5,
    opacity = 0.2,
    fillOpacity =0.5,
    layerId = ~OBJECTID,
    highlightOptions = highlightOptions(color = "red",
                                        weight = 2,
                                        bringToFront = TRUE))


