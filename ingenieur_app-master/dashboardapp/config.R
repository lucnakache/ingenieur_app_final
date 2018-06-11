# config

# load relevant rds
filename = "clean_bigds.rds"
bigds = readRDS(file = filename)
bigds$annee = as.numeric(bigds$annee)
filename = "clean_all_polygone_simp.rds"
france_region_polygone = readRDS(file = filename)
filename = "clean_ingenieur_df.rds"
ingenieur_df = readRDS(file = filename)
ingenieur_df$year = as.numeric(levels(ingenieur_df$year))[ingenieur_df$year]


# Calcul des barycentres des shapes
centroids_df = as.data.frame(centroid(france_region_polygone))
names(centroids_df) = c("long","lat")
france_region_polygone@data = cbind(france_region_polygone@data,centroids_df)


# la basemap de l'onglet cartographie
basemap = leaflet(france_region_polygone) %>% 
  addTiles() %>%
  addPolygons(
    color = "green",
    weight = 2,
    smoothFactor = 2,
    opacity = 0.8,
    fillOpacity = 0.2,
    layerId = ~OBJECTID,
    highlightOptions = highlightOptions(color = "white",
                                        weight = 2,
                                        bringToFront = TRUE))

# Toutes les annees
config_year_vector = sort(unique(bigds$annee))

# Tailles des promo
config_taille_promo_df = as.data.frame(table(ingenieur_df$year))
names(config_taille_promo_df) = c("year","taille")

# Le nombre de pays/regions
bigds$nb_eleve = apply(bigds[,names(bigds)[str_detect(string = names(bigds),pattern = "fdomaine_")]],1,sum)
bigds_y = bigds[bigds$nb_eleve>0,]
config_taille_pays = sapply(config_year_vector,function(annee){
  length(bigds_y$NAME_1[bigds_y$annee==annee])
})



# Les variables d'étude
config_variable = c("domaine")

# les modalites
config_modalite = lapply(config_variable,function(x){
  idx = str_detect(string = names(bigds),pattern = paste0("f",x))
  resultat = names(bigds)[idx]
  names(resultat) = gsub(pattern =paste0("f",x,"_") ,replacement = "",x = resultat,fixed = TRUE)
  return(resultat)
})
names(config_modalite) = config_variable



# Toutes les palettes
palette_brewer = row.names(brewer.pal.info[brewer.pal.info$category == "qual",])
palette_viridis = c("viridis", "magma", "inferno", "plasma")
config_palette = c(palette_brewer,palette_viridis)

# fonction qui convertir un dataframe popupdata en arbitrary html
# df_to_html = function(popup_data){
#   f2 = as.vector(t(popup_data[1,]))
#   f1 = colnames(popup_data)
#   res = paste0("<br/>",paste(paste(f1,f2,sep = ":"),collapse = "<br/>"))
#   return (res)
# }


df_to_html = function(popup_data,
                      palette_temporaire,
                      REGION_NAME,
                      ANNEE_NAME){
  f2 = as.vector(t(popup_data[1,]))
  f1 = colnames(popup_data)
  f1 = gsub(pattern =paste0("f","domaine","_") ,replacement ="",x = f1,fixed = TRUE)
  f1 = paste0("<B>",f1,"</B>")
  f1 = paste0("<FONT COLOR='",palette_temporaire,"'>",f1,"</FONT>")
  res = paste0("<br/>",paste(paste(f1,f2,sep = " : "),collapse = "<br/>"))
  to_add = paste0("<br/>",REGION_NAME,", ",ANNEE_NAME)
  res = paste0(to_add,res)
  return (res)
}



# fonction qui valule une serie pr plottert du hc
fun_compute_serie_for_plot = function(temp_ds,palette_temp){
  series=list()
  variable_name = names(temp_ds)
  for (i in seq(1:ncol(temp_ds))){
    series[[i]]=list(name=variable_name[i],
                     color=palette_temp[i],
                     data=temp_ds[,i])
  }
  return(series)
}

# fonction qui compute une serie pour le plot 2 de l'onglet 3 hc
fun_compute_serie_for_plot_2 = function(palette_temp,keep_col,inp_var){
  
  temp_ds = bigds[,c(keep_col,"annee")]
  
  temp_ds = temp_ds %>%
    group_by(annee) %>%
    summarise_all(funs(sum))
  temp_ds$annee = NULL
  
  
  series=list()
  variable_name = names(temp_ds)
  for (i in seq(1:ncol(temp_ds))){
    series[[i]]=list(name=gsub(pattern = paste0("f",inp_var,"_"),
                               replacement = "",
                               fixed = TRUE,
                               x = variable_name[i]),
                     color=palette_temp[i],
                     data=pull(temp_ds,i))
  }
  return(series)
}


stat_var = names(bigds)[str_detect(string = names(bigds),pattern = "fdomaine_")]


# fun pour aller a lannee la plus proche
fun_year_nearest = function(input_year){
  config_year_vector[which.min(abs(config_year_vector - input_year))]
}


