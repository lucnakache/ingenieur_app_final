fun_build_simulation_df = function(foldername = "C:/Users/Bar Yokhai/Desktop/projets/Blog/luc_ingenieur_app/",
                                   filename_2 = "france_region_polygone.rds",
                                   filename_1 = "bigds.rds",
                                   filename_3 = "ingenieur_data_df.rds",
                                   annee_n = 20,
                                   ingenieur_n = floor(runif(n = annee_n,
                                                             min = 300,
                                                             max = 3000)),
                                   domaine = c("chemin_fer",
                                               "ponts_chaussee",
                                               "industrie_chimique",
                                               "mecanique",
                                               "travaux_public",
                                               "construction"),
                                   recompense = c("medal_gold",
                                                  "medal_silver",
                                                  "medal_bronze",
                                                  "price_any"),
                                   year_start = 1800) {
  year_vector = seq(year_start,year_start + annee_n - 1)
  
  # LISTE IDS INGENIEUR PAR PROMOTION
  id_ingenieur = lapply(seq_along(year_vector),
                        function(idx_promotion){
                          paste0(year_vector[idx_promotion],
                                 "-",
                                 seq(1:ingenieur_n[idx_promotion])
                                 )
                        })
  
  

  
  
  
  # SPATIALPOLYGONEDATAFRAME CONTENANT LES REGIONS DE FRANCE
  france_regions<-getData('GADM', country='FRA', level=1)
  
  # LES LIEUX DE TRAVAIL DES INGENIEURS
  # LISTE CONTENANT DES SPATIALPOINTSDATAFRAME
  
  
  # promotion = id_ingenieur[[10]]
  stats_by_region_by_promotion = lapply(id_ingenieur,function(promotion){
    
    # init.
    france = france_regions
    
    # NOMBRE D INGENIEURS
    taille_de_la_promo = length(promotion)
    
    # LIEUX DE TRAVAIL DES INGENIEURS
    random_spatial_points <- spsample(france,
                                      n=taille_de_la_promo,
                                      type="random")
    # REGION DES INGENIEURS
    ingenieur_region_df = sp::over(random_spatial_points,france)
    taille_promo_par_domaine = as.vector(round(MCMCpack::rdirichlet(1, rep(1,length(domaine))) * taille_de_la_promo))
    q_to_add = taille_de_la_promo - sum(taille_promo_par_domaine)
    taille_promo_par_domaine[which.max(taille_promo_par_domaine)] = taille_promo_par_domaine[which.max(taille_promo_par_domaine)] + q_to_add
    random_domaine_deplied = sample(rep(domaine,times = taille_promo_par_domaine))
    ingenieur_region_df$domaine = random_domaine_deplied
    
    # LES RECOMPENSES DES INGENIEURS
    nombre_recompense = ceiling((runif(n = length(recompense),min =0.002 ,max =0.015 )) * taille_de_la_promo)
    ingenieur_region_df$recompense = rep(c(recompense,"aucune"),c(nombre_recompense,taille_de_la_promo-sum(nombre_recompense)))
    ingenieur_region_df$id_eleve = promotion
    
    # merger les informations des ingenieurs et leur biographie
    biography = unlist(lapply(promotion,function(x){
      b = paste0(sample(x = letters,size = runif(1,30,300),replace = TRUE),collapse = "")
    }))
    biographie_df = data.frame("id" = promotion,
                               "biographie" = biography)
    ingenieur_data = cbind (biographie_df,ingenieur_region_df)
    
    # with domaine by group
    dd = ingenieur_region_df %>% 
      group_by(domaine,NAME_1) %>% 
      summarise (fdomaine = n())
    dd = as.data.frame((dd))
    dd_melt = melt(dd)
    dd_cast <- dcast(dd_melt, NAME_1 ~ variable + domaine)
    dd_cast[is.na(dd_cast)] <- 0
    names(dd_cast)[1] = "polygone"
    
    
    # with domaine by group
    dd_1 = ingenieur_region_df %>% 
      group_by(recompense,NAME_1) %>% 
      summarise (frecompense = n())
    dd_1 = as.data.frame((dd_1))
    dd_melt_1 = melt(dd_1)
    dd_cast_1 <- dcast(dd_melt_1, NAME_1 ~ variable + recompense)
    dd_cast_1[is.na(dd_cast_1)] <- 0
    names(dd_cast_1)[1] = "polygone"
    
    
    
    
    polygon_temp_df = merge(x = france@data,
                            y = dd_cast,
                            by.x = "NAME_1",
                            by.y = "polygone",
                            all.x = TRUE)
    
    
    polygon_temp_df = merge(x = polygon_temp_df,
                            y = dd_cast_1,
                            by.x = "NAME_1",
                            by.y = "polygone",
                            all.x = TRUE)
    
    
    polygon_temp_df = polygon_temp_df[,c(names(france@data),
                                         names(dd_cast)[2:ncol(dd_cast)],
                                         names(dd_cast_1)[2:ncol(dd_cast_1)])]
    polygon_temp_df = polygon_temp_df[order(polygon_temp_df$ID_1),]
    idx = is.na(polygon_temp_df)
    polygon_temp_df[idx] = NA
    polygon_temp_df = polygon_temp_df[order(polygon_temp_df$ID_1),]
    polygon_temp_df$annee = names(promotion)
    france@data = polygon_temp_df
    
    return(list(france,random_spatial_points,france@data,ingenieur_data))
    
  })
  
  names(stats_by_region_by_promotion) = year_vector
  
  
  
  # le dataset ingenieur
  ingenieur_data_df = lapply(stats_by_region_by_promotion,"[[",4)
  ingenieur_data_df = do.call(rbind,ingenieur_data_df)
  split_year_id_list = strsplit(x = as.character(ingenieur_data_df$id),split = "-",fixed = TRUE)
  col_to_add = as.data.frame(do.call(rbind,split_year_id_list))
  names(col_to_add) = c("year","promotion_etudiant_id")
  ingenieur_data_df = cbind(ingenieur_data_df,col_to_add)
  
  
  
  # lebigdataset
  stats_by_region_by_promotion_polydf = lapply(stats_by_region_by_promotion,"[[",3)
  stats_by_region_by_promotion_polydf = lapply(seq_along(stats_by_region_by_promotion_polydf),
                                               function(idx_promotion){
                                                 df = stats_by_region_by_promotion_polydf[[idx_promotion]]
                                                 df$annee = year_vector[idx_promotion]
                                                 return(df)
                                               }
  )
  
  # bigds concat?nation de tout le d?lire
  bigds = bind_rows(stats_by_region_by_promotion_polydf)
  
  # sauvegarde d'un polynome file
  france_region_polygone = stats_by_region_by_promotion[[1]][[1]]
  
  
  
  
  
  # Sauvegarde de bigds
  pathfile = paste0(foldername,filename_1)
  # Sauvegarde  
  saveRDS(object = bigds,file = pathfile)
  # Sauvegarde de region france shape
  pathfile = paste0(foldername,filename_2)
  # Sauvegarde  
  saveRDS(object = france_region_polygone,file = pathfile)
  # Sauvegarde de region france shape
  pathfile = paste0(foldername,filename_3)
  # Sauvegarde  
  saveRDS(object = ingenieur_data_df,file = pathfile)
  
  
  return(list(bigds,france_region_polygone,ingenieur_data_df))
}
  

