# main
rm(list=ls())
foldername="C:/Users/Bar Yokhai/Desktop/projets/Blog/luc_ingenieur_app/"

# Chargement des packages et fonctions
source(paste0(foldername,"packages.R"))
source(paste0(foldername,"fun.R"))

# Lancement de la simulation avec les parameters par defaut
simulation_ingenieur = fun_build_simulation_df()

