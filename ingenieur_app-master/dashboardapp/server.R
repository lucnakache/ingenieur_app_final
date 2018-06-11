shinyServer(function(input, output, session) {

  
  # Mise à jour du champ des possibles de modalite
  observeEvent(input$o1_variable,{
    a = config_modalite[[input$o1_variable]]
    updateSelectInput(session,
                      "o1_modalite",
                      choices = a,
                      selected = a[1:3])
  })
  
  
  
  # Définition de la value box annee
  output$vb_annee <- renderValueBox({
    vals <- fun_year_nearest(input$o1_year)
    valueBox(
      value = vals,
      subtitle = "Année",
      icon = icon("hourglass",lib = "glyphicon"),color="navy"
    )
  })
  
  # Définition de la value box nb eleves
  output$vb_nb_eleve <- renderValueBox({
    vals <- config_taille_promo_df[config_taille_promo_df$year==fun_year_nearest(input$o1_year),"taille"]
    valueBox(
      value = vals,
      subtitle = "Nombre d'Elèves",
      icon = icon("hourglass",lib = "glyphicon"),color="navy"
    )
  })
  
  
  # Définition de la value box nb pays
  output$vb_nb_pays <- renderValueBox({
    vals <- config_taille_pays[config_taille_promo_df$year==fun_year_nearest(input$o1_year)]
    valueBox(
      value = vals,
      subtitle = "Nombre de pays",
      icon = icon("hourglass",lib = "glyphicon"),color="navy"
    )
  })
  
  
  
  
  # Création des couleurs
  ma_palette <- reactive({
    pal = colorFactor(palette = input$o1_palette,domain = NULL)
    res = pal(x = seq(1,length(input$o1_modalite)))
    return(res)
  })
  
  # Création de la carte
  
  observeEvent(input$o1_button_run_map,{
    
    
    
    output$map <- renderLeaflet({
      basemap %>%
        addMinicharts(lng = france_region_polygone@data$long,
                      lat = france_region_polygone@data$lat,
                      chartdata = bigds[bigds$annee==config_year_vector[1],input$o1_modalite],
                      type = "pie",
                      colorPalette =ma_palette(),
                      width = 50 * input$o1_zoom,
                      height = 50 * input$o1_zoom,
                      # width = log(as.numeric(apply(bigds[bigds$annee==config_year_vector[1],stat_var],1,sum))) * input$o1_zoom,
                      # height = log(as.numeric(apply(bigds[bigds$annee==config_year_vector[1],stat_var],1,sum))) * input$o1_zoom,
                      maxValues = NULL,
                      layerId = paste0("l",seq(1,nrow(france_region_polygone@data))))
      
    })
  })
  
  # 
  # output$map <- renderLeaflet({
  #   basemap %>%
  #     addMinicharts(lng = france_region_polygone@data$long,
  #                   lat = france_region_polygone@data$lat,
  #                   chartdata = france_region_polygone@data[,input$o1_modalite],
  #                   type = "pie",
  #                   colorPalette =ma_palette(),
  #                   width = log(as.numeric(apply(france_region_polygone@data[,c("fdomaine_mecanique",
  #                                                                               "fdomaine_ponts_chaussee",
  #                                                                               "fdomaine_travaux_public" )],1,sum))) * input$o1_zoom,
  #                   height = log(as.numeric(apply(france_region_polygone@data[,c("fdomaine_mecanique",
  #                                                                                "fdomaine_ponts_chaussee",
  #                                                                                "fdomaine_travaux_public" )],1,sum))) * input$o1_zoom,
  #                   maxValues = NULL,
  #                   layerId = paste0("l",seq(1,nrow(france_region_polygone@data))))
  # 
  # })
  
  
  reactive_promotion_df<- reactive({
    res = bigds[bigds$annee == as.numeric(fun_year_nearest(input$o1_year)),]
    res
  })
  

  

  output$checktest <- renderPrint({ forhc() })
  
  

  observeEvent(fun_year_nearest(input$o1_year),{

    leafletProxy("map", session) %>%
      updateMinicharts(layerId = paste0("l",seq(1,nrow(reactive_promotion_df()))),
                       chartdata = reactive_promotion_df()[,input$o1_modalite],
                       colorPalette =ma_palette(),
                       showLabels = input$o1_show_val
                       )

  })
  
  # Rezactive value qui compute le texte de la popup
  forpopup <- reactive({
    popup_data = reactive_promotion_df()[reactive_promotion_df()$OBJECTID==input$map_shape_click$id,input$o1_modalite]
    REGION_NAME = reactive_promotion_df()$NAME_1[reactive_promotion_df()$OBJECTID==input$map_shape_click$id]
    popup_text = df_to_html(popup_data,
                            ma_palette(),
                            REGION_NAME,
                            fun_year_nearest(input$o1_year))
    list(popup_data,REGION_NAME,ma_palette(),popup_text)
  })
  
  # Reactive value qui compute la series qu on vaplotter dans hc
  forhc <-reactive({
    temp_ds = bigds[bigds$OBJECTID == input$map_shape_click$id, input$o1_modalite]
    series = fun_compute_serie_for_plot(temp_ds,ma_palette())
    series
  })
  
  
  observeEvent(input$map_shape_click,{
    
    click_action = input$map_shape_click
    popup_data = reactive_promotion_df()[reactive_promotion_df()$OBJECTID==input$map_shape_click$id & reactive_promotion_df()$annee==fun_year_nearest(input$o1_year),input$o1_modalite]
    popup_text = forpopup()[[4]]
    
    leafletProxy("map", session) %>%
      updateMinicharts(layerId = paste0("l",seq(1,nrow(reactive_promotion_df()))),
                       chartdata = reactive_promotion_df()[,input$o1_modalite],
                       type = "pie",
                       colorPalette =ma_palette(),
                       showLabels = input$o1_show_val) %>%
      addPopups(lng = click_action$lng,
                lat = click_action$lat,
                popup = popup_text,
                layerId = "popupid",
                options = popupOptions(closeOnClick = TRUE))
    
  })
  
  
  
  output$o1_plot_ts=renderHighchart({
    highchart() %>%
      hc_add_series_list(forhc())%>%
      hc_xAxis(categories=config_year_vector) %>%
      hc_chart(backgroundColor = "#b2b2b2") 
    })

  
  
  
  # reactive value qui construitl a table qu on affichera
  fordico <-reactive({
    promo = input$o2_annee_promotion
    dom = input$o2_domaine
    
    if (is.null(promo)) {promo = ingenieur_df$year}
    if (is.null(dom)) {dom = ingenieur_df$domaine}
    names(ingenieur_df)
    keep_col = c("year","nom","domaine","city","NAME_1","lon","lat")
    temp_ds = ingenieur_df[ingenieur_df$year %in% promo & ingenieur_df$domaine %in% dom,keep_col]

   
  })
  
  

  
  
  # afficge la table
  output$dico_inge <- renderDataTable(fordico())
  
  
  # titre de plot ts de longlet 1
  output$o1_title_plot_ts <- renderText({
    paste(forpopup()[[2]])
  })


  

  
  
  
  
  
  
  # variable debug
  output$checktest_o2 <- renderPrint({ forhc2() })
  
  # reactive value qui compute les series pour le graph
  # de l'onglet 3
  forhc2 <-reactive({
    series = fun_compute_serie_for_plot_2(ma_palette(),
                                          input$o1_modalite,
                                          input$o1_variable)
    series
  })
  
  # Graphique de l'oonglet 3
  output$o3_plot_ts=renderHighchart({
    highchart() %>%
      hc_add_series_list(forhc2())%>%
      hc_xAxis(categories=config_year_vector) %>%
      hc_chart(backgroundColor = "#b2b2b2") 
  })
  
  
  # titre de plot ts de longlet 3
  output$o3_title_plot_ts <- renderText({
    paste(input$o1_variable)
  })
  
  
  
  
})