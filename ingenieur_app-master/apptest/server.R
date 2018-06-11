# server.R

# server function
function(input, output, session) {
  
  # Initialisation de la map avec les données de la premiere année
  output$map <- renderLeaflet({
    basemap %>%
      addMinicharts(lng = france_region_polygone@data$long,
                    lat = france_region_polygone@data$lat,
                    chartdata = france_region_polygone@data[,c("f_a","f_b","f_c")],
                    type = "pie",
                    colorPalette = c("red","green","blue"),
                    width = 40,
                    height = 40,
                    layerId = paste0("l",seq(1,nrow(france_region_polygone@data))))

  })
  
  
  

  
  reactive_promotion_df<- reactive({
    reactive_promotion_df = bigds[bigds$annee == input$year,]
    reactive_promotion_df
  })
  
  # Mise à jour de la map - cas 1
  observe({

    leafletProxy("map", session) %>%
      updateMinicharts(layerId = paste0("l",seq(1,nrow(reactive_promotion_df()))),
                       chartdata = reactive_promotion_df()[,c("f_a","f_b","f_c")],
                       type = "pie",
                       colorPalette = c("red","green","blue"),
                       width = 40,
                       height = 40,
                       transitionTime = 5000)

  })

  
  
  
  
  # Mise à jour de la map - cas 2
  observeEvent(input$map_shape_click,{
    
    click_action = input$map_shape_click
      
      popup_data = reactive_promotion_df()[reactive_promotion_df()$OBJECTID==input$map_shape_click$id,
                                           c("NAME_1","f_a","f_b","f_c")]
      popup_text = df_to_html(popup_data)
      
      
      leafletProxy("map", session) %>%
        updateMinicharts(layerId = paste0("l",seq(1,nrow(reactive_promotion_df()))),
                         chartdata = reactive_promotion_df()[,c("f_a","f_b","f_c")],
                         type = "pie",
                         colorPalette = c("red","green","blue"),
                         width = 40,
                         height = 40,
                         transitionTime = 5000) %>%
        addPopups(lng = click_action$lng,
                  lat = click_action$lat,
                  popup = popup_text,
                  layerId = "popupid",
                  options = popupOptions(closeOnClick = TRUE))
      
      
      
    
  })
  

  
  
  output$click_value <- renderPrint({ input$map_shape_click })
  
  
}
