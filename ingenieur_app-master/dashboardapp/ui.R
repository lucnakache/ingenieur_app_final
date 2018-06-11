source("packages.R")
source("config.R")

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Engineer"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    sidebarMenu(
      menuItem("CARTO", tabName = "onglet_cartographie", icon = icon("plane")),
      menuItem("BIO", tabName = "onglet_biographie", icon = icon("plane")),
      menuItem("STATS", tabName = "onglet_stats", icon = icon("plane")),
      
      # Mes controls
      sliderInput(inputId ="o1_year",
                  label = "Année",
                  min = config_year_vector[1],
                  max = config_year_vector[length(config_year_vector)],
                  value = config_year_vector[1],
                  step = 1,
                  animate = animationOptions(interval = 1000,
                                             loop = FALSE)),
      # La input variable
      selectInput(inputId="o1_variable",
                  label = "Variable à étudier",
                  choices=config_variable,
                  multiple = FALSE,
                  selected = config_variable[1],
                  width = NULL),
      
      # La input modalite
      selectInput(inputId="o1_modalite",
                  label = "Modalités à afficher",
                  choices=NULL,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = config_modalite[[config_variable[1]]][1],
                  width = NULL),
      
      # Le ptit boutton qui va bien
      actionButton("o1_button_run_map",
                   "Construire la map",
                   icon=icon("chevron-circle-down")),
      
      # La ptite checkbox input
      checkboxInput(inputId = "o1_show_val" ,
                    label = "Affichez les valeurs",
                    value = FALSE,
                    width = NULL)
    ) # fin sidebar menu
  ),
  dashboardBody(
    tags$style(HTML("
                    
                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#195EA0
                    }
                    
                    .box.box-solid.box-primary{
                    border-bottom-color:#195EA0;
                    border-left-color:#195EA0;
                    border-right-color:#195EA0;
                    border-top-color:#195EA0;
                    }
                    
                    ")),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabItems(
      
      
      
      
      
      # 1er onglet cartographie * * * * * * * * * * 
      tabItem(
        tabName = "onglet_cartographie",
        
        # Les value Box
        fluidRow(
          valueBoxOutput("vb_annee",width = 4),
          valueBoxOutput("vb_nb_eleve",width = 4),
          valueBoxOutput("vb_nb_pays",width = 4)
        ),
        
        # La map et le plot
        fluidRow(
          
          # La map
          box(title = "Map Temporelle",
              width = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              status = "primary",
              leafletOutput("map")
              ),
          box(title = textOutput("o1_title_plot_ts"),
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              collapsible = TRUE,
              collapsed = FALSE,
              highchartOutput("o1_plot_ts")
              )
        ),
        
        # Les controls
        fluidRow(
          box(title = "Control",
              solidHeader = TRUE,
              status = "primary",
              width = 3,
              collapsible = TRUE,
              collapsed = FALSE,
              # la input zoom
              sliderInput(inputId ="o1_zoom",
                          label = "Zoom pie",
                          min = 1,
                          max = 50,
                          value = 5,
                          step = 1),
              
              # La input palette
              selectInput(inputId="o1_palette",
                          label = "Palette",
                          choices=config_palette,
                          multiple = FALSE,
                          selected = "viridis",
                          width = NULL)
              )
          )
        
        ),
      
      
      
      
      
      # 2e onglet biographie
      tabItem(tabName = "onglet_biographie",
              
              # Control + table des ingenieurs
              fluidRow(
                box(title = "Filtres de recherche",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 3,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    selectInput(inputId="o2_annee_promotion",
                                label = "Promotion",
                                choices=config_year_vector,
                                multiple = TRUE,
                                selectize = TRUE,
                                selected = config_year_vector[1],
                                width = NULL),
                    selectInput(inputId="o2_domaine",
                                label = "Domaine",
                                choices=names(config_modalite[["domaine"]]),
                                multiple = TRUE,
                                selectize = TRUE,
                                selected = names(config_modalite[["domaine"]])[1],
                                width = NULL)
                    
                ),
                box(title = "Table des élèves",
                    width = 9,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    dataTableOutput('dico_inge')
                    )
              )
      ),
      
      # 3e onglet statistiques plus gloables
      tabItem(tabName = "onglet_stats",
              
              # Control + table des ingenieurs
              fluidRow(
                box(title = textOutput("o3_title_plot_ts"),
                    width = 12,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = FALSE,
                    highchartOutput("o3_plot_ts")
                    )
              )
      )
      
      
      
      
      
      
      
      
      
      
      
    ) # Fin des items
    
    
    
    ) # Fin de dashboardbody
  
  
  )# Fin de dashboard Page


