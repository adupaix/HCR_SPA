#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2025-01-30
#'@email : amael.dupaix@mnhn.fr
#'#***************************************************************************
#'@description : Script gérant la visualisations
#'               
#'#***************************************************************************

ui <- fluidPage(
  
  # Thème ----
  # Couleur de fond, police, ...
  # Exécuter bslib::bootswatch_themes() pour connaître les différents thèmes
  theme = shinythemes::shinytheme("lumen"),
  tags$head(tags$title("Réponse HCR"),
            tags$link(rel = 'icon', type = "image/png", href = "logoMNHN.png")),
  
  # Panneau avec le titre de l'application ----
  
  # La police du titre est automatiquement grande
  titlePanel(fluidRow(
    column(11,"Réponse de différentes paramétrisation de l'HCR à des variations d'abondance"),
    column(1,img(height = 60, src = 'logoMNHN.png'))
    
  )),
  
  
  # Groupe d'onglets ----
  
  # navlistPanel(
  tabsetPanel(
    # Au lieu d'avoir des onglets, il est possible d'avoir un menu de sélection vertical sur la gauche 
    # avec navlistPanel à la place de tabsetPanel 
    
    # Si navlistPanel, peut définir la largeur des colonnes (total = 12)
    # widths = c(1,11),
    
    ## Premier onglet - captures langoustes ----
    tabPanel("Paramètres",
             sidebarLayout(
               
               # Panneau barre latérale ----
               
               sidebarPanel(width = 3,
                 fluidRow(
                   column(6,
                          radioButtons(inputId = "rbScenar",
                                       label = h3("Scénario"),
                                       choices = scenarios
                          )),
                   column(6,
                          radioButtons(inputId = "rbIle",
                                       label = h3("Ile"),
                                       choices = Iles
                          ))
                 ),
                 h2("HCR"),
                 fluidRow(
                   column(6,
                          radioButtons(inputId = "rbLimLow",
                                       label = h3("Palier inférieur"),
                                       choices = limits.low
                          )),
                   column(6,
                          radioButtons(inputId = "rbAvePeriod",
                                       label = h3("Période moyenne I"),
                                       choices = averaged.period
                          ))
                 )
                 ),
               
               # Panneau principal ----
               
               mainPanel(
                 
                 tabBox(
                   width = NULL,
                   # tabPanel("Captures",
                   #          verbatimTextOutput(outputId = "value")),
                   tabPanel("Indicateur",
                            plotOutput(outputId = "Indicateur",
                                       height = "calc(90vh - 150px)")),
                   tabPanel("Captures",
                            plotOutput(outputId = "Captures",
                                       height = "calc(90vh - 150px)")),
                   tabPanel("Réponse de l'HCR",
                            plotOutput(outputId = "mainPlot",
                                       height = "calc(90vh - 150px)"))
                 )
               )
             )
             
             
    )
  )
  
)

