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
                            h3("Abondance"),
                 fluidRow(
                   column(4,
                          radioButtons(inputId = "rbScenar",
                                       label = h4("Scénario"),
                                       choices = scenarios,
                                       selected = 2
                          )),
                   column(4,
                          radioButtons(inputId = "rbIle",
                                       label = h4("Ile"),
                                       choices = Iles
                          )),
                   column(4,
                          radioButtons(inputId = "rbZone",
                                       label = h4("Zone"),
                                       choices = Zones
                          ))
                 ),
                 h3("HCR"),
                 fluidRow(
                   column(6,
                          sliderInput(inputId = "rbAvePeriod",
                                      label = h4("Moyenne de I sur"),
                                      min = 1, max = 5, value = 4
                          )),
                   column(6,
                          radioButtons(inputId = "rbAvePeriodCalc",
                                       label = h4("Calcul de la moyenne"),
                                       choices = m.period.calc
                          ))
                 ),
                 fluidRow(
                   column(6,
                          sliderInput(inputId = "slideAppPeriod",
                                       label = h4("Périodicité application"),
                                       min = 1, max = 5, value = 1
                          )),
                   column(6,
                          sliderInput(inputId = "slideAppStart",
                                       label = h4("Première application"),
                                       min = 2025, max = 2027,
                                       value = 2025, sep = ""
                          ))
                 ),
                 fluidRow(
                   column(6,
                          radioButtons(inputId = "rbLimLow",
                                       label = h4("Palier inférieur"),
                                       choices = limits.low
                          )),
                   column(6,
                          radioButtons(inputId = "rbLimHigh",
                                       label = h4("Palier supérieur"),
                                       choices = limits.high
                          ))
                 )
                 ),
               
               # Panneau principal ----
               
               mainPanel(
                 
                 tabBox(
                   width = NULL,
                   tabPanel("Description des paramètres",
                            h5("Attention, noter qu'en l'absence d'un modèle d'évaluation de stock robuste, l'abondance est fixée à priori. Elle ne réagit pas à une modification des captures.",style='color:red'),
                            h2('Abondance'),
                            h3('Scénario'),
                            h5("Chute: réaction de l'HCR dans le cas de la chute d'abondance observée dans les années 1980"),
                            h5("Stabilisation: réaction de l'HCR dans le cas de la stabilisation de l'abondance à une valeur supérieure à la cible"),
                            h5("Oscillations: réaction de l'HCR dans le cas d'oscillations de l'abondance"),
                            h3('Ile'),
                            h5("Situation à Amsterdam ou Saint-Paul"),
                            h3('Zone'),
                            h5("Plateau péri-insulaire complet ou application par zone (côtière ou profonde)"),
                            h2('HCR'),
                            h3('Moyenne de I sur'),
                            h5("Nombre d'années sur lesquelles on calcule l'indicateur d'abondance récente"),
                            h3("Calcul de la moyenne"),
                            h5("Moyenne: moyenne classique utilisée pour calculer l'indicateur d'abondance récente"),
                            h5("Approche de précaution: exemple pour 3 ans. On calcule la moyenne sur les 3 dernières années, sur les deux dernières années et la valeur de la dernière année. On garde ensuite la valeur la plus basse comme indicateur d'abondance récente."),
                            h3("Périodicité application"),
                            h5("Toutes les combien d'années on applique l'HCR"),
                            h3("Première application"),
                            h5("Est-ce qu'on applique l'HCR pour la première fois dès l'année prochaine, dans 2 ans ou dans 3 ans. Permet de jouer sur le décallage par rapport à la chute. A un sens uniquement sur la périodicité d'application est supérieure à 1 an."),
                            h3('Palier inférieur'),
                            h5("Pourcentage du seuil de limitation d'une baisse de captures"),
                            h3('Palier supérieur'),
                            h5("Pourcentage du seuil de limitation d'une augmentation de captures")),
                   tabPanel("Abondance",
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

