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
    column(11,"Réponse de différentes paramétrisation de l'HCR à différents scénarios d'abondance",
           h5("Attention, noter qu'en l'absence d'un modèle d'évaluation de stock robuste, le scénario d'abondance est fixé et ne réagit donc pas à une modification des captures.",style='color:red')),
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
                            h2("Abondance"),
                 fluidRow(
                   column(4,
                          radioButtons(inputId = "rbScenar",
                                       label = h5("Scénario"),
                                       choices = scenarios,
                                       selected = 1
                          )),
                   column(4,
                          radioButtons(inputId = "rbIle",
                                       label = h5("Ile"),
                                       choices = Iles
                          )),
                   column(4,
                          radioButtons(inputId = "rbZone",
                                       label = h5("Zone"),
                                       choices = Zones
                          ))
                 ),
                 h2("HCR"),
                 h3("Fixation du TAC"),
                 fluidRow(
                   column(6,
                          sliderInput(inputId = "rbAvePeriod",
                                      label = h5("Moyenne de I sur"),
                                      min = 1, max = 5, value = 4
                          )),
                   column(6,
                          radioButtons(inputId = "rbAvePeriodCalc",
                                       label = h5("Calcul de la moyenne"),
                                       choices = m.period.calc
                          ))
                 ),
                 fluidRow(
                   column(6,
                          sliderInput(inputId = "sliderLimLow",
                                      label = h5("Palier inférieur"),
                                      min = -50, max = -10, value = -10,
                                      step = 5
                          )),
                   column(6,
                          sliderInput(inputId = "sliderLimHigh",
                                      label = h5("Palier supérieur"),
                                      min = 5, max = 25, value = 5,
                                      step = 5
                          ))
                 ),
                 fluidRow(
                   column(6,
                          sliderInput(inputId = "slideAppPeriod",
                                       label = h5("Application tous les"),
                                       min = 1, max = 5, value = 1
                          )),
                   column(6,
                          sliderInput(inputId = "slideAppStart",
                                       label = h5("Première application"),
                                       min = 2025, max = 2027,
                                       value = 2025, sep = ""
                          ))
                 ),
                 h3('Révision du TAC'),
                 fluidRow(
                   column(6,
                          radioButtons(inputId = "rbSortie",
                                      label = h5("Mécanisme de sortie"),
                                      choices = list('Non' = F,
                                                     'Oui' = T)
                          )),
                   column(6,
                          sliderInput(inputId = "sliderPourcentSortie",
                                      label = h5("% de baisse provoquant la sortie"),
                                      min = 0, max = 50, value = 10,
                                      step = 10
                          ))
                 )
                 ),
               
               # Panneau principal ----
               
               mainPanel(
                 
                 tabBox(
                   width = NULL,
                   tabPanel("Description des paramètres",
                            h1('1.Scénario d\'abondance'),
                            h3('Scénario'),
                            p(strong("Historique puis chute"), ": réaction de l'HCR dans le cas de la chute d'abondance observée dans les années 1980. On considère la série historique des CPUE et captures comme base."),
                            p(strong("Historique puis stabilisation"),": réaction de l'HCR dans le cas de la stabilisation de l'abondance à une valeur supérieure à la cible. On considère la série historique des CPUE et captures comme base."),
                            p(strong("Constante puis chute"),": réaction de l'HCR dans le cas d'une chute d'abondance rapide. On part d'un équilibre au niveau de la valeur cible et fait chuter l'abondance de 15 % par an. Noter que dans le cas de ce scénario, les choix d'île et de zone n'ont pas d'influence."),
                            # p(strong("Oscillations"),": réaction de l'HCR dans le cas d'oscillations de l'abondance"),
                            h3('Ile'),
                            p("Situation à Amsterdam ou Saint-Paul"),
                            h3('Zone'),
                            p("Plateau péri-insulaire complet ou application par zone (côtière ou profonde)"),
                            h1('2.HCR'),
                            h2('2.1.Fixation du TAC'),
                            h3(p(tags$u("(i) Valeur du TAC"))),
                            h3('Moyenne de I sur'),
                            p("Nombre d'années sur lesquelles on calcule l'indicateur d'abondance récente"),
                            h3("Calcul de la moyenne"),
                            p(strong("Moyenne"), ": moyenne classique utilisée pour calculer l'indicateur d'abondance récente"),
                            p(strong("Approche de précaution"), ": exemple pour 3 ans. On calcule la moyenne sur les 3 dernières années, sur les deux dernières années et la valeur de la dernière année. On garde ensuite la valeur la plus basse comme indicateur d'abondance récente."),
                            h3('Palier inférieur'),
                            p("Pourcentage du seuil de limitation d'une baisse de captures"),
                            h3('Palier supérieur'),
                            p("Pourcentage du seuil de limitation d'une augmentation de captures"),
                            h3(p(tags$u(' (ii) Périodicité'))),
                            h3("Application tous les"),
                            p("Toutes les combien d'années on applique l'HCR"),
                            h3("Première application"),
                            p("Est-ce qu'on applique l'HCR pour la première fois dès l'année prochaine, dans 2 ans ou dans 3 ans. Permet de jouer sur le décallage par rapport à la chute. A un sens uniquement si la périodicité d'application est supérieure à 1 an."),
                            h2("2.2.Processus de révision du TAC"),
                            h3("Mécanisme de sortie"),
                            p("Possibilité de révision du TAC au cours de la période pluri-annuelle conditionnellement à la condition suivante"),
                            h3("% de baisse provoquant la sortie"),
                            p("Si 'Mécanisme de sortie' vaut oui, la révision du TAC est déclenchée par un % de baisse de cumulée calculée par rapport à l'indicateur d'abondance lors de la fixation du TAC. Noter que dans le cas d'un pourcentage à 0%, le TAC est révisé dès que l'indicateur baisse.")
                            ),
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

