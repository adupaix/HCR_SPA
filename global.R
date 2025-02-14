#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2025-01-30
#'@email : amael.dupaix@mnhn.fr
#'#***************************************************************************
#'@description : App pour visualiser la réaction de l'HCR langoustes à Saint-
#'               Paul et Amsterdam a différentes paramétrisations
#'               
#'#***************************************************************************

# global.R se charge avant ui.R et server.R
# Il permet de partager des variables entre ui et server : les variables sont dans l'environnement global
# Il permet par exemple de paramétrer les inputs dans ui en fonction de données lues dans un fichier


# Chargement des librairies ----
library(shiny)
library(shinythemes)
library(shinydashboard)

# Le package bslib permet d'appliquer et personnaliser des thèmes Bootstrap 
# pour modifier facilement l'apparence des applications Shiny
# https://rstudio.github.io/bslib/
library(bslib)

# Le package ggplot2 permet de créer des graphiques
# https://ggplot2.tidyverse.org/reference/index.html
library(ggplot2)
# Package pour organiser des graphiques ensembles
library(ggpubr)

# Les packages dplyr et tidyr permettent la manipulation de données
# https://dplyr.tidyverse.org/reference/index.html
library(dplyr)
library(tidyr)

# Le package DT est une implémentation dans R de la librairie JavaScript DataTables
# Matrices et data frames peuvent être affichés sous forme de tableaux sur des pages HTML
# DataTables permet notamment le filtrage, la pagination et le tri de ces tableaux. 
# https://rstudio.github.io/DT/
library(DT)


current_date <- Sys.Date()

# Chargement des données
TAC_SPA <- read.csv(file='data/TAC_an.csv', header=T, sep=';')
cpue_SP <- readRDS('data/cpue_SP.rds')
cpue_A <- readRDS('data/cpue_A.rds')
captures <- readRDS('data/captures_annuelles.rds')

# chargement fonctio
source('fun/HCR.JP.fun.R')


HCRArgs <- list(
  ref.yrs = factor(2001:2010), # reference years for the calculation of Icible and ref catch
  m.period = 4, # time window length considered to calculate Imean (in years)
  m.tac.period=5, # time window length considered for coastal/deep RBC distribution (from previous years TAC)
  var.limit.up = 0.05, # max authorized percentage of increase 
  var.limit.lo = 0.1, # max authorized percentage of decrease 
  var.limit = 0.5, 
  ratio.cpue.lim = 0.4, # ratio to calculate Ilim from Icible
  buffer= 0, # to add a buffer around Icible and ref catch
  cur.yr = 2024 #year of the last fishing campaign: if cur.yr is 2023, the last fishing
  # campaign considered will be 2023-24 and the reco will be done for
  # the following campaign (2024-25)
)

list2env(HCRArgs, globalenv())

# liste des choix
limits.low <- list("-10%" = 0.1,
                   "-20%" = 0.2,
                   "-30%" = 0.3,
                   "-40%" = 0.4,
                   "-50%" = 0.5)
limits.high <- list("5%" = 0.05,
                    "10%" = 0.1,
                    "15%" = 0.15,
                    "20%" = 0.2,
                    "25%" = 0.25)
averaged.period <- list('1 an' = 1,
                        '2 ans' = 2,
                        '3 ans' = 3,
                        '4 ans' = 4,
                        '5 ans' = 5)
m.period.calc = list('Moyenne' = 1,
                     'Approche de précaution' = 2)
scenarios <- list('Chute' = 1,
                  'Stabilisation' = 2,
                  "Oscillations" = 3)
HCR.application <- list('1 an' = 1,
                        '3 ans' = 3)
Iles <- list('Amsterdam' = 1,
            'Saint Paul' = 2)


# duree des simulations
n_years_sim <- 10
