#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(mapsf) # Package to plot maps

## Load vaccination data
vaccEPCI <- read.csv("data/vaccEPCI.csv", sep = ";")
vaccComm <- read.csv("data/vaccCom.csv", sep = ";")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tabsetPanel(tabPanel("Vaccination en France", 
    
    # Application title
    titlePanel("Taux de vaccination en France"),
    
    fluidRow(
        column(12, HTML("<p>Données Ameli (<a href = 'https://datavaccin-covid.ameli.fr/explore/?exclude.theme=Datavisualisation&sort=modified'> source</a>). Code source sur <a href = 'https://github.com/flodebarre/dataCoViz'>GitHub</a>. N'hésitez pas à soumettre une <a href = 'https://github.com/flodebarre/dataCoViz/issues'><i>issue</i></a> ou <a href = 'mailto:florence.debarre@normalesup.org?subject=ShinyApp_MapsFrance'>me contacter</a> si vous notez une erreur ou avez des suggestions pour mieux coder.<br/>
'Correction d'âge' signifie que les taux de vaccination sont calculés comme si tous les EPCI avaient la même composition en âges que celle de la France entière. </p>"))
#         , 
#         column(2, p("kfld;sfkd sfl;dk slf dsf;sd")),
#         column(5, 
# #        verbatimTextOutput("ag"),
# #        verbatimTextOutput("xx"),
#         textOutput("yy"),
# #        verbatimTextOutput("agevalue"),
# #        verbatimTextOutput("value")
#         ), 
# column(5, verbatimTextOutput("ag"))
    ),

    wellPanel(
    fluidRow(
        column(width = 3, radioButtons("var", "Variable à représenter", choices = list("Au moins une dose" = "1_inj", "Vaccination complète" = "termine"))),
        column(width = 3, radioButtons("typeVar", "Type de variable", choices = list("Taux de vaccination" = "pourcent_cumu_", "Écart relatif à la moyenne nationale" = "pourcent_diff_cumu_"))),
        column(width = 3, checkboxGroupInput("agcl", "Classe(s) d'âge incluse(s)", 
                                      choices = list("75 ans et +" = "75 et +", 
                                                     "65-74 ans" = "65-74", 
                                                     "55-64 ans" = "55-64", 
                                                     "40-54 ans" = "40-54", 
                                                     "20-39 ans" = "20-39", 
                                                     "00-19 ans" = "00-19"), 
                                      selected = c("75 et +", "65-74", "55-64", "40-54", "20-39", "00-19"))
               ), 
        column(width = 3, radioButtons("ageCorrection", "Correction d'âge", 
                                       choices = list("oui" = "TRUE", "non" = "FALSE"), 
                                       selected = "FALSE"))
    ), 
    
    fluidRow(
        column(width = 8, selectInput("pal", "Couleurs", 
                                      choices = list("Blue-Red", "Blue-Red 2", "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown", "Green-Brown", "Blue-Yellow 2", "Blue-Yellow 3",
                                                     "Green-Orange", "Cyan-Magenta", "Tropic", "Broc", "Cork", "Vik"))),
        
        column(width = 4, checkboxInput("invCol", "inverser", FALSE))
    ),
    
    
    fluidRow(
        column(width = 4, radioButtons("DROM", "Afficher DROM ('oui' bugge sur le serveur)", choices = list("oui" = "TRUE", "non" = "FALSE"), selected = "FALSE")),
        column(width = 4, radioButtons("communes", "Afficher communes", choices = list("oui" = "TRUE", "non" = "FALSE"))),
        column(width = 4, radioButtons("villes", "Afficher villes", choices = list("oui" = "TRUE", "non" = "FALSE")))
    ),
    
    
    fluidRow(
        column(width = 12, sliderInput("thedate",
                                      "Date",
                                      min = as.Date("2021-02-07","%Y-%m-%d"),
                                      max = as.Date(max(vaccEPCI$date),"%Y-%m-%d"),
                                      value = as.Date(max(vaccEPCI$date)),
                                      timeFormat = "%Y-%m-%d",
                                      step = 7
                                      ), offset = 0
        ))
    ), # End wellPanel

            # tags$head(tags$script('
            #                     var dimension = [0, 0];
            #                     $(document).on("shiny:connected", function(e) {
            #                         dimension[0] = window.innerWidth;
            #                         dimension[1] = window.innerHeight;
            #                         Shiny.onInputChange("dimension", dimension);
            #                     });
            #                     $(window).resize(function(e) {
            #                         dimension[0] = window.innerWidth;
            #                         dimension[1] = window.innerHeight;
            #                         Shiny.onInputChange("dimension", dimension);
            #                     });
            #                 ')),
            
fluidRow(
    column(12, align = "center", plotOutput("map", width = "600px", height = "500px"))
)

), # Tab1
tabPanel("Another tab", "a venir")
)
))



