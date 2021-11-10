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
vaccCom <- read.csv("data/vaccCom.csv", sep = ";")

# There are issues with the new data, "NS" transforms data into text instead of numeric
vaccEPCI[vaccEPCI$population_carto == "NS", "population_carto"] <- NA
vaccCom[vaccCom$population_carto == "NS", "population_carto"] <- NA

vaccEPCI$population_carto <- as.numeric(vaccEPCI$population_carto)
vaccCom$population_carto <- as.numeric(vaccCom$population_carto)

wd1 <- "60px"
hg1 <- "60px"

wd2 <- "150px"
hg2 <- "150px"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tabsetPanel(tabPanel("Vaccination en France", 
    
    # Application title
    titlePanel("Taux de vaccination en France"),
    
    fluidRow(
        column(12, HTML("<p>Les données sont publiques et disponibles sur le site Ameli (<a href = 'https://datavaccin-covid.ameli.fr/explore/?exclude.theme=Datavisualisation&sort=modified'>source</a>). <br/>Le code source de cette page est sur <a href = 'https://github.com/flodebarre/dataCoViz'>GitHub</a>. N'hésitez pas à soumettre une <a href = 'https://github.com/flodebarre/dataCoViz/issues'><i>issue</i></a> sur GitHub ou <a href = 'mailto:florence.debarre@normalesup.org?subject=ShinyApp_MapsFrance'>me contacter</a> si vous notez une erreur ou avez des suggestions pour mieux coder l'app et les cartes.</p>
"))
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

h4("Données à inclure"),
HTML("<p>Cliquez sur '<b>Calculer</b>' après avoir changé les valeurs de cette section.</p>"),
HTML("<p>'<b>Correction d'âge</b>' : si l'option 'non' est choisie, les taux bruts sont représentés. Si l'option 'oui' est choisie, les taux de vaccination sont calculés comme si tous les EPCI avaient la même composition en âges que celle de la France entière ; cette option permet d'éviter le biais de l'âge et de comparer des localités aux compositions démographiques différentes. </p>"),

wellPanel(
    fluidRow(
        column(width = 9, checkboxGroupInput("agcl", "Classe(s) d'âge incluse(s)", 
                                             choices = list("00-19 ans" = "00-19", 
                                                            "20-39 ans" = "20-39", 
                                                            "40-54 ans" = "40-54", 
                                                            "55-64 ans" = "55-64", 
                                                            "65-74 ans" = "65-74", 
                                                            "75- + ans" = "75 et +"
                                                            ), 
                                             selected = c("75 et +", "65-74", "55-64", "40-54", "20-39", "00-19"), inline = TRUE)
        ), 
        column(width = 3, align = "center", inline = TRUE, radioButtons("ageCorrection", "Correction d'âge", 
                                       choices = list("oui" = "TRUE", "non" = "FALSE"), 
                                       selected = "FALSE")),
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
        )
    ),
), 

fluidRow(column(2, column(width = 3, align = "center", actionButton("calcAg", label = "Calculer", class = "btn-primary")), offset = 5)
),

h4("Affichage"),
HTML("<i>Il n'y a pas besoin de cliquer sur 'Calculer' si vous ne changez que les options ci-dessous.</i>"),
wellPanel(
    fluidRow(
        column(width = 4, radioButtons("var", "Variable à représenter", choices = list("Au moins une dose" = "1_inj", "Vaccination complète" = "termine")), offset = 2),
        
        column(width = 4, radioButtons("typeVar", "Type de variable", choices = list("Taux de vaccination" = "pourcent_cumu_", "Écart relatif à la moyenne nationale" = "pourcent_diff_cumu_", "Écart absolu à la moyenne" = "diff_cumu_")))
    ),
    
    fluidRow(
        column(width = 3, selectInput("pal", "Couleurs", 
                                      choices = list("Bleu-Rouge" = "Blue-Red", 
                                                     "Bleu-Rouge 2" = "Blue-Red 2", 
                                                     "Bleu-Rouge 3" = "Blue-Red 3", 
                                                     "Rouge-Vert" = "Red-Green", 
                                                     "Violet-Vert" = "Purple-Green", 
                                                     "Violet-Brun" = "Purple-Brown", 
                                                     "Vert-Brun" = "Green-Brown", 
                                                     "Bleu-Jaune 2" = "Blue-Yellow 2", 
                                                     "Bleu-Jaune 3" = "Blue-Yellow 3",
                                                     "Vert-Orange" = "Green-Orange", 
                                                     "Cyan-Magenta" = "Cyan-Magenta", 
                                                     "'Tropique'" = "Tropic", 
                                                     "'Broc'" = "Broc", 
                                                     "'Cork'" = "Cork", 
                                                     "'Vik'" = "Vik"))),
        
        column(width = 3, checkboxInput("invCol", "Inverser les couleurs", FALSE)),
        column(width = 3, radioButtons("villes", "Afficher villes", choices = list("oui" = "TRUE", "non" = "FALSE"), inline = TRUE)), 
        #column(3, selectInput("formatImage", "Format téléchargement", selected = "png", choices = list("png", "svg"))), 
        
    ),
    
    

    fluidRow(

        )
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
),

# Source 
# https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
fluidRow(
    column(3, align = "center", downloadButton('downloadPlotFrance', 'Télécharger en .svg'), offset = 5)
),

tags$style(type='text/css', "#downloadPlotFrance { width:100%; margin-top: 0px;}"),

fluidRow(
    column(1, align = "center", plotOutput("mapGuadeloupe", width = wd1, height = hg1), offset = 4),
    column(1, align = "center", plotOutput("mapMartinique", width = wd1, height = hg1)),
    column(1, align = "center", plotOutput("mapGuyane", width = wd1, height = hg1)),
    column(1, align = "center", plotOutput("mapMayotte", width = wd1, height = hg1)),
    column(1, align = "center", plotOutput("mapReunion", width = wd1, height = hg1))
),


fluidRow(
    column(3, align = "center", plotOutput("mapParis", width = wd2, height = hg2), offset = 2),
    column(3, align = "center", plotOutput("mapLyon", width = wd2, height = hg2)),
    column(3, align = "center", plotOutput("mapMarseille", width = wd2, height = hg2))
),

fluidRow(HTML("&nbsp;"))

), # end of Tab1
tabPanel("Another tab", "D'autres pages à venir")
)
))



