
library(shiny)

library(mapsf) # Package to plot maps


## Load vaccination data
vaccEPCI <- read.csv("data/vaccEPCI.csv", sep = ";")
vaccCom <- read.csv("data/vaccCom.csv", sep = ";")

# 2022-01-14 New version of the dataset is with NAs
# There are issues with the new data, "NS" transforms data into text instead of numeric
# vaccEPCI[vaccEPCI$population_carto == "NS", "population_carto"] <- NA
# vaccCom[vaccCom$population_carto == "NS", "population_carto"] <- NA
# And they are already numeric now
# vaccEPCI$population_carto <- as.numeric(vaccEPCI$population_carto)
# vaccCom$population_carto <- as.numeric(vaccCom$population_carto)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Whether to apply age correction
    output$agevalue <- renderPrint(input$ageCorrection)
    
    # Chosen age classes
    output$ag <- renderPrint(input$agcl)
    
    
    ## (Commented out) Attempts to change figure size
    # output$dimension_display <- renderText({
    #     paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
    # })
#    output$figHeight <- renderText({wd <- input$dimension[1]
#    if(wd > 800){fgh <- "700px"}else{fgh <- paste0(wd, "px")}
#    wd})

    
    # Load data 
    ## Load geographic data
    load("data/mapFiles_withDepReg.RData")
    load("data/chefslieux.RData")
    
    # ## Subselect the data for this age class and date
    # ag <- reactive({renderPrint(input$agcl)})
    # subEPCI <- reactive(vaccEPCI[which(is.element(vaccEPCI$classe_age, ag()) & vaccEPCI$date == input$thedate), ])
    # subCom <- reactive(vaccCom[which(is.element(vaccCom$classe_age, ag()) & vaccCom$date == input$thedate), ])
    
    
    # Compute national mean
#    reactive({moy_EPCI <- sum(subEPCI[, reactive(paste0("effectif_cumu_", input$var))]) / sum(subEPCI$population_carto)
#             output$value <- renderText({ moyEPCI })
#    })


        
    # # Aggregate the values, with or without age correction

    thevar <- reactive({paste0(input$typeVar, input$var)})
    
    ag <- reactive({input$agcl})
    fsubEPCI <- reactive(vaccEPCI[which(is.element(vaccEPCI$classe_age, ag()) & vaccEPCI$date == input$thedate), ])
    fsubCom <- reactive(vaccCom[which(is.element(vaccCom$classe_age, ag()) & vaccCom$date == input$thedate), ])
    
    thedate <- eventReactive(input$calcAg, input$thedate, ignoreNULL = FALSE)
    ageCorr <- eventReactive(input$calcAg, input$ageCorrection, ignoreNULL = FALSE)
    ages <- eventReactive(input$calcAg, input$agcl, ignoreNULL = FALSE)
    
    ### Function to compute rates, relative differences and corrected distributions
    wrangleData <- function(subEPCI, subCom){
        
        #cat(file=stderr(), "before moy calculations\n")

        # Make sure that population is numerical
        subEPCI$population_carto <- as.numeric(subEPCI$population_carto)
        subCom$population_carto <- as.numeric(subCom$population_carto)
        
        # Compute unvaccinated variable
        subEPCI$effectif_cumu_unvaccinated <- subEPCI$population_carto - subEPCI$effectif_cumu_1_inj 
        subCom$effectif_cumu_unvaccinated <- subCom$population_carto  - subCom$effectif_cumu_1_inj
        
        subEPCI$taux_cumu_unvaccinated <- 1 - subEPCI$effectif_cumu_1_inj / subEPCI$population_carto 
        subCom$taux_cumu_unvaccinated <- 1 - subCom$effectif_cumu_1_inj / subCom$population_carto 

        # Compute national means on unweighted data
        moy_cumu_1_inj <- sum(subEPCI$effectif_cumu_1_inj, na.rm = TRUE) / sum(subEPCI$population_carto, na.rm = TRUE)
        moy_cumu_termine <- sum(subEPCI$effectif_cumu_termine, na.rm = TRUE) / sum(subEPCI$population_carto, na.rm = TRUE)
        moy_cumu_unvaccinated <- sum(subEPCI$effectif_cumu_unvaccinated, na.rm = TRUE) / sum(subEPCI$population_carto, na.rm = TRUE)

        #cat(file=stderr(), "before moy calculations\n")
        
        # What we do next depends on whether we want to apply the age correction or not
        if(input$ageCorrection == "TRUE" & length(input$agcl) > 1){
            
            # With age correction
            # 1) Compute whole France values
            totPop <- sum(subEPCI$population_carto, na.rm = TRUE) # Total population size in these data
            
            # Create table of subtotals per age class
            props <- data.frame("classe_age" = sort(unique(subEPCI$classe_age)), "pop" = rep(NA, length(unique(subEPCI$classe_age)))) # Initialize the table
            # Proportions of each age class
            for(age in sort(unique(subEPCI$classe_age))){
                subsub <- subEPCI[subEPCI$classe_age == age, ]
                props[props$classe_age == age, "pop"] <- sum(subsub$population_carto, na.rm = TRUE)
            }
            props$pop <- as.numeric(props$pop) # Turn numerical values back to numeric
            # Compute proportion of the age class
            props$proportionPop <- props$pop / totPop
            
            # 2) Correct the local values
            # Add the proportions information to the datasets
            subEPCI <- merge(subEPCI, props, by = "classe_age")
            subCom <- merge(subCom, props, by = "classe_age")
            
            # Aggregate by EPCI, weigh by the proportions, and sum to have weighted proportions
            aggEPCI <- aggregate(subEPCI$proportionPop * subEPCI[, c("taux_cumu_1_inj", "taux_cumu_termine", "taux_cumu_unvaccinated")], by =  list(epci = subEPCI$epci), FUN = sum)
            aggCom <- aggregate(subCom$proportionPop * subCom[, c("taux_cumu_1_inj", "taux_cumu_termine", "taux_cumu_unvaccinated")], by = list(commune_residence = subCom$commune_residence), FUN = sum)
            
        }else{
            # Without age correction
            
            # Sum up values
            aggEPCI <- aggregate(subEPCI[, c("population_carto", "effectif_1_inj", "effectif_termine", "effectif_cumu_1_inj", "effectif_cumu_termine", "effectif_cumu_unvaccinated")], by = list(epci = subEPCI$epci), FUN = sum)
            aggCom <- aggregate(subCom[, c("population_carto", "effectif_1_inj", "effectif_termine", "effectif_cumu_1_inj", "effectif_cumu_termine", "effectif_cumu_unvaccinated")], by = list(commune_residence = subCom$commune_residence), FUN = sum)
            
            # Recompute taux
            aggEPCI$taux_cumu_1_inj <- aggEPCI$effectif_cumu_1_inj / aggEPCI$population_carto
            aggEPCI$taux_cumu_termine <- aggEPCI$effectif_cumu_termine / aggEPCI$population_carto
            aggCom$taux_cumu_1_inj <- aggCom$effectif_cumu_1_inj / aggCom$population_carto
            aggCom$taux_cumu_termine <- aggCom$effectif_cumu_termine / aggCom$population_carto
            aggEPCI$taux_cumu_unvaccinated <- aggEPCI$effectif_cumu_unvaccinated / aggEPCI$population_carto
            aggCom$taux_cumu_unvaccinated <- aggCom$effectif_cumu_unvaccinated / aggCom$population_carto
            
        }
        
        subEPCI <- aggEPCI
        subCom <- aggCom
        
        
        # Compute relative differences
        subEPCI$pourcent_diff_cumu_1_inj <- 100 * (subEPCI$taux_cumu_1_inj - moy_cumu_1_inj) / moy_cumu_1_inj
        subEPCI$pourcent_diff_cumu_termine <- 100 * (subEPCI$taux_cumu_termine - moy_cumu_termine) / moy_cumu_termine
        subEPCI$pourcent_diff_cumu_unvaccinated <- 100 * (subEPCI$taux_cumu_unvaccinated - moy_cumu_unvaccinated) / moy_cumu_unvaccinated
        
        subCom$pourcent_diff_cumu_1_inj <- 100 * (subCom$taux_cumu_1_inj - moy_cumu_1_inj) / moy_cumu_1_inj
        subCom$pourcent_diff_cumu_termine <- 100 * (subCom$taux_cumu_termine - moy_cumu_termine) / moy_cumu_termine
        subCom$pourcent_diff_cumu_unvaccinated <- 100 * (subCom$taux_cumu_unvaccinated - moy_cumu_unvaccinated) / moy_cumu_unvaccinated
        
        # Compute absolute differences
        subEPCI$diff_cumu_1_inj <- 100 * (subEPCI$taux_cumu_1_inj - moy_cumu_1_inj)
        subEPCI$diff_cumu_termine <- 100 * (subEPCI$taux_cumu_termine - moy_cumu_termine)
        subEPCI$diff_cumu_unvaccinated <- 100 * (subEPCI$taux_cumu_unvaccinated - moy_cumu_unvaccinated)
        
        subCom$diff_cumu_1_inj <- 100 * (subCom$taux_cumu_1_inj - moy_cumu_1_inj)
        subCom$diff_cumu_termine <- 100 * (subCom$taux_cumu_termine - moy_cumu_termine)
        subCom$diff_cumu_unvaccinated <- 100 * (subCom$taux_cumu_unvaccinated - moy_cumu_unvaccinated)
        
        # Raw rates, Percentage instead of proportion
        subEPCI$pourcent_cumu_1_inj <- 100 * subEPCI$taux_cumu_1_inj
        subCom$pourcent_cumu_1_inj <- 100 * subCom$taux_cumu_1_inj

        subEPCI$pourcent_cumu_termine <- 100 * subEPCI$taux_cumu_termine
        subCom$pourcent_cumu_termine <- 100 * subCom$taux_cumu_termine
        
        subEPCI$pourcent_cumu_unvaccinated <- 100 * subEPCI$taux_cumu_unvaccinated
        subCom$pourcent_cumu_unvaccinated <- 100 * subCom$taux_cumu_unvaccinated
        
        list(subEPCI = subEPCI, subCom = subCom)
    }
    
    palsMB <- c("Cassatt1", "Cassatt2", "Derain", "Hiroshige", "Ingres", "Morgenstern", "OKeeffe1", "Veronese")
    ### Function to get the color palette
    getPalette <- function(typeVar, invCol, thepal){        
        # Number of colors and breaks depends on what we are plotting
        if(typeVar == "pourcent_cumu_"){
            ncols <- 20
            # Raw rate
            if(is.element(thepal, palsMB)){
                pal <- met.brewer(thepal, ncols, "continuous")
            }else{
                pal <- colorspace::diverge_hcl(n = ncols, palette = thepal)
            }
            # Order of the colors
            if(invCol){ pal <- rev(pal) }
            brks <- 100*seq(0, 1, length.out = ncols + 1)
        }
        if(typeVar == "pourcent_diff_cumu_"){
            # Relative Difference to the mean
            brks <- c(-300, -200, -100, -75, -50, -35, -25, -15, -5, 5, 15, 25, 35, 50, 75, 100, 200, 300)
            ncols <- length(brks) - 1
            if(is.element(thepal, palsMB)){
                pal <- met.brewer(thepal, ncols, "continuous")
            }else{
                pal <- colorspace::diverge_hcl(n = ncols, palette = thepal)
            }
            # Order of the colors
            if(invCol){ pal <- rev(pal) }
            pal[(length(pal)+1)/2] <- gray(0.975) # mid point in white
            # Remove -300 and -200, because they do not make sense (are only used to center the palettee)
            imin <- 3 # Position of -100
            pal <- pal[imin:length(pal)]
            brks <- brks[imin:length(brks)]
        }
        if(typeVar == "diff_cumu_"){
            # Absolute difference to the mean
            brks <- c(-100, -50, -30, -25, -20, -15, -10, -5, 5, 10, 15, 20, 25, 30, 50, 100)
            ncols <- length(brks) - 1
            if(is.element(thepal, palsMB)){
                pal <- met.brewer(thepal, ncols, "continuous")
            }else{
                pal <- colorspace::diverge_hcl(n = ncols, palette = thepal)
            }
            # Order of the colors
            if(invCol){ pal <- rev(pal) }
            pal[(length(pal)+1)/2] <- gray(0.975) # mid point in white
        }
        
        
        list(pal = pal, brks = brks)
    }
    
    
    ### Plotting parameters
    ##### Main plot
    brd.col <- gray(0.6, 1)
    brd.lwd <- 0.5
    
    colNA <- gray(0.75)
    
    ##### Insets
    # Size of the insets for DOM
    insetSize <- 0.075 # Size of the inset, fraction of plot
    dxy <- 0.025 # Space between them
    y0 <- 0.065  # Minimum y position
    
    # Other inset parameters
    box.lwd <- 0.5 # Line width of the box arount inset
    box.col <- gray(1) # Color of the box
    BB.inset <- c(0, 0, 0.5, 0) # Bounding box (does not seem to work)
    mar.inset <-c(0, 0, 0.6, 0) # Margins of the inset
    line.title <- 0.5 # Line position of the inset title
    line.title.2 <- 0.2 # Line position of the inset titles, communes
    cex.title.inset <- 0.8 # cex of the inset title
    cex.title.inset.2 <- 0.9 # cex of the inset title, communes
    
    getdata <- eventReactive(input$calcAg, wrangleData(fsubEPCI(), fsubCom()), ignoreNULL = FALSE)
    # ignoreNULL=FALSE evaluates the result by default
    
    # Function to plot map
    plotMap <- reactive({
        # Get the data
        tmpdata <- getdata()
        subEPCI <- tmpdata[["subEPCI"]]
        subCom <- tmpdata[["subCom"]]
        
        # Define palette
        tmppal <- getPalette(input$typeVar, input$invCol, input$pal)
        pal <- tmppal[["pal"]]
        brks <- tmppal[["brks"]]
        
        # Map theme
        mf_theme(mar = c(0, 0, 2.5, 0), #c(0, 0.2, 1.5, 0.5),
                 bg = gray(1, 1))
        
        ## France metropolitaine ##
        tmp <- merge(france, subEPCI, by.x = "SIREN", by.y = "epci")
        
        # Initiate a base map
        mf_init(x = tmp, expandBB = rep(0, 4))#c(0, 0.1, 0.05, 0.175))
        
        # Plot choropleth 
        mf_map(tmp,
               var = thevar(),
               type = "choro", add = TRUE,
               leg_pos = "topleft", leg_title = "",
               breaks = brks, pal = pal,
               border = brd.col, lwd = brd.lwd, leg_val_rnd = 0,
               col_na = colNA,
               leg_no_data = "pas de données"
        )
        
        # Add departements
        mf_map(mdeps, type = "base", add = TRUE, col = gray(0, 0), lwd = 0.5, border = "black")
        # Add regions
        mf_map(mregions.metro, type = "base", add = TRUE, col = gray(0, 0), border = "black", lwd = 1)
        # Add cities
        chfl.dep$pt <- 1
        chfl.reg$pt <- 1
        colCity <- gray(0.8)
        mf_map(x = chfl.dep, type = "prop", var = "pt", inches = 0.02, col = colCity, leg_pos = "n")
        mf_map(x = chfl.reg, type = "prop", var = "pt", inches = 0.04, col = colCity, leg_pos = "n")
        # Add names chefs lieux regions
        mf_label(x = chfl.reg, var = "nom", halo = TRUE, bg = gray(1, 0.5), adj = c(0.5, -1), r = 0.1, cex = 0.8, overlap = FALSE)
        
        
        # Title
        # Prepare title elements
        if(input$typeVar == "pourcent_cumu_"){
            v1 <- ""
        }
        if(input$typeVar == "pourcent_diff_cumu_"){
            v1 <- "écart relatif à la moyenne nationale (%), "
        }
        if(input$typeVar == "diff_cumu_"){
            v1 <- "écart absolu à la moyenne nationale (points), "
        }
        
        if(input$var == "termine"){
            v2 <- "vaccination terminée"
        }
        if(input$var == "1_inj"){
            v2 <- "vaccination au moins une injection"
        }
        if(input$var == "unvaccinated"){
            v2 <- "non-vaccination"
        }
        
        if(ageCorr() == "TRUE"){
            v3 <- "avec correction d'âge"
        }else{
            v3 <- "sans correction d'âge"
        }
        
        if(length(ages()) == 6){
            v4 <- "tous âges"
        }else{
            v4 <- paste(paste(ages(), collapse = ", "), "ans")
        }
        
        mf_title(txt = paste0("Pourcentage de ", v2, ", ", v1, "
au ", format(as.Date(thedate()), "%d/%m/%Y"), ", par lieu de résidence 
", v4, " (", v3, ")"
                              # Classe d'age ", ncl, 
                              #                       #", Moyenne nationale ", round(100*get(paste0("moy_cumu_", var)), 1), "%"
        ), bg = gray(1, 1), fg = gray(0, 1),
        font = 1, tab = FALSE, line = 0, inner = FALSE)
        
        mf_credits(paste0("@flodebarre\n",
                          "Données vaccination : Ameli https://datavaccin-covid.ameli.fr/\n",
                          "Contours EPCI : Banatic, ",
                          "Contours communes : © les contributeurs d'OpenStreetMap sous licence ODbL. ",
                          "mapsf ",
                          packageVersion("mapsf")), cex = 0.8)

    })
    
    output$map <- renderPlot({
        print({
            mf_init(x = france, expandBB = rep(0, 4))#c(0, 0.1, 0.05, 0.175))
            plotMap()}
            )

    
        }) # End plot map


    plotDROM <- function(map, tit){
        
        # Get the data
        tmpdata <- getdata()
        subEPCI <- tmpdata[["subEPCI"]]
        subCom <- tmpdata[["subCom"]]
        
        # Define palette
        tmppal <- getPalette(input$typeVar, input$invCol, input$pal)
        pal <- tmppal[["pal"]]
        brks <- tmppal[["brks"]]
        
        # Merge map and data
        tmp <- merge(map, subEPCI, by.x = "SIREN_EPCI", by.y = "epci")
        
        mf_theme(mar = mar.inset)
        mf_init(tmp, expandBB = BB.inset)
        mf_map(tmp, type = "choro", 
               var = thevar(),
               breaks = brks, pal = pal,
               border = brd.col, lwd = brd.lwd,
               leg_pos = "n",
               col_na = colNA
        )
        mf_title(tit, tab = TRUE, inner = FALSE, cex = cex.title.inset, fg = 1, bg = gray(0, 0), line = line.title, pos = "center")
    }    
    
    ## Function to plot the Communes insets
    plotCommunes <- function(tit, deps){
        
        # Get the data
        tmpdata <- getdata()
        subEPCI <- tmpdata[["subEPCI"]]
        subCom <- tmpdata[["subCom"]]
        
        # Define palette
        tmppal <- getPalette(input$typeVar, input$invCol, input$pal)
        pal <- tmppal[["pal"]]
        brks <- tmppal[["brks"]]
        
        # Subset of departements for the localities by deps
        tmp <- merge(tca, subCom[is.element(substr(subCom$commune_residence, 1, 2), deps), ], by.x = "insee", by.y = "commune_residence")
        
        # Plot map
        mf_theme(mar = mar.inset)
        mf_map(tmp, type = "choro",
               var = thevar(),
               breaks = brks, pal = pal,
               border = brd.col, lwd = brd.lwd,
               leg_pos = "n",
               col_na = colNA
        )

        mf_title(tit, tab = TRUE, inner = FALSE, cex = cex.title.inset.2, fg = 1, bg = gray(0, 0), line = line.title.2, pos = "center")
    }
    
    output$mapGuadeloupe <- renderPlot({
        plotDROM(guadeloupe, "Guadeloupe")
    })

    output$mapMartinique <- renderPlot({
        plotDROM(martinique, "Martinique")
    })
    
    output$mapGuyane <- renderPlot({
        plotDROM(guyane, "Guyane")
    })

    output$mapReunion <- renderPlot({
        plotDROM(reunion, "La Réunion")
    })

    output$mapMayotte <- renderPlot({
        plotDROM(mayotte, "Mayotte")
    })
    
    output$mapParis <- renderPlot({
        plotCommunes("Métropole du Grand Paris", deps = c("75", "91", "92", "93", "94", "95"))
    })
    
    output$mapLyon <- renderPlot({
        plotCommunes("Métropole de Lyon", deps = c("69"))
    })

    output$mapMarseille <- renderPlot({
        plotCommunes("Métropole d'Aix-Marseille-Provence", deps = c("13", "83", "84"))
    })
    
    
    ### Functions for download ----------------------------------------------------
    
    plotMap2 <- reactive({
        
        # Get the data
        tmpdata <- getdata()
        subEPCI <- tmpdata[["subEPCI"]]
        subCom <- tmpdata[["subCom"]]
        
        # Define palette
        tmppal <- getPalette(input$typeVar, input$invCol, input$pal)
        pal <- tmppal[["pal"]]
        brks <- tmppal[["brks"]]
        
        # Map theme
        mf_theme(mar = c(0, 0, 2.5, 0), #c(0, 0.2, 1.5, 0.5),
                 bg = gray(1, 1))
        
        ## France metropolitaine ##
        tmp <- merge(france, subEPCI, by.x = "SIREN", by.y = "epci")
        
        # Plot choropleth
        mf_map(tmp,
               var = thevar(),
               type = "choro", add = TRUE,
               leg_pos = "topleft", leg_title = "",
               breaks = brks, pal = pal,
               border = brd.col, lwd = brd.lwd, leg_val_rnd = 0,
               col_na = colNA, 
               leg_no_data = "pas de données"
        )
        
        # Add names of the main cities
        if(input$villes == "TRUE"){
            # Select EPCI with population size bigger than threshold
            fbig <- france[france$POPULATION > 450000,]
            # Hard code short names...
            fbig$shortname <- c("Rouen", "Nice", "Grenoble", "Lyon", "Paris", "Marseille", "Lille", "Toulouse", "Bordeaux", "Montpellier", "Rennes", "Nantes", "Strasbourg")
            fbig$pt <- 1
            # Place dots
            mf_map(x = fbig, type = "prop", var = "pt", inches = 0.025, col = gray(0.5), leg_pos = "n")
            # Place names    
            mf_label(x = fbig, var = "shortname", halo = TRUE, bg = gray(1, 0.5), adj = c(0.5, -1), r = 0.1, cex = 1, overlap = FALSE)
            #mf_map(st_centroid(st_geometry(fbig)), col = gray(0.5)) 
            
        }
        
    
        
        
        ### END INSETS
        ###-----------------------------------------------------------------------------------
        
        # Title
        # Prepare title elements
        if(input$typeVar == "pourcent_cumu_"){
            v1 <- ""
        }else{
            v1 <- "écart à la moyenne nationale, "
        }
        
        if(input$var == "termine"){
            v2 <- "terminée"
        }else{
            v2 <- "au moins une injection"
        }
        
        if(ageCorr() == "TRUE"){
            v3 <- "avec correction d'âge"
        }else{
            v3 <- "sans correction d'âge"
        }
        
        if(length(ages()) == 6){
            v4 <- "tous âges"
        }else{
            v4 <- paste(paste(ages(), collapse = ", "), "ans")
        }
        
        mf_title(txt = paste0("Pourcentage de vaccination ", v2, ", ", v1, "au ", format(as.Date(thedate()), "%d/%m/%Y"), ", par lieu de résidence, ", v4, " (", v3, ")"
                              # Classe d'age ", ncl, 
                              #                       #", Moyenne nationale ", round(100*get(paste0("moy_cumu_", var)), 1), "%"
        ), bg = gray(1, 1), fg = gray(0, 1),
        font = 1, tab = TRUE, line = 0, inner = TRUE, 
        cex = 0.7)
        
        mf_credits(paste0("@flodebarre\n",
                          "Données vaccination : Ameli https://datavaccin-covid.ameli.fr/\n",
                          "Contours EPCI : Banatic, ",
                          "Contours communes : © les contributeurs d'OpenStreetMap sous licence ODbL. ",
                          "mapsf ",
                          packageVersion("mapsf")), cex = 0.7)
        
        
    })
    
    output$downloadPlotFrance <- downloadHandler(
        filename = function() { paste('france', '.', "svg", sep='') },
        content = function(file) {
            wdt <- 8
            hgt <- 7
            
            mf_export(france, export = "svg", filename = file, width = wdt * 1, height = hgt * 1, 
                      expandBB = c(0, 0, 0 ,0))
            
            plotMap2()
            
            dev.off()
        })
    
    
})
