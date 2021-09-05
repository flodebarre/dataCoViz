
library(shiny)

library(mapsf) # Package to plot maps

## Load vaccination data
vaccEPCI <- read.csv("data/vaccEPCI.csv", sep = ";")
vaccCom <- read.csv("data/vaccCom.csv", sep = ";")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$agevalue <- renderPrint(input$ageCorrection)
    
    output$ag <- renderPrint(input$agcl)
    
    # output$dimension_display <- renderText({
    #     paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
    # })
    

#    output$figHeight <- renderText({wd <- input$dimension[1]
#    if(wd > 800){fgh <- "700px"}else{fgh <- paste0(wd, "px")}
#    wd})

    
    # Load data 
    ## Load geographic data
    load("data/mapFiles.RData")
    
    # ## Subselect the data for this age class and date
    # ag <- reactive({renderPrint(input$agcl)})
    # subEPCI <- reactive(vaccEPCI[which(is.element(vaccEPCI$classe_age, ag()) & vaccEPCI$date == input$thedate), ])
    # subCom <- reactive(vaccCom[which(is.element(vaccCom$classe_age, ag()) & vaccCom$date == input$thedate), ])
    
    
    # Compute national mean
#    reactive({moy_EPCI <- sum(subEPCI[, reactive(paste0("effectif_cumu_", input$var))]) / sum(subEPCI$population_carto)
#             output$value <- renderText({ moyEPCI })
#    })


        
    # # Aggregate the values, with or without age correction
    agCor <- reactive({input$ageCorrection})
    
    

    output$map <- renderPlot({
        
        thevar <- reactive({paste0(input$typeVar, input$var)})
        
        ag <- reactive({input$agcl})
        subEPCI <- vaccEPCI[which(is.element(vaccEPCI$classe_age, ag()) & vaccEPCI$date == input$thedate), ]
        subCom <- vaccCom[which(is.element(vaccCom$classe_age, ag()) & vaccCom$date == input$thedate), ]
        
        # Compute national means on unweighted data
        moy_cumu_1_inj <- sum(subEPCI$effectif_cumu_1_inj, na.rm = TRUE) / sum(subEPCI$population_carto, na.rm = TRUE)
        moy_cumu_termine <- sum(subEPCI$effectif_cumu_termine, na.rm = TRUE) / sum(subEPCI$population_carto, na.rm = TRUE)
        
        
        
        if(input$ageCorrection == "TRUE" & length(input$agcl) > 1){
            # With age correction

            # 1) Compute whole France values
            totPop <- sum(subEPCI$population_carto) # Total population size in these data

            # Create table of subtotals per age class
            props <- c(0, 0) # Initialize the table (there's maybe a cleaner way to do it)
            # Proportions of each age class
            for(age in sort(unique(subEPCI$classe_age))){
                subsub <- subEPCI[subEPCI$classe_age == age, ]
                props <- rbind(props, c(age, sum(subsub$population_carto)))
            }
            props <- as.data.frame(props[-1, ]) # Remove the initialization line
            names(props) <- c("classe_age", "pop")
            props$pop <- as.numeric(props$pop) # Turn numerical values back to numeric
            # Compute proportion of the age class
            props$proportionPop <- props$pop / totPop

            # 2) Correct the local values
            # Add the proportions information to the datasets
            subEPCI <- merge(subEPCI, props, by = "classe_age")
            subCom <- merge(subCom, props, by = "classe_age")

            # Aggregate by EPCI
            aggEPCI <- aggregate(subEPCI$proportionPop * subEPCI[, c("taux_cumu_1_inj", "taux_cumu_termine")], by =  list(epci = subEPCI$epci), FUN = sum, na.rm = TRUE)

            aggCom <- aggregate(subCom$proportionPop * subCom[, c("taux_cumu_1_inj", "taux_cumu_termine")], by =  list(commune_residence = subCom$commune_residence), FUN = sum, na.rm = TRUE)

        }else{
            # Sum up values
            aggEPCI <- aggregate(subEPCI[, c("population_carto", "effectif_1_inj", "effectif_termine", "effectif_cumu_1_inj", "effectif_cumu_termine")], by = list(epci = subEPCI$epci), FUN = sum, na.rm = TRUE)
            aggCom <- aggregate(subCom[, c("population_carto", "effectif_1_inj", "effectif_termine", "effectif_cumu_1_inj", "effectif_cumu_termine")], by = list(commune_residence = subCom$commune_residence), FUN = sum, na.rm = TRUE)

            # Recompute taux
            aggEPCI$taux_cumu_1_inj <- aggEPCI$effectif_cumu_1_inj / aggEPCI$population_carto
            aggEPCI$taux_cumu_termine <- aggEPCI$effectif_cumu_termine / aggEPCI$population_carto
            aggCom$taux_cumu_1_inj <- aggCom$effectif_cumu_1_inj / aggCom$population_carto
            aggCom$taux_cumu_termine <- aggCom$effectif_cumu_termine / aggCom$population_carto

        }

        subEPCI <- aggEPCI
        subCom <- aggCom



        # Compute relative differences
        subEPCI$pourcent_diff_cumu_1_inj <- 100 * (subEPCI$taux_cumu_1_inj - moy_cumu_1_inj) / moy_cumu_1_inj
        subEPCI$pourcent_diff_cumu_termine <- 100 * (subEPCI$taux_cumu_termine - moy_cumu_termine) / moy_cumu_termine

        subCom$pourcent_diff_cumu_1_inj <- 100 * (subCom$taux_cumu_1_inj - moy_cumu_1_inj) / moy_cumu_1_inj
        subCom$pourcent_diff_cumu_termine <- 100 * (subCom$taux_cumu_termine - moy_cumu_termine) / moy_cumu_termine

        # Raw rates, Percentage instead of proportion
        subEPCI$pourcent_cumu_1_inj <- 100 * subEPCI$taux_cumu_1_inj
        subCom$pourcent_cumu_1_inj <- 100 * subCom$taux_cumu_1_inj

        subEPCI$pourcent_cumu_termine <- 100 * subEPCI$taux_cumu_termine
        subCom$pourcent_cumu_termine <- 100 * subCom$taux_cumu_termine



        # Define palette

        # Number of colors and breaks depends on what we are plotting
        if(input$typeVar == "pourcent_cumu_"){
            # Raw rate
            pal <- colorspace::diverge_hcl(n = 20, palette = input$pal) 
            brks <- 100*seq(0, 1, length.out = 21)
        }else{
            # Difference to the mean
            brks <- c(-100, -50, -25, -15, -5, 5, 15, 25, 50, 100)
            pal <- colorspace::diverge_hcl(length(brks)-1, palette = input$pal) 
            pal[(length(pal)+1)/2] <- gray(0.975) # mid point in white
        }
        
        # Order of the colors
        if(input$invCol){
            pal <- rev(pal)
        }


brd.col <- gray(0.6, 1)
brd.lwd <- 0.5

colNA <- gray(0.7)

# Map theme
mf_theme(mar = c(0, 0.2, 1.5, 0.5),
         bg = gray(1, 1))

## France metropolitaine ##
tmp <- merge(france, subEPCI, by.x = "SIREN", by.y = "epci")
output$xx <- renderPrint(tmp[1,])

output$yy <- renderPrint(tmp[1, thevar()])
# Initiate a base map
mf_init(x = tmp, expandBB = c(0, 0.1, 0.05, 0.175))
# Plot a shadow
# mf_shadow(france, add = TRUE)
# plot municipalities
mf_map(tmp,
       var = thevar(),
       type = "choro", add = TRUE,
       leg_pos = "topleft", leg_title = "",
       breaks = brks, pal = pal,
       border = brd.col, lwd = brd.lwd, leg_val_rnd = 0,
       col_na = colNA, 
       leg_no_data = "pas de données"
)

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
}


# mf_map(tmp,
#        var = thevar(),
#        type = "choro", add = TRUE,
#        leg_pos = "topleft", leg_title = "BLABLA",
#        pal = pal, breaks = brks, 
#        leg_pos = "topleft", leg_title = "")

#mf_title(txt = format(as.Date(input$thedate), "%d/%m/%Y"))

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

    mf_title(txt = paste0("Pourcentage de vaccination ", v2, ", ", v1, "
au ", format(as.Date(input$thedate), "%d/%m/%Y"), ", par lieu de résidence"
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


    #-----------------------------------------------------------------------
    ### INSETS ###
    
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
    cex.title.inset <- 0.6 # cex of the inset title
    cex.title.inset.2 <- 0.7 # cex of the inset title, communes
    
    
    if(input$DROM == "TRUE"){
    ## Guadeloupe ##
    
    mf_inset_on(fig = c(0, insetSize, y0 + 2*insetSize + 2*dxy, y0 + 3*insetSize + 2*dxy))
    # Merge map and data
    tmp <- merge(guadeloupe, subEPCI, by.x = "SIREN_EPCI", by.y = "epci")
    # Plot map
    
    mf_theme(mar = mar.inset)
    mf_init(tmp, expandBB = BB.inset)
    mf_map(tmp, type = "choro", 
           var = thevar(),
           breaks = brks, pal = pal, 
           border = brd.col, lwd = brd.lwd, 
           leg_pos = "n", 
           col_na = colNA
    )
    mf_title("Guadeloupe", tab = TRUE, inner = FALSE, cex = cex.title.inset, fg = 1, bg = gray(0, 0), line = line.title, pos = "center")
    
    # add a frame
    box(lwd = box.lwd, col = box.col)
    mf_inset_off()
    
    
    ## Martinique ##
    
    mf_inset_on(fig = c(0, insetSize, y0 + 1*insetSize + 1*dxy, y0 + 2*insetSize + 1*dxy))
    # Merge map and data
    tmp <- merge(martinique, subEPCI, by.x = "SIREN_EPCI", by.y = "epci")
    # Plot map
    mf_theme(mar = mar.inset)
    mf_init(tmp, expandBB = BB.inset)
    mf_map(tmp, type = "choro", 
           var = thevar(),
           breaks = brks, pal = pal, 
           border = brd.col, lwd = brd.lwd, 
           leg_pos = "n", 
           col_na = colNA
    )
    mf_title("Martinique", tab = TRUE, inner = FALSE, cex = cex.title.inset, fg = 1, bg = gray(0, 0), line = line.title, pos = "center")
    # add a frame
    box(lwd = box.lwd, col = box.col)
    mf_inset_off()
    
    
    ## Guyane ## 
    
    mf_inset_on(fig = c(0, insetSize, y0 + 0*insetSize + 0*dxy, y0 + 1*insetSize + 0*dxy))
    # Merge map and data
    tmp <- merge(guyane, subEPCI, by.x = "SIREN_EPCI", by.y = "epci")
    # Plot map
    mf_theme(mar = mar.inset)
    mf_init(tmp, expandBB = BB.inset)
    mf_map(tmp, type = "choro", 
           var = thevar(),
           breaks = brks, pal = pal, 
           border = brd.col, lwd = brd.lwd, 
           leg_pos = "n", 
           col_na = colNA
    )
    mf_title("Guyane", tab = TRUE, inner = FALSE, cex = cex.title.inset, fg = 1, bg = gray(0, 0), line = line.title, pos = "center")
    # add a frame
    box(lwd = box.lwd, col = box.col)
    mf_inset_off()
    
    
    ## Mayotte ##
    
    mf_inset_on(fig = c(insetSize + dxy, 2*insetSize + dxy, y0 + 1*insetSize + 1*dxy, y0 + 2*insetSize + 1*dxy))
    # Merge map and data
    tmp <- merge(mayotte, subEPCI, by.x = "SIREN", by.y = "epci")
    # Plot map
    mf_theme(mar = mar.inset)
    mf_init(tmp, expandBB = BB.inset)
    mf_map(tmp, type = "choro", 
           var = thevar(),
           breaks = brks, pal = pal, 
           border = brd.col, lwd = brd.lwd, 
           leg_pos = "n", 
           col_na = colNA
    )
    mf_title("Mayotte", tab = TRUE, inner = FALSE, cex = cex.title.inset, fg = 1, bg = gray(0, 0), line = line.title, pos = "center")
    # add a frame
    box(lwd = box.lwd, col = box.col)
    mf_inset_off()
    
    
    ## La Réunion ##
    
    mf_inset_on(fig = c(insetSize + dxy, 2*insetSize + dxy, y0 + 0*insetSize + 0*dxy, y0 + 1*insetSize + 0*dxy))
    # Merge map and data
    tmp <- merge(reunion, subEPCI, by.x = "SIREN_EPCI", by.y = "epci")
    # Plot map
    mf_theme(mar = mar.inset)
    mf_init(tmp, expandBB = BB.inset)
    mf_map(tmp, type = "choro", 
           var = thevar(),
           breaks = brks, pal = pal, 
           border = brd.col, lwd = brd.lwd, 
           leg_pos = "n", 
           col_na = colNA
    )
    mf_title("La Réunion", tab = TRUE, inner = FALSE, cex = cex.title.inset, fg = 1, bg = gray(0, 0), line = line.title, pos = "center")
    # add a frame
    box(lwd = box.lwd, col = box.col)
    mf_inset_off()
    
        
    }# End inset DROM
    
    ### Insets communes ###
    
    #lines(0, 0, col = 2)
    
    if(input$communes == "TRUE"){
    insetSize2 <- 0.225
    dxy2 <- 0.025
    
    
    ## Paris ## 
    
    deps <- c("75", "91", "92", "93", "94", "95")
    
    mf_inset_on(fig = c(1 - insetSize2, 1, 1 - insetSize2, 1))
    # Subset of departements for Ile-de-France
    tmp <- merge(tca, subCom[is.element(substr(subCom$commune_residence, 1, 2), deps), ], by.x = "insee", by.y = "commune_residence")
    dim(tmp)
    # Plot map
    mf_theme(mar = mar.inset)
    mf_map(tmp, type = "choro", 
           var = thevar(),
           breaks = brks, pal = pal, 
           border = brd.col, lwd = brd.lwd, 
           leg_pos = "n", 
           col_na = colNA
    )
    mf_title("Paris et autour", tab = TRUE, inner = FALSE, cex = cex.title.inset.2, fg = 1, bg = gray(0, 0), line = line.title.2, pos = "center")
    box(lwd = box.lwd, col = box.col)
    mf_inset_off()
    
    
    ## Lyon ##
    
    deps <- c("69")
    
    mf_inset_on(fig = c(1 - insetSize2, 1, 1 - 2*insetSize2 - dxy2, 1 - insetSize2 - dxy2))
    # Subset of departements for Lyon
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
    mf_title("Lyon et autour", tab = TRUE, inner = FALSE, cex = cex.title.inset.2, fg = 1, bg = gray(0, 0), line = line.title.2, pos = "center")
    box(lwd = box.lwd, col = box.col)
    mf_inset_off()
    
    
    ## Marseille ##
    
    deps <- c("13", "83", "84")
    
    mf_inset_on(fig = c(1 - insetSize2, 1, 1 - 3*insetSize2 - 2*dxy2, 1 - 2*insetSize2 - 2*dxy2))
    # Subset of departements for Marseille
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
    mf_title("Marseille et autour", tab = TRUE, inner = FALSE, cex = cex.title.inset.2, fg = 1, bg = gray(0, 0), line = line.title.2, pos = "center")
    box(lwd = box.lwd, col = box.col)
    mf_inset_off()
    
    } # End inset communes
    #-----------------------------------------------------------------------

    
        }) # End plot map

})
