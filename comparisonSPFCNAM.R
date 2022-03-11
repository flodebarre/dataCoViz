# Initializations ####
library("MetBrewer")


# Load data ####
if(!exists("vaccEPCI")){
  vaccEPCI <- read.csv("data/vaccEPCI.csv", sep = ";")
}

thedate <- max(vaccEPCI$date)

# Compute proportions of vaccinated people at a given date, by age class
getProps <- function(thedate){
  tmp <- vaccEPCI[vaccEPCI$date == thedate & vaccEPCI$classe_age != "TOUT_AGE", ]
  tmp$pop <- as.numeric(tmp$population_carto)
  tmp$n1 <- as.numeric(tmp$effectif_cumu_1_inj)
  tmp$n2 <- as.numeric(tmp$effectif_cumu_termine)
  
  dim(tmp)
  sum(tmp$pop, na.rm = TRUE)
  
  agg <- aggregate(tmp[, c("n1", "n2", "pop")], by = list(clAge = tmp$classe_age), FUN = sum, na.rm = TRUE)
  agg$p1 <- agg$n1 / agg$pop
  agg$p2 <- agg$n2 / agg$pop
  agg$date <- thedate
  agg
}

# Compute global mean vaccination rate among adults (20+)
tmp <- vaccEPCI[vaccEPCI$date == thedate & vaccEPCI$classe_age != "TOUT_AGE"& vaccEPCI$classe_age != "00-19", ]
sum(as.numeric(tmp$effectif_cumu_1_inj), na.rm = TRUE)/sum(as.numeric(tmp$population_carto), na.rm = TRUE)

# Compute vaccination rates by age
out <- getProps(max(vaccEPCI$date))
for(dt in sort(unique(vaccEPCI$date), decreasing = TRUE)[-1]){
  out <- rbind(out, getProps(dt))
}
out$date <- as.Date(out$date)


# Define colors for the different age classes
agCls <- sort(unique(out$clAge))
palAg <- met.brewer("Isfahan2", length(agCls), "continuous")
names(palAg) <- sort(agCls)

rdt <- range(out$date)


#............................................................
# PLOT 1: COVERAGE OVER TIME, CNAM DATA ####
wpng <- 8
hpng <- 4.5
respng <- 300
marpng <- c(5, 4, 3, 4)

fname1 <- "../covid_vaccination/pics/vaccCNAMOverTime.png"

png(filename = fname1, width = wpng, height = hpng, units = "in", res = respng)
par(las = 1, mar = marpng)
plot(0, xlim = rdt, ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "Prop. vaccinés")
dtt <- 7
axis(2, lwd = 0, at = 100 * seq(0, 1, by = 0.1), pos = rdt[1] - dtt, lwd.ticks = 1)
axis(4, lwd = 0, at = 100 * seq(0, 1, by = 0.1), pos = rdt[2] + dtt, lwd.ticks = 1)
for(i in seq(0, 1, by = 0.1)) abline(h = 100 * i, col = gray(0.9), lwd = 1)
for(i in seq(0, 1, by = 0.1) - 0.05) abline(h = 100 * i, col = gray(0.9), lwd = 0.5)
mnths <- seq(as.Date("2020-12-27"), Sys.Date() - 7, by = "month")
axis(1, at = mnths, labels = rep("", length(mnths)), pos = 0)
axis(1, at = mnths, labels = format(mnths, "%b\n%Y"), lwd = 0)


for(ag in agCls){
  tmpp <- out[out$clAge == ag, ]
  points(tmpp$date, 100 * tmpp$p1, type = "l", 
         col = palAg[ag], lwd = 2)
}

legend("topleft", col = rev(palAg), legend = rev(names(palAg)), lty = 1, title = "Classe d'âge", inset = c(0.01, 0), box.lwd = 0)

title(main = "Vaccination au moins une dose")
mtext(side = 1, text = paste0("@flodebarre, ", Sys.Date(), "\n",
                          "Données vaccination : Assurance Maladie https://datavaccin-covid.ameli.fr/"), col = gray(0.5), adj = 0, family = "mono", cex = 0.7, line = 3)
dev.off()
system(paste0("open ", fname1))
#............................................................

maxAge <- 95
# Compute finale proportions
datCNAM <- getProps(max(vaccEPCI$date))
datCNAM$agMin <- as.numeric(substr(datCNAM$clAge, 1, 2))
datCNAM$agMax <- as.numeric(substr(datCNAM$clAge, 4, 5))
datCNAM$agMax[which(datCNAM$clAge == "75 et +")] <- maxAge


# SPF DATA ####
URL <- "https://www.data.gouv.fr/fr/datasets/r/dc103057-d933-4e4b-bdbf-36d312af9ca9"
fileSPF <- tempfile()
download.file(URL, fileSPF)
datSPF <- read.csv(fileSPF, sep = ";")
# Print global vaccination rate, all ages
datSPF[datSPF$clage_vacsi == 0, ]
# Print vaccination rates by age
datSPF <- datSPF[datSPF$clage_vacsi > 0, ]

# Define extremes of the age class
agMin <- c(0, 5, 10, 12, 18, 25, 30, 40, 50, 60, 65, 70, 75, 80)
agMax <- c(4, 9, 11, 17, 24, 29, 39, 49, 59, 64, 69, 74, 79, maxAge)
names(agMin) <- c("4", "9", "11", "17", "24", "29", "39", "49", "59", "64", "69", "74", "79", "80")
names(agMax) <- names(agMin)

datSPF$agMax <- agMax[as.character(datSPF$clage_vacsi)]
datSPF$agMin <- agMin[as.character(datSPF$clage_vacsi)]

#...............................................................
# PLOT 2: Comparison SPF and CNAM ####
fname2 <- "../covid_vaccination/pics/vaccComparisonCNAMSPF.png"
png(filename = fname2, width = wpng, height = hpng, units = "in", res = respng)
par(las = 1, mar = marpng)
lwdd <- 3.5
plot(0, type = "n", xlim = c(0, max(agMax)), ylim = c(0, 100), 
     frame.plot = FALSE, 
     xlab = "", ylab = "Proportion au moins une dose", 
     axes = FALSE, xaxs = "i", yaxs = 'i')
axis(2, lwd = 0, pos = 0, lwd.ticks = 1, at = seq(0, 100, by = 10))
axis(1, at = seq(0, 100, by = 10), lwd = 0, lwd.ticks = 1, pos = 0)
mtext(side = 1, text = "ages", line = 2)
for(i in seq(0, 100, by = 10)) abline(h = i, col = gray(0.8))
for(i in seq(0, max(agMax))) abline(v = i, col = gray(0.9))
pal <- met.brewer("Cassatt1", 2, "discrete")
colSPF = pal[1]
for(i in seq_len(nrow(datSPF))){
  arrows(datSPF[i, "agMin"], datSPF[i, ]$couv_tot_dose1, 
         datSPF[i, "agMax"], datSPF[i, ]$couv_tot_dose1, 
         code = 0, col = colSPF, lwd = lwdd, lend = "butt")
}

colCNAM <- pal[2]
for(i in seq_len(nrow(datCNAM))){
  arrows(datCNAM[i, "agMin"], 100 * datCNAM[i, ]$p1, 
         datCNAM[i, "agMax"], 100 * datCNAM[i, ]$p1, 
         code = 0, col = colCNAM, lwd = lwdd, lend = "butt")
}

legend("bottomright", col = pal, legend = c("SPF", "Assurance Maladie"), lwd = 3, lty = 1, box.lwd = 0, inset = c(0, 0.05))

title(main = "Comparaison couvertures vaccinales SPF vs. Assurance Maladie")

mtext(side = 1, text = paste0("@flodebarre, ", Sys.Date(), "\n",
                              "Données vaccination : 
- Assurance Maladie https://datavaccin-covid.ameli.fr/
- SPF https://www.data.gouv.fr/en/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/"), col = gray(0.5), adj = 0, family = "mono", cex = 0.6, line = 4)

dev.off()
system(paste0("open ", fname2))
#...............................................................

sum(datSPF[datSPF$agMin > 17, "n_tot_dose1"]) / sum(datSPF[datSPF$agMin > 17, "pop"])
