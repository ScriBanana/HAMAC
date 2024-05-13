
#  SENEGAL CATTLE GPS DATA
#  PLOTS - Freely adapted from moveHMM source code
#  Arthur SCRIBAN - JANVIER 2024


library(moveHMM)
library(ggplot2)


setwd("/home/scriban/Dropbox/Thèse/DonneesEtSauvegardes/WorkspaceR/HAMAC")
# setwd("D:/USERS/SergeEtArthur/WorkspaceR/hamac")

rm(list=ls())

addDensityPlot <- function (
    m, distxmax, distymax, angymax,
    stepylab, angylab, graphtitle, dispLegend = FALSE
    ) {
  
  nbStates <- ncol(m$mle$stepPar)
  animals <- NULL
  ask <- FALSE
  breaks <- "Sturges"
  col <- NULL
  plotTracks <- TRUE
  plotCI <- TRUE
  alpha <- 0.95
  
  getPalette <- function(nbStates) {
    if(nbStates < 8) {
      # color-blind friendly palette
      pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      col <- pal[1:nbStates]
    } else {
      # to make sure that all colours are distinct (emulate ggplot default palette)
      hues <- seq(15, 375, length = nbStates + 1)
      col <- hcl(h = hues, l = 65, c = 100)[1:nbStates]
    }
    return(col)
  }
  
  # prepare colours for the states (used in the maps and for the densities)
  if(is.null(col) | (!is.null(col) & length(col) != nbStates)) {
    col <- getPalette(nbStates = nbStates)
  }
  
  #################################
  ## State decoding with Viterbi ##
  #################################
  if(nbStates > 1) {
    cat("Decoding states sequence... ")
    states <- viterbi(m)
    cat("DONE\n")
  } else {
    states <- rep(1,nrow(m$data))
  }
  
  ########################################
  ## Plot state-dependent distributions ##
  ########################################
  par(mar = c(5, 4, 4, 2) - c(0, 0, 2, 1)) # bottom, left, top, right
  par(ask = ask)
  
  distData <- getPlotData(m = m, type = "dist")
  
  # setup line options
  legText <- c(paste("state", 1:nbStates), "total")
  # legText <- c("Resting", "Grazing", "Travelling", "Total")
  lty <- c(rep(1, nbStates), 2)
  lwd <- c(rep(1, nbStates), 2)
  lineCol <- c(col, "black")
  
  # define ymax for step histogram
  h <- hist(m$data$step, plot = FALSE, breaks = breaks)
  ymax <- 1.3 * max(h$density)
  maxdens <- max(distData$step$total)
  if(maxdens > ymax & maxdens < 1.5 * ymax) {
    ymax <- maxdens
  }
  breaks <- "Sturges"
  
  
  # step length histogram
  hist(m$data$step, ylim = c(0, distymax), xlim = c(0, distxmax),
       prob = TRUE, main = "",
       xlab = "Step length (km)",
       ylab = stepylab,
       col = "lightgrey", border = "white",
       breaks = breaks)
  for(i in 1:(nbStates + 1)) {
    lines(distData$step$step, distData$step[,i+1], col = lineCol[i],
          lty = lty[i], lwd = lwd[i])
  }
  if (dispLegend) {
    legend("top", legText, lwd = lwd, col = lineCol, lty = lty, bty = "n")
  }
  
  # define ymax and breaks for angle histogram
  h1 <- hist(m$data$angle, plot = FALSE, breaks = breaks)
  breaks <- seq(-pi, pi, length = length(h1$breaks))
  h2 <- hist(m$data$angle, plot = FALSE, breaks = breaks)
  ymax <- 1.3 * max(h2$density)
  
  # turning angle histogram
  hist(m$data$angle, ylim = c(0, angymax), prob = TRUE,
       xlab = "Turning angle (rad)",
       ylab = angylab,
       main = graphtitle,
       col = "lightgrey", border = "white",
       breaks = breaks, xaxt = "n")
  axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
       labels = expression(-pi, -pi/2, 0, pi/2, pi))
  for(i in 1:(nbStates + 1)) {
    lines(distData$angle$angle, distData$angle[,i+1], col = lineCol[i],
          lty = lty[i], lwd = lwd[i])
  }
  # legend("top", legText, lwd = lwd, col = lineCol, lty = lty, bty = "n")
}


### Figure

cheminSorties <- "./2_Fits_outputs/"
m <- readRDS(paste0(cheminSorties ,"240213150956-HAMAC-SN-ModHMM-3Et.rds"))

# À calibrer
distxmax <- 1.0
distymax <- 8
angymax <- 0.28

png(paste0("/home/scriban/Dropbox/Thèse/Productions/Articles/Mobi/Figures/",
  format(Sys.time(), format = "%y%m%d"), "-DensiPlot.png"),
  width = 1000, height = 500)

par(mfcol = c(2, 3))
addDensityPlot(
  readRDS(paste0(cheminSorties ,"240213150956-HAMAC-SN-ModHMM-3Et.rds")),
  distxmax, distymax, angymax, "Density", "Density", "Global HMM"
  )
addDensityPlot(
  readRDS(paste0(cheminSorties ,"240131195610-HAMAC-SN-ModHMM-3EtTranshu.rds")),
               distxmax, distymax, angymax, "", "", "Transhumant herds", TRUE
)
addDensityPlot(
  readRDS(paste0(cheminSorties ,"240201190315-HAMAC-SN-ModHMM-3EtResid.rds")),
               distxmax, distymax, angymax, "", "", "Resident herds"
)

dev.off()

