source("R/date_calculation.R")
source("R/plot_theme.R")
source("R/data_access.R")
source("R/optimization.R")
source("R/output_plot.R")
source("R/output_text.R")

insts <- read.csv("data/output.csv", header = T, sep = ",")
insts[is.na(insts)] <- 0  # NA to 0

rawUps <- NULL
  
valueX <- 8760

priceFile = "data/ec2-price.csv"
priceData <- rawPrice(priceFile)
priceData <- priceData[
    (priceData$Platform == "Linux") & 
    (priceData$Region == "US East (N. Virginia)"), 
    ]

performOptimization(insts, rawUps, valueX, priceData, 
    100, 100, 100,
    100, 100, F)