library(openxlsx)
library(shiny)
library(ggplot2)
library(countrycode)
library(plotly)
library(RColorBrewer)
library(reshape)
library(dplyr)

setwd("/home/carlo/Entwicklung/R/Hausarbeit/")

pasteq <- function(...) paste(..., sep = "")

source("helper.R")

dataLabels <- getDataLabels()

source("preprocessing.R")
source("ui.R")
source("server.R")

shinyApp(ui=ui, server=server)

