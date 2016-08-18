# ShinyApp for analysing global terrorism dataset
#
# global.R loads libraries, sets workspace, handles sources
# and is starting the app
#
# Author:  Carlo Michaelis
# License: Attribution-ShareAlike 4.0 International
# Date:    Aug. 2016

library(openxlsx)
library(shiny)
library(ggplot2)
library(countrycode)
library(plotly)
library(RColorBrewer)
library(reshape)
library(dplyr)

# set workspace path for sources and data
setwd("/home/carlo/Entwicklung/R/gtd-shiny/")

# load sources with necessary functions/objects
source("preprocessing.R")

# load prepared data
# if no prepared data is available it will be created using raw data files
# important: if considered variables have changed, delete prepared data files
# before running this function (like cleaning cache)
loadData()

# load sources with ui object and server function
source("ui.R")
source("server.R")

# start shiny app wirh ui and server
shinyApp(ui=ui, server=server)
