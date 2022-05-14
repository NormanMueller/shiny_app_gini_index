library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(DT)
library(dplyr)

data <- read.csv(file = 'Shiny_Data2.csv')
data$year <-as.numeric(substr(data$year, 6, 10))
stats <- read.csv(file = 'stats.csv')
geo <- read.csv(file = 'geo.csv')
gdp <- read.csv(file = 'gdp.csv')



