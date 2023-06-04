#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: RIVOLUZIONE ENERGETICA: ANALISI DELLE FONTI E DEI CONSUMI PER UN FUTURO SOSTENIBILE
#' 
#' BY:ALESSANDRO TROIANO
#' 
#' SCRIPT:Script che permette di creare un grafico a barre che mostra i consumi di energia su scala globale nel periodo preso in considerazione, 
#' comparandoli con quelli derivati da fonti energetiche fossili
#'  
#'  
#'  
#'
#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


#SETTING UP THE ENVIRONMENT

#cleaning of the environment and setting of working directory
rm(list=ls())

setwd("Scrivania/Università/3° anno/II semestre/business data analytics/project/Resources")

#INSTALLING OF PACKAGES AND IMPORTING OF THEM
install.packages("tidyverse")
library("tidyverse")
library(readr)
library("dplyr")
library("tibble")


#IMPORTING DATA, PREPROCESSING AND TYDIFICATION 

#'the data are filtered by year,
#'then we select the right columns and rename few variables

Global_fossil_fuel_consumption <- read_csv("Fossil fuel consumption: which countries use the most energy from fossil fuels.csv")%>%
  select(-Code)%>%
  filter(Year>=2000&Year<=2019)%>%
  rename("Nazioni"="Entity")%>%
  rename("Anno"="Year")%>%
  rename("Fossile"="Fossil fuels (TWh)")


#'the data are filtered by year and by nation,
#'then we select the right columns and rename few variables

energy_consumption_per_year <- read_csv("energy consumption per year.csv")%>%
  select(-Code)%>%
  filter(Year>=2000&Year<=2019)%>%
  rename("Nazioni"="Entity")%>%
  rename("Anno"="Year")%>%
  rename("Totale"="Primary energy consumption (TWh)")%>%
  filter(Nazioni=="World")

#'we create a new dataframe on which we will create the plot, through the merge function 
#'and through a pivot_longer function that is used to make the dataframe tidy

grafico1<-merge(Global_fossil_fuel_consumption,energy_consumption_per_year)%>%
  pivot_longer(cols = c("Fossile","Totale"),names_to = "Fonte", values_to = "Energia_consumata_in_TWh")


#DATA VISUALIZATION

#'we use the ggplot2 library to visualize the average consume of every citizen of Italy,Europe and world in the period: 2000-2019, through a line chart

ggplot(grafico1, mapping = aes(x=Anno, y=Energia_consumata_in_TWh, color=Fonte, fill=Fonte))+
  geom_col(position = position_dodge(width = 0.5))+
  theme_minimal()+
  scale_y_continuous(breaks = seq(0, 165000, by = 5000))+
  scale_x_continuous(breaks = seq(2000,2019, by=1))+
  labs(x="Anni",y="Energia consumata(in kwh)",title="Energia consumata globalmente da fonti fossili e complessivamente ")
