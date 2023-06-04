#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: RIVOLUZIONE ENERGETICA: ANALISI DELLE FONTI E DEI CONSUMI PER UN FUTURO SOSTENIBILE
#' 
#' BY:ALESSANDRO TROIANO
#' 
#' SCRIPT:Script che permette di creare un grafico che mostra l'andamento dei consumi nel periodo 2000-2019 delle 
#' seguenti nazioni:Italia, Germania, Spagna, Francia, Regno Unito, Turchia, Paesi Bassi e Russia
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

#'the data are filtered by year and by Nation,
#'then we select the right column and rename few variables

energy_consuption_per_year_cleaned <- read_csv("energy consumption per year.csv")%>%
  rename("Energy_consumption"="Primary energy consumption (TWh)")%>%
  rename("Nazioni"="Entity")%>%
  select(-Code)%>%
  filter(Year>=2000&Year<=2019)%>%
  filter(Nazioni=="Italy" | 
           Nazioni=="Germany" |
           Nazioni=="United Kingdom" | 
           Nazioni=="France" |
           Nazioni=="Spain" | 
           Nazioni=="Russia" | 
           Nazioni=="Netherlands"|
           Nazioni=="Turkey" )

#'we calculate the average of the consume of all the selected Contry in the considered period
#'then we add the new value to the previous dataset

for (i in 2000:2019) {
  row<-as.numeric(summarise_all(energy_consuption_per_year_cleaned%>%filter(Year==i),mean)%>%select(Energy_consumption));
  energy_consuption_per_year_cleaned <- add_row(energy_consuption_per_year_cleaned, Nazioni="Average",Year=i, Energy_consumption=row)
}


#DATA VISUALIZATION

#'we use the ggplot2 library to visualize the consume of every Nation through a line chart

ggplot(energy_consuption_per_year_cleaned, mapping = aes(x=Year, y=Energy_consumption, color=Nazioni))+
  geom_line(size=1.25)+
  geom_point(energy_consuption_per_year_cleaned, mapping = aes(x=Year, y=Energy_consumption, color=Nazioni))+
  labs(x="Anni",y="Energia consumata nell'anno (in Twh)",title="Energia consumata da ogni nazione anno per anno") +
  scale_y_continuous(breaks = seq(100, 10000, by = 200))+
  scale_x_continuous(breaks = seq(2000,2019, by=1))+
  theme_minimal()

