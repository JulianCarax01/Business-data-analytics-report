#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: RIVOLUZIONE ENERGETICA: ANALISI DELLE FONTI E DEI CONSUMI PER UN FUTURO SOSTENIBILE
#' 
#' BY:ALESSANDRO TROIANO
#' 
#' SCRIPT:Script che permette di creare un grafico che mostra nel periodo 2000-2019 l'andamento dei consumi medio di ogni cittadino delle 
#' seguenti nazioni :Italia, Germania, Spagna, Francia, Regno Unito, Turchia, Paesi Bassi e Russia
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
#'then we select the right columns and rename few variables

energy_consuption_per_person_cleaned <- read_csv("energy consumption per person.csv")%>%
  rename("Energy_consumption"="Primary energy consumption per capita (kWh/person)")%>%
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
  row<-as.numeric(summarise_all(energy_consuption_per_person_cleaned%>%filter(Year==i),mean)%>%select(Energy_consumption));
  energy_consuption_per_person_cleaned <- add_row(energy_consuption_per_person_cleaned, Nazioni="Average",Year=i, Energy_consumption=row)
}


#DATA VISUALIZATION

#'we use the ggplot2 library to visualize the average consume of every citizen of every Nation in the period: 2000-2019, through a line chart

ggplot(energy_consuption_per_person_cleaned, mapping = aes(x=Year, y=Energy_consumption, color=Nazioni))+
  geom_line(size=1.25)+
  geom_point(energy_consuption_per_person_cleaned, mapping = aes(x=Year, y=Energy_consumption, color=Nazioni))+
  labs(x="Anni",y="Energia consumata nell'anno per persona (in kwh)",title="Energia consumata in media da ogni cittadino di ogni nazione anno per anno") +
  scale_y_continuous(breaks = seq(1000, 80000, by = 1000))+
  scale_x_continuous(breaks = seq(2000,2019, by=1))+
  theme_minimal()
