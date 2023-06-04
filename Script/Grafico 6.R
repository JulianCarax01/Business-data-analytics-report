#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: RIVOLUZIONE ENERGETICA: ANALISI DELLE FONTI E DEI CONSUMI PER UN FUTURO SOSTENIBILE
#' 
#' BY:ALESSANDRO TROIANO
#' 
#' SCRIPT:Script che permette di creare una griglia di visualizzazione della percentuale 
#' dell'uso di energie da fonti rinnovabili, per ogni paese analizzato, nel periodo 2000-2019
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

#'the data are filtered by year and by nation,
#'then we select the right columns and rename few variables


Share_of_energy_from_renewables <- read_csv("How much of our primary energy comes from renewables .csv")%>%
  rename("Percentual_of_renewable"="Renewables (% equivalent primary energy)")%>%
  rename("Nazioni"="Entity")%>%
  rename("Anno"="Year")%>%
  select(-Code)%>%
  filter(Anno>=2000&Anno<=2019)%>%
  filter(Nazioni=="Italy"|
           Nazioni=="Germany" |
           Nazioni=="United Kingdom" |
           Nazioni=="France" |
           Nazioni=="Spain" |
           Nazioni=="Russia" |
           Nazioni=="Netherlands"|
           Nazioni=="Turkey" )

#DATA VISUALIZATION

#'we use the ggplot2 library to visualize the share of use of green energy for every analyzed country, in the period: 2000-2019, through a grid of line charts

ggplot(Share_of_energy_from_renewables, mapping = aes(x=Anno, y=Percentual_of_renewable, color=Nazioni, fill=Nazioni))+
  geom_col()+
  facet_grid(rows = vars(Nazioni))+
  theme_minimal()+
  scale_y_continuous(breaks = seq(0, 25, by = 5))+
  scale_x_continuous(breaks = seq(2000,2019, by=1))+
  labs(x="Anni",y="Percentuale di energia derivante da fonti rinnovabili",title="Percentuale dell'uso di energie da fonti rinnovabili, per ogni paese, nel periodo 2000-2019")
  
  
  
  
  


