#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: RIVOLUZIONE ENERGETICA: ANALISI DELLE FONTI E DEI CONSUMI PER UN FUTURO SOSTENIBILE
#' 
#' BY:ALESSANDRO TROIANO
#' 
#' SCRIPT:Script che permette di creare diversi grafici a torta che mostrano, per ogni paese analizzato, 
#' le principali fonti energetiche utilizzate nell'anno 2019
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

#'the data are filtered by year and by Code(some continent are recorded with a NA code),
#'then we select the right columns and rename few variables

Fossil_fuel_consumption <- read_csv("fossil fuel consumption by fuel type country per country.csv")%>%
  rename("Nazioni"="Entity")%>%
  rename("Anno"="Year")%>%
  rename("gas"="Gas Consumption - TWh")%>%
  rename("carbone"="Coal Consumption - TWh")%>%
  rename("petrolio"="Oil Consumption - TWh")%>%
  filter(Anno==2019)%>%
  drop_na()%>%
  filter(Nazioni=="Italy"|
           Nazioni=="Germany" |
           Nazioni=="United Kingdom" |
           Nazioni=="France" |
           Nazioni=="Spain" |
           Nazioni=="Russia" |
           Nazioni=="Netherlands"|
           Nazioni=="Turkey" )%>%
  select(-Code)



Renewable_energy_consumption <- read_csv("Modern renewable energy generation by source, World .csv")%>%
  rename("Nazioni"="Entity")%>%
  rename("Anno"="Year")%>%
  rename("eolica"="Electricity from wind (TWh)")%>%
  rename("idroelettrica"="Electricity from hydro (TWh)")%>%
  rename("solare"="Electricity from solar (TWh)")%>%
  rename("altro"="Other renewables including bioenergy (TWh)")%>%
  filter(Anno==2019)%>%
  drop_na()%>%
  filter(Nazioni=="Italy"|
           Nazioni=="Germany" |
           Nazioni=="United Kingdom" |
           Nazioni=="France" |
           Nazioni=="Spain" |
           Nazioni=="Russia" |
           Nazioni=="Netherlands"|
           Nazioni=="Turkey" )%>%
  select(-Code)




Nuclear_energy_consumption <- read_csv("Nuclear energy consumption by country.csv")%>%
  rename("Nazioni"="Entity")%>%
  rename("nucleare"="Nuclear (TWh - equivalent)")%>%
  rename("Anno"="Year")%>%
  filter(Anno==2019)%>%
  drop_na()%>%
  filter(Nazioni=="Italy"|
           Nazioni=="Germany" |
           Nazioni=="United Kingdom" |
           Nazioni=="France" |
           Nazioni=="Spain" |
           Nazioni=="Russia" |
           Nazioni=="Netherlands"|
           Nazioni=="Turkey" )%>%
  select(-Code)


#' after we create 3 different dataframes, we merge them into a new one,
#'  which we want to make tidy.

dataframe<-merge(Fossil_fuel_consumption, merge(Nuclear_energy_consumption,Renewable_energy_consumption))%>%
  pivot_longer(cols = c("carbone","petrolio","gas","nucleare","eolica","idroelettrica","solare","altro"), names_to = "Fonte", values_to = "Energia_consumata_in_TWh")


#' we create a different vector for every of the analyzed nation, 
#' filtering the dataframe gotten at the previous point by nation


Italy<-filter(dataframe, Nazioni=="Italy")
Germany<-filter(dataframe, Nazioni=="Germany")
France<-filter(dataframe, Nazioni=="France")
Spain<-filter(dataframe, Nazioni=="Spain")
Turkey<-filter(dataframe, Nazioni=="Turkey")
UK<-filter(dataframe, Nazioni=="United Kingdom")
Russia<-filter(dataframe, Nazioni=="Russia")
Netherlands<-filter(dataframe, Nazioni=="Netherlands")


#DATA VISUALIZATION



#' finally we plot a pie chart for every analyzed nation, 
#' that shows the main energetic source for the 2019,
#' through the ggplot2 library


ggplot(Italy,mapping = aes(x=Anno, y=Energia_consumata_in_TWh ,color=Fonte, fill=Fonte))+
  labs(title = "Energia consumata nel 2019 fonte per fonte in Italia",x="",y="")+
  theme_void()+
  geom_col()+
  coord_polar(theta = "y")




ggplot(Germany,mapping = aes(x=Anno, y=Energia_consumata_in_TWh ,color=Fonte, fill=Fonte))+
  labs(title = "Energia consumata nel 2019 fonte per fonte in Germania",x="",y="")+
  theme_void()+
  geom_col()+
  coord_polar(theta = "y")



ggplot(France,mapping = aes(x=Anno, y=Energia_consumata_in_TWh ,color=Fonte, fill=Fonte))+
  labs(title = "Energia consumata nel 2019 fonte per fonte in Francia",x="",y="")+
  theme_void()+
  geom_col()+
  coord_polar(theta = "y")



ggplot(Spain,mapping = aes(x=Anno, y=Energia_consumata_in_TWh ,color=Fonte, fill=Fonte))+
  labs(title = "Energia consumata nel 2019 fonte per fonte in Spagna",x="",y="")+
  theme_void()+
  geom_col()+
  coord_polar(theta = "y")


ggplot(Turkey,mapping = aes(x=Anno, y=Energia_consumata_in_TWh ,color=Fonte, fill=Fonte))+
  labs(title = "Energia consumata nel 2019 fonte per fonte in Turchia",x="",y="")+
  theme_void()+
  geom_col()+
  coord_polar(theta = "y")


ggplot(UK,mapping = aes(x=Anno, y=Energia_consumata_in_TWh ,color=Fonte, fill=Fonte))+
  labs(title = "Energia consumata nel 2019 fonte per fonte nel Regno Unito",x="",y="")+
  theme_void()+
  geom_col()+
  coord_polar(theta = "y")


ggplot(Russia,mapping = aes(x=Anno, y=Energia_consumata_in_TWh ,color=Fonte, fill=Fonte))+
  labs(title = "Energia consumata nel 2019 fonte per fonte in Russia",x="",y="")+
  theme_void()+
  geom_col()+
  coord_polar(theta = "y")


ggplot(Netherlands,mapping = aes(x=Anno, y=Energia_consumata_in_TWh ,color=Fonte, fill=Fonte))+
  labs(title = "Energia consumata nel 2019 fonte per fonte nei Paesi Bassi",x="",y="")+
  theme_void()+
  geom_col()+
  coord_polar(theta = "y")





