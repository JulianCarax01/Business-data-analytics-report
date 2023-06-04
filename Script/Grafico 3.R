#' --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#' REPORT BUSINESS DATA ANALYTICS
#' 
#' TITOLO: RIVOLUZIONE ENERGETICA: ANALISI DELLE FONTI E DEI CONSUMI PER UN FUTURO SOSTENIBILE
#' 
#' BY:ALESSANDRO TROIANO
#' 
#' SCRIPT:Script che permette di creare un grafico a barre per visualizzare i consumi annui
#' dell'Italia a confornto con quelli del resto d'Europa e del resto del mondo
#'  
#'  
#'  
#'Ps: I'm referring to the geographical Europe
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

energy_consumption_per_person <- read_csv("energy consumption per person.csv")%>%
  rename("Energy_consumption"="Primary energy consumption per capita (kWh/person)")%>%
  rename("Nazioni"="Entity")%>%
  drop_na()%>%
  select(-Code)%>%
  filter(Year>=2000&Year<=2019)

#'then we select just the nation we want to confront with the rest of world and Europe
Italy_energy_consumption_per_person<-filter(energy_consumption_per_person,Nazioni=="Italy")%>%rename("Anno"="Year")

#'we create and empty dataframe with a starting unused record
World_consumption_per_person<-data.frame(Nazioni="",Anno=0,Energy_consumption=0);


#'through a for cicle we calculate the average of the consume of every citizen of all the world in the considered period
#'then we add the new value to the previous dataset

for (i in 2000:2019) {
  value<-as.numeric(summarise_all(energy_consumption_per_person%>%filter(Year==i),mean)%>%select(Energy_consumption));
  World_consumption_per_person<-add_row(World_consumption_per_person,Nazioni="Mondo",Anno=i,Energy_consumption=value)
}

#'we drop the unused record

World_consumption_per_person<-filter(World_consumption_per_person, Anno!=0)

#' we select just the nation of Europe

Europe_energy_consumption_per_person<-filter(energy_consumption_per_person, 
                                             Nazioni=="Italy"|
                                               Nazioni=="Albania"|
                                               Nazioni=="Andorra"|
                                               Nazioni=="Armenia"|
                                               Nazioni=="Austria"|
                                               Nazioni=="Azerbaijan"|
                                               Nazioni=="Belgium"|
                                               Nazioni=="Belarus"|
                                               Nazioni=="Bosnia and Herzegovina"|
                                               Nazioni=="Bulgaria"|
                                               Nazioni=="Cyprus"|
                                               Nazioni=="Croatia"|
                                               Nazioni=="Denmark"|
                                               Nazioni=="Estonia"|
                                               Nazioni=="Finland"|
                                               Nazioni=="France"|
                                               Nazioni=="Georgia"|
                                               Nazioni=="Germany"|
                                               Nazioni=="Greece"|
                                               Nazioni=="Ireland"|
                                               Nazioni=="Iceland"|
                                               Nazioni=="Kazakhstan"|
                                               Nazioni=="Kosovo"|
                                               Nazioni=="Latvia"|
                                               Nazioni=="Lithuania"|
                                               Nazioni=="Luxembourg"|
                                               Nazioni=="North Macedonia"|
                                               Nazioni=="Malta"|
                                               Nazioni=="Moldova"|
                                               Nazioni=="Montenegro"|
                                               Nazioni=="Norway"|
                                               Nazioni=="Netherlands"|
                                               Nazioni=="Poland"|
                                               Nazioni=="Portugal"|
                                               Nazioni=="United Kingdom"|
                                               Nazioni=="Czechia"|
                                               Nazioni=="Romania"|
                                               Nazioni=="Russia"|
                                               Nazioni=="Serbia"|
                                               Nazioni=="Slovakia"|
                                               Nazioni=="Slovenia"|
                                               Nazioni=="Spain"|
                                               Nazioni=="Sweden"|
                                               Nazioni=="Switzerland"|
                                               Nazioni=="Turkey"|
                                               Nazioni=="Ukraine"|
                                               Nazioni=="Hungary")

#'we create and empty dataframe with a starting unused record

Europe_consumption_per_person<-data.frame(Nazioni="",Anno=0,Energy_consumption=0);

#'through a for cicle we calculate the average of the consume of every citizen of all the Europe in the considered period
#'then we add the new value to the previous dataset

for (i in 2000:2019) {
  value<-as.numeric(summarise_all(Europe_energy_consumption_per_person%>%filter(Year==i),mean)%>%select(Energy_consumption));
  Europe_consumption_per_person<-add_row(Europe_consumption_per_person,Nazioni="Europa",Anno=i,Energy_consumption=value)
}

#'we drop the unused record

Europe_consumption_per_person<-filter(Europe_consumption_per_person, Anno!=0)

#'we attach the created dataframes
final_dataframe<-rbind(Italy_energy_consumption_per_person,Europe_consumption_per_person,World_consumption_per_person)

#DATA VISUALIZATION

#'we use the ggplot2 library to visualize the average consume of every citizen of Italy,Europe and world in the period: 2000-2019, through a line chart

ggplot(final_dataframe,mapping=aes(x=Anno, y=Energy_consumption, color=Nazioni, fill=Nazioni))+
  geom_col(position = position_dodge2(width = 0.5, padding=0.1))+
  labs(x="Anni",y="Energia consumata nell'anno per persona (in kwh)",title="Energia media consumata da ogni cittadino di Italia,Europa e Mondo per ogni anno") +
  scale_y_continuous(breaks = seq(0, 100000, by = 1500))+
  scale_x_continuous(breaks = seq(2000,2019, by=1))+
  theme_minimal()

