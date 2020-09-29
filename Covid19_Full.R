install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("tidyr")
install.packages("scales")
install.packages("reshape2")
install.packages("ploty")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(tidyr)
library(scales)
library(reshape2)
library(plotly)

setwd("C:/Users/Akkapop/Documents/RProject")
#import COVID-19 data
covid19 <- read.csv("covid_19_clean_complete.csv")

covid19 <- as_tibble(covid19)

covid19$Date <- as.Date(covid19$Date, format = "%m/%d/%Y")

covid19 <- covid19 %>% rename(Country = Country.Region)

covid19_Total <- covid19 %>%
  arrange(Date)

covid19 <- covid19 %>%
  arrange(Date) %>%
  arrange(Country) %>%
  group_by(Country) %>%
  mutate(Infected = Confirmed-Recovered)

US <- covid19 %>%
  filter(Country == "Thailand") %>%
  gather(key = Status , value = Value , c(Confirmed,Recovered,Infected,Deaths)) %>% ## view(covid19)
  ggplot(mapping=aes(Date,Value, color = Status))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c( "#36b5b3","black","#b53e36","#52b536"))

ggplotly(US)

US
attach(covid19)
confirmed.lm <- lm(Confirmed ~ Date)
newdata = data.frame(waiting=80)
predict(confirmed.lm, newdata, interval="predict") 
  #geom_point(aes(totalInfected,colour =  factor(totalInfected))) +
 # geom_smooth()+
 # geom_point(aes(totalDeath,col=totalDeath)) +
 # geom_smooth()+
  #geom_point(aes(totalRecovered,col=totalRecovered)) +
  #geom_smooth()

