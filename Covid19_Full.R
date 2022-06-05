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
covid19 <- read.csv("covid_19_clean_complete.csv") # Import raw data to covid19 dataframe

covid19 <- as_tibble(covid19) # make it tibble

covid19$Date <- as.Date(covid19$Date, format = "%m/%d/%Y") # Change date format for make it compatable with R 

covid19 <- covid19 %>% rename(Country = Country.Region) # I don't want variable to contain [dot] So I rename it

covid19_Total <- covid19 %>% 
  arrange(Date)

# Just in case of world wide data -- leave it alone

covid19 <- covid19 %>%
  arrange(Date) %>%
  arrange(Country) %>%
  group_by(Country) %>%
  mutate(Infected = Confirmed-Recovered)

# rearrabge data in covid19 dataframe and make it sorted by date then country
# and group country togather
# then create new column (Mutate) named "Infected" to represent amount of people who currently infected covid-19

US <- covid19 %>%
  filter(Country == "Thailand") %>%
  gather(key = Status , value = Value , c(Confirmed,Recovered,Infected,Deaths)) %>% ## view(covid19)
  ggplot(mapping=aes(Date,Value, color = Status))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = c( "#36b5b3","black","#b53e36","#52b536"))

# selected country (for visualize)
# change type of data from wide to narrow
# plot graph and assign color to each data (contained Comfirmed case, Recovered case, Currently infected case, and Death case)

ggplotly(US) #display ploted graph

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

# leave it alone


# test commit 1


# comment 2


# branch 1