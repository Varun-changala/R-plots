library(ggplot2)
library(tidyverse)
library(readr)
library(maps)
library(dplyr)

getwd()
setwd('Z://R')
maindata <- read.csv("Minimum Wage Data old.csv")
colnames(maindata)
maindata <- select(maindata,Year,State,State.Minimum.Wage,Federal.Minimum.Wage,Effective.Minimum.Wage,CPI.Average)
head(maindata)
maps<-map_data("state")
Data1<-maindata
Data1$Term<-NA
for(i in 1:nrow(Data1))
{
  if(Data1$Year[i]>=1968 && Data1$Year[i]<=1980)
  {
    Data1$Term[i]<-"1968-1983"
  }
  else if(Data1$Year[i]>=1981 && Data1$Year[i]<=1993)
  {
    Data1$Term[i]<-"1987-1993"
  }
  else if(Data1$Year[i]>=1994 && Data1$Year[i]<=2006)
  {
    Data1$Term[i]<-"1994-2006"
  }
  else
  {
    Data1$Term[i]<-"2007-2020"
    }
}
Data1<-Data1 %>%
  group_by(State,Term) %>%
  summarise(AVG=mean(State.Minimum.Wage, na.rm=TRUE))
Data1$region<-tolower(Data1$State)
mapwage<-inner_join(maps,Data1,by="region")
ggplot()+geom_polygon(mapwage,mapping = aes(x=long,y=lat,group=group,fill=AVG),
                      color="white")+facet_wrap(~Term)+
  labs(title = "Term wise Average Minimum Wage for each State from 1968-2020",
       x="Longitude",
       y="Latitude") +
  scale_fill_continuous(name="Minimum Wage", type = "viridis")
