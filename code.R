setwd("C:/Users/ldr/Desktop/capstone/AFG_district_398_noNA")
install.packages( "stargazer") #print table as latex form
library(stargazer) 
library(sf)
library(tmap)
library(tidyverse)
library(ggplot2)
library(dplyr)

#Map
data1<-st_read("district398.shp")
plot(st_geometry(data1))
data2<-read.csv("afgdata4capstone.csv")
afg<-merge(data1,data2,by="DISTID")
afg1<-afg %>% mutate(a2s=accessible2survey)

afg1 %>%
  ggplot() +
  geom_sf(aes(fill = a2s))

afg %>%
  ggplot() +
  geom_sf(aes(fill = revenue)) 
 
#count state capacity
afg %>%
  count(accessible2survey,sort=1)
##311/398=78.1407% 

#Regression
##linear regression
plot(y= afg$accessible2survey,x=afg$revenue,ann = "n")
title(xlab= 'revenue', ylab = 'accessible2survey')
boxplot(revenue~accessible2survey,data=afg,main="Recency")#boxplot is better for binary variable

model_lm<-lm(accessible2survey ~ 
               revenue,data = afg)
abline(model_lm,col="orange")
summary(model_lm)#the statistic description of the regression
plot(model_lm)

##probit model
model_p<-glm(accessible2survey ~ revenue,family=binomial(link=probit),data=afg)
summary(model_p)
plot(model_p)

##logit model
model_lo<-glm(accessible2survey ~ revenue,family=binomial(link=logit),data=afg)
summary(model_lo)
plot(model_lo)
