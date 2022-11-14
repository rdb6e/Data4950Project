setwd("C:/Users/ryan1/Documents/Fall 2022 MTSU/ECON 4620")
getwd()

library(tidyverse)
library(dplyr)
library(psych)
library(knitr)
library(pander)
library(ggplot2)
library(gridExtra)
library(scales)
library(ggcorrplot)
library(tinytex)
options(warn=-1)

college<-read.csv("ForbesAmericasTopColleges2019.csv",as.is=TRUE)

dim(college)

head(college)

tail(college)

columns <- colnames(college)

for (name in columns) {  
  cat(name, ':', length(unique(college[[name]])), "\n")
}

check_null_values <- function(dataset) {
  columns <- colnames(dataset)
  
  for (name in columns) {
    cat(name, ":", sum(is.na(dataset[[name]]) | dataset[[name]] == ""), "\n")
  }
}

check_null_values(college)

college <- within(college, rm("City","State", "SAT.Lower","SAT.Upper","ACT.Lower","ACT.Upper", "Website"))

head(college, 3)

check_null_values(college)

#Rank and Acceptance Rate
figure_0 <- college %>% 
  ggplot(aes(x = Rank, y = Acceptance.Rate , color = Public.Private)) +
  geom_line(aes(alpha =0.5)) + theme_bw() + ylab('Acceptance Rate') +
  xlab('Rank') + labs(color = 'Type') +
  ggtitle("Rank and Acceptance Rate")
figure_0

#Rank and Total Cost
figure_1 <- college  %>% 
  ggplot(aes(x =Rank , y = Total.Annual.Cost , color = Public.Private)) + 
  geom_point(aes(alpha = 0.5)) + theme_bw() + ylab('Total Cost') +
  xlab('Rank') + labs(color = 'Type') + 
  geom_smooth(method = 'lm', formula = 'y~x')+ guides(shape = "none") +
  ggtitle("Rank and Total Cost")
figure_1

#Rank and Total Student Population
figure_2 <- college %>% ggplot(aes(x = Rank, y = Student.Population, colour = Public.Private )) + 
  geom_point(alpha = 0.5) + theme_bw() + ylab('Student Population') +
  xlab('Rank')+ geom_smooth(method = 'lm', formula = 'y~x')+
  ggtitle("Rank and Total Student Population")
figure_2

#Total Cost and Average Grant
figure_3 <- college  %>% 
  ggplot(aes(x = Total.Annual.Cost, y = Average.Grant.Aid, color = Public.Private )) + 
  geom_point(aes(alpha = 0.5)) + theme_bw() + ylab('Average Grant') +
  xlab('Total Annual Cost') + labs(color = 'Type') + 
  geom_smooth(method = 'loess', formula = 'y~x')+ guides(shape = "none")+
  ggtitle("Total Cost and Average Grant")
figure_3

#Ranking and Average Grants
figure_4 <- college  %>% 
  ggplot(aes(x = Rank, y = Average.Grant.Aid, color = Public.Private )) + 
  geom_point(aes(alpha = 0.5)) + theme_bw() + ylab('Average Grant') +
  xlab('Rank') + labs(color = 'Aid') + 
  geom_smooth(method = 'loess', formula = 'y~x')+ guides(shape = "none")+
  ggtitle("Ranking and Average Grants")

#Rank and Alumni Salary
figure_5 <- college  %>% 
  ggplot(aes(x = Rank, y = Alumni.Salary, color = Public.Private )) + 
  geom_point(aes(alpha = 0.5)) + theme_bw() + ylab('Alumni Salary') +
  xlab('Rank') + labs(color = 'Salary') + 
  geom_smooth(method = 'loess', formula = 'y~x')+ guides(shape = "none")+
  ggtitle("Rank and Alumni Salary")

grid.arrange(figure_4,figure_5, nrow = 2)