## Install Packages

library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(gapminder)
library(ggridges)


## Visualizations

DRC <- as.data.frame(DRC)

agrland<- lapply(DRC$agrland, as.numeric)



## Plotting Urban Population by Year Using Plot Function

DRC$Year <- as.numeric(as.character(DRC$Year))
DRC$urbpop <- as.numeric(as.character(DRC$urbpop))

plot(DRC$Year, DRC$urbpop, pch=0.75, col = "Red", na.rm = FALSE, main= "Urban Population from 1960 to 2020")

## Explanation

## In the above plot, I am showing the relationship between the Year and urban population. Based on 
## the graph, there is an exponential increase in the urban population as the years go on. The graph
## has an exponential growth. 

## Remove Duplicate Values
colnames(DRC) <- make.unique(names(DRC))


## 2D Histogram Showing Relationship between Year and Urban Population
ggplot(DRC, aes(x=Year, y=urbpop)) +
  geom_bin2d() +
  theme_bw() +
  labs(title="Urban Population Growth between Years of 1960 and 2020")

ggplot(DRC, aes(x=Year, y=urbpop) ) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() +
  labs(title="Urban Population Growth between Years of 1960 and 2020")

## Explanation

## I am visualizing the relationship between Year and Urban Population using a 2D histogram. This is 
## another way to visualize the data besides using the plot function. I labelled the x-axis to show 
## the year and y-axis urbpop.By looking at the visualization, I can see there is an exponential 
## growth between Year and Urban Population. This graph explains that as the years go by, the urban 
## population also increases.

## Linear Model

h <- ggplot(DRC, mapping=aes(x=Year, y=agrland)) +
  geom_point() +
  geom_smooth(method=lm, color="Green", se=FALSE)
h

## Explanation of the Linear Model Plot

## I created a scatter plot and used the geom_smooth argument to create a linear model using the 
## regression line. In the scatter plot, I can see as the years increase, the agricultural land (in km)
## is seen to be increasing. Around the year 2000, it starts to decrease and in the year 2010 increases 
## again. In between the years of 2012-2018, there is a cluster of points starting from 315000 km and 
## goes up to 3200000. There are some points closer to the regression line, and some points below the 
## regression line are far away. 

## How the Growth in Agricultural Land Relates to Urban Population.

## Around the years 1960-1990, there was not much agricultural land (in km). This could be because there
## was not many people living in urban areas. The population was not that much and less land was needed
## to do agriculture. As the years passed and the population increased, more land was needed to do 
## agriculture and grow different types of crops. In the years 2010-2018, the agricultural land 
## increased to a value of 315000 km.

## Line Chart

ggplot(DRC, aes(x=Year, y=CO2emissionsgaseouskt)) +
  geom_line(color="purple")

## Explanation of Line Chart

## From the years of 1960-2018, the CO2 emissions are shown to be constant around the value zero.
## Around the year 2016, the CO2 emissions value is seen to be 3.667 per kiloton. When the year was 
## 2010, the CO2 gases emitted are seen to be 44.004. There are N/A values between the years of 
## 2017-2020. 

## How it Relates to Other Variables

## In the years 1960-1990, the CO2 emissions are seen to be constant around zero. One of the main 
## reasons why this could have occurred is because there were not many people which led to the 
## population to be less. Also, the number of cars driven by the population were less as compared to 
## the 2000s.


 








 





