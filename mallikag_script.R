## Install Packages

library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(tidyverse)

climate_change_cod <- read_csv("climate_change_cod.csv")

View(climate_change_cod)


## Two Visualizations

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










 





