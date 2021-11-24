#######
#Mallika Gupta and Samantha Rose Lawrence
#######

library(dplyr)
library(base)
library(ggplot2)
library(corrgram)
library(scales)
library(tidyverse)
library(readr)
library(gapminder)
library(ggridges)
library(plotly)


ccc <- read.csv("climate-change_cod.csv")
cod_dataset <- ccc
cod_dataset <- select(cod_dataset, Year, Value)

#Slicing and dicing our chosen variables into a df called 'DRC'
## Slicing Variables ##

#Malika

CO2emissionsgaseouskt <- slice(cod_dataset, 459:515)
CO2emissionsgaseouskt <- rename(CO2emissionsgaseouskt, CO2emissionsgaseouskt = "Value")

populationgrowth <- slice(cod_dataset, 1582:1641)
populationgrowth <- rename(populationgrowth, populationgrowth = "Value")
DRC<-merge(CO2emissionsgaseouskt, populationgrowth, by = "Year", all = TRUE)


avgprec <- slice(cod_dataset, 249:260)
avgprec <- rename(avgprec, avgprec= "Value")
DRC <- merge(DRC, avgprec, by = "Year", all = TRUE)

totalgreenhousekt <- slice(cod_dataset, 1105:1153)
totalgreenhousekt <- rename(totalgreenhousekt, totalgreenhousekt = "Value")
DRC <- merge(DRC, totalgreenhousekt, by = "Year", all = TRUE)

agrland <- slice(cod_dataset, 2:59)
agrland <- rename(agrland, agrland = "Value")
DRC <- merge(DRC, agrland, by = "Year", all = TRUE)

urbpop <- slice(cod_dataset, 1763:1823)
urbpop <- rename(urbpop, urbpop = "Value")
DRC <- merge(DRC, urbpop, by = "Year", all = TRUE)   

#Samantha Roses

agp  <- slice(cod_dataset, 60:175)       #agriculture groth %
agp <- rename(agp, ag.lnd.per = "Value")
DRC <- merge(DRC, agp, by = "Year", all = TRUE)

arbland <- slice(cod_dataset, 118:175)   #arable land %
arbland <- rename(arbland, arbland = "Value")

forestkm <-slice(cod_dataset, 191:219)  #forest land per km^2
forestkm <- rename(forestkm, fkm = "Value")
DRC <- merge(DRC, forestkm, by= "Year", all = TRUE)



ate <- slice(cod_dataset, 369:388)     #access to electricity by pop. % 
ate <- rename(ate, ate = "Value")
DRC <- merge(DRC, ate, by = "Year", all = "TRUE")


rec <-slice(cod_dataset, 389:414)      #renewable energy consumption by %
rec <-rename(rec, rec = "Value")
DRC <- merge(DRC, rec, by = "Year", all = TRUE)


upa <- slice(cod_dataset, 1344:1404)   #urban pop. agglomeration
upa <- rename(upa, upa = "Value")
DRC<- merge(DRC, upa, by = "Year", all = TRUE)


tpop <- slice(cod_dataset, 1642:1702)   #total population
tpop <- rename(tpop, tpop = "Value")
DRC <- merge(DRC, tpop, by = "Year", all = TRUE)


c02kt <- slice(cod_dataset, 632:690)   #total c02em by kt
c02kt <- rename(c02kt, c02kt = "Value")
DRC <- merge(DRC, c02kt, by = "Year", all = TRUE)

liquidc02 <- slice(cod_dataset, 691:744)   #c02 from liquid fuel consumption by kt
liquidc02 <- rename(liquidc02, liquidc02 = "Value")
DRC <- merge(DRC, liquidc02, by = "Year", all = TRUE)


solidc02 <- slice(cod_dataset, 922:978)   #c02 em from solid fuel consumption by kt
solidc02 <- rename(solidc02, solidc02 = "Value")
DRC <- merge(DRC, solidc02, by = "Year", all = TRUE)



otherc02 <- slice(cod_dataset, 1036:1082)   #other greenhouse gas emissions(HFCs, PFCs, SF6, etc) by kt
otherc02 <- rename(otherc02, otherc02 = "Value")
DRC <- merge(DRC, otherc02, by = "Year", all = TRUE)


tgg <- slice(cod_dataset, 1105:1153)   #total greenhouse gas emissions by kt
tgg <- rename(tgg, tgg = "Value")
DRC <- merge(DRC, tgg, by = "Year", all = TRUE)


meth <- slice(cod_dataset, 1181:1229)   #methane emissions by kt
meth <- rename(meth, meth = "Value")
DRC <- merge(DRC, meth, by = "Year", all = TRUE)

noxide <- slice(cod_dataset, 1252:1300)   #total population
noxide <- rename(noxide, noxide = "Value")
DRC <- merge(DRC, oxide, by = "Year", all = TRUE)





#removing all the dataframes created from this method of slicing
remove(CO2emissionsgaseouskt, populationgrowth, avgprec, totalgreenhousekt, agrland, urbpop,
       popg, upa, tpop, rec, ate, forestkm, agkm, agp, noxide, meth, tgg, solidc02, otherc02,c02kt)
#making DRC dataframe
DRC <- as.data.frame(DRC)

DRC[] <- lapply(DRC, as.numeric)



########################################################################################################################3
######################################################################################################################################
##########MALLIKAS PLOTS#############################################################################################################################
###########################################################################################################################

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
  geom_line(color="purple", linetype=1, alpha=0.5, size=2.5) +
  ggtitle("Increase in CO2 Emissions Over Different Years")

## Explanation of Line Chart

## From the years of 1960-2018, the CO2 emissions are shown to be constant around the value zero.
## Around the year 2016, the CO2 emissions value is seen to be 3.667 per kiloton. When the year was 
## 2010, the CO2 gases emitted are seen to be 44.004. There are N/A values between the years of 
## 2017-2020. The CO2 emissions are seen to be constant around the value zero and then shown to be 
## increasing.In between the years of 2008 - 2010, the CO2 emissions are seen to be constant.Around
## the year 2020, the CO2 emissions are seen to be decreasing as seen in the plot.Around the year 2012,
## it is seen to be increasing.From the years 2012-2014, the CO2 emissions rate is shown to be constant.
## Around the year 2016, it is shown to be decreasing.

## How it Relates to Other Variables

## In the years 1960-1990, the CO2 emissions are seen to be constant around zero. One of the main 
## reasons why this could have occurred is because there were not many people which led to the 
## population to be less. Also, the number of cars driven by the population were less as compared to 
## the 2000s.

######################################################################################################
###############################################################################################
##########SAMANTHAS PLOTS#################################################################################################
############################################################################################################


#Plotting total population growth rate

plot(DRC$Year, DRC$populationgrowth, pch = 10, col = "orange", cex = 0.5,  main = "Population Growth Rate in Percent from 1960 to 2020",
     xlab = "Year", ylab = "Growth Rate (%)", type = "s")

#Plotting total population

DRC <- as.data.frame(DRC)
plot(DRC$Year, DRC$tpop, pch = 5, na.rm = TRUE, main = "Total Population 1960 to 2020")


#Creating pop. parameters plot


colnames(DRC) <- make.unique(names(DRC)) #if getting duplicate 'data' error
DRC <- as.data.frame(lapply(DRC, na.omit)) #trying to remove NA values



g <- ggplot(data = DRC)+
  geom_point(mapping = aes(Year, ate, color = "Access to Electricity"), na.rm = TRUE,) + 
  geom_point(mapping = aes(Year, populationgrowth, color = "Population Growth"), na.rm = TRUE) +
  geom_point(mapping = aes(Year, upa, color = "Urban Agglomeration Populations"), na.rm = TRUE) +
  scale_color_discrete(name = "Statistic by Percentage") +
  theme_minimal() +
  xlab("Year") +
  ylab("Growth Rate (%)") +
  labs(title = "Electricity Access, Population, and Urban Population Growth Rates")

ggplotly(g)


###########
#Total greenhouse gas emissions plotly graph

fig <- ggplot() +
  geom_smooth(DRC, mapping = aes(Year, tgg), color = "forestgreen", size = 1.22 ) +
  labs(main = "Total Greenhouse Gas Emissions by Kiloton", ylab = "Kilotons")
fig


q<-ggplot(DRC, aes(x = Year, y = tgg)) +
  geom_smooth(method = "loess",
              se = FALSE,
              formula = 'y ~ x',
              span = 0.8, color = "forestgreen") +
  stat_smooth(se=FALSE, geom="area",
              method = 'loess', alpha=.5,
              span = 0.8, fill = "forestgreen") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(title = "Total Greenhouse Gas Emissions in Kilotons") +
  labs(y = "Kilotons")

q


#########
#Plotly graph of all the greenhouse gas emission data in kilotons

labels <- c(otherc02 = "Other C02 Sources", meth = "Methane", c02kt = "C02")

p <- ggplot()+
  geom_line(DRC, mapping = aes(Year, otherc02, color = "otherc02"), na.rm = TRUE) +
  geom_line(DRC, mapping = aes(Year, meth, color = "meth"), na.rm = TRUE) +
  geom_line(DRC, mapping = aes(Year, c02kt, color = "c02kt"), na.rm = TRUE) +
  labs(title = "Greenhouse Gas per Kiloton Comparison") +
  labs(y = "Kilotons") +
  scale_color_discrete(name = "Gas") +
  theme_bw()



ggplotly(p)



################3

#Land Use Graphs-farm versus forest

g <- ggplot(data = DRC)+
  geom_point(mapping = aes(Year, fkm, color = "Forest Land"), cex = 1, alpha = 0.75, pch = 4, na.rm = TRUE,) + 
  geom_point(mapping = aes(Year, agrland, color = "Agricultural Land"), cex = 1, pch = 4, alpha = 0.75, na.rm = TRUE) +
  scale_color_discrete(name = "Land Use") +
  theme_classic() +
  xlab("Year") +
  ylab("Square Kilometres") +
  labs(title = "Land Use-Forest vs. Farm")


ggplotly(g)



#############

#Plotting the percent of arable land


plot(arbland$Year, arbland$arbland, main = "Percent of Arable Land", type = "l", xlab = "Year", ylab = "Percent", cex = 1.5,
     col = "olivedrab")

