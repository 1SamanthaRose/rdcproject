#######
#Data Visualisation first
#######

library(dplyr)
library(base)
library(ggplot2)
library(corrgram)
library(scales)
library(tidyverse)
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

arbland <- slice(cod_dataset, 118:175)   #arable land %
arbland <- rename(arbland, arbland = "Value")
arbland <- merge(DRC, arbland, by = "Year", all = TRUE)


#removing all the dataframes created from this method of slicing
remove(CO2emissionsgaseouskt, populationgrowth, avgprec, totalgreenhousekt, agrland, urbpop,
       popg, upa, tpop, rec, ate, forestkm, agkm, agp, arbland)
#making DRC dataframe
DRC <- as.data.frame(DRC)



                                                      