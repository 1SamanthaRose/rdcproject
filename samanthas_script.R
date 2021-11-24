#Plotting total pop. growth
DRC$Year <- as.numeric(as.character(DRC$Year))
DRC$popg <- as.numeric(as.character(DRC$popg))


plot(DRC$Year, DRC$populationgrowth, pch = 0.5, na.rm = FALSE, main = "Population Growth 1960 to 2020")

#Plotting total pop.

DRC <- as.data.frame(DRC)
plot(DRC$Year, DRC$tpop, pch = 5, na.rm = TRUE, main = "Total Population 1960 to 2020")


#Creating pop. parameters plot


colnames(DRC) <- make.unique(names(DRC)) #if getting duplicate 'data' error
DRC <- as.data.frame(lapply(DRC, na.omit)) #trying to remove NA values
DRC$upa <- as.numeric(as.character(DRC$upa))
DRC$ate <- as.numeric(as.character(DRC$ate))
DRC$rec <- as.numeric(as.character(DRC$rec))
DRC$ag.lnd.per <- as.numeric(as.character(DRC$ag.lnd.per))


g <- ggplot(data = DRC)+
  geom_point(mapping = aes(Year, ate, color = "Access to Electricity"), na.rm = TRUE,) + 
  geom_point(mapping = aes(Year, popg, color = "Population Growth"), na.rm = TRUE) +
  geom_point(mapping = aes(Year, upa, color = "Urban Agglomeration Populations"), na.rm = TRUE) +
  scale_color_discrete(name = "Statistic by Percentage") +
  theme_minimal() +
  xlab("Year") +
  ylab("Percent") +
  title(main = "Electricity, Pop. and Urban Pop.")

g


############
#Correlogram between Year, Ag. Arable, precipitation, 

#First creating new DF with variables I want

correl.df <- c(DRC$arbland, DRC$forestkm, DRC$agrland, DRC$avgprec, DRC$Year )
#making them all numeric not chr.

DRC$arbland <- as.numeric(as.character(DRC$arbland))
DRC$agrland <- as.numeric(as.character(DRC$agrland))
DRC$avgprec <- as.numeric(as.character(DRC$avgprec))


library(corrgram)
corrgram(correl.df, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="") 


