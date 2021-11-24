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

