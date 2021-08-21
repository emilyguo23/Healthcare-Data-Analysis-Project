rm(list = ls())
setwd("~/Desktop/CMU/73-265")
library(tidyverse)
library(dplyr)
library(readxl)

spendingTotal <- read_csv("FP-SpendingTotal.csv")
infantMortality <- read_csv("FP-childMortality.csv")
lifeExpectancy <- read_csv("FP-life-expectancy.csv")
spendingPerCap <- read_excel("FP-expenditurePerCapita-DWB.xls", range = "A4:BJ268")
spendingGDP <- read_csv("FP-public-health-expenditure-share-GDP.csv")
numberOfPhysicians <- read_excel("FP-numberOfPhysicians.xls", range = "A4:BJ268")



#Tidy and Merge ----
spendingPerCap <- subset(spendingPerCap, select = -c(3,4:44))
spendingPerCap <- pivot_longer(spendingPerCap, !c(`Country Name`, `Country Code`),
                                  names_to = "Year", values_to = "Expenditure per Capita")

numberOfPhysicians <- subset(numberOfPhysicians, select = -c(3,4:44))
numberOfPhysicians <- pivot_longer(numberOfPhysicians, !c(`Country Name`, `Country Code`),
                              names_to = "Year", 
                              values_to = "Number of Physicians per 1000 People")

spendingTotal <- subset(spendingTotal, select = -c(2:5, 8))
spendingTotal <- rename(spendingTotal, 
                        Code = LOCATION, Year = TIME, `Total Expenditure` = Value)
spendingTotal$Country <- ifelse(spendingTotal$Code == "BEL", "Beligum", spendingTotal$Code)
spendingTotal$Country <- ifelse(spendingTotal$Code == "DEU", "Germany", spendingTotal$Country)
spendingTotal$Country <- ifelse(spendingTotal$Code == "JPN", "Japan", spendingTotal$Country)
spendingTotal <- spendingTotal[,2:4]
spendingTotal <- spendingTotal[, c(3,1,2)]
spendingTotal <- subset(spendingTotal, Year >= 2000)

infantMortality <- subset(infantMortality, select = -c(4:6))

spendingGDP <- rename(spendingGDP, Country = Entity)
spendingPerCap <- rename(spendingPerCap, Country = `Country Name`)
infantMortality <- rename(infantMortality, Country = Entity)
lifeExpectancy <- rename(lifeExpectancy, Country = Entity)
numberOfPhysicians <- rename(numberOfPhysicians, Country = `Country Name`)

list <- list(spendingGDP, spendingPerCap, infantMortality, 
             lifeExpectancy, numberOfPhysicians)
countries <- c("Belgium", "Germany", "Japan")

data <- lapply(list, function(x) 
  x[-c(2)])

data2 <- lapply(data, subset, Country %in% countries & Year >= 2000)

dataset <- merge(data2[[1]], data2[[2]], all = T)
dataset <- merge(dataset, data2[[3]], all = T)
dataset <- merge(dataset, data2[[4]], all = T)
dataset <- merge(dataset, data2[[5]], all = T)
dataset <- merge(dataset, spendingTotal, all = T)



dataset <- rename(dataset, 
                  PercentGDP = 'Public expenditure on health %GDP (OWID extrapolated series)',
                  PerCap = 'Expenditure per Capita',
                  ChildMortality = 'Mortality rate, under-5 (per 1,000 live births)',
                  LifeExp = 'Life expectancy',
                  numPhysicians = 'Number of Physicians per 1000 People',
                  totalExpend = 'Total Expenditure')
dataset$System <- "Bismarck"
dataset$Ratio <- LifeExp/PercentGDP

## function to replace the NAYears of numPhysicians with the average of
## the nearestYear before and after them
replace_NA_years <- function(dataset, Year, Code, lower_bound, na_rows){
  ## base case: if we've reached the end of our Years bound, then just replace
  ## any remaining NAs and return our edited dataset
  if (Year == 2018){
    for (na_year in na_rows){
      dataset[dataset$Year == na_year & dataset$Code == Code,]$numPhysicians <- lower_bound
    }
    return (dataset)
  } else {
    ## recursive case
    if (is.na(dataset[dataset$Year == Year & dataset$Code == Code,]$numPhysicians)){
      ## if our current Year has an NA value, add it to our list of NA values
      ## and then call the function on the next Year
      na_rows <- append(na_rows, Year)
      replace_NA_years(dataset, Year + 1, lower_bound, na_rows)
    } else {
      ## otherwise, our new (real) value is the upper bound for all current NAs,
      ## we can reassign their values to be the average of lower and upper,
      ## we have no more current NAs, and this real value is our new lower bound;
      ## then call the function again on the next Year
      upper_bound <- dataset[dataset$Year == Year & dataset$Code == Code,]$numPhysicians
      av <- mean(c(upper_bound, lower_bound), na.rm = TRUE)
      for (na_year in na_rows){
        dataset[dataset$Year == na_year & dataset$Code == Code,]$numPhysicians <- av
      }
      na_rows <- c()
      lower_bound <- upper_bound
      replace_NA_years(dataset,Year + 1, lower_bound, na_rows)
    }
  }
}


#Plots ----
#need to fix aesthetics
#create homogemous theme package:
#theme <- ... (reference vis challenge?)

#Plot 1 -- Expenditure per capita vs child mortality (stack 3 countries, scatterplot)
plot1 <- ggplot(data = dataset, aes(x = PerCap, y = infantMortality)) +
  geom_point(aes(color = Code)) +
  ggtitle("Bismarck Healthcare System") +
  xlab("Healthcare Expenditure per Capita in USD") +
  ylab("Total Number of Deaths Under 5 Years of Age") + 
  scale_color_discrete(name = "Country", labels = c("Belgium", "Germany", "Japan"))
plot1


#Plot 2 -- Expenditure per capita vs life expectancy (stack 3 countries, scatterplot)
plot2 <- ggplot(data = dataset, aes(x = PerCap, y = lifeExp)) +
  geom_point(aes(color = Code)) +
  ggtitle("Bismarck Healthcare System") +
  xlab("Healthcare Expenditure per Capita in USD") +
  ylab("Average Life Expectancy") + 
  scale_color_discrete(name = "Country", labels = c("Belgium", "Germany", "Japan", "USA"))
plot2


#Plot 3 -- Expenditure per capita vs (medical doctors/1000 people) (stack 3 countries, line graph)
plot3 <- ggplot(data = dataset, aes(x = PerCap, y = numPhysicians)) +
  geom_point(aes(color = Code)) + #play with shapes? #not sure about line vs point
  ggtitle("Bismarck Healthcare System") +
  xlab("Healthcare Expenditure per Capita in USD") +
  ylab("Number of Physicians per 1000 People") + 
  scale_color_discrete(name = "Country", labels = c("Belgium", "Germany", "Japan", "USA"))
plot3


#Plot 4 -- [life expectancy/(expenditure%gdp)] vs year (stack 3 countries, line)
plot4 <- ggplot(data = dataset, aes(x = Year, y = lifeExp / PercentGDP)) +
  geom_line(aes(color = Code, group = Code)) + 
  ggtitle("Bismarck Healthcare System") +
  xlab("Year") +
  ylab("Life Expectancy / Healthcare Expenditure in %GDP") + 
  scale_x_discrete(breaks = seq(2000, 2018, by = 2)) + 
  scale_color_discrete(name = "Country", labels = c("Belgium", "Germany", "Japan", "USA"))
plot4

#Plot 5 -- Life expectancy vs years (line graph - 3 countries stack)
plot5 <- ggplot(data = dataset) +
  geom_line(aes(x = Year, y = lifeExp, group = Code, color = Code)) + 
  ggtitle("Bismarck Healthcare System") +
  xlab("Year") +
  scale_x_discrete(breaks = seq(2000, 2019, by = 2)) + 
  ylab("Average Life Expectancy") + 
  scale_color_discrete(name = "Country", labels = c("Belgium", "Germany", "Japan", "USA"))
plot5


#Plot 6 -- Child Mortality vs years (line graph - 3 countries stack)
plot6 <- ggplot(data = dataset) +
  geom_line(aes(x = Year, y = infantMortality, group = Code, color = Code)) + 
  ggtitle("Bismarck Healthcare System") +
  xlab("Year") +
  scale_x_discrete(breaks = seq(2000, 2019, by = 2)) + 
  ylab("Total Number of Deaths Under 5 Years of Age") + 
  scale_color_discrete(name = "Country", labels = c("Belgium", "Germany", "Japan"))
plot6


#Plot 7 -- (medical doctors/1000 people) vs years (line graph - 3 countries stack)
plot7 <- ggplot(data = dataset) +
  geom_line(aes(x = Year, y = numPhysicians, group = Code, color = Code)) + 
  ggtitle("Bismarck Healthcare System") +
  xlab("Year") +
  scale_x_discrete(breaks = seq(2000, 2019, by = 2)) + 
  ylab("Number of Physicians per 1000 People") + 
  scale_color_discrete(name = "Country", labels = c("Belgium", "Germany", "Japan", "USA"))
plot7

#Export Data ----

library(writexl)
write_xlsx(dataset,"BismarckData.xlsx")


#Faceting graphs ----


#Regressions ----
