
#DAY 2
#R DATA ANALYSIS
#Questions
#mean life expectancy
#mean life expectancy for most recent year
#mean lf in each year
#total gdp for each country
#relationship btw gdp and co2 emission?
#percent of total co2 emission accounted for by north america


#TOOLS

#dplyr verbs:
#summarize
#filter
#mutate
#select
#group_by
#arrange

#reshaping functions:
#pivot_longer
#pivot_wider

#joins:
#inner_join
#outer_join
#anti_join
#full_join

library(tidyverse)
library(readr)

co2_un_data <- read_csv("Data/co2-un-data.csv")
gapminder_data <- read_csv("Data/gapminder_data.csv")

#what is the mean life expectancy?
summarize(gapminder_data, averagelifeExp = mean(lifeExp))

#using the piper: 
#Rstudio pipe operator for windows: control+shift+m produces %>%
#sending the results of a function to the next function
gapminder_data %>% summarize(averagelifeExp = mean(lifeExp))
#another formatting: can chain multiple functions together: tabulate
gapminder_data %>% 
  summarize(averagelifeExp = mean(lifeExp))

#to save the results into an object:
gapminder_data_summarize <- gapminder_data %>% 
  summarize(averagelifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(meanPop = mean(pop))

#all at the same time:
gapminder_data %>% 
  summarize(averagelifeExp = mean(lifeExp),
            meanPop = mean(pop))

#mean life expectancy for the most recent year?
#using the dplyr verb filter tool: filter()
#first step: find max year
gapminder_data %>% 
  summarize(RecentYear = max(year))

#second step: filter to only max year
gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(meanlifeExp = mean(lifeExp))

#in one:
gapminder_data %>% 
  filter(year == max(year)) %>% 
  summarize(meanlifeExp = mean(lifeExp))

#what is the mean gdp per capita for the earliest year?
gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(GDP = mean(gdpPercap))

#min, mean, mode, median, sd, max......
# conditions: < , > , != (does not equal), == (equals)

#mean life expectancy for each year?
#will use group_by()
gapminder_data %>%
  group_by(year) %>% 
  summarize(meanlifeExp = mean(lifeExp))

#mean life expectancy for each continent?
gapminder_data %>%
  group_by(continent) %>% 
  summarize(meanlifeExp = mean(lifeExp))

#mean life expectancy and mean gdp per capita for each continent?
gapminder_data %>%
  group_by(continent) %>% 
  summarize(meanlifeExp = mean(lifeExp),
            GDP = mean(gdpPercap))

#what is the gdp (not per capita)?
#we'll use "mutate()"
#adds a new column to the data --> attached to the existing data
gapminder_data %>%
  mutate(gdp = gdpPercap * pop)

GDP <- gapminder_data %>%
  mutate(GDP = gdpPercap * pop)

#make a new column for population in millions
gapminder_data %>%
  mutate(popMil = pop/1000000)

#all at once:
gapminder_data %>%
  mutate(gdp = gdpPercap * pop) %>% 
  mutate(popMil = pop/1000000)

#all at once and add to the original data (override):
gapminder_data <- gapminder_data %>%
  mutate(gdp = gdpPercap * pop,
         popMil = pop/1000000)


#more tools for...
#is there a relation between co2 emission and gdp and stuff

#select()
#picks a subset of columns from the data set
gapminder_data %>% 
  select(year, pop)

#how to leave out columns and pick the rest
gapminder_data %>% 
  select(-continent)

#create a tibble with only country, contient, year, and lifeexp
gapminder_data %>% 
  select(-pop, -gdpPercap)

#select has helper functions
#starts_with(), ends_with(), contains()
gapminder_data %>% 
  select(year, starts_with("c"))

#if we have unique identifiers we'd like to filter for:
my_data %>% 
  filter(column == "specific id")

#can get more ids at the same time:
my_data %>% 
  filter(column %in% c("id_1", "id_2", "id_3"))

gapminder_data %>% 
  select(contains("e"))

#google: simple expressions r

#vectors
#can be numbers OR characters, but not a mix
#each column is a vector
# c()
my_vec <- c("dog", "cat", "cow", "puffin", "orca", "elephant")
my_vec2 <- c(1, 2, 3, 4, 5, 6)
proof <- gapminder_data %>% 
  pull(year)

#reshaping functions
#pivot_longer(), pivot_wider()
#to create long or wide format
data %>% 
  pivot_wider(
    names_from = "",
    values_from = ""
  )

#can be more specific too

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(
    names_from = year,
    values_from = lifeExp
  )

#pivot_wider, populate values with gdp per cap
gapminder_data %>% 
  select(country, continent, year, gdpPercap) %>% 
  pivot_wider(
    names_from = year,
    values_from = gdpPercap
  )

#pivot_longer; specifiy which columns to pivot, and specify the names of the new columns
gapminder_data %>%
  pivot_longer(cols = c(pop, lifeExp, gdpPercap), 
               names_to = "measurement_type",
               values_to = "measurement")
  
#is there a relationship between co2 emissions and gdp?
#focusing on just one year
#filter for year 2007, continent Americas only
#select out year and continent columns, because we don't need them anymore (since we only use year 2007 in the Americas)
#assign to a new dataframe

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)

co2_emissions_dirty <- read_csv("C:/Users/csill/OneDrive/Desktop/un-report/Data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

#have to have the two data sets match up in dimensions

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series,
              values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

#joining the data sets
#inner_join() takes all the observations (countries here) 
#that are all in the data sets; it removes the rest

inner_join(gapminder_data_2007, co2_emissions)
#this joins by country
#otherwise can specify: join_by()

inner_join(gapminder_data_2007, co2_emissions, by = "country")

