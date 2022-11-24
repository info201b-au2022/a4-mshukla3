library(tidyverse)
library(reshape2)
library(tmap)
library(sf)
library(ggplot2)
library(tmaptools)
library(leaflet)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## load data set

incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#
#calculate average black and white prison population per year
avg_per_year <- incarceration_trends %>% 
  select(year, black_prison_pop, white_prison_pop) %>% 
  drop_na() %>%
  group_by(year) %>% 
  mutate(average_black_pop = mean(black_prison_pop)) %>% 
  mutate(average_white_pop = mean(white_prison_pop)) %>% 
  distinct(year, average_black_pop, average_white_pop)

avg_per_year$average_black_pop <- round(avg_per_year$average_black_pop, digit=2)
avg_per_year$average_white_pop <- round(avg_per_year$average_white_pop, digit=2)

#calculate difference between black and white average prison populations
avg_per_year$population_difference <- 
  c(avg_per_year$average_black_pop - avg_per_year$average_white_pop)

#create list where variables will be stored
summary_list <- list()

# max average black mael population
summary_list$max_black_pop <- max(avg_per_year$average_black_pop)

#year with max average black population
summary_list$year_max_black_pop <- avg_per_year %>% 
  select(year, average_black_pop) %>% 
  filter(average_black_pop == 326.94) %>% 
  pull(year)

#white male population during the year with max black population
summary_list$white_pop_2000 <- avg_per_year %>% 
  select(year, average_white_pop) %>%
  filter(year == "2000") %>% 
  pull(average_white_pop)

#max average white male population
summary_list$max_white_pop <- max(avg_per_year$average_white_pop)

#year with max white population
summary_list$year_max_white_pop <- avg_per_year %>% 
  select(year, average_white_pop) %>% 
  filter(average_white_pop == 275.56) %>% 
  pull(year)

#black male population in the same year with max white population
summary_list$black_pop_2010 <- avg_per_year %>% 
  select(year, average_black_pop) %>% 
  filter(year == "2010") %>% 
  pull(average_black_pop)

#max difference between white male and black male population
summary_list$max_difference <- max(avg_per_year$population_difference)

#year with max difference between white and black male populations
summary_list$year_max_diff <- avg_per_year %>% 
  select(year, population_difference) %>% 
  filter(year == "1998") %>% 
  pull(year)


#----------------------------------------------------------------------------#
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

# This function returns a data frame that includes the year and the total jail population from that year>
get_year_jail_pop <- function() {
  by_year <- incarceration_trends %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(year, total_jail_pop)
  by_year<- aggregate(total_jail_pop~.,by_year,FUN=sum) 
  return(by_year)   
}
View(get_year_jail_pop())

# This function returns a bar chart that plots year against total jail population>
plot_jail_pop_for_us <- function()  {
  year_jail_pop <- get_year_jail_pop() 
  total_pop_plot <- ggplot(year_jail_pop, aes(x=year, y=total_jail_pop)) +
    geom_bar(stat='identity') + xlab("Year") + 
    ylab("Total Jail Popultion") + 
    ggtitle("Increase of Jail Population in U.S. (1970-2018)")
  return(total_pop_plot)   
} 

plot_jail_pop_for_us()

#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# This function returns a data frame of jail populations by state
get_jail_pop_by_states <- function(states){
  state_change <- incarceration_trends %>%
    drop_na() %>% 
    select(total_jail_pop, state, year)
  states_selected <- state_change %>% 
    filter(state %in% states)
  return(states_selected)  
}


#This function returns a line graph that plots the change in jail population by state
plot_jail_pop_by_states <- function(states) {
  state_line_plot <- ggplot(get_jail_pop_by_states(states), aes(x=year, y=total_jail_pop, color= state))+
    stat_summary(fun="mean", geom= "line")+ labs( title = "Increase of Jail Population in U.S. States (1970-2018)",
                                                  x = "Year",
                                                  y = "Total Jail Population")
  return(state_line_plot)
}


#----------------------------------------------------------------------------#




## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
max_avg_black <- max(avg_per_year$average_black_pop)
max_avg_white <- max(avg_per_year$average_white_pop)

sec_5 <- data.frame(year = c('1970', '2000', '2010', '2016'),
                    Avg_White_Pop = c(25.29, 231.92, 275.56, 261.62),
                    Avg_Black_Pop = c(47.71, 326.94, 314.85, 267.56))
sec_5.melt <- reshape2::melt(sec_5, id="year")
p <- ggplot(data=sec_5.melt, aes(x=year, y=value, fill = variable)) + geom_col(position = "dodge") + 
              theme_classic() + labs(title= "Comparison of Average Black and White Jail Population",
              subtitle = "1970, 2000, 2016",
              fill = "Race",
              x = "Year",
              y = "Jail Population")
p
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>


#creates data frame that contains jail population per state in 2008
get_state_jail_pop <- function() {
  by_state <- incarceration_trends %>% 
    select(year, state, total_jail_pop) %>% 
    drop_na() %>%
    filter(year == "2008") %>% 
    group_by(state) %>% 
    summarise(state, total_jail_pop)
  by_state<- aggregate(total_jail_pop~.,by_state,FUN=sum) 
  return(by_state)
}
# reformats data set in order to work with state shape data set
state_data <- get_state_jail_pop()

state_names <- c("alaska", "alabama", "arkansas", "arizona", "california", "colorado", "district of columbia",
                 "florida", "georgia", "iowa", "idaho", "illinois", "indiana", "kansas", "kentucky", "louisiana",
                 "massachusetts", "maryland", "maine", "michigan", "minnesota", "missouri", "mississippi", "montana",
                 "north carolina", "north dakota", "nebraska", "new hampshire", "new jersey", "new mexico",
                 "nevada", "new york", "ohio", "oklahoma", "oregon", "pennsylvania", "south carolina",
                 "south dakota", "tennessee", "texas", "utah", "virginia", "washington", "wisconsin", "west virginia",
                 "wyoming")
state_data <- subset(state_data, select = -c(state))
state_data$state = state_names

#creates a map that shows the jail populations per state in 2008
state_shape <- map_data("state")
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", size = .1) + coord_map() 

state_shape <- map_data("state") %>% 
  rename(state = region) %>% 
  left_join(state_data, by="state") 

#fills each state in differing color oppacity based on jail populations
map_graph <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "white", 
    size = .1        
  ) +
  coord_map() + 
  scale_fill_continuous(low = "#132B43", high = "Turquoise") +
  labs(fill = "Jail Population", title= "Comparison of Jail Population by State",
       subtitle = "from 2008")

map_graph

#----------------------------------------------------------------------------#

## Load data frame ---- 

# Data loaded at the top of the file

