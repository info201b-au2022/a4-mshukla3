library(tidyverse)
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

#calculate year with max black prison population 
summary_list$max_black_pop_year <- avg_per_year %>% 
  select(average_black_pop, year) %>% 
  filter(average_black_pop == max(average_black_pop)) %>% 
  pull(year)

#calculate year with max white prison population
summary_list$max_white_pop_year <- avg_per_year %>% 
  select(average_white_pop, year) %>% 
  filter(average_white_pop == max(average_white_pop)) %>% 
  pull(year)

#calculate max black prison population 
summary_list$max_black_pop <- avg_per_year %>% 
  select(average_black_pop) %>% 
  filter(average_black_pop == max(average_black_pop)) %>% 
  pull(average_black_pop)

#calculate max white prison population 
summary_list$max_white_pop <- avg_per_year %>% 
  select(average_white_pop) %>% 
  filter(average_white_pop == max(average_white_pop)) %>% 
  pull(average_white_pop)

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
states <- c("CO", "WA", "OR", "AZ")

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

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


