library(lubridate)
library(here)
library(tidyverse)
library(scales)


# making data ---------------------------------------------------------------
#pop size as of 2019
counties<-c("San Diego", "San Bernardino", "Los Angeles", "Riverside", "Orange", "Alameda")
popsize<-c(3338330, 2180085, 10039107, 2470546, 3175692, 1671329 )

population<-data.frame(counties, popsize)

cases <- read.csv("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv")
tests <- read.csv("https://data.ca.gov/dataset/efd6b822-7312-477c-922b-bccb82025fbe/resource/b6648a0d-ff0a-4111-b80b-febda2ac9e09/download/statewide_testing.csv")
hosp <- read.csv("https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv")


#looks like the test data is useless, so lets focus on deaths for now
cases$date<-as.character(cases$date)
cases$date<-as.Date(cases$date)

time_interval_in_days <- 14

counties <- c("San Diego", "San Bernardino", "Los Angeles", "Alameda", "Riverside", "Orange")

lump_cases<-cases %>%
  group_by(county, lump = as.integer(floor((max(date) - date) / time_interval_in_days))) %>%
  filter(n() == time_interval_in_days) %>%
  summarize(start_date = min(date),
            end_date = max(date),
            new_cases = sum(newcountconfirmed),
            new_deaths = sum(newcountdeaths)) %>%
  dplyr::select(-lump) %>%
  arrange(start_date)

graph_data <- lump_cases%>%
select(-start_date) %>%
  pivot_longer(-c(end_date, county)) %>%
  mutate(name = str_remove(name, "new_") %>% str_to_title()) %>%
  mutate(name = fct_relevel(name, c("Cases", "Deaths")))%>%
  left_join(population, by=c("county"="counties"))%>%
  mutate(per_100k=(value/popsize)*100000)

counties1 <- c("San Diego", "San Bernardino", "Los Angeles", "Riverside", "Orange")

counties2 <-c("San Diego",  "Los Angeles",  "Orange", "Alameda")


death_graph1 <- graph_data%>%
  filter(county %in% counties1 & name=="Deaths")%>%
  ggplot(aes(end_date, per_100k, color=county)) +
  geom_line() +
  geom_point() +
  xlab("Date") +
  ylab("New Deaths") +
  ggtitle("Deaths per 100,000 people",
          subtitle = str_c("Proportions over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  theme_bw()

death_graph2 <- graph_data%>%
  filter(county %in% counties2 & name=="Deaths")%>%
  ggplot(aes(end_date, per_100k, color=county)) +
  geom_line() +
  geom_point() +
  xlab("Date") +
  ylab("New Deaths") +
  ggtitle("Deaths per 100,000 people",
          subtitle = str_c("Proportions over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  theme_bw()


