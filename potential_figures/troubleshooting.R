#troubleshooting

library(lubridate)
library(here)
library(tidyverse)
library(scales)
library(tidyquant)

library(ckanr) # library to interact with CA Gov. Public Health Portal
library(dplyr)

counties <- c("San Diego", "San Bernardino", "Los Angeles", "Riverside", "Orange", "Alameda", "Santa Clara")
popsize <- c(3338330, 2180085, 10039107, 2470546, 3175692, 1671329, 1927852)

counties2 <-c("San Diego",  "Los Angeles",  "Orange", "Alameda", "Santa Clara")

time_interval_in_days <- 14


# Source 1 For Cases ------------------------------------------------------


ckanr_setup(url="https://data.ca.gov")
# connect to CKAN instance
ckan <- ckanr::src_ckan("https://data.ca.gov")

# get resources
res <- resource_search("name:covid-19", as = "table")$results
res <- rbind(res, # hospital data not in covid-19 group for some reason
             resource_search("name:hospitals by county", as = "table")$results)

# get resource ids
res_ids <- list(cases = res$resource_id[res$name == "COVID-19 Cases"],
                tests = res$resource_id[res$name == "COVID-19 Testing"],
                hosp = res$resource_id[res$name == "Hospitals By County"]
)

# pull resources into data frames (adds extra cols _id and _full_text)
cases <- dplyr::tbl(src = ckan$con, from = res_ids$cases) %>% as_tibble(.)%>%
  dplyr::select(-`_full_text`,-`_id`)


population <- data.frame(counties, popsize)

#looks like the test data is useless, so lets focus on deaths for now
cases$date <- as.character(cases$date)
cases$date <- as.Date(cases$date)



graph_cases <- cases%>%
  pivot_longer(-c(date, county)) %>%
  left_join(population, by=c("county"="counties"))%>%
  filter(value>0)%>%
  mutate(per_100k=(value/popsize)*100000,
         per_1mil=(value/popsize)*1000000)%>%
  rename("County"="county")%>%
  arrange(County, date)

# Source 2 For Cases ------------------------------------------------------
cases1 <- read.csv("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv")

cases1$date <- as.character(cases1$date)
cases1$date <- as.Date(cases1$date)



graph_cases1 <- cases1%>%
  pivot_longer(-c(date, county)) %>%
  left_join(population, by=c("county"="counties"))%>%
  filter(value>0)%>%
  mutate(per_100k=(value/popsize)*100000,
         per_1mil=(value/popsize)*1000000)%>%
  rename("County"="county")%>%
  arrange(County, date)


# The Graphs Are Different ------------------------------------------------
death_graph <- graph_cases%>%
  filter(County %in% counties2 & name=="newcountdeaths")%>%
  ggplot(aes(date, per_1mil, color=County)) +
  geom_ma(linetype="solid",ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Deaths") +
  ggtitle("Mean Deaths per 1,000,000 people",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  #coord_x_date(xlim = c(min(graph_cases$date)+14,NA))+
  theme_bw()


death_graph1 <- graph_cases1%>%
  filter(County %in% counties2 & name=="newcountdeaths")%>%
  ggplot(aes(date, per_1mil, color=County)) +
  geom_ma(linetype="solid",ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Deaths") +
  ggtitle("Mean Deaths per 1,000,000 people",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  #coord_x_date(xlim = c(min(graph_cases$date)+14,NA))+
  theme_bw()

# The Data are Different? (May 20th) --------------------------------------------------------


library(zoo)
orange <- graph_cases%>%
  filter(County=="Orange" & name=="newcountdeaths")%>%
  arrange(date)%>%
  mutate(
    SMA=TTR::SMA(per_1mil),
    zooSMA=rollmean(per_1mil,k=14,  na.pad=TRUE)
  )%>%
  arrange(date)

orange1 <- graph_cases1%>%
  filter(County=="Orange" & name=="newcountdeaths")%>%
  arrange(date)%>%
  mutate(
    SMA=TTR::SMA(per_1mil),
    zooSMA=rollmean(per_1mil,14, na.pad=TRUE)
  )%>%
  arrange(date)


diff<-orange%>%
  anti_join(orange1)

diff1<-orange1%>%
  anti_join(orange)
