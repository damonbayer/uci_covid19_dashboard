library(lubridate)
library(here)
library(tidyverse)
library(scales)
library(tidyquant)

library(ckanr) # library to interact with CA Gov. Public Health Portal
library(dplyr)

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

tests <- dplyr::tbl(src = ckan$con, from = res_ids$test) %>% as_tibble(.)%>%
  dplyr::select(-`_full_text`,-`_id`)

hosp <- dplyr::tbl(src = ckan$con, from = res_ids$hosp) %>% as_tibble(.)%>%
  dplyr::select(-`_full_text`,-`_id`)

# making data ---------------------------------------------------------------
#pop size as of 2019
counties <- c("San Diego", "San Bernardino", "Los Angeles", "Riverside", "Orange", "Alameda", "Santa Clara")
popsize <- c(3338330, 2180085, 10039107, 2470546, 3175692, 1671329, 1927852)

population <- data.frame(counties, popsize)

#looks like the test data is useless, so lets focus on deaths for now
cases$date <- as.character(cases$date)
cases$date <- as.Date(cases$date)

hosp$todays_date <- as.character(hosp$todays_date)
hosp$todays_date <- as.Date(hosp$todays_date)

time_interval_in_days <- 14

counties <- c("San Diego", "San Bernardino", "Los Angeles", "Alameda", "Riverside", "Orange")

counties1 <- c("San Diego", "San Bernardino", "Los Angeles", "Riverside", "Orange")

counties2 <-c("San Diego",  "Los Angeles",  "Orange", "Alameda", "Santa Clara")




# hospitalizations --------------------------------------------------------
hosp<-hosp %>%
  mutate(hosp_total=hospitalized_covid_confirmed_patients+hospitalized_suspected_covid_patients,
          mean_icu_conf=icu_covid_confirmed_patients,
          icu_total=icu_covid_confirmed_patients+icu_available_beds,
          percent_hosp_beds_conf=hospitalized_covid_confirmed_patients/all_hospital_beds,
          percent_hosp_beds_total=hosp_total/all_hospital_beds)

hosp_graph_data <- hosp%>%
  filter(county %in% counties2)%>%
  pivot_longer(-c(todays_date, county)) %>%
  left_join(population, by=c("county"="counties"))%>%
  mutate(per_100k=(value/popsize)*100000,
         percent=value*100,
         per_1mil=(value/popsize)*1000000)%>%
  rename("County"="county")%>%
  arrange(County, todays_date)


#icu beds avvailable per 100000 people

icu_available <- hosp_graph_data%>%
  filter(name=="icu_available_beds" & !is.na(value))%>%
  ggplot(aes(todays_date, per_1mil, color=County)) +
  geom_ma(linetype="solid",ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean ICU Beds") +
  ggtitle("Mean ICU Beds Available per 1,000,000 People",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  coord_x_date(xlim = c(min(hosp_graph_data$todays_date)+14,NA))+
  theme_bw()

#hospital beds occupied by covid patients per 1 mil
hosp_graph <- hosp_graph_data%>%
  filter(name=="hosp_total" & !is.na(value))%>%
  ggplot(aes(todays_date, per_1mil, color=County)) +
  geom_ma(linetype="solid",ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Hospital Beds") +
  ggtitle("Mean Hospitalized COVID Patients by 1,000,000 People",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  coord_x_date(xlim = c(min(hosp_graph_data$todays_date)+14,NA))+
  theme_bw()

# cases and deaths -----------------------------------------------------------------
#this is for making rolling averages because I am dumb
graph_cases <- cases%>%
  filter(county %in% counties2 )%>%
  pivot_longer(-c(date, county)) %>%
  left_join(population, by=c("county"="counties"))%>%
  filter(value>0)%>%
  mutate(per_100k=(value/popsize)*100000,
         per_1mil=(value/popsize)*1000000)%>%
  rename("County"="county")%>%
  arrange(County, date)


#deaths per 1mil
death_graph <- graph_cases%>%
  filter(name=="newcountdeaths")%>%
  ggplot(aes(date, per_1mil, color=County)) +
  geom_ma(linetype="solid",ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Deaths") +
  ggtitle("Mean Deaths per 1,000,000 people",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  coord_x_date(xlim = c(min(graph_cases$date)+14,NA))+
  theme_bw()




#cases per 1mil

cases_graph <-graph_cases%>%
  filter(County %in% counties2 & name=="newcountconfirmed")%>%
  ggplot(aes(date, per_1mil, color=County)) +
  geom_ma(linetype="solid",ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Cases") +
  ggtitle("Mean Cases per 1,000,000 people",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  coord_x_date(xlim = c(min(graph_cases$date)+14,NA))+
  theme_bw()


#test positivity (nvm LA dashboard is borked)
oc_test <- read.csv("potential_figures/oc_test_data.csv")%>%
  dplyr::select(posted_date, new_tests)


