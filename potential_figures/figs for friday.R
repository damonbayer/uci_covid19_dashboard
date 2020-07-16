library(lubridate)
library(here)
library(tidyverse)
library(scales)


# making data ---------------------------------------------------------------
#pop size as of 2019
counties<-c("San Diego", "San Bernardino", "Los Angeles", "Riverside", "Orange", "Alameda", "Santa Clara")
popsize<-c(3338330, 2180085, 10039107, 2470546, 3175692, 1671329, 1927852)

population<-data.frame(counties, popsize)

cases <- read.csv("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv")
tests <- read.csv("https://data.ca.gov/dataset/efd6b822-7312-477c-922b-bccb82025fbe/resource/b6648a0d-ff0a-4111-b80b-febda2ac9e09/download/statewide_testing.csv")
hosp <- read.csv("https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv")


#looks like the test data is useless, so lets focus on deaths for now
cases$date<-as.character(cases$date)
cases$date<-as.Date(cases$date)

hosp$todays_date<-as.character(hosp$todays_date)
hosp$todays_date<-as.Date(hosp$todays_date)

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
  pivot_longer(-c(todays_date, county)) %>%
  left_join(population, by=c("county"="counties"))%>%
  mutate(per_100k=(value/popsize)*100000,
         percent=value*100)


#icu beds avvailable per 100000 people
icu_avalaible1 <- hosp_graph_data%>%
  filter(county %in% counties1 & name=="icu_available_beds" & !is.na(value))%>%
  ggplot(aes(todays_date, per_100k, color=county)) +
  geom_ma(aes(linetype="solid"),ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean ICU Beds") +
  ggtitle("Mean ICU Beds Available per 100,000 People",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  theme_bw()

icu_avalaible2 <- hosp_graph_data%>%
  filter(county %in% counties2 & name=="icu_available_beds" & !is.na(value))%>%
  ggplot(aes(todays_date, per_100k, color=county)) +
  geom_ma(aes(linetype="solid"),ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean ICU Beds") +
  ggtitle("Mean ICU Beds Available per 100,000 People",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  theme_bw()



# cases and deaths -----------------------------------------------------------------
#this is for making rolling averages because I am dumb
library(tidyquant)
graph_cases <- cases%>%
  pivot_longer(-c(date, county)) %>%
  left_join(population, by=c("county"="counties"))%>%
  filter(value>0)%>%
  mutate(per_100k=(value/popsize)*100000)

death_graph1 <- graph_cases%>%
  filter(county %in% counties1 & name=="newcountdeaths")%>%
  ggplot(aes(date, per_100k, color=county)) +
  geom_ma(aes(linetype="solid"),ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Deaths") +
  ggtitle("Mean Deaths per 100,000 people",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  theme_bw()

death_graph2 <- graph_cases%>%
  filter(county %in% counties2 & name=="newcountdeaths")%>%
  ggplot(aes(date, per_100k, color=county)) +
  geom_ma(aes(linetype="solid"),ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Deaths") +
  ggtitle("Mean Deaths per 100,000 people",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  theme_bw()

negdeath<-cases%>%
  filter(newcountdeaths<0)

#cases per 100k
cases_graph1 <-graph_cases%>%
  filter(county %in% counties1 & name=="newcountconfirmed")%>%
  ggplot(aes(date, per_100k, color=county)) +
  geom_ma(aes(linetype="solid"),ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Cases") +
  ggtitle("Mean Cases per 100,000 people",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  theme_bw()

cases_graph2 <-graph_cases%>%
  filter(county %in% counties2 & name=="newcountconfirmed")%>%
  ggplot(aes(date, per_100k, color=county)) +
  geom_ma(aes(linetype="solid"),ma_fun=SMA, n=14)+
  xlab("Date") +
  ylab("Mean Cases") +
  ggtitle("Mean Cases per 100,000 people",
          subtitle = str_c("Rolling Means over",
                           time_interval_in_days,
                           "day periods", sep = " ")) +
  scale_x_date(breaks=c("14 day"), date_labels = "%b %d")+
  theme_bw()


