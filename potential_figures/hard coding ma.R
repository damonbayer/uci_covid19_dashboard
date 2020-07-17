#hard code rolling mean
knitr::opts_chunk$set(echo = F)
library(tidyverse)
library(lubridate)
library(scales)
library(tidyquant)
library(glue)
library(ckanr) # library to interact with CA Gov. Public Health Portal

theme_set(theme_bw())

# connect to CKAN instance
ckanr_setup(url="https://data.ca.gov")
ckan <- ckanr::src_ckan("https://data.ca.gov")

# get resources
resources <- rbind(resource_search("name:covid-19", as = "table")$results,
                   resource_search("name:hospitals by county", as = "table")$results)

# get resource ids
resource_ids <- list(cases = resources$resource_id[resources$name == "COVID-19 Cases"],
                     tests = resources$resource_id[resources$name == "COVID-19 Testing"],
                     hosp = resources$resource_id[resources$name == "Hospitals By County"])

# pull resources into data frames (adds extra cols _id and _full_text)
cases <- tbl(src = ckan$con, from = resource_ids$cases) %>%
  as_tibble( )%>%
  select(-starts_with("_")) %>%
  mutate(date = as.Date(date))

tests <- tbl(src = ckan$con, from = resource_ids$test) %>%
  as_tibble() %>%
  select(-starts_with("_")) %>%
  mutate(date = as.Date(date))

hosp <- tbl(src = ckan$con, from = resource_ids$hosp) %>%
  as_tibble() %>%
  select(-starts_with("_")) %>%
  mutate(date = as.Date(todays_date)) %>%
  select(-todays_date)


county_pop <- read_csv("data/county_pop.csv") %>% rename_all(str_to_lower)

counties_of_interest <- c("San Diego",  "Los Angeles",  "Orange", "Alameda", "Santa Clara")


ma_n <- 14
per_n_people <- 1e5

hosp_tidy <- hosp %>%
  filter(county %in% counties_of_interest) %>%
  pivot_longer(cols = -c(date, county)) %>%
  drop_na() %>%
  left_join(county_pop) %>%
  arrange(county, date)%>%
  mutate(
    per_100k=(value/population)*per_n_people,
    SMA=TTR::SMA(per_100k, ma_n)
  )

hosp_tidy %>%
  filter(name == "icu_available_beds") %>%
  ggplot(aes(date, SMA, group = county, color = county)) +
  geom_line(linetype = "solid") +
  ggtitle(glue("ICU Beds Available per {comma(per_n_people)} People"),
          subtitle = glue("{ma_n} Day Moving Average")) +
  scale_color_discrete(name = "County") +
  scale_x_date(name = "Date",
               breaks = "14 day",
               date_labels = "%b %d") +
  scale_y_continuous(name = NULL,
                     label = comma) +
  coord_x_date(xlim = c(min(hosp_tidy$date) + ma_n, NA))
