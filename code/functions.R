library(tidyverse)
library(lubridate)
library(scales)
library(TTR)
library(glue)
library(ggtext)
library(ckanr) # library to interact with CA Gov. Public Health Portal
library(cowplot)


# create watermarks -------------------------------------------------------
watermark <- function(plt) ggdraw(plt) + draw_label("UC Irvine COVID Awareness Group", color = "#0064A4", alpha=0.4, size = 15, angle = 0, x=0.20, y=0.85, hjust = 0, vjust = 0)
# Could also use stamp function
# stamp(plot1, label = "DRAFT", color = "red")

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


# county plots ------------------------------------------------------------
get_county_plots <- function(counties_of_interest){
  # connect to CKAN instance
  ckanr_setup(url="https://data.ca.gov")
  ckan <- quiet(ckanr::src_ckan("https://data.ca.gov"))

  # get resources
  resources <- rbind(resource_search("name:covid-19", as = "table")$results,
                     resource_search("name:hospitals by county", as = "table")$results)

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

  # stay at home order start and end dates
  sah_start <- ymd("2020-03-19")
  sah_end <- ymd("2020-05-04")

  per_n_people <- 1e5 # denominator for reporting counts (e.g, per million people)
  ma_n <- 7 # moving average window length in days

  # Tidy Data
  hosp_tidy <- hosp %>%
    filter(county %in% counties_of_interest) %>%
    pivot_longer(cols = -c(date, county)) %>%
    drop_na() %>%
    left_join(county_pop) %>%
    arrange(county, date)

  cases_tidy <- cases %>%
    filter(county %in% counties_of_interest) %>%
    pivot_longer(cols = -c(date, county)) %>%
    drop_na() %>%
    left_join(county_pop) %>%
    arrange(county, date)


  # Theme options
  theme_set(theme_bw(base_size = 12.5) +
              theme(legend.position = "bottom",
                    legend.title = element_text(size = 12)))

  # keep color palette consistent across county sets
  cbPalette <- c(Alameda = "#56B4E9",
                 `Los Angeles` = "#009E73",
                 Orange = "#E69F00",
                 `San Diego` = "#CC79A7",
                 `Santa Clara` = "#0072B2",
                 Riverside = "#A857E6",
                 `San Bernardino` = "#1980E6",
                 Sacramento = "#CC625E",
                 `Contra Costa` = "#CAA1E6",
                 `San Francisco` = "#E6CA5C")

  sah_alpha <- 0.2

  gglayers = list(
    geom_line(size = 1.5),
    scale_color_manual(name = "County", values = cbPalette),
    scale_x_date(name = "Date",
                 breaks = "21 day",
                 date_labels = "%b %d",
                 expand = expansion(add=c(0,7))),
    scale_y_continuous(label = comma)
  )

  # Hospitalizations
  hospitalizations_plot_data <- hosp_tidy %>%
    filter(name == "hospitalized_covid_patients") %>%
    group_by(county) %>%
    mutate(value = runMean(value, ma_n)) %>%
    drop_na()

  hospitalizations_plot <- hospitalizations_plot_data %>%
    ggplot(aes(date, value / population * per_n_people, group = county, color = county)) +
    ggtitle(glue("Hospitalized Patients with COVID-19 <span style='font-size:14pt'>({ma_n} Day Moving Average)</span>")) +
    #subtitle = glue("{ma_n} Day Moving Average"))
    theme(
      plot.title = element_markdown()
    ) +
    gglayers +
    ylab(glue("Hospitalized Patients with COVID-19\nper {comma(per_n_people)} People")) +
    annotate("rect",
             xmin = if_else(min(hospitalizations_plot_data$date) < sah_start,
                            sah_start,
                            min(hospitalizations_plot_data$date)),
             xmax = sah_end, ymin = -Inf, ymax = Inf, alpha = sah_alpha) +
    annotate("text", y = 175/10, x = sah_end+8, label = "Stay at Home\nOrder Ended")

  # ICU Occupancy
  icu_plot_data <- hosp_tidy %>%
    filter(name %in% c("icu_covid_confirmed_patients", "icu_suspected_covid_patients")) %>%
    group_by(county, date, population) %>%
    summarize(value = sum(value)) %>%
    group_by(county) %>%
    mutate(value = runMean(value, ma_n)) %>%
    drop_na()  %>%
    filter(date >= min(hospitalizations_plot_data$date))

  icu_plot <- icu_plot_data %>%
    ggplot(aes(date, value / population * per_n_people, group = county, color = county)) +
    ggtitle(glue("ICU Patients with COVID-19 <span style='font-size:14pt'>({ma_n} Day Moving Average)</span>")) +
    #          subtitle = glue("{ma_n} Day Moving Average")) +
    theme(
      plot.title = element_markdown()
    ) +
    gglayers +
    ylab(glue("ICU Patients with COVID-19\nper {comma(per_n_people)} People")) +
    annotate("rect",
             xmin = if_else(min(icu_plot_data$date) < sah_start, sah_start, min(icu_plot_data$date)),
             xmax = sah_end, ymin = -Inf, ymax = Inf, alpha = sah_alpha) +
    annotate("text", y = 51/10, x = sah_end+8, label = "Stay at Home\nOrder Ended")


  # Deaths
  deaths_plot_data <- cases_tidy %>%
    filter(name == "newcountdeaths") %>%
    group_by(county) %>%
    mutate(value = runMean(value, ma_n)) %>%
    drop_na() %>%
    filter(date >= min(hospitalizations_plot_data$date))

  deaths_plot <- deaths_plot_data %>%
    ggplot(aes(date, value / population * per_n_people, group = county, color = county)) +
    ggtitle(glue(" New Daily Deaths due to COVID-19 <span style='font-size:14pt'>({ma_n} Day Moving Average)</span>")) +
    #          subtitle = glue("{ma_n} Day Moving Average")) +
    theme(
      plot.title = element_markdown()
    ) +
    gglayers +
    ylab(glue("New Daily Deaths due to COVID-19\nper {comma(per_n_people)} People")) +
    annotate("rect",
             xmin = if_else(min(deaths_plot_data$date) < sah_start,
                            sah_start,
                            min(deaths_plot_data$date)),
             xmax = sah_end, ymin = -Inf, ymax = Inf, alpha = sah_alpha) +
    annotate("text", y = 3/10, x = sah_end+8, label = "Stay at Home\nOrder Ended")

  # Cases
  cases_plot_data <- cases_tidy %>%
    filter(name == "newcountconfirmed") %>%
    group_by(county) %>%
    mutate(value = runMean(value, ma_n)) %>%
    drop_na() %>%
    filter(date >= min(hospitalizations_plot_data$date))

  cases_plot <- cases_plot_data %>%
    ggplot(aes(date, value / population * per_n_people, group = county, color = county)) +
    ggtitle(glue("New Daily COVID-19 Cases <span style='font-size:14pt'>({ma_n} Day Moving Average)</span>")) +
    #          subtitle = glue("{ma_n} Day Moving Average")) +
    theme(
      plot.title = element_markdown()
    ) +
    gglayers +
    ylab(glue("New Daily Confirmed COVID-19 Cases\nper {comma(per_n_people)} People")) +
    annotate("rect",
             xmin = if_else(min(cases_plot_data$date) < sah_start,
                            sah_start,
                            min(cases_plot_data$date)),
             xmax = sah_end, ymin = -Inf, ymax = Inf, alpha = sah_alpha) +
    annotate("text", y = 250/10, x = sah_end+8, label = "Stay at Home\nOrder Ended")

  return(list(hospitalizations_plot = hospitalizations_plot,
              icu_plot = icu_plot,
              cases_plot = cases_plot,
              deaths_plot = deaths_plot,
              data_date = max(hosp_tidy$date, cases_tidy$date)))
}


# oc city plots -----------------------------------------------------------
get_city_plots <- function(cities_of_interest){

  #first step is to grab hospital data so that we're starting on the same date as the other graphs
  # connect to CKAN instance
  ckanr_setup(url="https://data.ca.gov")
  ckan <- quiet(ckanr::src_ckan("https://data.ca.gov"))

  # get resources
  resources <- rbind(resource_search("name:covid-19", as = "table")$results,
                     resource_search("name:hospitals by county", as = "table")$results)

  resource_ids <- list(cases = resources$resource_id[resources$name == "COVID-19 Cases"],
                       tests = resources$resource_id[resources$name == "COVID-19 Testing"],
                       hosp = resources$resource_id[resources$name == "Hospitals By County"])


  hosp <- tbl(src = ckan$con, from = resource_ids$hosp) %>%
    as_tibble() %>%
    select(-starts_with("_")) %>%
    mutate(date = as.Date(todays_date)) %>%
    select(-todays_date)

  counties_of_interest <- c("San Diego",  "Los Angeles",  "Orange", "Alameda", "Santa Clara")

  county_pop <- read_csv("data/county_pop.csv") %>% rename_all(str_to_lower)


  hosp_tidy <- hosp %>%
    filter(county %in% counties_of_interest) %>%
    pivot_longer(cols = -c(date, county)) %>%
    drop_na() %>%
    left_join(county_pop) %>%
    arrange(county, date)

  hospitalizations_plot_data <- hosp_tidy %>%
    filter(name == "hospitalized_covid_patients") %>%
    group_by(county) %>%
    mutate(value = runMean(value, 7)) %>%
    drop_na()

  min_date <- min(hospitalizations_plot_data$date)
  # now read in city data
  # cities_of_interest <- c("Anaheim", "Santa Ana", "Irvine", "Huntington Beach", "Garden Grove")

  city_data <- read.csv("data/oc_city_data.csv")

  city_data$posted_date <- as.Date(city_data$posted_date)

  city_pop <- read_csv("data/city_pop.csv") %>% rename_all(str_to_lower)

  # stay at home order start and end dates
  sah_start <- ymd("2020-03-19")
  sah_end <- ymd("2020-05-04")

  #max date truncated for reporting delays
  max_date <- max(city_data$posted_date)-days(11)

  per_n_people <- 1e5 # denominator for reporting counts (e.g, per million people)
  ma_n <- 7 # moving average window length in days

  # Tidy Data
  tidy_city <- city_data%>%
    filter(city %in% cities_of_interest)%>%
    left_join(city_pop)%>%
    mutate(
      positivity=(new_cases/new_tests)*100
    )%>%
    filter(posted_date >= sah_start  & posted_date <= max_date)


  # Theme options
  theme_set(theme_bw(base_size = 12.5) +
              theme(legend.position = "bottom",
                    legend.title = element_text(size = 12)))

  # keep color palette consistent across county sets

  cbPalette <- c("#56B4E9", "#009E73", "#E69F00", "#CC79A7", "#0072B2",
                 "#A857E6", "#1980E6", "#CC625E", "#CAA1E6", "#E6CA5C")

  sah_alpha <- 0.2

  gglayers = list(
    geom_line(size = 1.5),
    scale_color_manual(name = "City", values = cbPalette),
    scale_x_date(name = "Date",
                 breaks = "14 day",
                 date_labels = "%b %d",
                 expand = expansion(add=c(0,7))),
    scale_y_continuous(label = comma)
  )

  # Positivity
  positive_plot_data <- tidy_city %>%
    group_by(city) %>%
    mutate(
            sum_pos=runSum(new_cases, ma_n),
            sum_test=runSum(new_tests, ma_n),
            value = (sum_pos/sum_test)*100) %>%
    drop_na()%>%
    filter(posted_date >= min_date)

  positive_plot <- positive_plot_data %>%
    ggplot(aes(posted_date, value, group = city, color = city)) +
    ggtitle(glue("Percent Positive COVID-19 Tests <span style='font-size:14pt'>({ma_n} Day Moving Window)</span>")) +
    #subtitle = glue("{ma_n} Day Moving Average"))
    theme(
      plot.title = element_markdown()
    ) +
    gglayers +
    ylab(glue("Percent Positive COVID-19 Tests")) +
    annotate("rect",
             xmin = if_else(min(positive_plot_data$posted_date) < sah_start,
                            sah_start,
                            min(positive_plot_data$posted_date)),
             xmax = sah_end, ymin = -Inf, ymax = Inf, alpha = sah_alpha) +
    annotate("text", y = 15, x = sah_end+8, label = "Stay at Home\nOrder Ended")
  # Testing
  testing_plot_data <- tidy_city %>%
    group_by(city) %>%
    mutate(value = runMean(new_tests, ma_n)) %>%
    drop_na()%>%
    filter(posted_date >= min_date)

  testing_plot <- testing_plot_data %>%
    ggplot(aes(posted_date, value / population * per_n_people, group = city, color = city)) +
    ggtitle(glue("Total COVID-19 Tests Peformed <span style='font-size:14pt'>({ma_n} Day Moving Average)</span>")) +
    #          subtitle = glue("{ma_n} Day Moving Average")) +
    theme(
      plot.title = element_markdown()
    ) +
    gglayers +
    ylab(glue("Total COVID-19 Tests Performed\nper {comma(per_n_people)} People")) +
    annotate("rect",
             xmin = if_else(min(testing_plot_data$posted_date) < sah_start,
                            sah_start,
                            min(testing_plot_data$posted_date)),
             xmax = sah_end, ymin = -Inf, ymax = Inf, alpha = sah_alpha) +
    annotate("text", y = 175, x = sah_end+8, label = "Stay at Home\nOrder Ended")


  # Deaths
  deaths_plot_data <- tidy_city %>%
    group_by(city) %>%
    mutate(value = runMean(new_deaths, ma_n)) %>%
    drop_na()%>%
    filter(posted_date >= min_date)

  deaths_plot <- deaths_plot_data %>%
    ggplot(aes(posted_date, value / population * per_n_people, group = city, color = city)) +
    ggtitle(glue("New Daily Deaths due to COVID-19 <span style='font-size:14pt'>({ma_n} Day Moving Average)</span>")) +
    #          subtitle = glue("{ma_n} Day Moving Average")) +
    theme(
      plot.title = element_markdown()
    ) +
    gglayers +
    ylab(glue("New Daily Deaths due to COVID-19\nper {comma(per_n_people)} People")) +
    annotate("rect",
             xmin = if_else(min(deaths_plot_data$posted_date) < sah_start,
                            sah_start,
                            min(deaths_plot_data$posted_date)),
             xmax = sah_end, ymin = -Inf, ymax = Inf, alpha = sah_alpha) +
    annotate("text", y = 1, x = sah_end+8, label = "Stay at Home\nOrder Ended")

  # Cases
  cases_plot_data <- tidy_city %>%
    group_by(city) %>%
    mutate(value = runMean(new_cases, ma_n)) %>%
    drop_na()%>%
    filter(posted_date >= min_date)

  cases_plot <- cases_plot_data %>%
    ggplot(aes(posted_date, value / population * per_n_people, group = city, color = city)) +
    ggtitle(glue("New Daily COVID-19 Cases <span style='font-size:14pt'>({ma_n} Day Moving Average)</span>")) +
    #          subtitle = glue("{ma_n} Day Moving Average")) +
    theme(
      plot.title = element_markdown()
    ) +
    gglayers +
    ylab(glue("New Daily Confirmed COVID-19 Cases\nper {comma(per_n_people)} People")) +
    annotate("rect",
             xmin = if_else(min(cases_plot_data$posted_date) < sah_start,
                            sah_start,
                            min(cases_plot_data$posted_date)),
             xmax = sah_end, ymin = -Inf, ymax = Inf, alpha = sah_alpha) +
    annotate("text", y = 25, x = sah_end+8, label = "Stay at Home\nOrder Ended")

  return(list(positive_plot = positive_plot,
              testing_plot = testing_plot,
              cases_plot = cases_plot,
              deaths_plot = deaths_plot,
              data_date = max(tidy_city$posted_date)))
}


