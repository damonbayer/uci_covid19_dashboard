library(tidyverse)
library(lubridate)
library(glue)
library(scales)
library(cowplot)
library(fs)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

oc_zip_month_data <- read_csv("data/oc_zip_month_data.csv",
                              col_types = cols(.default = col_integer(),
                                               zip = col_character(),
                                               city = col_character(),
                                               SeptIncid = col_skip()))

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
  annotate("text", y = 175/10, x = sah_end+21, label = "Stay at Home\nOrder Ended")

make_city_plots <- function(city_name, per_n_people = 1e5, start_date = "0000-01-01", end_date = "9999-12-31") {
  tmp_dat <- oc_zip_month_data %>%
    filter(city == city_name) %>%
    select(-city) %>%
    pivot_longer(-c(zip, population)) %>%
    separate(name, c("type", "year", "month")) %>%
    mutate(day = "01") %>%
    unite(date, c("year", "month", "day"), sep = "-") %>%
    mutate(date = ymd(date)) %>%
    filter(date >= start_date, date <= end_date)

  tests_plot <- tmp_dat %>%
    filter(type == "tests") %>%
    ggplot(aes(date, value / population * per_n_people, group = zip, color = zip)) +
    geom_line() +
    geom_point() +
    cowplot::theme_cowplot() +
    scale_x_date(name = "Month", date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(name = glue("Tests per {comma(per_n_people)} People"), labels = comma) +
    ggtitle(city_name)

  cases_plot <- tmp_dat %>%
    filter(type == "cases") %>%
    ggplot(aes(date, value / population * per_n_people, group = zip, color = zip)) +
    geom_line() +
    geom_point() +
    cowplot::theme_cowplot() +
    scale_x_date(name = "Month", date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(name = glue("Cases per {comma(per_n_people)} People"), labels = comma) +
    ggtitle(city_name)

  positivity_plot <- tmp_dat %>%
    pivot_wider(names_from = type, values_from = value) %>%
    mutate(positivity = cases / tests) %>%
    ggplot(aes(date, positivity, group = zip, color = zip)) +
    geom_line() +
    geom_point() +
    cowplot::theme_cowplot() +
    scale_x_date(name = "Month", date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(name = glue("Positivity"), labels = percent) +
    ggtitle(city_name)

  list(tests = tests_plot, cases = cases_plot, positivity = positivity_plot)
}

city_name <- "Santa Ana"
make_city_plots(city_name, end_date = "2020-11-01") %>%
  imap(~ggsave(plot = .x + theme(legend.position = "bottom") + scale_color_manual(values = cbPalette), filename = path("~", "Desktop", glue("{city_name} {.y}"), ext = ".png"), width = 5, height = 4))
city_name <- "Anaheim"
make_city_plots(city_name, end_date = "2020-11-01") %>%
  imap(~ggsave(plot = .x + theme(legend.position = "bottom") + scale_color_manual(values = cbPalette), filename = path("~", "Desktop", glue("{city_name} {.y}"), ext = ".png"), width = 5, height = 4))
