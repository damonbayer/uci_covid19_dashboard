library(tidyverse)
library(lubridate)
library(here)

# Use this script to convert the CSVs provided by OCHCA to the format needed for the functions.R

negative_test_synonyms <- c("not detected",
                            "negative",
                            "coronavirus 2019 novel not detected",
                            "negative (qualifier value)",
                            "not detected (qualifier value)",
                            "sars cov-2 negative",
                            "undetected",
                            "inst_negative",
                            "neg-see report",
                            "sars-cov-2 rna not detected by naa",
                            "none detected",
                            "not detected in pooled specimen",
                            "not detected in pooled specimen (qualifier value)")

positive_test_synonyms <- c("detected",
                            "coronavirus 2019 novel positive",
                            "positive",
                            "positive (qualifier value)",
                            "sars cov-2 positive",
                            "detected (qualifier value)",
                            "presumptive pos",
                            "positive for 2019-ncov",
                            "presumptive positive",
                            "coronavirus 2019 novel presumptive pos",
                            "coronavirus 2019 novel detected",
                            "yes",
                            "coronavirus 2019 novel",
                            "presumptive positive for 2019-ncov",
                            "sars cov-2 presumptive pos",
                            "presumptive pos. for 2019-ncov",
                            "presumptive positive (qualifier value)",
                            "presumptive detected",
                            "reactive",
                            "sars-cov-2")

other_test_synonyms <- c("inconclusive",
                         "indeterminate",
                         "specimen unsatisfactory",
                         "invalid",
                         "test not performed",
                         "not provided (qualifier value)",
                         "see comment",
                         "tnp",
                         "coronavirus 2019 novel inconclusive",
                         "not tested",
                         "phoned results (and readback confirmed) to:",
                         "see note",
                         "unknown",
                         "clotted",
                         "coronavirus 2019 novel unsatisfactory",
                         # "cryptococcus neoformans",
                         "equivocal",
                         "non reactive",
                         "result comments",
                         "sars cov-2 inconclusive",
                         "test not done",
                         "test not perf",
                         "not pregnant",
                         "acinetobacter baumannii (organism)",
                         "multiple drug-resistant serratia marcescens",
                         "genus enterococcus",
                         "biofiresarsneg",
                         "equivocal result",
                         "coronavirus 2019 novel inconcluside",
                         "unsatisfactory",
                         "undefined",
                         "*corrected report* by",
                         "specimen unsatifactory for evaluation",
                         "warning....please disregard results.",
                         "presumptive result to be confirmed",
                         "indeterminate (qualifier value)",
                         "invalid result",
                         "specimen unsatisfactory for evaluation",
                         "specimen received mislabeled",
                         "enterococcus faecalis",
                         "carbapenem resistant pseudomonas aeruginosa",
                         "enterobacter cloacae complex (organism)",
                         "interpretive information: 2019 novel coronavirus sars-cov-2 by pcr"
)

read_line_list <- function(line_list_name, negative_line_list_name) {
  new_deaths_tbl <- read_csv(here::here("data", line_list_name)) %>%
    drop_na() %>%
    transmute(posted_date = lubridate::ymd(DtDeath)) %>%
    count(posted_date, name = "new_deaths") %>%
    arrange(posted_date)



  neg_line_list <- read_csv(here::here("data", negative_line_list_name),
                            col_types = cols(PersonId = col_integer(),
                                             Resulted.Organism = col_character(),
                                             .default = col_character())) %>%
    filter(!is.na(Resulted.Organism)) %>%
    mutate(
      Specimen.Collected.Date = lubridate::mdy(Specimen.Collected.Date),
      test_result = fct_collapse(trimws(str_to_lower(Resulted.Organism)),
                                  negative = negative_test_synonyms,
                                  positive = positive_test_synonyms,
                                  other = other_test_synonyms)
    ) %>%
    select(id = PersonId, posted_date = Specimen.Collected.Date, test_result) %>%
    filter(posted_date >= lubridate::ymd("2020-01-01")) %>%
    group_by(id) %>%
    arrange(posted_date) %>%
    ungroup()

  if(length(levels(neg_line_list$test_result)) != 3) warning("New test result category not accounted for.")

  first_pos <- neg_line_list %>%
    filter(test_result == "positive") %>%
    group_by(id) %>%
    summarise(first_pos = min(posted_date))

  neg_line_list_filtered <- left_join(neg_line_list, first_pos) %>%
    mutate(first_pos = replace_na(first_pos, lubridate::ymd("9999-12-31"))) %>%
    filter(posted_date <= first_pos) %>%
    select(-first_pos) %>%
    distinct()

  neg_line_list_filtered %>%
    count(posted_date, test_result) %>%
    pivot_wider(names_from = test_result, values_from = n) %>%
    full_join(new_deaths_tbl) %>%
    replace(is.na(.), 0) %>%
    mutate(new_cases = positive, new_tests = negative + positive + other) %>%
    select(posted_date, new_cases, new_tests, new_deaths)
}

read_line_list_by_city <- function(line_list_name, negative_line_list_name) {
  neg_line_list <- read_csv(here::here("data", negative_line_list_name),
                            col_types = cols(PersonId = col_integer(),
                                             Resulted.Organism = col_character(),
                                             Zip = col_character())) %>%
    filter(!is.na(Resulted.Organism)) %>%
    mutate(
      Specimen.Collected.Date = lubridate::mdy(Specimen.Collected.Date),
      test_result = fct_collapse(trimws(str_to_lower(Resulted.Organism)),
                                  negative = negative_test_synonyms,
                                  positive = positive_test_synonyms,
                                  other = other_test_synonyms)
    ) %>%
    select(id = PersonId, posted_date = Specimen.Collected.Date, test_result, zip = Zip) %>%
    mutate(zip = str_sub(zip, end = 5)) %>%
    filter(posted_date >= lubridate::ymd("2020-01-01")) %>%
    drop_na() %>%
    group_by(id) %>%
    arrange(posted_date) %>%
    ungroup()


  new_deaths_tbl <- read_csv(here::here("data", line_list_name),
                             col_types = cols(Zip = col_character(),
                                              DtDeath = col_character())) %>%
    drop_na() %>%
    mutate(posted_date = lubridate::ymd(DtDeath)) %>%
    select(posted_date, zip = Zip) %>%
    count(posted_date, zip, name = "new_deaths") %>%
    arrange(posted_date)

  if(length(levels(neg_line_list$test_result)) != 3) warning("New test result category not accounted for.")

  first_pos <- neg_line_list %>%
    filter(test_result == "positive") %>%
    group_by(id) %>%
    summarise(first_pos = min(posted_date))

  neg_line_list_filtered <- left_join(neg_line_list, first_pos) %>%
    mutate(first_pos = replace_na(first_pos, lubridate::ymd("9999-12-31"))) %>%
    filter(posted_date <= first_pos) %>%
    select(-first_pos) %>%
    distinct()


  oc_zips <- read_csv(here("data", "oc_zips.csv"),
                      col_types = cols(Zip = col_character())) %>%
    rename_all(str_to_lower) %>%
    group_by(city) %>%
    summarize(zip = list(zip),
              population = sum(population))


  neg_line_list_filtered_city <- neg_line_list_filtered %>%
    right_join(oc_zips %>% select(-population) %>% unnest(zip)) %>%
    count(posted_date, test_result, city) %>%
    pivot_wider(names_from = test_result, values_from = n)


  new_deaths_tbl_city <- new_deaths_tbl %>%
    right_join(oc_zips %>% select(-population) %>% unnest(zip)) %>%
    drop_na() %>%
    count(posted_date, city, wt = new_deaths, name = "new_deaths")

  full_join(neg_line_list_filtered_city, new_deaths_tbl_city) %>%
    replace(is.na(.), 0) %>%
    mutate(new_cases = positive, new_tests = negative + positive + other) %>%
    select(posted_date, city, new_cases, new_tests, new_deaths) %>%
    arrange(city, posted_date)
}


## Do the conversion

oc_data <- read_line_list(
  line_list_name = "11.9.20 release to UCI team.csv",
  negative_line_list_name = "All ELR PCR tests updated 11.9.20.csv"
)

oc_data_by_city <- read_line_list_by_city(
  line_list_name = "11.9.20 release to UCI team.csv",
  negative_line_list_name = "All ELR PCR tests updated 11.9.20.csv"
)

write.csv(oc_data, here("data", "oc_data.csv"))
write.csv(oc_data_by_city, here("data", "oc_city_data.csv"))
