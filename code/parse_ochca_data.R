library(tidyverse)
library(lubridate)
library(here)

# Use this script to convert the CSVs provided by OCHCA to the format needed for the functions.R 

# Get Web Data ------------------------------------------------------------
get_ochca_web_data <- function(raw_html = read_lines("https://occovid19.ochealthinfo.com/coronavirus-in-oc"),
                               death_csv = file.path("data", "oc", "oc_deaths.csv"),
                               keep_extras = F) {
  process_table <- function(tbl) {
    tbl %>%
      str_remove_all("\\\\|\"") %>%
      str_extract_all("\\[[^\\[].*?\\]") %>%
      unlist() %>%
      str_remove_all("\\[|\\]") %>%
      enframe(name = NULL)
  }

  processed_tables <- raw_html %>%
    str_trim() %>%
    `[`(str_starts(., "\\w+ = \\[\\[")) %>%
    enframe(name = NULL, value = "raw_table") %>%
    mutate(name = as.vector(str_match(raw_table, "\\w+(?= =)"))) %>%
    select(name, raw_table) %>%
    mutate(raw_table = map(raw_table, process_table)) %>%
    deframe()

  split_tables <- list(
    testData = separate(processed_tables$testData, value, into = c("posted_date", "extra_testData1", "new_tests", "total_tests", "extra_testData2", "new_cases"), sep = ","),
    caseArr = separate(processed_tables$caseArr, value, into = c("posted_date", "new_cases", "total_cases", "extra_caseArr1"), sep = ","),
    caseAvg = separate(processed_tables$caseAvg, value, into = c("posted_date", "avg_trend"), sep = ","),
    hospitalArr = separate(processed_tables$hospitalArr, value, into = c("posted_date", "current_hospitalized", "current_icu", "hospital_completeness25"), sep = ",")
  )

  death_table <- read_csv(death_csv,
                          col_types = cols(
                            .default = col_integer(),
                            posted_date = col_date(format = "%m/%d/%y")))

  joined_table <- map(split_tables, ~mutate(., posted_date = lubridate::mdy(posted_date)) %>%
                        mutate_if(is.character, as.integer)) %>%
    append(list(death_table)) %>%
    reduce(full_join) %>%
    arrange(posted_date)

  if(!keep_extras) {
    joined_table <- select(joined_table, -starts_with("extra"))
  }

  joined_table
}

# write_rds(get_ochca_web_data(), here("data", "oc", "ochca_covid.rds"))

# Read and Sanitize -------------------------------------------------------
read_santize_ochca_covid <- function(file_name = "ochca_covid.csv") {
  read_csv(file_name,
           col_types = cols(.default = col_character(),
                            POSTED_DATE = col_date(format = "%m/%d/%Y"))) %>%
    rename_all(~str_to_lower(.) %>%
                 str_replace_all("_covid19_", "_") %>%
                 str_replace_all(" ", "_") %>%
                 str_remove(":")) %>%
    rename(new_tests = "people_tested_by_hca_public_health_lab_(phl)_and_commercial_labs_since_yesterday",
           total_tests = "total_people_tested_by_phl_and_commercial_labs_to_date") %>%
    mutate_if(is.character, ~na_if(., "Unavailable")) %>%
    mutate_if(is.character, ~str_trim(.) %>%
                str_replace("-", "0") %>%
                str_remove_all(",")) %>%
    mutate_if(is.character, as.integer)
}


# Get Line List Data ------------------------------------------------------
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
                            "not detected in pooled specimen")

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
                         "enterobacter cloacae complex (organism)"
)

read_line_list <- function(line_list_name = "10.19.20 release to UCI team.csv",
                           negative_line_list_name = "All ELR PCR tests updated 10.19.20.csv") {
  new_deaths_tbl <- read_csv(here::here("data", "oc", line_list_name),
                             col_types = cols(.default = col_skip(),
                                              `DtDeath` = col_date("%m/%d/%Y"))) %>%
    drop_na() %>%
    transmute(posted_date = `DtDeath`) %>%
    count(posted_date, name = "new_deaths") %>%
    arrange(posted_date)



  neg_line_list <- read_csv(here::here("data", "oc", negative_line_list_name),
                            col_types = cols(.default = col_skip(),
                                             PersonId = col_integer(),
                                             Specimen.Collected.Date = col_date("%m-%d-%Y"),
                                             Resulted.Organism = col_character())) %>%
    filter(!is.na(Resulted.Organism)) %>%
    mutate(test_result = fct_collapse(str_to_lower(Resulted.Organism),
                                      negative = negative_test_synonyms,
                                      positive = positive_test_synonyms,
                                      other = other_test_synonyms)) %>%
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

read_line_list_by_city <- function(line_list_name = "10.19.20 release to UCI team.csv",
                                   negative_line_list_name = "All ELR PCR tests updated 10.19.20.csv") {
  neg_line_list <- read_csv(here::here("data", "oc", negative_line_list_name),
                            col_types = cols(.default = col_skip(),
                                             PersonId = col_integer(),
                                             Specimen.Collected.Date = col_date("%m-%d-%Y"),
                                             Resulted.Organism = col_character(),
                                             Zip = col_character())) %>%
    filter(!is.na(Resulted.Organism)) %>%
    mutate(test_result = fct_collapse(str_to_lower(Resulted.Organism),
                                      negative = negative_test_synonyms,
                                      positive = positive_test_synonyms,
                                      other = other_test_synonyms)) %>%
    select(id = PersonId, posted_date = Specimen.Collected.Date, test_result, zip = Zip) %>%
    mutate(zip = str_sub(zip, end = 5)) %>%
    filter(posted_date >= lubridate::ymd("2020-01-01")) %>%
    drop_na() %>%
    group_by(id) %>%
    arrange(posted_date) %>%
    ungroup()


  new_deaths_tbl <- read_csv(here::here("data", "oc", line_list_name),
                             col_types = cols(.default = col_skip(),
                                              `DtDeath` = col_date("%m/%d/%Y"),
                                              Zip = col_character())) %>%
    drop_na() %>%
    select(posted_date = `DtDeath`, zip = Zip) %>%
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


  oc_zips <- read_csv(here("data", "oc", "oc_zips.csv"),
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



oc_data <- read_line_list()

oc_data_by_city <- read_line_list_by_city()

write.csv(oc_data, "oc_data.csv")
write.csv(oc_data_by_city, "oc_city_data.csv")
