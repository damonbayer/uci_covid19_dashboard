library(ckanr) # library to interact with CA Gov. Public Health Portal
library(dplyr)

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
cases <- dplyr::tbl(src = ckan$con, from = res_ids$cases) %>% as_tibble(.)
tests <- dplyr::tbl(src = ckan$con, from = res_ids$test) %>% as_tibble(.)
hosp <- dplyr::tbl(src = ckan$con, from = res_ids$hosp) %>% as_tibble(.)


