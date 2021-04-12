# small file converting 2019 census estimate into city pop
library(tidyverse)
library(here)


city_pops_old <- read.csv("data/city_pop.csv")
city_pops <- read_csv("data/ca_census_pop_2010_2019.csv")

city_pops <- city_pops %>%
  dplyr::select(City, Population = `2019`)

city_pops$City <- str_split_fixed(city_pops$City, ",", n = 2)[,1]

city_pops$City <- str_replace(city_pops$City, "city", "")

city_pops$City <-str_trim(city_pops$City)


testing <- city_pops %>%
  left_join(city_pops_old, by= "City") %>%
  drop_na()

write.csv(city_pops, "data/city_pop.csv")
