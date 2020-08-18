library(zoo)
library(readxl)
library(knitr)
library(tidyverse)
covid = read.csv("data/covid.csv")

covid %>%
  filter(state == "California") %>%
  group_by(county, state) %>%
  mutate(dailycases = cases - lag(cases)) %>%
  ungroup()
