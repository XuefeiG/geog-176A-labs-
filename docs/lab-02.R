library(zoo)
library(readxl)
library(knitr)
library(tidyverse)
covid = read.csv("data/covid.csv")

CAcases <- covid %>%
  filter(state == "California") %>%
  group_by(county, state) %>%
  mutate(DailyCases = cases - lag(cases)) %>%
  ungroup()

TopCounties <- CAcases %>%
  filter(date == max(date)) %>%
  group_by(county) %>%
  summarize(TopCases = max(cases)) %>%
  arrange(-TopCases) %>%
  head(5)
  kable(TopCounties,
        caption = 'Cumulative Cases in the 5 Worst Counties',
        col.names = c('County', 'Cumulative Cases'))

DailyNew <- CAcases %>%
  filter(date == max(date)) %>%
  slice_max(DailyCases, n=5) %>%
  select(county, DailyCases)
kable(DailyNew,
      caption = 'Total NEW Cases in the 5 Worst Counties',
      col.names = c('County', 'Daily New Cases'))

PopEs = read_excel("data/PopulationEstimates.xls", skip = 2)
