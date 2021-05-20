library(tidyverse)
library(janitor)
library(lubridate)

cell_towers <- read_csv(
  "~/dataLocal/cellTowers/cell_towers.csv",
  col_type = cols(
    radio = col_factor(),
    mcc = col_double(),
    net = col_double(),
    area = col_double(),
    cell = col_double(),
    unit = col_double(),
    lon = col_double(),
    lat = col_double(),
    range = col_double(),
    samples = col_double(),
    changeable = col_double(),
    created = col_double(),
    updated = col_double(),
    averageSignal = col_double())
)

egypt <- cell_towers %>% 
  filter(mcc == 602)

egypt <- egypt %>% 
  mutate(created = as.POSIXct(created, origin="1970-01-01"),
         updated = as.POSIXct(updated, origin="1970-01-01"))
egypt <- egypt %>% 
  mutate(created_date = as.Date(created),
         created_week = floor_date(created, unit = "week"),
         created_month = floor_date(created, unit = "month"))

write_csv(egypt, "EgyptCellTower.csv")

egypt %>% 
  tabyl(net, radio) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns()

egypt %>% 
  group_by(created_week, net, radio) %>% 
  count() %>% 
  ungroup(created_week) %>% 
  mutate(cells = cumsum(n)) %>% 
  ggplot(aes(x = created_week, y = cells, col = radio)) +
  geom_line() +
  facet_wrap(vars(net))
