# setwd("C:/Users/SinghV54/Desktop/GPRS Research/Codementor/Self Learning/Tidy Tuesday/US Dairy Consumption")

# title: "US Dairy Analysis"
# author: "Vaibhav"
# date: "April 4, 2019"
# output: html_document



library(tidyverse)
theme_set(theme_light())
milk_products_facts <- read_csv("milk_products_facts.csv")

head(milk_products_facts)

milk_products_tidied <-   milk_products_facts %>%  
  gather(product, lbs_per_person, -year) %>% 
  separate(product, c("category","product"),fill = "right", extra = "merge") %>% 
  mutate(product = coalesce(product, category),
         product = str_to_title(str_replace_all(product, "_", " ")),
         category = str_to_title(category),
         product = ifelse(product == "Other", paste(product, category), product),
         year= as.Date("0000-01-01")+years(year))

milk_products_tidied %>% 
  group_by(category) %>% 
  count(category, product)

milk_products_tidied %>% 
  ggplot(aes(year, lbs_per_person))+
  geom_line()+
  facet_wrap(~product, scales = "free")+
  expand_limits(y=0)

library(forecast)
library(broom)
library(sweep)
library(timetk)
library(lubridate)

milk_product_ts <- milk_products_tidied %>% 
  nest(-category, -product) %>% 
  mutate(ts=map(data, tk_ts, start=1975, freq=1))

milk_product_ets <- milk_product_ts %>%
  mutate(model = map(ts, ets))

milk_product_ets %>%
  unnest(map(model, sw_glance))


milk_product_ets %>%
  unnest(map(model, sw_augment))




milk_product_ets %>% crossing(model_name = c("auto.arima", "ets")) %>%
  mutate(model = map2(model_name, ts, ~ invoke(.x, list(.y))),
         forecast = map(model, forecast, h = 10)) %>%
  unnest(map(forecast, sw_sweep)) %>%
  ggplot(aes(index, lbs_per_person, color = model_name, lty = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80), alpha = .5) +
  facet_wrap(~ product, scales = "free_y") +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = c(1980, 2000, 2020)) +
  scale_linetype_discrete(guide = FALSE) +
  labs(x = "Year",
       y = "Average US consumption (lbs per person)",
       title = "Forecasted consumption of dairy products",
       subtitle = "Based on USDA data 1975-2017. Showing 80% prediction intervals.",
       color = "Model")



