library(tidyverse)
library(lubridate)

read_csv('Oakland_WY2013-2021_Cleaned_hrly.csv') -> raw.data

raw.data %>%
  filter(HourlyPrecipitation > 0) %>%
  arrange(DATE) %>%
  mutate(last_precip = lag(DATE),
         event_start = ifelse(time_length(DATE - last_precip, unit='hour')>=6, 1, 0),
         event_start= ifelse(is.na(event_start),1,event_start),
         event_number = cumsum(event_start)) %>%
  group_by(event_number) %>% 
  summarize(start = min(DATE),
            end = max(DATE),
            length = time_length(max(DATE) - min(DATE), unit='hour') +1,
            total_precip = sum(HourlyPrecipitation)) %>%
  filter(total_precip >= 0.5) %>%
  arrange(start) %>%
  mutate(last_end = lag(end),
         hours_since_last = time_length(start - last_end, unit='hour')) %>%
  filter(hours_since_last >= 48) %>%
  mutate(wy = year(floor_date(start,'month') + months(3))) %>%
  group_by(wy) %>%
  summarize(sampling_events = n()) -> sampling.events

sampling.events

sampling.events %>%
  summarize(mean(sampling_events),
            var(sampling_events))
