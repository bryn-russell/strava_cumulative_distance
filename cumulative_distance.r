library(rStrava)
library(tidyverse)
library(scales)
library(ggrepel)
app_name <- '' # chosen by user
app_client_id  <- '' # an integer, assigned by Strava
app_secret <- '' # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))


my_acts <- get_activity_list(stoken)

data <- compile_activities(my_acts)

runs <- data %>%
  filter(type %in% "Run") %>%
  mutate(year = strtrim(start_date,4),
         date = as.Date(start_date)) %>%
  arrange(date) %>%
  select(date, distance, year) %>%
  mutate(fake_date = substr(date,5,10)) %>%
  mutate(fake_date = paste0("2000", fake_date)) %>%
  mutate(fake_date = as.Date(fake_date)) %>%
  group_by(year) %>%
  mutate(cumulative_distance = cumsum(distance))
runs  


pdf("cumulate_distance.pdf")
runs %>%
  mutate(label = if_else(date == max(date), as.character(year), NA_character_)) %>% 
  ggplot(aes(x = fake_date, y = cumulative_distance, color = year)) + geom_line() +
  theme_bw() + 
  xlab("Month") + ylab("Cumulative Distance (km)") + 
  scale_x_date(date_breaks = '1 month', date_labels = "%b") + 
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE)  + 
  theme(legend.position="none")
dev.off()
