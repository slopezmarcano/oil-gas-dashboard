#SLM
#Created:


#--DESCRIPTION--#
#Data creation for the risk assesment portion of the oil and gas dashboard

#--LIBRARIES--#
suppressPackageStartupMessages(library(tidyverse)) #data wrangling and management
#suppressPackageStartupMessages(library(ggthemes)) #ggplot themes
#suppressPackageStartupMessages(library(ggtext)) #add and modify text to ggpplot
#suppressPackageStartupMessages(library(showtext)) #fonts
#font_add_google("Lato") #theme font
#showtext_auto()
#suppressPackageStartupMessages(library(arrrow)) #if using big data
suppressPackageStartupMessages(library(lubridate))

#--INSTALL LIBRARIES--#
#install.libraries(c("tidyverse", "ggthemes", "ggtext", "showtext", "arrow"))

#--DATASET--#
# Set seed for reproducibility
set.seed(123)

#Data for the past 2 years
dates <- seq(as.Date("2021-03-01"), as.Date("2023-02-28"), by = "day")
df <- data.frame(date = dates)

#Week of month variable
df$week_of_month <- week(df$date) - week(floor_date(df$date, "month")) + 1

#Risk assessment event type
df$type <- sample(c("Negligible", "Marginal", "Critical", "Catastrophic"), size = nrow(df), replace = TRUE, 
                  prob = c(0.7, 0.2, 0.08, 0.02))

#Targeted probabilities for each event type
neg_prob <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.9, 0.8, 0.7, 0.6, 0.5)
mar_prob <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.8, 0.7, 0.6, 0.5)
cri_prob <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.4, 0.35, 0.3, 0.25)
cat_prob <- c(0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.04, 0.035, 0.03, 0.025)

#Initial counts are 0
neg_count <- 0
mar_count <- 0
cri_count <- 0
cat_count <- 0

#Loop through each event type and probabilities
for (i in 1:nrow(df)) {
  week <- df$week_of_month[i]
  if (df$type[i] == "Negligible") {
    prob <- neg_prob[week]
    if (runif(1) < prob) {
      neg_count <- neg_count + sample(1:3, 1)
    }
  } else if (df$type[i] == "Marginal") {
    prob <- mar_prob[week]
    if (runif(1) < prob) {
      mar_count <- mar_count + 1
    }
  } else if (df$type[i] == "Critical") {
    prob <- cri_prob[week]
    if (runif(1) < prob) {
      cri_count <- cri_count + 1
    }
  } else if (df$type[i] == "Catastrophic") {
    prob <- cat_prob[week]
    if (runif(1) < prob) {
      cat_count <- cat_count + 1
    }
  }
  df$neg_count[i] <- neg_count
  df$mar_count[i] <- mar_count
  df$cri_count[i] <- cri_count
  df$cat_count[i] <- cat_count
}

# Reshape the dataframe to long format using gather()
df_long <- df %>% 
  select(!c(type)) %>%
  gather(key = "event_type", value = "count", -date, -week_of_month) 



df_diff <- df_long %>% 
  mutate(date = format(date, "%Y-%m")) %>%
  group_by(event_type) %>% 
  arrange(date) %>% 
  mutate(count_diff = count - lag(count, default = 0)) %>%
  filter(!is.na(count_diff)) %>% 
  group_by(date, event_type) %>%
  summarise(sum = sum(count_diff)) %>%
  ungroup() %>%
  filter(!date == '2021-03')

ggplot(df_diff, aes(x = date, y = sum, group = event_type)) + 
  geom_line(size = 1) +
  scale_y_continuous(breaks = c(0,1,5,10,20,30,40))+
  #scale_x_date(date_labels = "%b %d") +
  labs(x = "Monthly Reports", y = "Accumulated Events Submitted")+ 
  coord_flip()+
  facet_grid('event_type', 
              labeller = labeller(event_type=c("neg_count"="Negligible Events", "mar_count" = "Marginal Events", "cri_count" = "Critical Events", "cat_count"="Catastrophic Events")))




#--EXPORTING DATA--#
ggsave('outputs/insert_file_name_here.pdf', width=, height=)