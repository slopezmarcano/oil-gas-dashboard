#SLM
#Created:10032023


#--DESCRIPTION--#
#Data creation for the waste assesment portion of the oil and gas dashboard

#--LIBRARIES--#
suppressPackageStartupMessages(library(tidyverse)) #data wrangling and management
#suppressPackageStartupMessages(library(ggthemes)) #ggplot themes
#suppressPackageStartupMessages(library(ggtext)) #add and modify text to ggpplot
#suppressPackageStartupMessages(library(showtext)) #fonts
#font_add_google("Lato") #theme font
#showtext_auto()
#suppressPackageStartupMessages(library(arrrow)) #if using big data
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggridges))


#--INSTALL LIBRARIES--#
#install.libraries(c("tidyverse", "ggthemes", "ggtext", "showtext", "arrow"))

#--DATASET--#
#create the date range for the month of interest
dates <- seq(as.Date("2017-01-01"), as.Date("2023-03-31"), by = "month")

#create a dataframe with columns for the date, process type, waste type, and waste amount
df <- expand.grid(date = dates, process_type = c("exploration", "extraction", "development", "production"),
                  waste_type = c("waste_water", "solid_waste"), industry = c("oil", "gas"))

#random waste data
df$waste_amount <- round(runif(nrow(df), min = 0, max = 10500), 2)

#--PREDICTION DATAFRAME--#
#fit linear regression models to the data
oil_ww_lm <- lm(waste_amount ~ date, data = filter(df, industry == "oil" & waste_type == "waste_water"))
gas_ww_lm <- lm(waste_amount ~ date, data = filter(df, industry == "gas" & waste_type == "waste_water"))
oil_sw_lm <- lm(waste_amount ~ date, data = filter(df, industry == "oil" & waste_type == "solid_waste"))
gas_sw_lm <- lm(waste_amount ~ date, data = filter(df, industry == "gas" & waste_type == "solid_waste"))

#predict waste amounts from the rest of 2023 till the end of 2026
oil_ww_preds <- predict(oil_ww_lm, newdata = data.frame(date = seq(as.Date("2023-04-01"), as.Date("2026-12-31"), by = "month")), interval = "confidence")
gas_ww_preds <- predict(gas_ww_lm, newdata = data.frame(date = seq(as.Date("2023-04-01"), as.Date("2026-12-31"), by = "month")), interval = "confidence")
oil_sw_preds <- predict(oil_sw_lm, newdata = data.frame(date = seq(as.Date("2023-04-01"), as.Date("2026-12-31"), by = "month")), interval = "confidence")
gas_sw_preds <- predict(gas_sw_lm, newdata = data.frame(date = seq(as.Date("2023-04-01"), as.Date("2026-12-31"), by = "month")), interval = "confidence")

#combine predicted values and confidence intervals into a single dataframe
pred_df <- data.frame(date = seq(as.Date("2023-04-01"), as.Date("2026-12-31"), by = "month"),
                      oil_ww_pred = oil_ww_preds[, 1], oil_ww_ci_lower = oil_ww_preds[, 2], oil_ww_ci_upper = oil_ww_preds[, 3],
                      gas_ww_pred = gas_ww_preds[, 1], gas_ww_ci_lower = gas_ww_preds[, 2], gas_ww_ci_upper = gas_ww_preds[, 3],
                      gas_sw_pred = gas_sw_preds[, 1], gas_sw_ci_lower = gas_sw_preds[, 2], gas_sw_ci_upper = gas_sw_preds[, 3],
                      oil_sw_pred = oil_sw_preds[, 1], oil_sw_ci_lower = oil_sw_preds[, 2], oil_sw_ci_upper = oil_sw_preds[, 3])


#--EXPORT DATAFRAMES--#
write.csv(df, 'data/waste_df.csv')
write.csv(pred_df, 'data/waste_pred.csv')




