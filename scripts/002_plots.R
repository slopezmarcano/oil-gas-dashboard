suppressPackageStartupMessages(library(tidyverse)) #data wrangling and management
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(patchwork))
#--READ DATAFRAMES--#
waste_df <- read_csv('data/waste_df.csv')
waste_pred <- read_csv('data/waste_pred.csv')

#--WASTE AMOUNT CHANGES DURING 2022--#
p1<-ggplot(filter(waste_df, year(date) == 2022), aes(x = date, y = waste_amount, color = process_type)) +
  geom_line(size=1.5) +
  facet_wrap(~ process_type)+
  scale_color_manual(values = palettini) +
  labs(x = 'Years', y = 'Waste Amount', title= 'Waste Amount Across Productio Stages for Oil and Gas in 2022', subtitle='')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


p2 <- ggplot(waste_df, aes(x = waste_amount, y = industry, fill = waste_type)) +
  geom_density_ridges() +
  facet_grid(rows = vars(waste_type), labeller = labeller(waste_type =c("solid_waste" = "Solid Waste", "waste_water"="Water Waste")))+
  scale_fill_brewer(palette = 4) +
  labs(x = 'Industry', y = 'Waste Amount', title= 'Distribution of Waste Across Oil and Gas during 2017-2022', subtitle='')

p3<-ggplot(waste_pred, aes(x = date)) +
  geom_line(aes(y = oil_ww_pred, color = "Oil - Waste Water"), size = 1) +
  geom_ribbon(aes(ymin = oil_ww_ci_lower, ymax = oil_ww_ci_upper), alpha = 0.3, fill = "#36367b") +
  geom_line(aes(y = gas_ww_pred, color = "Gas - Waste Water"), size = 1) +
  geom_ribbon(aes(ymin = gas_ww_ci_lower, ymax = gas_ww_ci_upper), alpha = 0.3, fill = "#c463df") +
    scale_color_manual(name = "Industry - Waste Type",
                     values = c("#36367b", "#c463df"),
                     labels = c("Oil - Waste Water", "Gas - Waste Water")) +
  xlab("Years") +
  ylab("Waste Amount") +
  ggtitle("Predicted Waste Water Amounts for 2023-2026")+
  labs(subtitle = '')+
  theme(legend.position = c(0.2,0.9))

#SOLID WASTE
p4<-ggplot(waste_pred, aes(x = date)) +
  geom_line(aes(y = oil_sw_pred, color = "Oil - Solid Waste"), size = 1) +
  geom_ribbon(aes(ymin = oil_sw_ci_lower, ymax = oil_sw_ci_upper), alpha = 0.3, fill = "#af3a52") +
  geom_line(aes(y = gas_sw_pred, color = "Gas - Solid Waste"), size = 1) +
  geom_ribbon(aes(ymin = gas_sw_ci_lower, ymax = gas_sw_ci_upper), alpha = 0.3, fill = "#43c0ff") +
    scale_color_manual(name = "Industry - Waste Type",
                     values = c("#00A6FB", "#A63A50"),
                     labels = c("Oil - Solid Waste", "Gas - Solid Waste")) +
  xlab("Years") +
  ylab("Waste Amount (tonnes)") +
  ggtitle("Predicted Solid Waste Amounts for 2023-2026")+
  labs(subtitle = '')+
  theme(legend.position = c(0.2,0.9))

#--DASHBOARD ARRANGEMENT--#
(p1+p2)/(p3+p4)

#--EXPORTING DATA--#
ggsave('outputs/dashboard_a.pdf', width=30, height=15, dpi=400)
