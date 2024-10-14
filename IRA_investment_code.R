library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)
library(scales)

#GGplot theme from https://datavizs23.classes.andrewheiss.com/example/05-example.html, with modifications
my_pretty_theme <- theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.7)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(1.3), color = "grey70"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.7), 
                                    color = "grey70", hjust = 0),
        # Bold legend titles
        legend.title = element_text(face = "bold"),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(1.1), hjust = 0),
        # Bold axis titles
        axis.title = element_text(face = "bold"),
        # Add some space above the x-axis title and make it left-aligned
        axis.title.x = element_text(margin = margin(t = 10), hjust = 0),
        # Add some space to the right of the y-axis title and make it top-aligned
        axis.title.y = element_text(margin = margin(r = 10), hjust = 1),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_rect(fill = "grey90", color = NA),
        # Add a thin grey border around all the plots to tie in the facet titles
        panel.border = element_rect(color = "grey90", fill = NA),
        axis.text.x = element_text(angle = 30, hjust = 1)
        )

#Quarterly investments summary
quarterly_investments <- read_csv("quarterly_actual_investment.csv") %>% 
  clean_names() %>% 
  drop_na(period)
  
#Individual facility investments data
investments_meta <- read_csv("manufacturing_energy_and_industry_facility_metadata.csv") %>% 
  clean_names() %>% 
  mutate(announcement_date = mdy(announcement_date))

#GGRF communities data
communities <- read_csv("GGRF_1.0_communities.csv") %>% 
  clean_names() %>% 
  rename(census_tract_2010_geoid = census_tract_2010_id)

#Congressional district data for maps
congressional_post <- read_csv("congressional_district_actual_mfg_energy_and_ind_investment.csv") %>% 
  clean_names() %>% 
  filter(period == "Post-IRA")

congressional_post_r <- read_csv("congressional_district_actual_mfg_energy_and_ind_investment.csv") %>% 
  clean_names() %>% 
  filter(period == "Post-IRA", us_representative_party == "Republican")

congressional_post_d <- read_csv("congressional_district_actual_mfg_energy_and_ind_investment.csv") %>% 
  clean_names() %>% 
  filter(period == "Post-IRA", us_representative_party == "Democratic")

congressional_post_r %>% write_csv("congressional_post_IRA_republican.csv")
congressional_post_d %>% write_csv("congressional_post_IRA_democrat.csv")
  
#Summarize estimated CapEx by type and technology from individual investments list

#Define equal periods pre-IRA and post-IRA. The data goes to June 30, 2024, which is 684 days after the passage of the IRA. October 1, 2020 is 684 days before the passage of the IRA.
pre_IRA_start <- as.Date("2020-10-01")
pre_IRA_end <- as.Date("2022-08-16")
  
post_IRA_start <- as.Date("2022-08-16")
post_IRA_end <- as.Date("2024-06-30")

#Filter for quarters that fall in the pre-IRA and post-IRA periods (these flags were added in Excel)
investments_by_period <- quarterly_investments %>% 
  filter(period %in% c("Pre-IRA", "Post-IRA"))

#Create flags for records that fall in the pre-IRA and post-IRA periods
investments_by_period_meta <- investments_meta %>% 
  drop_na(total_facility_capex_estimated) %>% 
  mutate(period = case_when(
    announcement_date >= pre_IRA_start & announcement_date < pre_IRA_end ~ "Pre-IRA",
    announcement_date >= post_IRA_start & announcement_date <= post_IRA_end ~ "Post-IRA",
    TRUE ~ "Neither")) %>% 
  filter(period %in% c("Pre-IRA", "Post-IRA"),
         current_facility_status == c("A", "U"))

#Summarize pre-IRA vs. post-IRA and export to csv
summary_data <- investments_by_period %>% 
  group_by(segment, period) %>% 
  summarize(total_capex = sum(estimated_actual_quarterly_expenditure))

summary_data %>% write_csv("IRA_summary_data.csv")

#Create data set for manufacturing investments
manufacturing_investments_by_period <- investments_by_period %>%
  filter(segment == "Manufacturing") %>%
  group_by(technology, period) %>%
  summarize(capex_sum = sum(estimated_actual_quarterly_expenditure)*1000000) %>%
  arrange(desc(capex_sum))

quarterly_investments_manufacturing <- quarterly_investments %>% 
  filter(segment == "Manufacturing")

#Order the periods for graphing
manufacturing_investments_by_period$period <- factor(manufacturing_investments_by_period$period, levels = c("Pre-IRA", "Post-IRA"))

#Graph CapEx by technology and period
manufacturing_investments_by_period_graph <- manufacturing_investments_by_period %>%
  ggplot(aes(x=reorder(technology, -capex_sum), y=capex_sum, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Technology Type", y = "Capital Expenditure",
       title = "After the IRA, cleantech manufacturing investment surged",
       subtitle = "Clean energy related manufacturing investment by technology type",
       fill = "Period",
       caption = "Showing actual investment in each two-year period. Source: Clean Investment Monitor."
  ) +
  my_pretty_theme +
  scale_y_continuous(
    labels = dollar_format(scale = 1e-9, suffix = "B", accuracy = 1),
    breaks = c(0, 20e9, 40e9, 60e9)
  )+  scale_fill_manual(values=c('#9654e5','#019c00'))

manufacturing_investments_by_period_graph

ggsave("manufacturing_investments_by_period_graph.jpeg", manufacturing_investments_by_period_graph, width = 2500, units = "px")

manufacturing_investments_quarterly_graph <- quarterly_investments_manufacturing %>%
  ggplot(aes(x=quarter, y=estimated_actual_quarterly_expenditure, fill=technology)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Quarter", y = "Capital Expenditure",
       title = "Cleantech manufacturing investment is still growing",
       subtitle = "Clean energy related manufacturing investment by quarter and technology type",
       fill = "Technology Type",
       caption = "Showing actual investment in each quarter. Source: Clean Investment Monitor."
  ) +
  my_pretty_theme +
  scale_y_continuous(
    labels = dollar_format(scale = 1e-3, suffix = "B",accuracy = 1)
  ) + 
  scale_fill_manual(values=c('#9654e5','#019c00','#4328e7','#ff8800','#1aa7ee','#019c00','#ff6283'))

manufacturing_investments_quarterly_graph

ggsave("manufacturing_investments_quarterly_graph.jpeg", width = 2500, units = "px")

#Create table comparing post-IRA investment by congressional district
investment_by_color <- congressional_post %>% 
  group_by(us_representative_party) %>% 
  summarize(capex_sum = sum(estimated_actual_quarterly_expenditure))
