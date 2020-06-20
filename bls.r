# Author: Kelsey Gonzalez
# Email: kelseygonzalez@email.arizona.edu
# Twitter: #KelseyEGonzalez
# Github: kelseygonzalez
# Date: 05/24/2020
# Goal - visualize the extremity of the changes in unemployment by US state using sparklines and small multiples
# learn more about  sparklines and small multiples here: https://datavizm20.classes.andrewheiss.com/content/08-content/

#####
# Load packages and fonts
require(blscrapeR) # to scrape the BLS API
require(geofacet) # for plotting grid
require(tidyverse)
require(scales)
extrafont::loadfonts(device = "win", quiet = TRUE)

#####
# scrape the data from the US Bureau of Labor Statistics 
bls <- 
  bls_api("LASST010000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Alabama") %>%
  bind_rows(bls_api("LASST040000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Arizona")) %>%
  bind_rows(bls_api("LASST050000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Arkansas")) %>%
  bind_rows(bls_api("LASST060000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "California")) %>%
  bind_rows(bls_api("LASST080000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Colorado")) %>%
  bind_rows(bls_api("LASST090000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Connecticut")) %>%
  bind_rows(bls_api("LASST100000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Delaware")) %>%
  bind_rows(bls_api("LASST110000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "District of Columbia")) %>%
  bind_rows(bls_api("LASST120000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Florida")) %>%
  bind_rows(bls_api("LASST130000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Georgia")) %>%
  bind_rows(bls_api("LASST160000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Idaho")) %>%
  bind_rows(bls_api("LASST170000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Illinois")) %>%
  bind_rows(bls_api("LASST180000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Indiana")) %>%
  bind_rows(bls_api("LASST190000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Iowa")) %>%
  bind_rows(bls_api("LASST200000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Kansas")) %>%
  bind_rows(bls_api("LASST210000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Kentucky")) %>%
  bind_rows(bls_api("LASST220000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Louisiana")) %>%
  bind_rows(bls_api("LASST230000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Maine")) %>%
  bind_rows(bls_api("LASST240000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Maryland")) %>%
  bind_rows(bls_api("LASST250000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Massachusetts")) %>%
  bind_rows(bls_api("LASST260000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Michigan")) %>%
  bind_rows(bls_api("LASST270000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Minnesota")) %>%
  bind_rows(bls_api("LASST280000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Mississippi")) %>%
  bind_rows(bls_api("LASST290000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Missouri")) %>%
  bind_rows(bls_api("LASST300000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Montana")) %>%
  bind_rows(bls_api("LASST310000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Nebraska")) %>%
  bind_rows(bls_api("LASST320000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Nevada")) %>%
  bind_rows(bls_api("LASST330000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "New_Hampshire")) %>%
  bind_rows(bls_api("LASST340000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "New_Jersey")) %>%
  bind_rows(bls_api("LASST350000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "New_Mexico")) %>%
  bind_rows(bls_api("LASST360000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "New_York")) %>%
  bind_rows(bls_api("LASST370000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "North_Carolina")) %>%
  bind_rows(bls_api("LASST380000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "North_Dakota")) %>%
  bind_rows(bls_api("LASST390000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Ohio")) %>%
  bind_rows(bls_api("LASST400000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Oklahoma")) %>%
  bind_rows(bls_api("LASST410000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Oregon")) %>%
  bind_rows(bls_api("LASST420000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Pennsylvania")) %>%
  bind_rows(bls_api("LASST440000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Rhode_Island")) %>%
  bind_rows(bls_api("LASST450000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "South_Carolina")) %>%
  bind_rows(bls_api("LASST460000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "South_Dakota")) %>%
  bind_rows(bls_api("LASST470000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Tennessee")) %>%
  bind_rows(bls_api("LASST480000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Texas")) %>%
  bind_rows(bls_api("LASST490000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Utah")) %>%
  bind_rows(bls_api("LASST500000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Vermont")) %>%
  bind_rows(bls_api("LASST510000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Virginia")) %>%
  bind_rows(bls_api("LASST530000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Washington")) %>%
  bind_rows(bls_api("LASST540000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "West_Virginia")) %>%
  bind_rows(bls_api("LASST550000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Wisconsin")) %>%
  bind_rows(bls_api("LASST560000000000003", startyear = 2020, endyear = 2020) %>% mutate(state = "Wyoming")) %>%
  dplyr::select(-c(year,period, latest, footnotes,seriesID))

write_csv(bls, path = "bls_May.csv")

#### 
# read in data and wrangle

bls <- read_csv("bls_May.csv") %>% 
  pivot_wider(id_cols = "state", names_from = periodName, values_from = value) %>% # change from long for to wide form
  mutate(diff = (May - January)/100) %>%  # calculate the difference between april and january for color plotting 
  pivot_longer(cols = -c(state, diff), names_to = "month", values_to = "value") %>% # switch back to long form for plotting 
  mutate(month = fct_relevel(as.factor(month), c("January", "February", "March", "April", "May")), # turn the month into a factor and reorder it 
         state = str_replace(state, "_", " "), # replace the "_" in the state names with spaces
         value = value / 100) # to probably render the percents

#### 
# plot

ggplot(bls, aes(x=month, y = value, group = state)) +
  geom_line(aes(color = diff), size = 2) +
  scale_color_gradient(name = "Rise in\nUnemployment",
                       low = "#fffeea", high = "#c03728", 
                       label = label_percent(accuracy = 1,
                                             trim = FALSE)) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 14) +
  theme(axis.text = element_blank(),
        axis.title = element_text(family = "Roboto Condensed Light"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = c(.9,.3),
        legend.title = element_text(size = 8, family = "Roboto Condensed Light"),
        strip.text = element_text(size = 12, "Roboto Condensed Light"),
        legend.text = element_text(size = 10)) +
  facet_geo(~ state, grid = "us_state_grid2", label = "code") +
  labs(title = "Which states have the sharpest increase in unemployment due to COVID-19?",
       caption = "Source: Bureau of Labor Statistics\ngithub.com/kelseygonzalez",
       x = "Month of 2020 (January - April)",
       y = "Unemployment Rate") 



ggsave("bls_unemployment.png", width = 10, height = 6)


