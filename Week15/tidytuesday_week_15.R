

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(geofacet)


# Import data + cleaning
tuesdata <- tidytuesdayR::tt_load(2022, week = 15)

fuel_gdp <- tuesdata$fuel_gdp
fuel_access <- tuesdata$fuel_access
death_timeseries <- tuesdata$death_timeseries
death_source <- tuesdata$death_source
death_fuel <- tuesdata$death_fuel
indoor_pollution <- tuesdata$indoor_pollution

region <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
  clean_names() %>% 
  select(code=alpha_3,code2=alpha_2,region)

indoor_pollution <- indoor_pollution %>% 
  clean_names()


indoor_pollution <- left_join(
  indoor_pollution,
  region
) %>%
rename(var=deaths_cause_all_causes_risk_household_air_pollution_from_solid_fuels_sex_both_age_age_standardized_percent) %>% 
mutate(code2=replace(code2, code=="NAM", "NAM")) %>% 
  filter(year==2019)


africa_countries_grid1 <- africa_countries_grid1 %>% 
  mutate(name2=name) %>% 
  mutate(name2=recode(name2,
                      'Central African Republic'='C.A. Republic',
                      "Democratic Republic of the Congo"="Congo, D. Rep.",
                      "Republic of the Congo"="Congo, Rep.",
                      "Equatorial Guinea" = "Equat. Guinea",
                      "São Tomé and Principe"="S.T. and Princ."
                      )
  )



# Misc
f1 <- "Roboto Condensed"
bg <- "#E5E3C9"
txt_col <- "black"


# Plot
indoor_pollution %>% 
  filter(region=="Africa") %>% 
  ggplot(aes(y="",x="")) +
  geom_point(aes(size=var,color=var)) +
  facet_geo(~ code2,
            grid = africa_countries_grid1,
            label="name2") +
  scale_size_continuous(range = c(4, 12),
                        breaks = c(0,5,10,15,20),
                        limits = c(0,20)) +
  scale_color_gradient(low = "#f6c200",
                        high = "#a00e00",
                       breaks = c(0,5,10,15,20),
                       limits = c(0,20)
                        ) +
  guides(color = guide_legend(title="Share of deaths, from any cause,\nwhich are attributed to indoor air pollution (%)"),
         size = guide_legend(title.position="top", 
                             title.hjust =0.5,
                             title="Share of deaths, from any cause,\nwhich are attributed to indoor air pollution (%)")
         ) +
  labs(title = "Share of Deaths Which Are Attributed to Indoor Air Pollution in Africa",
       caption ="Gilbert Fontana | #TidyTuesday Week 15 | Data: Our World in Data"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(family = f1, hjust=.5, size=10,face="bold"),
    legend.text = element_text(family = f1),
    legend.margin=margin(30,0,0,0),
    plot.background = element_rect(fill = bg, color = bg),
    strip.text = element_text(family = f1, color=txt_col,size=8),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 30, 20, 30),
    plot.title = element_text(colour=txt_col, size=18, family = f1, face="bold", hjust=.5, margin = margin(20,10,30,10)),
    plot.caption = element_text(hjust=0.5,colour=txt_col,family = f1)
  )

# Save
ggsave("tidytuesday_week_15.png",
       height = 25,
       width = 20,
       units = "cm",
       type = "cairo",
       dpi=320
)  





