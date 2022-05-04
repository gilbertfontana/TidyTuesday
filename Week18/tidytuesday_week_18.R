

# Lib
library(tidyverse)
library(MetBrewer)
library(showtext)
library(ggtext)

# Import data
capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')


# Data manipulation
solar_wind <- full_join(
  solar,
  wind
) %>% 
  select(date, solar_mwh, wind_mwh) %>% 
  pivot_longer(
    !date,
    names_to = "type",
    values_to = "values"
  )

# Misc
font <- "Smooch Sans"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 

pal <- c("#f6c200", "#0086a8")
bg <- "#082032"
txt_col <- "grey90"

# Plot
ggplot(solar_wind, aes(x=date,y=values, groups=type)) +
  geom_point(aes(color=type,size=values),shape=21, stroke=1.5) +
  geom_smooth(span=1, aes(color=type,fill = type)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values=pal) +
  labs(title = "PPA prices for <span style='color:#f6c200;'>**solar**</span> and <span style='color:#0086a8;'>**wind**</span>",
       caption = "Gilbert Fontana | #TidyTuesday Week 18 | Data: Berkeley Lab"
  ) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = font) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=14),
    plot.title = element_text(hjust=.5, size=60, color=txt_col, margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5,margin=margin(30,0,0,0), face="bold", size=12, color=txt_col),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
  ) +
  theme(
    plot.title = element_markdown()
  )


#Save

showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_18.png",
       height = 8,
       width = 12,
       dpi=320,
       
)  

showtext_auto(FALSE)

