
# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

# Misc
font <- "Rajdhani"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#F0EBE3"
txt_col <- "black"

# Plot
wheels %>% 
  filter(status=="Operating") %>% 
  select(name, height, diameter, country) %>%
  na.omit() %>% 
  arrange(height) %>%    
  mutate(name=factor(name, levels=name)) %>% 
  slice_tail(n=10) %>% 
  ggplot() +
  geom_segment(aes(x=name, xend=name, y=0, yend=height)) +
  geom_point(aes(x=name, y=height, size=diameter, color=country)) + 
  geomtextpath::coord_curvedpolar() +
  scale_size_continuous(range = c(5, 10), guide = 'none') +
  scale_color_met_d(name="Tam") +
  labs(
    title="The worlds highest operating Ferris wheels",
    caption = "Gilbert Fontana | #TidyTuesday Week 32 | Data: @Emil_Hvitfeldt {ferriswheels}"
  ) +
  annotate("text", x = 8.15, y = 221.5, family = font, label = "Height", size=3.5
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text.x = element_text(size=10, color=txt_col),
    axis.text.y = element_blank(),
    plot.title = element_text(size=28, color=txt_col, hjust=.5, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=10, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
    legend.text = element_text(size=12),
    legend.title = element_text(size=14)
  ) +
  guides(color=guide_legend(title="Circle size represent the wheel diameter",
                            title.position = "top",
                            title.hjust = .5,
                            override.aes = list(size=8)))


# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_32.png",
       height = 10,
       width = 10,
       dpi=320,
       
)  

showtext_auto(FALSE)


