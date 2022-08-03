
# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv') %>% 
  clean_names()

df <- frogs %>% 
  group_by(hab_type, water) %>% 
  summarise(count=n()) %>% 
  mutate(percent = count/sum(count)*100)

# Misc
font <- "Abel"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "grey85"
txt_col <- "black"

# Plot
df %>% 
  ggplot() +
  geom_bar(aes(x=percent, y=hab_type, fill=water), stat="identity",position = "stack") +
  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_met_d(name = "Hokusai1") +
  
  annotate("text", x = 68.5, y = 1, family = font, label = "Deep water", color="grey85", size=5
           ) +
  annotate("text", x = 44.5, y = 2, family = font, label = "Shallow water", size=5
  ) +
  annotate("text", x = 11.5, y = 3, family = font, label = "Unknown\nwater", color="grey85", lineheight=.8, size=5
  ) +
  annotate("text", x = 79.5, y = 3.6, family = font, label = "No water", size=5
  ) +
  
  annotate("segment", x = 79.5, xend = 79.5, y = 3.55, yend = 3,
           colour = txt_col, size=1) +
  
  labs(
    title="Oregon Spotted Frog Habitats",
    caption = "Gilbert Fontana | #TidyTuesday Week 31 | Data: USGS",
    x= "Percent of observed frogs"
    
  ) +
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.title.x  = element_text(size=16, color=txt_col, margin = margin(10,0,0,0)),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(size=12, color=txt_col),
    axis.text.y = element_text(size=20, color=txt_col),
    plot.title = element_text(size=32, color=txt_col, hjust=.5, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=10, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
    )

# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_31.png",
       height = 10,
       width = 10,
       dpi=320,
       
)  

showtext_auto(FALSE)
  
  
  
  
  
