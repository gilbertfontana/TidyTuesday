
#### LIBS ####
library(tidyverse)
library(janitor)
library(scico)
library(showtext)

#### DATA ####
chips <- read_csv("path") %>% 
  clean_names()

glimpse(chips)

df <- chips %>% 
  mutate(year=lubridate::year(as.Date(release_date))) %>% 
  select(year, transistors_million, process_size_nm) %>% 
  na.omit()

#### MISC ####
font <- "Outfit"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "black"

set.seed(10); df %>% 
  ggplot(aes(y=transistors_million, x=year)) +
  geom_jitter(height=1000, width=.8,shape=21, aes(fill=year), color=bg, size=9, alpha=.9) +
  scale_fill_scico(palette = "acton", direction = 1, begin = .8, end=0) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(10000,60000,10000),
                     limits = c(-1000,60000),
                     labels=function(x) format(x,
                                               big.mark = " ",
                                               scientific = FALSE)
                     ) +
  scale_size(range = c(5, 10)) +
  coord_cartesian(clip="off") +
  labs(title="Number of Transistors in Processing Units Over Time",
       caption = "Gilbert Fontana | #TidyTuesday Week 34 | Data: CHIP Dataset",
       y="Number of transistors (millions)") +
  theme(
    axis.text.x = element_text(color=txt_col, size=12, face="bold", margin=margin(20,0,0,0)),
    axis.text.y = element_text(color=txt_col, size=12, face="bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color=txt_col, size=14, margin=margin(0,20,0,0)),
    panel.grid = element_blank(),
    plot.title = element_text(size=24, color=txt_col, face="bold", hjust=0, margin=margin(0,0,30,0)),
    plot.title.position = "plot",
    plot.caption = element_text(size=10, color=txt_col, hjust=0.5, margin=margin(20,0,0,0)),
    plot.background = element_rect(fill = bg, color=bg),
    panel.background = element_rect(fill = bg,color=bg),
    legend.position = "none",
    plot.margin = margin(30,30,30,30)
  )

#### SAVE ####
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_34.png",
       height = 10,
       width = 10,
       dpi=320
       
)  


         
         