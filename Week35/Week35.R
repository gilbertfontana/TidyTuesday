


#### LIBS ####
library(tidyverse)
library(janitor)
library(MetBrewer)
library(scico)
library(showtext)
library(ggstream)

#### DATA ####
pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv') %>% 
  clean_names()

df <- pell %>% filter(name %in% c("Brown University","Columbia University","Cornell University","Dartmouth College","Harvard University","University of Pennsylvania","Princeton University","Yale University")
) %>% 
  select(name, year, recipient)

#### MISC ####
font <- "Rajdhani"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#C0A6AA"
txt_col <- "black"

#### PLOT ####
df %>% 
  ggplot() +
  geom_stream(aes(x=year, y=recipient, fill=name), bw = 1, color=NA, alpha=.9, extra_span = .2) +
  scale_x_continuous(limits = c(1999,2017),breaks = seq(1999,2017,1),
    sec.axis = sec_axis(trans=~.)) +
  scale_fill_scico_d(palette = "bamO", begin = 0, end = 1) +
  coord_cartesian(clip = "off") +
  labs(title="Ivy League Pell Grant Recipients",
       caption = "Gilbert Fontana | #TidyTuesday Week 35 | Data: US Department of Education") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted"),
    axis.line.x = element_line(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(color=txt_col, size=10),
    axis.text.x.top = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size=32, color=txt_col, hjust=0.5,lineheight=1, face="bold", margin=margin(0,0,40,0)),
    plot.subtitle = element_text(size=20, color=txt_col, hjust=0.5,lineheight=1, face="plain", margin=margin(0,0,0,0)),
    plot.caption = element_text(size=10, color=txt_col, hjust=0.5, margin=margin(20,0,0,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(60,30,60,30),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(title.position= "top",
                             title.hjust = .5,
                             nrow=2,byrow=TRUE))


#### SAVE ####
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_35.png",
       height = 10,
       width = 10,
       dpi=320
       
)  
