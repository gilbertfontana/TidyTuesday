


# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(zoo)

# Data
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv') %>% 
  clean_names()

df <- flights %>% 
  mutate(flt_date2=as.yearmon(flt_date)) %>% 
  group_by(flt_date2) %>% 
  summarise(sum=sum(flt_tot_1))

df2 <- df %>% 
  filter(flt_date2==max(flt_date2))

# Misc
font <- "Josefin Sans"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "black"
col <- "#008A81"
col2 <- "#8A0009"


# Plot
df %>% 
  ggplot(aes(y=sum, x=flt_date2)) +
  geom_line(color=col) +
  geom_point(data=df2, aes(y=sum, x=flt_date2), color=col) +
  annotate("text", x=2018, y=1850000,
           label="The total number of arrivals and departures\nexceeded 1.6 million in july 2019",
           color=col2,
           family=font,
           fontface="bold",
           size=3,
           hjust=0.5,
           vjust=0,
           lineheight=.8) +
  geom_curve(
    aes(x = 2019.2, y = 1850000, xend =2019.5 , yend = 1700000),
    curvature = -0.25,
    arrow = arrow(length = unit(0.015, "npc")),
    color=col2
  ) +
  annotate("text", x=2018, y=300000,
           label="The total number of arrivals and departures fell to\nless than 150 thousand in april 2020",
           color=col2,
           family=font,
           fontface="bold",
           size=3,
           hjust=0.5,
           vjust=0,
           lineheight=.8) +
  geom_curve(
    aes(x = 2019, y = 250000, xend =2020.1, yend = 140000),
    curvature = 0.25,
    arrow = arrow(length = unit(0.015, "npc")),
    color=col2
  ) +
  annotate("text", x=2021.2, y=1600000,
           label="As of may 2022, the total number of arrivals\nand departures exceeded 1.3 million",
           color=col2,
           family=font,
           fontface="bold",
           size=3,
           hjust=0.5,
           vjust=0,
           lineheight=.8) +
  geom_curve(
    aes(x = 2021.2, y = 1550000, xend =2022.25, yend = 1350000),
    curvature = 0.25,
    arrow = arrow(length = unit(0.015, "npc")),
    color=col2
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(labels=function(x) format(x/1000, big.mark = " ", scientific = FALSE),
                     expand = c(0,0),
                     limits = c(0,2000000),
                     breaks = seq(500000,2000000,500000)) +
  labs(
    title = "Commercial flight activity in Europe",
    caption = "Gilbert Fontana | #TidyTuesday Week 28 | Data: Eurocontrol",
    y= "Total number of flights (arrivals and departures), thousands"
  ) +
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(color=txt_col, size=12),
    axis.text = element_text(color=txt_col, size=10),
    axis.line = element_line(),
    plot.title = element_text(size=20, color=txt_col, hjust=.5,lineheight=1, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
    legend.title = element_text(color=txt_col),
    legend.justification = "center",
    legend.title.align=0.5,
  ) 

# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_28.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)



            