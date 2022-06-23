

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(ggtext)

# Data
tuesdata <- tidytuesdayR::tt_load(2020, week = 25)

df <- tuesdata$blackpast


df2 <- df %>% 
  filter(subject=="Black Hollywood") %>% 
  mutate(year=as.numeric(year))

df2 <- df2 %>% 
  mutate(pos=c(1,3,.5,2.5,1,1.5,3,1),
         pos2=c(1:8))

# Misc
font <- "Space Grotesk"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#F2D1C4"
txt_col <- "grey10"

# Plot
df2 %>% 
  ggplot() +
  geom_textbox(data=df2 %>% filter(pos2!=3) ,aes(x=pos, y=year, label=events),
               fill = NA,
               box.size = NA,
               family=font,
               size=2.5,
               hjust=0,
               vjust=.5) +
  geom_textbox(data=df2 %>% filter(pos2==3) ,aes(x=pos, y=year, label=events),
               fill = NA,
               box.size = NA,
               family=font,
               size=2.5,
               hjust=0,
               vjust=.3) +
  geom_point(aes(x=pos, y=year)) +
  geom_segment(aes(x=0, xend=pos, y=year, yend=year)) +
  scale_x_continuous(limits = c(0,4), expand = c(0,0)) +
  scale_y_reverse() +
  coord_cartesian(clip="off") +
  labs(
    title='Black Hollywood',
    subtitle = "- A glimpse of the history",
    caption = "Gilbert Fontana | #TidyTuesday Week 25 | Data: Black Past",
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text.y = element_text(color=txt_col),
    axis.text.x = element_blank(),
    axis.line.y = element_line(),
    plot.title = element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,60,30,30),
    legend.position = "bottom",
    legend.title = element_text(color=txt_col),
    legend.justification = "center",
    legend.title.align=0.5,
  )


# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_25.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)



