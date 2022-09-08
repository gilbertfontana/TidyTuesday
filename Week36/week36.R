
#### LIBS ####
library(tidyverse)
library(janitor)
library(MetBrewer)
library(scico)
library(showtext)
library(patchwork)

#### DATA ####
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')


df1 <- sets %>% 
  group_by(year) %>% 
  summarise(mean_parts=mean(num_parts))


df2 <- sets %>% 
  arrange(num_parts) %>% 
  slice_tail(n=10) %>% 
  select(name, year, num_parts) %>% 
  mutate(name_year=paste0(name," ","(",year,")"))

#### MISC ####
font <- "Chakra Petch"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "grey70"
txt_col <- "black"

#### PLOT ####
p1 <- df1 %>% 
  ggplot() +
  geom_line(aes(x=year, y=mean_parts), size=.75, alpha=.8, color=scico(1, palette = 'tokyo')) +
  geom_point(data = subset(df1,year==2022), aes(x=year, y=mean_parts), size=3, alpha=.8, color=scico(1, palette = 'tokyo')) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1950,2020,10), name="Release date") +
  scale_y_continuous(expand = c(0,0), limits = c(0,350), name="Average number of pieces") +
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_line(linetype = "dotted", colour = txt_col),
    axis.line = element_line(),
    axis.title.x  = element_text(color=txt_col, size=10, margin=margin(10,0,0,0)),
    axis.title.y  = element_text(color=txt_col, size=10, margin=margin(0,10,0,0)),
    axis.text.x = element_text(color=txt_col, size=9),
    axis.text.y = element_text(color=txt_col, size=9),
    plot.title = element_text(size=32, color=txt_col, hjust=0.5,lineheight=1, face="bold", margin=margin(0,0,40,0)),
    plot.subtitle = element_text(size=20, color=txt_col, hjust=0.5,lineheight=1, face="plain", margin=margin(0,0,0,0)),
    plot.caption = element_text(size=10, color=txt_col, hjust=0.5, margin=margin(20,0,0,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(0,0,0,0),
    legend.position = "bottom",
    legend.title = element_blank()
  )

p1

p2 <- df2 %>% 
  arrange(num_parts) %>%
  mutate(name_year=factor(name_year, levels=name_year)) %>% 
  ggplot() +
  geom_col(aes(x=num_parts, y=name_year, fill=num_parts), alpha=.8) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,12000),
                     breaks=seq(3000,12000,3000),
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE),
                     position = "top",
                     name="Number of pieces",
                     sec.axis = sec_axis(trans=~.,
                                         breaks=seq(3000,12000,3000),
                                         labels=function(x) format(x, big.mark = ".", scientific = FALSE))) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_scico(palette = 'tokyo', direction = 1,begin = .4,end=0) +
  coord_cartesian(clip="off") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", colour = txt_col),
    axis.line.y = element_line(),
    axis.title.x  = element_text(color=txt_col, size=10, margin=margin(0,0,0,0)),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(color=txt_col, size=9),
    axis.text.x.top = element_blank(),
    axis.text.y = element_text(color=txt_col, size=9, face="bold"),
    plot.title = element_text(size=32, color=txt_col, hjust=0.5,lineheight=1, face="bold", margin=margin(0,0,40,0)),
    plot.subtitle = element_text(size=20, color=txt_col, hjust=0.5,lineheight=1, face="plain", margin=margin(0,0,0,0)),
    plot.caption = element_text(size=10, color=txt_col, hjust=0.5, margin=margin(20,0,0,0)),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(0,0,0,0),
    legend.position = "none"
  )

p2

text <- tibble(
  x = 0, y = 0,
  label = "The average number of pieces in LEGO sets has increased over time. The biggest LEGO set, the **World Map**, was released in 2021. The set consists of 11.695 pieces, which is 1.708 pieces more than the second biggest LEGO set, **The Ultimate Battle for Chima**. Other notable LEGO sets are the **Colosseum** and **Taj Mahal**, inspired by the New 7 Wonders of the World. Well-known Star Wars vehicles, such as the **Millennium Falcon** and the **AT-AT** walker, are also present on the top list of the biggest LEGO sets ever."
)

p3 <- ggplot(text, aes(x = x, y = y)) +
  ggtext::geom_textbox(
    aes(label = label),
    box.color = bg, fill=bg, width = unit(15, "lines"), height = unit(17, "lines"),
    family=font, size = 4.5, lineheight = 1.2
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color=bg, fill=bg))

p3

((p3 + p2)/ p1) + 
  plot_annotation(title="The Number of Pieces in LEGO Sets",
                  caption = "Gilbert Fontana | #TidyTuesday Week 36 | Data: LEGO database") &
  theme(
    plot.title = element_text(size=28, color=txt_col, hjust=0.5,lineheight=1, face="bold", margin=margin(0,0,0,0)),
    plot.caption = element_text(size=10, color=txt_col, hjust=0.5, margin=margin(20,0,0,0)),
    plot.margin = margin(30,30,30,30),
    plot.background = element_rect(color=bg, fill=bg)
  )


#### SAVE ####
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_36.png",
       height = 10,
       width = 10,
       dpi=320,
       bg=bg)  
