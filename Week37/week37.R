

#### LIBS ####
library(tidyverse)
library(janitor)
library(MetBrewer)
library(scico)
library(showtext)
library(patchwork)
library(sf)
library(maps)
library(ggtext)


#### DATA ####
bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')


#### MISC ####
font <- "Albert Sans"
font_add_google(family=font, font,db_cache = FALSE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#DFE3E8"
txt_col <- "black"
col1 <- "#1A3399"
col2 <- "#7E1900"

#### PLOT ####

# Map
map <- bigfoot %>% 
  filter(as.character(date) %in% c("1998-10-15","2000-07-15")) %>% 
ggplot() + 
  geom_polygon(data=map_data("state"), aes(x=long, y=lat, group=group),
                color=bg, size=1, fill="#60C3D4") +
  geom_point(aes(x = longitude, y = latitude, color=as.character(date)), size=3) +
  scale_color_manual(values = c(col1,col2)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_map(clip="off") +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=20, color=txt_col, hjust=.5,lineheight=1, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(0,0,0,0),
    legend.position ="none"
  )

map

# TEXT (subtitle)
text <- tibble(
  x = 0, y = 0,
  label = "Throughout history, Bigfoot has been spotted on several occasions during the same day.
  Of the 5,021 reported Bigfoot sightings in the United States, 665 occurred concurrently.
  On <span style = 'color:#1A3399;'>**October 15th, 1998**</span> and <span style = 'color:#7E1900;'>**July 15th, 2000**</span>,
  Bigfoot was spotted **8** times across several states."
)

p3 <- ggplot(text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg, fill=bg, width = unit(20, "lines"),
    family=font, size = 6, lineheight = 1
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color=bg, fill=bg))

text2 <- tibble(
  x = 0, y = 0,
  label = "**Bigfoot Strives Over a Vast Area**"
)

# TITLE
p4 <- ggplot(text2, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg, fill=bg, width = unit(22, "lines"),
    family=font, size = 18, lineheight = 1
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color=bg, fill=bg))

# CAPTION
caption_text  <- str_glue("Gilbert Fontana | #TidyTuesday Week 37 | Data: Bigfoot Field Researchers Organization (BFRO)<br>",
                          "<span style='font-family: \"fa-brands\"'>&#xf09b;</span> gilbertfontana ",
                          "<span style='font-family: \"fa-brands\"'>&#xf099;</span> GilbertFontana")
                          
#### PATCHWORK ####
(p4+p3)/map + 
  plot_layout(heights = c(1,3)) +
  plot_annotation(caption = caption_text) &
  theme(plot.caption = element_markdown(size=12, color=txt_col,face = "plain", hjust=0.5, margin=margin(0,0,0,0), lineheight = 1.4),
        plot.margin = margin(30,30,30,30))

#### SAVE ####
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_37.png",
       height = 10,
       width = 10,
       dpi=320,
       bg=bg) 

