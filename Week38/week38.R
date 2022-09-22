

#### LIBS ####

library(tidyverse)
library(janitor)
library(MetBrewer)
library(scico)
library(showtext)
library(ggtext)
library(patchwork)
library(maps)


#### DATA ####
HydroWASTE_v10 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv') %>% 
  clean_names()

HydroWASTE_v10 %>% 
  group_by(status) %>% 
  count()

df1 <- HydroWASTE_v10 %>% 
  filter(!status %in% c("Closed","Decommissioned", "Non-Operational", "Projected", "Under Construction")) %>% 
  select(waste_id, iso_a3=cntry_iso, wwtp_name, lat_wwtp, lon_wwtp, pop_served, df)

map <- as_tibble(map_data("world")) %>% 
  filter(region !="Antarctica")


#### MISC ####
font <- "Josefin Sans"
font_add_google(family=font, font,db_cache = FALSE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "black"

# CAPTION
caption_text  <- str_glue("Gilbert Fontana | #TidyTuesday Week 38 | Data: Macedo et al (2022)<br>",
                          "<span style='font-family: \"fa-brands\"'>&#xf09b;</span> gilbertfontana ",
                          "<span style='font-family: \"fa-brands\"'>&#xf099;</span> GilbertFontana")

text <- tibble(
  x = 0, y = 0,
  label = "<span style = 'color:#646049;'>**Dilution factor equal to 10 or more**</span><br><span style = 'color:#77333B;'>**Dilution factor less than 10**</span>"
)

p_text <- ggplot(text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg, fill=bg, width = unit(15, "lines"),
    family=font, size = 4, lineheight = 1
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color=bg, fill=bg))

#### PLOT ####

p <- map %>% 
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group),fill="#84B6A2",color="white", size=.1) +
  geom_point(data=df1 %>% filter(df>=10), aes(x=lon_wwtp, y=lat_wwtp), color="#646049", alpha=.6, shape=16) +
  geom_point(data=df1 %>% filter(df<10), aes(x=lon_wwtp, y=lat_wwtp), color="#77333B", alpha=.6, shape=16) +
  coord_map(projection = "mercator", xlim=c(-180,180)) +
  labs(title="Wastewater Plants Around the Globe",
       caption = caption_text) +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=40, color=txt_col, hjust=0.5,lineheight=1, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_markdown(size=12, color=txt_col,face = "plain", hjust=0.5, margin=margin(0,0,0,0), lineheight = 1.4),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(30,5,30,5),
    legend.position ="bottom"
  )

p + inset_element(p_text, .2, .4, .2, .2)

#### SAVE ####

showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_38.png",
       height = 10,
       width = 15,
       dpi=320) 

