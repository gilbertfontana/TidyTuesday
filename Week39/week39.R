


#### #TIDYTUESDAY WEEK 39 ####
#### Artists in the USA ####

#### LIBS ####

library(tidyverse)
library(janitor)
library(MetBrewer)
library(scico)
library(showtext)

library(ggtext)


#### DATA ####

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')


df <- artists %>%
  mutate(race_agg= case_when(
    race=="White" ~ "white",
    TRUE ~ "non_white"
  )) %>% 
  group_by(state, race_agg) %>% 
  summarise(
    sum_artist=sum(artists_n, na.rm = T)
  ) %>% 
  ungroup() %>% 
  left_join(
    artists %>%
      mutate(race_agg= case_when(
        race=="White" ~ "white",
        TRUE ~ "non_white")) %>% 
  select(state, race, all_workers_n,race_agg) %>% 
  distinct() %>% 
  group_by(state, race_agg) %>% 
  summarise(
    sum_workers=sum(all_workers_n, na.rm = T)
  ) %>% 
  ungroup()
  ) %>% 
  mutate(share=sum_artist/sum_workers*100) %>% 
  select(-sum_workers,-sum_artist) %>% 
  pivot_wider(names_from = "race_agg", values_from = "share") %>% 
  left_join(
    artists %>%
      select(state, race, all_workers_n) %>% 
      distinct() %>% 
      group_by(state) %>% 
      summarise(
        sum_workers=sum(all_workers_n, na.rm = T)
        )
  )

#### MISC ####
font <- "Lexend"
font_add_google(family=font, font,db_cache = FALSE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "black"

# CAPTION
caption_text  <- str_glue("Gilbert Fontana | #TidyTuesday Week 39 | Data: arts.gov<br>",
                          "<span style='font-family: \"fa-brands\"'>&#xf09b;</span> gilbertfontana ",
                          "<span style='font-family: \"fa-brands\"'>&#xf099;</span> GilbertFontana")

#### PLOT ####

df %>% 
  ggplot() +
  geom_point(data = df %>% filter(state!="California"),
             aes(
                 x=non_white,
                 y=white,
                 size=sum_workers),
             shape=21,
             fill="#E5C279",
             color=bg) +
  annotate("segment", x = 0.8, xend = 1.44, y = 4.25, yend = 3.50) +
  annotate("text", x = 0.8, y = 4.4,
           label = "Circlesize represent the size\nof the total workforce",
           hjust=0.5,
           family=font,
           size=4,
           lineheight=.8) +
  geom_point(data = df %>% filter(state=="California"),
             aes(
                 x=non_white,
                 y=white,
                 size=sum_workers),
             shape=21,
             fill="#E5C279",
             color=txt_col) +
  annotate("text", x = 1.70, y = 4.52,
           label = "District of Columbia",
           hjust=0,
           family=font,
           size=4) +
  annotate("text", x = 1.67, y = 3.50,
           label = "California",
           hjust=0,
           family=font,
           size=4) +
  annotate("text", x = 1.85, y = 2.95,
           label = "New York",
           hjust=0,
           family=font,
           size=4) +
  annotate("text", x = 1.27, y = 2.72,
           label = "Hawaii",
           hjust=0,
           family=font,
           size=4) +
  scale_radius(limits = c(0, NA), range = c(5, 20)) + 
  geom_abline(intercept = 0, linetype="dotted") +
  scale_x_continuous(limits = c(0,5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0,0)) +
  labs(
    title = "The Artist Race Gap",
    caption = caption_text,
    x = "Percent of **non-white** artists in the non-white workforce",
    y = "Percent of **white** artists in the white workforce"
  ) +
  theme(
    axis.line = element_line(),
    axis.title.x = element_markdown(size=13, color=txt_col, margin = margin(10,0,0,0)),
    axis.title.y = element_markdown(size=13, color=txt_col, margin = margin(0,10,0,0)),
    axis.text = element_text(size=12, color=txt_col),
    plot.title = element_text(size=40, color=txt_col, hjust=0.5,lineheight=1, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_markdown(size=12, color=txt_col,face = "plain", hjust=0.5, margin=margin(20,0,0,0), lineheight = 1.4),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(30,30,30,30),
    legend.position ="none"
  )
  
#### SAVE ####

showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_39.png",
       height = 10,
       width = 10,
       dpi = 320) 

