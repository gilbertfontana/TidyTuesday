
#### LIBS ####
library(tidyverse)
library(janitor)
library(MetBrewer)
library(scico)
library(showtext)
library(ggtext)
library(ggbeeswarm2)

#### DATA ####
product_hunt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')

#### MISC ####
font <- "Gemunu Libre"
font_add_google(family=font, font,db_cache = FALSE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#FBF6F1"
txt_col <- "black"

# CAPTION
caption_text  <- str_glue("Gilbert Fontana | #TidyTuesday Week 40 | Data: components.one<br>",
                          "<span style='font-family: \"fa-brands\"'>&#xf09b;</span> gilbertfontana ",
                          "<span style='font-family: \"fa-brands\"'>&#xf099;</span> GilbertFontana")

#### PLOT ####
product_hunt %>% 
  mutate(year=lubridate::year(release_date),
         day=weekdays(release_date),
         diff=as.Date(product_of_the_day_date)-as.Date(release_date)
         ) %>%
  filter(!is.na(product_ranking)) %>% 
  filter(upvotes<(mean(product_hunt$upvotes) + (1.96*sd(product_hunt$upvotes)))) %>% 
  ggplot(aes(x=upvotes, y=as.factor(product_ranking), fill=as.factor(year))) +
  geom_beeswarm(spacing = .65, shape=22, color=bg, method = "hex") +
  scale_fill_met_d(name="Tam") +
  scale_x_continuous(expand = c(0,0), name = "Number of upvotes") +
  scale_y_discrete(labels = c("Rank 1", "Rank 2", "Rank 3", "Rank 4", "Rank 5")) +
  coord_cartesian(clip="off") +
  labs(
    title = "Upvotes on **Product Hunt** by product ranking",
    caption = caption_text
  ) +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color=txt_col, linetype = "dashed", size=.2),
    axis.title.x = element_markdown(size=12, color=txt_col, margin = margin(20,0,0,0), hjust=1),
    axis.title.y = element_blank(),
    axis.text = element_text(size=10, color=txt_col),
    plot.title = element_markdown(size=28, color=txt_col, hjust=0.5,lineheight=1, margin=margin(0,0,30,0)),
    plot.caption = element_markdown(size=10, color=txt_col,face = "plain", hjust=0.5, margin=margin(30,0,0,0), lineheight = 1.4),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "plot",
    plot.margin = margin(30,30,30,30),
    legend.title = element_blank(),
    legend.position =c(.8,.98),
    legend.direction = "horizontal",
    legend.text = element_text(size=10, color=txt_col),
    legend.background = element_rect(color=bg, fill=bg)
  )

#### SAVE ####
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_40.png",
       height = 10,
       width = 10,
       dpi = 320)



  
  