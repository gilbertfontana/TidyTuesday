

# Libs
library(tidyverse)
library(janitor)
library(scico)
library(showtext)
library(lubridate)
library(zoo)
library(maps)
library(gganimate)
library(magick)
library(transformr)


# Data
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

drought_fips2 <- 
  drought_fips %>% 
  filter(State=="CA") %>% 
  mutate(date=as.yearqtr(date),
         date2=as.Date(date)) %>% 
  group_by(State,FIPS,date,date2) %>% 
  summarise(avg_dsci=mean(DSCI)) %>% 
  ungroup()


df_fips <- county.fips %>%
  as_tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") %>% 
  mutate(fips=as.character(fips)) %>% 
  mutate(
    fips =
    case_when(
      str_length(fips)<5 ~ paste0("0", fips),
      TRUE ~ fips
    )
  )

df <- drought_fips2 %>% 
  clean_names() %>% 
  left_join(
    df_fips
  ) 

df2 <- df %>% 
  left_join(
  map_data("county") 
  ) %>% 
  as_tibble()
  


# Misc
font <- "Saira Extra Condensed"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "ivory"
txt_col <- "grey20"

# Plot
ani <- 
  ggplot(data=df2, aes(long, lat, group = group)) +
  geom_polygon(aes(fill=avg_dsci), color=txt_col, size=.2) +
  scale_fill_scico(palette = "lajolla",
                   name="Drought level",
                   breaks=c(25,475),
                   limits=c(0,500),
                   labels = c("low", "high"),
                   begin = 0.2,
                   end=.8
                   ) +
  labs(
    title='Droughts in California',
    subtitle='{closest_state}',
    caption = "Gilbert Fontana | #TidyTuesday Week 24 | Data: National Integrated Drought Information System",
  ) +
  coord_map(clip = "off") +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust=0,size=28, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0,size=16, color=txt_col, margin=margin(10,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=12, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = c(.75,.75),
    legend.title = element_text(color=txt_col,size=12, face="bold"),
    legend.text = element_text(color=txt_col,size=12),
    legend.direction = "horizontal"
    ) + 
    guides(fill=guide_colorbar(ticks.colour = NA, title.position = "top", title.hjust = .5)) +
  transition_states(date)

mapGIF <- animate(ani, height = 700, width = 700, fps=20, duration = 20,  bg = bg)


anim_save("tidytuesday_week_24.gif", animation=mapGIF)




