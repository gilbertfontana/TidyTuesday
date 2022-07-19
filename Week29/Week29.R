


# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(maptools)
library(broom)
library(patchwork)

# Data
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

codes <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
  clean_names()

technology2 <- technology %>% 
  left_join(
    codes %>% select(iso3c=alpha_3, region, sub_region)
  )

df <- technology2 %>% 
  filter(variable=="pctimmunizmeas" & region=="Africa") %>% 
  select(iso3c, year, label, value)

data("wrld_simpl")
africa <-wrld_simpl[wrld_simpl$REGION==2,]

africa2 <- tidy(africa)

df2 <- africa2 %>% rename(iso3c=id) %>% 
  left_join(df)


df1985 <- africa2 %>% rename(iso3c=id) %>% 
  left_join(df %>% filter(year==1985)) %>% 
  mutate(year = case_when(
    is.na(year) ~ 1985,
    TRUE ~ year
  )
           )

df2019 <- africa2 %>% rename(iso3c=id) %>% 
  left_join(df %>% filter(year==2019)) %>% 
  mutate(year = case_when(
    is.na(year) ~ 2019,
    TRUE ~ year
  )
  )




# Misc
font <- "Josefin Sans"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#E8DCB8"
txt_col <- "black"
nacol <- "grey80"

# Plot
plot1985 <- df1985 %>% 
  ggplot() +
  geom_polygon(aes(fill = value, x = long, y = lat, group = group)
               ,size=0, color="black") +
  scico::scale_fill_scico(palette="bamako", direction = -1, na.value=nacol, limits=c(0,100), name="Immunization coverage (%)") +
  coord_map() +
  labs(title="1985") +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=20, color=txt_col, hjust=.5,lineheight=1, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(0,0,0,0),
    legend.title = element_text(size=16),
    legend.text = element_text(size=12)
  ) + guides(fill=guide_colorbar(ticks.colour = NA, title.position="top",barwidth = 14))

plot1985
  


plot2019 <- df2019 %>% 
  ggplot() +
  geom_polygon(aes(fill = value, x = long, y = lat, group = group)
               ,size=0, color="black") +
  scico::scale_fill_scico(palette="bamako", direction = -1, na.value=nacol, limits=c(0,100), name="Immunization coverage (%)") +
  coord_map() +
  labs(title="2019") +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=20, color=txt_col, hjust=.5,lineheight=1, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(0,0,0,0),
    legend.title = element_text(size=16),
    legend.text = element_text(size=12)
  ) + guides(fill=guide_colorbar(ticks.colour = NA, title.position="top",barwidth = 14))

plot2019

# Patchwork
plot1985 + plot2019 + plot_annotation(
  title = 'Children Receiving Measles Immunization',
  caption = "Gilbert Fontana | #TidyTuesday Week 29 | Data: NBER",
  ) + plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        plot.title = element_text(size=26, color=txt_col, hjust=.5,lineheight=1, face="bold", margin=margin(0,0,30,0)),
        plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=10, color=txt_col, face="bold"),
        plot.margin = margin(10,10,10,10),
        plot.background = element_rect(color=bg, fill=bg)
        )


# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_29.png",
       height = 10,
       width = 10,
       dpi=320,
       
)  

showtext_auto(FALSE)



