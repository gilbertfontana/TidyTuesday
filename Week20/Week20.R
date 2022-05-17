

# Libs
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
tuesdata <- tidytuesdayR::tt_load(2022, week = 20)
eurovision <- tuesdata$eurovision
voted <- tuesdata$`eurovision-votes`
country_distance <- read_csv("https://raw.githubusercontent.com/mlschneid/country-distance/master/output/distance-matrix.csv") %>% 
clean_names() %>% 
pivot_longer(!x1) %>% 
  mutate(name=str_to_upper(name)) %>% 
  rename(x2=name)
country_codes <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>% 
  clean_names()


df <- voted %>% 
filter(semi_final=="f", is.na(duplicate)) %>% 
group_by(from_country, to_country) %>% 
summarise(avg_points=mean(points))

df2 <- 
  left_join(
    df,
    country_codes %>% select(name, alpha_2),
    by=c("from_country"="name")
  ) %>% 
  left_join(
    country_codes %>% select(name, alpha_2),
    by=c("to_country"="name")
  ) %>% 
  rename(from_country_alpha=alpha_2.x,
         to_country_alpha=alpha_2.y
         ) %>% 
  relocate(from_country,
           from_country_alpha,
           to_country,
           to_country_alpha,
           avg_points)

# Check NA
country_na <- df2 %>%
  filter(is.na(from_country_alpha)) %>% 
  distinct(from_country)

# Update codes manually
df2 <- df2 %>% 
  mutate(from_country_alpha = replace(from_country_alpha,  from_country== "Bosnia & Herzegovina", "BA"),
         from_country_alpha = replace(from_country_alpha,  from_country== "Czech Republic", "CZ"),
         from_country_alpha = replace(from_country_alpha,  from_country== "F.Y.R. Macedonia", "MK"),
         from_country_alpha = replace(from_country_alpha,  from_country== "Moldova", "MD"),
         from_country_alpha = replace(from_country_alpha,  from_country== "Russia", "RU"),
         from_country_alpha = replace(from_country_alpha,  from_country== "The Netherlands", "NL"),
         from_country_alpha = replace(from_country_alpha,  from_country== "United Kingdom", "GB"),
         
         to_country_alpha = replace(to_country_alpha,  to_country== "Bosnia & Herzegovina", "BA"),
         to_country_alpha = replace(to_country_alpha,  to_country== "Czech Republic", "CZ"),
         to_country_alpha = replace(to_country_alpha,  to_country== "F.Y.R. Macedonia", "MK"),
         to_country_alpha = replace(to_country_alpha,  to_country== "Moldova", "MD"),
         to_country_alpha = replace(to_country_alpha,  to_country== "Russia", "RU"),
         to_country_alpha = replace(to_country_alpha,  to_country== "The Netherlands", "NL"),
         to_country_alpha = replace(to_country_alpha,  to_country== "United Kingdom", "GB")
  )


df3 <- left_join(
  df2,
  country_distance,
  by=c("from_country_alpha"="x1", "to_country_alpha"="x2")
)

# Misc
font <- "Montserrat"
font_add_google(family=font, font)
bg <- "grey20"
txt_col <- "grey90"
theme_set(theme_minimal(base_family = font))
showtext_auto(enable = TRUE) 

# Plot
df3 %>% 
  filter(!from_country %in% c("Australia","Yugoslavia","Serbia & Montenegro"),
         !to_country %in% c("Australia","Yugoslavia","Serbia & Montenegro")
         ) %>% 
  ggplot(aes(y=avg_points,x=value)) +
  geom_jitter(width = .5, height = .5,aes(color=rev(avg_points))) +
  #geom_point(aes(color=avg_points)) +
  scale_colour_gradientn(colors=met.brewer("Signac"))  +
  scale_x_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE),
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,12,2)) +
  coord_cartesian(clip = "off") +
  labs(title="Do countries tend give higher points to\ntheir neighbours in Eurovision?",
       subtitle = "The relationship beetween the average points given to a country\nand their geographical distance is close to non-existent",
       caption = "Gilbert Fontana | #TidyTuesday Week 20 | Data: Eurovision & Data.World"
       ) +
  xlab("Geographical distance (kilometers)") +
  ylab("Average points given (between 1975 - 2022)") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(hjust=.5, color=txt_col, size=9, margin = margin(10,0,0,0)),
    axis.title.y = element_text(hjust=.5, color=txt_col, size=9, margin = margin(0,10,0,0)),
    axis.text = element_text(color=txt_col, size=8),
    axis.text.y = element_text(color=txt_col, size=8),
    plot.title = element_text(hjust=-0, size=18, color=txt_col,lineheight=1, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0, size=12, color=txt_col, margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5,margin=margin(10,0,0,0), size=7, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
  )
  

# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_20.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)  



