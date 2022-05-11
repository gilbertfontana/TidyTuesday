

# Lib
library(tidyverse)
library(gender)
library(MetBrewer)
library(showtext)
library(ggtext)

# Data
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# Datacleaning
nyt_titles2 <- nyt_titles %>% 
  mutate(first_name=word(author,1))

gender <- gender(nyt_titles2$first_name) %>%
  select(name, gender) %>% 
  distinct() %>% 
  rename(first_name=name)

df <- left_join(
  nyt_titles2,
  gender
) %>% 
mutate(gender=replace_na(gender, "unknown"))

df2 <- df %>% 
  group_by(year,gender) %>% 
  summarise(avg_weeks=mean(total_weeks),count = n())

# Misc
font <- "Abril Fatface"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
bg <- "#DE8971"
txt_col <- "black"


# Plot
df2 %>% 
  filter(gender!="unknown") %>% 
  ggplot(aes(x=year, y=count, size=avg_weeks)) +
  geom_smooth(aes(color=gender), se=FALSE, show.legend = FALSE) +
  geom_point(aes(color=gender), alpha=.7) +
  geom_point(data=df2 %>% filter(gender=="unknown"), aes(color=gender), alpha=.7) +
  scale_color_manual(values = met.brewer("Demuth", type = "discrete", n=3)) +
  scale_size(name = "Average total weeks\non best sellers list") +
  scale_x_continuous(breaks = c(1930,1945,1960,1975,1990,2005,2020)) +
  scale_y_continuous(limits = c(0,120)) +
  coord_cartesian(clip="off") +
  labs(title="On average, <span style='color:#f7c267;'>men</span> appear more frequently on
  <br/>The New York Times fiction bestseller list
  <br/>compared to <span style='color:#b64f32;'>women</span>",
  caption = "Gilbert Fontana | #TidyTuesday Week 19 | Data: Post45 Data Collective") +
  ylab("Number of authors") +
  annotate("text",x = 2015, y = 27, label = "Unknown gender", family=font, color="#5d6174", size=3) +
  annotate("text",x = 2021, y = 108, label = "Men", family=font, color="#f7c267", size=3, hjust=0) +
  annotate("text",x = 2021, y = 100, label = "Women", family=font, color="#b64f32", size=3, hjust=0) +
  theme_minimal(base_family = font) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(color=txt_col, size=12),
    plot.title = element_text(hjust=.5, size=30, color=txt_col, margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5,margin=margin(30,0,0,0), face="bold", size=12, color=txt_col),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
  ) + 
  theme(plot.title = element_markdown()) +
  guides(color="none",
         size = guide_legend(title.position="top", title.hjust = 0.5))


# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_19.png",
       height = 8,
       width = 12,
       dpi=320,
       
)  

showtext_auto(FALSE)





