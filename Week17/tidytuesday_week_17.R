



# Libraries
library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(rtweet)
library(ggtext)

# Import data + cleaning
tuesdata <- tidytuesdayR::tt_load(2022, week = 17)
hidden_gems <- tuesdata$hidden_gems


hidden_gems <- hidden_gems %>% 
 mutate(twitter_id=sub(".*/", "", link_twitter))

twitter_info <- lookup_tweets(statuses = hidden_gems$twitter_id) %>% 
  select(
    twitter_id=status_id,
    favorite_count,
    retweet_count
  )

df <- left_join(
  hidden_gems,
  twitter_info) %>% 
  select(vol,date, favorite_count, retweet_count) %>% 
  distinct() %>% 
  pivot_longer(!vol & !date, names_to = 'reaction', values_to = 'count')

# Misc
font <- "Space Grotesk"
bg <- "#E7E7DE"

# Plot
ggplot(df,aes(x=date, y=count, group=reaction)) +
  geom_line(aes(color=reaction), size=.5) +
  geom_point(data = df %>% filter(reaction=="favorite_count") %>% 
               arrange(desc(count)) %>% 
               slice_head(n=3),
             aes(x=date, y=count),shape=1, size=3) +
  geom_textbox(aes(x = as.Date("2020-05-15"), y = 156.5), color = "black", family = font,
               label = "**Episode 1**
                            <br/>- Glmnet, XGBoost, and SVM Using tidymodels
                            <br/>- Breathe India: COVID-19 effect on Pollution
                            <br/>- U.S. Commercial Flights Tracker Map",
               fill = NA, box.size = NA, width = unit(20, "in"), hjust = 0, vjust=1,
               size = 3) +
  geom_textbox(aes(x = as.Date("2020-12-11"), y = 111.5), color = "black", family = font,
               label = "**Episode 31**
                            <br/>- Adversarial Rainforest
                            <br/>- EDA Google SunRoof w/ Beautiful Choropleths & Maps
                            <br/>- Monet - Visualization and Augmentation",
               fill = NA, box.size = NA, width = unit(20, "in"), hjust = 0, vjust=1,
               size = 3) +
  geom_textbox(aes(x = as.Date("2021-06-19"), y = 88.5), color = "black", family = font,
               label = "**Episode 50**
                            <br/>- Earthquakes And Tectonic Plates: Seismic Analysis
                            <br/>- EDA, Hypothesis Testing, MLR | Stats Report Style
                            <br/>- Beware of trolls",
               fill = NA, box.size = NA, width = unit(20, "in"), hjust = 0, vjust=1,
               size = 3) +
  geom_segment(aes(x = as.Date("2021-04-20"), y = 68, xend = as.Date("2021-06-20"), yend = 85 )) +
  scale_x_date() +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(values = c("#a00e00", "#0086a8"),
                     name="Twitter reaction",
                     labels=c("Favorite", "Retweet"),
                     guide = guide_legend(title.position = "top")) +
  labs(title = "**Most Popular Hidden Gems on Kaggle**
       <br/>- According to Twitter",
       caption = "Gilbert Fontana | #TidyTuesday Week 17 | Data: Kaggle") +
  ylab("Number of reactions") +
  xlab("Date") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(family=font,hjust = 1),
    axis.title.x = element_text(family=font,hjust = 1),
    axis.text = element_text(family=font, color = "black"),
    axis.text.y = element_text(),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    panel.background = element_rect(fill = bg, colour = bg),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(30,30,30,30),
    plot.title = element_text(family=font, size=20, margin=margin(0,0,30,0), lineheight = 1.2),
    plot.caption = element_text(family=font, hjust=.5, margin = margin(20,0,0,0), face="bold"),
    legend.position = c(0.9,0.9),
    legend.title = element_text(family=font,hjust=.5, face="bold"),
    legend.text = element_text(family=font)) +
  theme(plot.title = element_markdown())


ggsave("tidytuesday_week_17.png",
       height = 20,
       width = 25,
       units = "cm",
       type = "cairo",
       dpi=320
)  


