

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)


# Data
sevens <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')
fifteens <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')


# Misc
font <- "Reem Kufi"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#EDE6DB"
txt_col <- "grey10"
showtext_auto(enable = TRUE) 

# Plot
fifteens %>%
  select(team_1,team_2, home_away_win) %>% 
  filter(home_away_win!="N") %>% 
  pivot_longer(
    !home_away_win,
    names_to = "team",
    values_to = "team2"
  ) %>% 
  select(-team) %>%
  group_by(team2) %>%
  count(home_away_win) %>% 
  mutate(win_freq = round((n/sum(n))*100),
         sum_games=sum(n)) %>%
  ungroup() %>% 
  filter(sum_games>=20) %>% 
  select(team2, home_away_win, win_freq) %>% 
  filter(home_away_win=="A") %>%
  mutate(team2 = fct_reorder(team2, win_freq)) %>% 
  ggplot() +
  geom_bar(aes(x=win_freq, y=team2, fill=win_freq), stat = "identity") +
  geom_vline(xintercept = 50, color=txt_col, linetype="dotted") +
  scale_fill_gradientn(colors = met.brewer("Demuth", type = "continuous")) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,75),
                     breaks = seq(0,75,25)) +
  labs(title = "Share of away wins in\nwomens fifteens rugby",
       subtitle = "- Teams playing atleast 20 matches",
       caption = "Gilbert Fontana | #TidyTuesday Week 21 | Data: ScrumQueens",
       x = "Share of away wins (%)"
       ) +
  theme(
    panel.grid = element_blank(),
    axis.title.y  = element_blank(),
    axis.title.x  = element_text(color=txt_col, size=10, hjust=1),
    axis.text = element_text(color=txt_col, size=8),
    plot.title = element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.position = "none"
  ) 


# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_21.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)






  


