
# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')


df <- reputation %>% 
  filter(name=="GROWTH") %>% 
  mutate(
    industry_agg = case_when(
      industry=="Retail" ~ industry,
      industry=="Tech" ~ industry,
      industry=="Food & Beverage" ~ industry,
      industry=="Financial Services" ~ industry,
      industry=="Automotive" ~ industry,
      TRUE ~ "Other"
    )
  )

# Misc
font <- "Reem Kufi"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "grey10"

# Plot
df %>% 
mutate(company = fct_reorder(company, score)) %>%
ggplot() +
  geom_hline(yintercept= df %>% summarise(mean(score)) %>% pull()) +
  geom_point(aes(x=company, y=score, color=industry_agg)) +
  geom_segment(aes(x=company, xend=company, y=df %>% summarise(mean(score)) %>% pull(), yend=score,color=industry_agg)) +
  geom_text(data=df %>% filter(score==max(score)),
            aes(x = 101, y = score, label=company, color=industry_agg),
            hjust=0,
            size=3,
            family=font,
            fontface="bold",
            show.legend=FALSE) +
  geom_text(data=df %>% filter(score==min(score)),
            aes(x = 2, y = score, label=company, color=industry_agg),
            hjust=0,
            size=3,
            family=font,
            fontface="bold",
            show.legend=FALSE) +
  annotate(geom="text", x=0, y=df %>% summarise(mean(score)) %>% pull()+2,
           label="Average\ngrowth\nprospects",
           hjust=0,
           size=3,
           family=font,
           color=txt_col,
           lineheight=.8) +
  scale_y_continuous(limits = c(50, 90))  +
  scale_color_manual(values=met.brewer("Peru1", type="discrete",direction = -1,)) +
  labs(
    title = "Growth Prospects",
    subtitle = "According to the Axios and Harris poll, growth prospects varies substantially\nbetween different companies. Target is placed well above average while\nThe Kroger Company is far below.",
    caption = "Gilbert Fontana | #TidyTuesday Week 22 | Data: Axios and Harris Poll"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust=0,size=24, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0,size=14, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,60,30,60),
    legend.position = c(.8,.4),
    legend.title = element_blank()
  ) + 
  guides(color=guide_legend(nrow=3,byrow=TRUE))


# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_22.png",
       height = 7,
       width = 9,
       dpi=320,
       
)  

showtext_auto(FALSE)


