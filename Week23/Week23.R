


# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv') %>% 
  clean_names()

# Misc
font <- "Voltaire"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#839AA8"
txt_col <- "grey90"


pride_aggregates %>% 
  filter(number_of_politicians_contributed_to>35 | total_contributed>500000
         )

# Plot
pride_aggregates %>% 
  filter(company!="Grand Total") %>% 
  ggplot() +
  geom_point(aes(x=number_of_politicians_contributed_to,
                 y=total_contributed,
                 size=number_of_states_where_contributions_made,
                 color=number_of_politicians_contributed_to),
             alpha=.8) +
  geom_text(data = pride_aggregates %>% filter(company!="Grand Total",
                               number_of_politicians_contributed_to>25 | total_contributed>500000
                               ),
            aes(x = number_of_politicians_contributed_to + 2, y = total_contributed,
                label=company,
                color=number_of_politicians_contributed_to),
            hjust=0,
            size=3,
            family=font,
            fontface="bold",
            alpha=.8,
            show.legend=FALSE) +
  scale_y_continuous(breaks = seq(250000,75000000,250000),
                     limits = c(0,750000),
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_continuous(breaks = seq(0,75,25),
                     limits = c(0,75)) +
  scale_color_gradientn(colors=met.brewer("Tam", direction = 1), guide = "none") +
  scale_size_continuous(name = "Number of states where\ncontributions were made") +
  coord_cartesian(clip="off") +
  labs(
    title = "RAINBOW WASHING",
    subtitle = "Pride sponsors contributing to anti-LGBTQ campaigns",
    caption = "Gilbert Fontana | #TidyTuesday Week 23 | Data: Data For Progress",
    x="Number of politicians contributed to",
    y="Total amount contributed ($)"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_text(color=txt_col, hjust=1, size=10),
    axis.text = element_text(color=txt_col, size=8),
    axis.line = element_line(color=txt_col),
    plot.title = element_text(hjust=0,size=28, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0,size=16, color=txt_col, margin=margin(5,0,20,0)),
    plot.title.position = "plot",
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=12, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,30,30,30),
    legend.title = element_text(color=txt_col,size=10),
    legend.text = element_text(color=txt_col),
    legend.position = "bottom",
    legend.box="vertical"
  ) + guides(size = guide_legend(title.position="top",
                                 title.hjust = 0.5,
                                 nrow=1,
                                 byrow=TRUE,
                                 override.aes=list(colour=txt_col)
                                 )
             )

# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_23.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)
