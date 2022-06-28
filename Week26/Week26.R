


# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)

# Data
paygap <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')


df2 <- paygap %>%
  filter(substr(due_date,1,4)=="2022",
         employer_size=="20,000 or more")

# Misc
font <- "Space Grotesk"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#FDF7F7"
txt_col <- "#77575A"

# Plot
df2 %>% 
  ggplot() +
  geom_hline(yintercept = 50, linetype="dashed") +
  geom_vline(xintercept = 50, linetype="dashed") +
  geom_point(aes(x=female_top_quartile, y=female_lower_quartile), color="#F1BDC2") +
  scale_x_continuous(expand= c(0,0), limits = c(0,100), breaks = seq(25,100,25)) +
  scale_y_continuous(expand= c(0,0), limits = c(0,100), breaks = seq(25,100,25))  +
  annotate("text", x=55, y=25,
           label="No employer have a majority of females in the\ntop hourly pay quarter in combination with a\nminority in the bottom hourly pay quarter",
           color=txt_col,
           family=font,
           fontface="bold",
           size=3,
           hjust=0,
           vjust=0) +
  coord_cartesian(clip = "off") +
  labs(
    title ='Percentage of females in the top\nand bottom hourly pay quarters',
    subtitle = "UK employers with at least 20 000 employees",
    caption = "Gilbert Fontana | #TidyTuesday Week 26 | Data: gender-pay-gap.service.gov.uk.",
    x = "Females in the top pay quarter (%)",
    y = "Females in the bottom pay quarter (%)"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title  = element_text(color=txt_col),
    axis.text = element_text(color=txt_col),
    axis.line = element_line(),
    plot.title = element_text(hjust=0.5,size=20, color=txt_col,lineheight=.8, face="bold", margin=margin(0,0,0,0)),
    plot.subtitle = element_text(hjust=0.5,size=14, color=txt_col,lineheight=.8, face="bold", margin=margin(10,0,20,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(30,60,30,30),
    legend.position = "bottom",
    legend.title = element_text(color=txt_col),
    legend.justification = "center",
    legend.title.align=0.5,
  )


# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_26.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)


