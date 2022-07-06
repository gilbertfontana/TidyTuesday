

# Libs
library(tidyverse)
library(janitor)
library(MetBrewer)
library(showtext)
library(ggstream)

# Data
new_construction <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/new_construction.csv')



# Misc
font <- "Poppins"
font_add_google(family=font, font)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "white"
txt_col <- "black"

# Plot

new_construction %>% 
  ggplot() +
  geom_stream(aes(x=year, y=sfproduction, fill=county),type = "ridge") +
  scale_x_continuous(expand=c(0,0), limits = c(1990,2018), breaks = c(1990,2000,2010,2018)) +
  scale_y_continuous(expand=c(0,0), limits = c(0,18000), breaks = seq(4500,18000,4500),
                     labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_fill_manual(values = met.brewer("Renoir"), name="County") +
  labs(
    title = "Single-Family Housing Construction\nin the San Francisco Bay Area",
    caption = "Gilbert Fontana | #TidyTuesday Week 27 | Data: Vital Signs",
    y = "Number of units produced"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(color=txt_col, size=12),
    axis.text = element_text(color=txt_col, size=10),
    axis.line = element_line(),
    plot.title = element_text(size=20, color=txt_col,lineheight=1, face="bold", margin=margin(0,0,30,0)),
    plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="bold"),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "panel",
    plot.margin = margin(30,30,30,30),
    legend.position = "bottom",
    legend.title = element_text(color=txt_col),
    legend.justification = "center",
    legend.title.align=0.5,
  ) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5,nrow=3,byrow=TRUE))


# Save
showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_27.png",
       height = 7,
       width = 7,
       dpi=320,
       
)  

showtext_auto(FALSE)