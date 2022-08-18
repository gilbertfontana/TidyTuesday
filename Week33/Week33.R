

#### LIBS ####
library(tidyverse)
library(janitor)
library(showtext)
library(patchwork)
library(ggfx)


#### DATA ####
tuesdata <- tidytuesdayR::tt_load(2022, week = 33)

df1 <- tuesdata$characters %>% 
  filter(uni_name=="Star Wars")

df2 <- tuesdata$myers_briggs %>% 
  filter(uni_name=="Star Wars")

df3 <- tuesdata$psych_stats %>% 
  filter(uni_name=="Star Wars")

traits <- c("competent", "devoted", "prideful", "resolute", "perceptive")

df <- df1 %>% 
  select(char_name=name) %>% 
  left_join(
    df3 %>% 
    filter(personality %in% traits) %>% 
      select(char_name, personality, avg_rating)
  )

df4 <- df %>% 
  filter(char_name %in% c("Darth Vader", "Luke Skywalker", "Obi-Wan Kenobi"))


#### MISC ####
  font <- "Space Grotesk"
  font_add_google(family=font, font)
  showtext_auto(enable = TRUE) 
  theme_set(theme_minimal(base_family = font))
  bg <- "black"
  txt_col <- "white"
  
#### PLOT ####
  p1 <- ggplot() +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="competent"), aes(x=0,xend=avg_rating, y=char_name,yend=char_name, color=char_name), size=3,lineend= "round")) +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="competent"), aes(x=0,xend=avg_rating-.5, y=char_name,yend=char_name), size=1.5, color="white",lineend= "round")) +
    geom_segment(data=df4 %>% filter(personality=="competent"), aes(x=0,xend=-1, y=char_name,yend=char_name),color="grey50", size=5) +
    geom_segment(data=df4 %>% filter(personality=="competent"), aes(x=-1,xend=-9, y=char_name,yend=char_name),color="grey50", size=3) +
    geom_segment(data=df4 %>% filter(personality=="competent"), aes(x=-9,xend=-10, y=char_name,yend=char_name),color="grey50", size=4, lineend= "butt") +
    labs(title="Competent") +
    scale_x_continuous(limits = c(-10,100), expand = c(0,0)) +
    theme(
      axis.text.y = element_text(color="white", face="bold", size=14),
      axis.text.x= element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size=20, color=txt_col, face="plain", hjust=0, margin=margin(0,0,0,0)),
      plot.title.position = "panel",
      plot.background = element_rect(fill = bg, color=bg),
      panel.background = element_rect(fill = bg,color=bg),
      legend.position = "none",
      plot.margin = margin(30,30,30,30)
    )
  
  
  p2 <- ggplot() +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="devoted"), aes(x=0,xend=avg_rating, y=char_name,yend=char_name, color=char_name), size=3,lineend= "round")) +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="devoted"), aes(x=0,xend=avg_rating-.5, y=char_name,yend=char_name), size=1.5, color="white",lineend= "round")) +
    geom_segment(data=df4 %>% filter(personality=="devoted"), aes(x=0,xend=-1, y=char_name,yend=char_name),color="grey50", size=5) +
    geom_segment(data=df4 %>% filter(personality=="devoted"), aes(x=-1,xend=-9, y=char_name,yend=char_name),color="grey50", size=3) +
    geom_segment(data=df4 %>% filter(personality=="devoted"), aes(x=-9,xend=-10, y=char_name,yend=char_name),color="grey50", size=4, lineend= "butt") +
    labs(title="Devoted") +
    scale_x_continuous(limits = c(-10,100), expand = c(0,0)) +
    theme(
      axis.text.y = element_text(color="white", face="bold", size=14),
      axis.text.x= element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size=20, color=txt_col, face="plain", hjust=0, margin=margin(0,0,0,0)),
      plot.title.position = "panel",
      plot.background = element_rect(fill = bg, color=bg),
      panel.background = element_rect(fill = bg,color=bg),
      legend.position = "none",
      plot.margin = margin(30,30,30,30)
    )


  p3 <- ggplot() +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="perceptive"), aes(x=0,xend=avg_rating, y=char_name,yend=char_name, color=char_name), size=3,lineend= "round")) +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="perceptive"), aes(x=0,xend=avg_rating-.5, y=char_name,yend=char_name), size=1.5, color="white",lineend= "round")) +
    geom_segment(data=df4 %>% filter(personality=="perceptive"), aes(x=0,xend=-1, y=char_name,yend=char_name),color="grey50", size=5) +
    geom_segment(data=df4 %>% filter(personality=="perceptive"), aes(x=-1,xend=-9, y=char_name,yend=char_name),color="grey50", size=3) +
    geom_segment(data=df4 %>% filter(personality=="perceptive"), aes(x=-9,xend=-10, y=char_name,yend=char_name),color="grey50", size=4, lineend= "butt") +
    labs(title="Perceptive") +
    scale_x_continuous(limits = c(-10,100), expand = c(0,0)) +
    theme(
      axis.text.y = element_text(color="white", face="bold", size=14),
      axis.text.x= element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size=20, color=txt_col, face="plain", hjust=0, margin=margin(0,0,0,0)),
      plot.title.position = "panel",
      plot.background = element_rect(fill = bg, color=bg),
      panel.background = element_rect(fill = bg,color=bg),
      legend.position = "none",
      plot.margin = margin(30,30,30,30)
    )
  
  
  p4 <- ggplot() +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="resolute"), aes(x=0,xend=avg_rating, y=char_name,yend=char_name, color=char_name), size=3,lineend= "round")) +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="resolute"), aes(x=0,xend=avg_rating-.5, y=char_name,yend=char_name), size=1.5, color="white",lineend= "round")) +
    geom_segment(data=df4 %>% filter(personality=="resolute"), aes(x=0,xend=-1, y=char_name,yend=char_name),color="grey50", size=5) +
    geom_segment(data=df4 %>% filter(personality=="resolute"), aes(x=-1,xend=-9, y=char_name,yend=char_name),color="grey50", size=3) +
    geom_segment(data=df4 %>% filter(personality=="resolute"), aes(x=-9,xend=-10, y=char_name,yend=char_name),color="grey50", size=4, lineend= "butt") +
    labs(title="Resolute") +
    scale_x_continuous(limits = c(-10,100), expand = c(0,0)) +
    theme(
      axis.text.y = element_text(color="white", face="bold", size=14),
      axis.text.x= element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size=20, color=txt_col, face="plain", hjust=0, margin=margin(0,0,0,0)),
      plot.title.position = "panel",
      plot.background = element_rect(fill = bg, color=bg),
      panel.background = element_rect(fill = bg,color=bg),
      legend.position = "none",
      plot.margin = margin(30,30,30,30)
    )
  

  p5 <- ggplot() +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="prideful"), aes(x=0,xend=avg_rating, y=char_name,yend=char_name, color=char_name), size=3,lineend= "round")) +
    with_outer_glow(geom_segment(data=df4 %>% filter(personality=="prideful"), aes(x=0,xend=avg_rating-.5, y=char_name,yend=char_name), size=1.5, color="white",lineend= "round")) +
    geom_segment(data=df4 %>% filter(personality=="prideful"), aes(x=0,xend=-1, y=char_name,yend=char_name),color="grey50", size=5) +
    geom_segment(data=df4 %>% filter(personality=="prideful"), aes(x=-1,xend=-9, y=char_name,yend=char_name),color="grey50", size=3) +
    geom_segment(data=df4 %>% filter(personality=="prideful"), aes(x=-9,xend=-10, y=char_name,yend=char_name),color="grey50", size=4, lineend= "butt") +
    labs(title="Prideful") +
    scale_x_continuous(limits = c(-10,100), expand = c(0,0)) +
    theme(
      axis.text.y = element_text(color="white", face="bold", size=14),
      axis.text.x= element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size=20, color=txt_col, face="plain", hjust=0, margin=margin(0,0,0,0)),
      plot.title.position = "panel",
      plot.background = element_rect(fill = bg, color=bg),
      panel.background = element_rect(fill = bg,color=bg),
      legend.position = "none",
      plot.margin = margin(30,30,30,30)
    )
  

  text <- tibble(
    x = 0, y = 0, label = "Jedi and Sith Psychometrics"
  )
  
  pt <- ggplot(text, aes(x = x, y = y)) +
    ggtext::geom_textbox(
      aes(label = label),
      box.color = bg, fill=bg, width = unit(23, "lines"),
      family = font, color = txt_col, size = 12, lineheight = 1, fontface="bold"
    ) +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = bg, color=bg),
                panel.background = element_rect(fill = bg,color=bg))
  

  #### PATCHWORK ####
  (pt+p5)/(p1+p2)/(p3+p4) + plot_annotation(
      caption = 'Gilbert Fontana | #TidyTuesday Week 33 | Data: Open-Source Psychometrics Project'
    ) &
    theme(
          plot.caption = element_text(hjust=.5, margin=margin(20,0,0,0), size=10, color=txt_col, face="bold"),
          plot.margin = margin(30,30,30,30),
          plot.background = element_rect(color=bg, fill=bg)
    )

  #### SAVE ####
  showtext_opts(dpi = 320) 
  
  ggsave("tidytuesday_week_33.png",
         height = 10,
         width = 10,
         dpi=320,
         bg=bg
         
  )  

  

  