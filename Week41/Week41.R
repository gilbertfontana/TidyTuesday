
#### LIBS ####
library(tidyverse)
library(janitor)
library(MetBrewer)
library(scico)
library(showtext)
library(ggtext)
library(ggdist)


#### DATA ####
yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

top10 <- yarn %>% 
  group_by(yarn_company_name) %>% 
  summarise(rating_c=sum(rating_count,na.rm = TRUE),
            rating_t=sum(rating_total,na.rm = TRUE)) %>% 
  mutate(avg_raing=rating_t/rating_c) %>% 
  mutate(yarn_company_name=str_to_title(yarn_company_name)) %>% 
  mutate(yarn_company_name=recode(yarn_company_name, 
                                  "Cascade Yarns ®"="Cascade Yarns")) %>% 
  arrange(desc(rating_c)) %>% 
  slice_head(n=10)

df <- yarn %>% 
  mutate(yarn_company_name=recode(yarn_company_name, 
                                  "Cascade Yarns ®"="Cascade Yarns")) %>% 
  mutate(yarn_company_name=str_to_title(yarn_company_name)) %>% 
  filter(yarn_company_name %in% top10$yarn_company_name) %>% 
  select(yarn_company_name, rating_average) %>% 
  na.omit()

order <- top10 %>% 
  arrange(avg_raing) %>% 
  mutate(yarn_company_name=recode(yarn_company_name, 
                                  "Cascade Yarns ®"="Cascade Yarns")) %>% 
  pull(yarn_company_name)
  

#### MISC ####
font <- "Sen"
font_add_google(family=font, font,db_cache = FALSE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
showtext_auto(enable = TRUE) 
theme_set(theme_minimal(base_family = font))
bg <- "#CED9CF"
txt_col <- "black"

# CAPTION
caption_text  <- str_glue("Gilbert Fontana | #TidyTuesday Week 41 | Data: ravelry.com<br>",
                          "<span style='font-family: \"fa-brands\"'>&#xf09b;</span> gilbertfontana ",
                          "<span style='font-family: \"fa-brands\"'>&#xf099;</span> GilbertFontana")

#### PLOT ####
df %>% 
  mutate(yarn_company_name=factor(yarn_company_name, levels=order)) %>% 
  ggplot() +
  stat_histinterval(aes(x=rating_average, y=yarn_company_name, fill= stat(x)),
                    fill_type="gradient",
                    color="#04598C"
                    ) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_scico(palette = "corkO",begin = .2,end = .8,
                   direction = 1) +
  coord_cartesian(clip="off") +
  labs(title = "**Brand Rating Distribution on Ravelry**",
       subtitle = "The 10 brands with the highest number of submitted ratings",
       caption = caption_text,
       x="Rating"
         ) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_markdown(size=16, color=txt_col, margin = margin(10,0,0,0), hjust=1),
    axis.title.y = element_blank(),
    axis.text = element_text(size=16, color=txt_col),
    axis.text.x = element_text(margin=margin(20,0,0,0)),
    axis.text.y = element_text(face="bold"),
    plot.title = element_markdown(size=28, color=txt_col, hjust=0.5,lineheight=1, margin=margin(0,0,10,0)),
    plot.sub = element_markdown(size=20, color=txt_col, hjust=0.5,lineheight=1, margin=margin(0,0,20,0)),
    plot.caption = element_markdown(size=12, color=txt_col, face = "plain", hjust=0.5, margin=margin(30,0,0,0), lineheight = 1.4),
    plot.background = element_rect(color=bg, fill=bg),
    plot.title.position = "plot",
    plot.margin = margin(30,30,30,30),
    legend.position ="none",
    legend.background = element_rect(color=bg, fill=bg)
  )

showtext_opts(dpi = 320) 

ggsave("tidytuesday_week_41.png",
       height = 10,
       width = 10,
       dpi = 320)







