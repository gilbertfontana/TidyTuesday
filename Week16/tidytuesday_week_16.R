


library(tidytuesdayR)
library(tidyverse)
library(ggridges)
library(MetBrewer)
library(ggtext)
library(glue)
library(patchwork)


# Import data
tuesdata <- tidytuesdayR::tt_load(2022, week = 16)
big_dave <- tuesdata$big_dave
times <- tuesdata$times

# Combine datasets
df_comb <- big_dave %>% 
bind_rows(times) %>% 
  select(-rowid)



# Prep dataset
top_10 <- df_comb %>% 
  mutate(definition=str_to_upper(definition)) %>% 
  filter(!is.na(definition)) %>% 
  group_by(definition) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  slice_head(n=10)

df <- df_comb%>% 
  mutate(definition=str_to_upper(definition),
         answer2 = str_replace_all(string=answer, pattern=" ", repl=""),
         char_length=str_length(answer2)) %>% 
  filter(definition %in% top_10$definition) %>% 
  mutate(definition=str_to_title(definition),
         char_length=as.numeric(char_length)) %>% 
  select(definition,char_length, answer) %>% 
  drop_na()





# Misc
font <- "Space Grotesk"
bg <- "rosybrown2"

factor_order <- rev(str_to_title(top_10$definition))


# Plot

plot1 <- df %>% 
  mutate(definition = fct_relevel(definition, factor_order)) %>% 
ggplot(aes(x = char_length, y = definition, fill=definition)) +
  geom_density_ridges2(quantile_lines=TRUE,
                       quantiles = 2) +
  scale_fill_manual(values = met.brewer("Demuth")) +
  scale_y_discrete(expand = c(0,0),labels=c("States","Girls","Cities","Games","Fruits","Fishes","Countries","Drinks","Plants","Birds")) +
  scale_x_continuous(expand = c(0,0)) +
  xlab("The number of characters in the answer") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family=font, color = "black"),
    axis.text = element_text(family=font, color = "black"),
    axis.text.y = element_text(face="bold"),
    panel.background = element_rect(fill = bg, colour = bg),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.position = "none"
  )

plot1


# Title

title <- ggplot() +
  theme_void() +
  geom_richtext(aes(x = 0, y = 25),  family = font,
                label = "Crossword<br>Puzzles",
                size = 20, fill = NA, label.color = NA, vjust=1,
                hjust = 0,color = "brown4", fontface="bold", lineheight=.5) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 40), expand = c(0, 0))

title

#Subtitle + caption

text <- ggplot() +
  theme_void() +
  geom_textbox(aes(x = 0, y = 13), color = "black", family = font,
               label = glue(
                "A common strategy for solving crossword puzzles is to find the shortest answers first. ",
               "According to data from **Big Dave's Crossword Blog** and **Times for the Times Blog**, ",
               "which includes over 300 thousand observations of crossword clues and answers, clues related ",
               "to **Birds**, **Plants** and **Drinks** are the most common. ",
               "On average, the answers related to **Games** are longer while ",
               "the answers related to **Girls** are shorter compared to answers in the other common categories."
               ),
               fill = NA, box.size = NA, width = unit(4, "in"), hjust = 0, vjust=.5,
               size = 3.5) +
  geom_textbox(aes(x = 0, y = 0), color = "black", family = font,
               label = " Gilbert Fontana | #TidyTuesday Week 16 | Data: cryptics.georgeho.org",
               fill = NA, box.size = NA, width = unit(4, "in"), hjust = 0,
               size = 2.5, fontface="bold") +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 40), expand = c(0, 0))


text

# Patchwork

(title + text) / plot1 +
  plot_layout(heights = c(1, 2), widths = c(1,2)) &
  theme(
    panel.background = element_rect(fill = bg, colour = bg),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(10,10,10,10)
  )

# Save



ggsave("tidytuesday_week_16.png",
       height = 20,
       width = 25,
       units = "cm",
       type = "cairo",
       dpi=300
)  


