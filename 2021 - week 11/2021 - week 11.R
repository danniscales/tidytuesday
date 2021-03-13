library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(ggimage)
library(magick)

require(ggimage) 
img = "/Users/mbxds4/Dropbox/TidyTuesday/2021 - week 11/filmreel.png"

# load data
tuesdata <- tuesdata <- tidytuesdayR::tt_load(2021, week = 11)
movies <- tuesdata$movies

# data cleanup
p <- movies %>%
  mutate(main_genre = str_remove(word(genre, 1), ","),
         main_genre = fct_lump_n(main_genre, 10, other_level = "Other"),
         binary = str_to_title(binary)) %>%
  filter(!is.na(main_genre)) %>% 
  count(binary, main_genre) %>%
  mutate(main_genre_factor = as.numeric(main_genre)) %>%
  group_by(main_genre) %>%
  mutate(percent = n / sum(n)) %>%
  filter(binary == "Pass") %>%
  
  # ggplot
  ggplot(aes(x = reorder(main_genre_factor, -percent), y = percent, fill=percent)) +
  geom_hline(yintercept = 0.25, color = "gray60", linetype='dashed') +
  geom_hline(yintercept = 0.5, color = "gray60", linetype='dashed') +
  geom_bar(stat='identity', position = position_stack(reverse = TRUE)) +
  coord_polar(clip = "off") +
  
  # scales
  scale_y_continuous(limits = c(-0.25, 0.65),
                     expand = c(0,0)) +
  scale_x_discrete(limits=rev,
                   labels=c("Mystery","Action","Animation","Fantasy","Crime","Other","Adventure","Biography","Drama","Comedy","Horror")) +

  # captions and labels
  labs(title = "\nPercentage of film genres which pass \nthe Bechdel Test 1970-2013",
       subtitle = "To pass the Bechdel test films must: \n   i) contain at least 2 women \n   ii) who talk to each other \n   iii) about something other than a man",
       caption = "@danni_scales | source: FiveThirtyEight\n") +
  annotate(geom="text", x=0.73, y=0.3, label="25%",
           color="gray40", family ="Andale Mono", size = 4) +
  annotate(geom="text", x=0.64, y=0.55, label="50%",
           color="gray40", family ="Andale Mono", size = 4) +
  
  # theme
  theme(
    text = element_text(family ="Andale Mono", size = 10, color="white"), 
    plot.title = element_text(size = 14, color="black"),
    plot.subtitle = element_text(size = 10, color="black"),
    plot.caption = element_text(size = 8, color="black"),
    plot.background = element_rect(fill = "grey90"),
    panel.background = element_rect(fill = "grey90", colour = "grey90", size = 0.5, linetype = "solid"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size=10, colour = 'black'),
    axis.text.y = element_blank(),
    axis.title = element_blank()) +

#colours
scale_fill_gradientn(colours = c("#FB607F", # Mystery
             "#FB607F", # Action
             "#FF91A4", # Animation
             "#FF91A4", # Fantasy
             "#FADADD", # Crime
             "#FADADD", # Other
             "#9bd280", # Adventure
             "#9bd280", # Biography
             "#83C760", # Drama
             "#83C760", # Comedy
             "#66B032")) # Horror

ggbackground(p, img)

ggsave("week 11.pdf", width=7, height=6)
