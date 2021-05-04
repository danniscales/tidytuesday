library(tidyverse)
library(tidytuesdayR)
library(gender)
library(genderdata)
library(remotes)
library(treemapify)
library(ggtext)

# load data
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

view(departures)

departures %>%
  select(fyear,exec_fullname) %>%
  mutate(first_name = str_remove(word(exec_fullname, 1), " ")) %>%
  filter(!str_detect(first_name, "\\.")) %>% # remove first names followed by a period, ie. initials
  arrange(first_name) %>%
  count(first_name) %>%
  arrange(desc(n)) %>%
  mutate(gender = gender::gender(first_name)$gender, method = c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo")) #assign gender to names

  # ggplot
  ggplot(aes(area = n, label = paste0(first_name,"\n", n))) +
  geom_treemap(fill= "#7BBFA6", colour = "white") +
  geom_treemap_text(family ="System Font", size = 12, colour = "white", place = "topright",
                    grow = FALSE) +
  
  # captions and labels
  labs(title = "<span style = 'font-size:30pt; font-family:Georgia;'>Who run the world? Girls?</span>",
       subtitle = "Most common names of CEO departures between 2000 and 2018<br>
       <span style = 'color: #7BBFA6;'>Male</span> and <span style = 'color: pink;'>Female</span> CEO's.",
       caption = "@danni_scales | source: investors.com") +

#theme
  theme(
    text = element_text(family ="System Font", color="#1E4E5E"),
    plot.title = element_markdown(face = 'bold'), # required to enable html
    plot.subtitle = element_markdown(), # required to enable html
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.border = element_blank(),
    legend.position="none"
  ) 

ggsave("week_18.png", width=10, height=7)