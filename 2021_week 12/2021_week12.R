library(tidyverse)
library(tidytuesdayR)
library(extrafont)

# load data
tuesdata <- tuesdata <- tidytuesdayR::tt_load(2021, week = 12)
games <- tuesdata$games

view(games)

# identify top 7 games with highest number of average players
top_games <- games %>%
  arrange(desc(avg)) %>% # arrange average number of players in descending order
  distinct(gamename, .keep_all = TRUE) %>% # filter out unique game names
  subset(gamename %in% head(unique(gamename),8)) # pick top 8 (Cyberpunk 2077 excluded as only 3 months data available)
  
# data cleanup
games %>%
  filter(gamename %in% c("PLAYERUNKNOWN'S BATTLEGROUNDS",
                         "Counter-Strike: Global Offensive",
                         "Dota 2",
                         "Fallout 4",
                         "Grand Theft Auto V",
                         "Monster Hunter: World",
                         "Destiny 2")) %>%
  mutate(month_year = parse_date(paste0(month, " ", year), "%B %Y")) %>% # combine month and year columns
  mutate(avgfactor=cut(avg,breaks=c(0,1,50000,75000,100000,max(avg,na.rm=T)),
                         labels=c("No data available","<50000","50001-75000","75001-100000",">100000"))) %>% # bin the continuous data into levels and represent each level as a discrete colour
  complete(month_year,gamename) %>% # add in missing data
  mutate(avgfactor = replace_na(avgfactor, "No data available")) %>% # replace NA's with "No data available"

  # ggplot
  ggplot(aes(x = month_year,
             y = fct_rev(factor(gamename, 
                        level = c("PLAYERUNKNOWN'S BATTLEGROUNDS",
                                  "Counter-Strike: Global Offensive",
                                  "Dota 2",
                                  "Fallout 4",
                                  "Grand Theft Auto V",
                                  "Monster Hunter: World",
                                  "Destiny 2"), order = TRUE)),
             colour=avgfactor,fill=avgfactor)) +
  geom_tile(color="white", size=0.5) +
  coord_fixed(ratio = 30) + # increase the ratio between y:x axis to get square tiles (to increase ratio between x:y use a fraction eg. 1/30)

  
  # scales
  scale_x_date(breaks = as.Date(c("2017-01-01","2018-01-01","2019-01-01","2020-01-01","2021-01-01")),
               labels = c("2017","2018","2019","2020","2021"),
               limits = as.Date(c('2016-12-01','2021-03-01"')),
               expand=c(0,0),
               position = "top") +
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_manual(values=c("#EBEDF0", # grey
                             "#9BE9A8", # green1 (light)
                             "#64C464", # green2
                             "#52A250", # green3
                             "#356F3A")) + # green4 (dark)

  # captions and labels
  labs(title = "Average number of monthly players for the top seven games on Steam",
       subtitle = "Note: Cyberpunk 2077 was excluded as only three months data was available",
       caption = "@danni_scales | source: Steam") +
  labs(x="",y="") +
  
  # theme
  theme(
    text = element_text(family ="Arial", color="grey30"),
    plot.title = element_text(face = "bold"),
    plot.background = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position="bottom",
    legend.justification = "right",
    legend.title=element_blank()
    ) 


ggsave("week_12.png", width=12, height=3)
