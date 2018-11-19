require(tidyverse)


url <- "https://www.teamrankings.com/nba/stat/points-per-game?date=2016-06-20"

fifteenstat <- read_html(url) %>% 
  html_nodes("table") %>% 
  .[[1]] %>%
  html_table()

fifteen <-  fifteenstat %>% select(Team, `2014`, `2015`)

url_17 <- 'https://www.teamrankings.com/nba/stat/points-per-game?date=2018-06-09'


seventeen <- read_html(url_17) %>% 
  html_nodes("table") %>% 
  .[[1]] %>%
  html_table() %>% 
  select(Team, `2016`, `2017`)

join_3 <- full_join(seventeen, fifteenstat)

url_18 <- "https://www.teamrankings.com/nba/stat/points-per-game?date=2018-11-18"


eighteen <- read_html(url_18) %>% 
  html_nodes("table") %>% 
  .[[1]] %>%
  html_table() %>% 
  select(Team, `2018`)


last_4 <- full_join(join_3, eighteen)

last_4 <- last_4 %>% select(Team, `2014`, `2015`, `2016`, `2017`, `2018`)

last_4 <- last_4 %>% 
  gather(key = 'year_id', value = 'av_point', `2014` : `2018`)%>% 
  group_by(year_id) %>% 
  mutate(rank = dense_rank(desc(av_point)))




###get champs

url_champs <- "https://en.wikipedia.org/wiki/List_of_NBA_champions#Champions"

champs_list <- read_html(url_champs) %>% 
  html_nodes(".hlist ul , #NBA_champions") %>% 
  html_text()

champs_list <- champs_list[[6]]

champs_list <- champs_list %>% str_replace_all("Trail Blazers", "Trailblazers")

#parse list to df
champs_df <- data.frame(do.call('rbind',(strsplit(champs_list, "\n", fixed = TRUE))))

#transpose easy

champs_t <- as.data.frame(t(champs_df))

champs_t$V1 <- as.character(champs_t$V1)

year_champs <- as.data.frame(str_split_fixed(champs_t$V1, ":", 2)) 

year_champs <- year_champs %>% rename(year_id = V1,
                                      team = V2)

last_4$Team[last_4$Team == 'Golden State Warriors'] <- " Golden State Warriors"
last_4$Team[last_4$Team == 'Warriors'] <- " Golden State Warriors"
last_4$Team[last_4$Team == 'Golden State'] <- " Golden State Warriors"
last_4$Team[last_4$Team == 'San Antonio Spurs'] <- " San Antonio Spurs"
last_4$Team[last_4$Team == ' Cleveland Cavaliers'] <- " Cleveland Cavaliers"

four_final <- full_join(last_4, year_champs, by = 'year_id')

four_final$team <- as.character(four_final$team)

four_final2 <- four_final %>% mutate(result = ifelse(Team == team, "champs", "DNQ"))


final_four_3 <- four_final2 %>% filter(year_id > 2013)

final_table_3 <- rename(final_four_3, fran_id = Team,
                        finish = result)

final_table_3$year_id <- as.numeric(final_table_3$year_id)

final_table_4 <- left_join(clean_rank, final_table_3)

clean2 <- select(clean_rank, year_id, fran_id, rank, av_point, result, finish)

final_table_5 <- rbind(clean2, final_table_3)

final_table_5 <- unique(final_table_5) %>% 
  select(-result)

#learn how to parse JS at some point

champ_plot2 <- ggplot(final_table_5 %>% group_by(year_id), aes(year_id, av_point, color = finish, alpha = finish)) +
  labs(title = "Defence Wins Championships", 
       subtitle = "Times the NBA Champions have also averaged \nthe most points per season",
       x = "Season (year ending)",
       y = "Average Points Per Season") + 
  geom_point(aes(size = 12, shape = finish)) +
  geom_text_repel(data = filter(final_table_5, finish == "champs" & rank == 1), 
                  aes(year_id, av_point, size = 12, label = paste("",fran_id,",",year_id,"")), 
                  show.legend = FALSE, nudge_x = -7) +
  scale_shape_manual(values = c(20, 21, 6)) +
  scale_alpha_manual(values = c(0.8, 0.3)) +
  scale_color_discrete(name = "NBA Champions", labels = c("Champions", "DNQ", "Playoffs"))

champ_plot2 <- champ_plot2 + scale_x_continuous(breaks = seq(min(1940), max(2019), by = 5)) +
  theme(plot.title = element_text(size = 32, family = "Arial", colour = "white", face = "bold"),
        plot.subtitle = element_text(size = 20, family = "Arial", colour = "white", face = "italic"),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        legend.background = element_blank(),
        axis.text.x = element_text(colour =  "white", size = 13),
        axis.text.y = element_text(colour =  "white", size = 13),
        axis.title.x = element_text(colour =  "white", size = 20, face = 'bold'),
        axis.title.y = element_text(colour =  "white", size = 20, face = 'bold'),
        legend.text = element_text(colour =  "white", size = 16),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey25"),
        panel.grid.minor.x = element_line(color = "grey18"),
        legend.title = element_text(color = 'white', family = "Arial", size = 22),
        legend.key = element_blank()) +
  guides(size = FALSE, shape = FALSE, result = FALSE, alpha = FALSE) +
  annotate("label", x = 1983, y = 65, 
           label = "There have been 3 instances in the History of the NBA where the highest average scoring team has won the championship \n two of which have occurred in the past 3 years",
           color = "grey60",
           fill = 'black') +
  scale_shape_manual(values = c(20, 21, 7, 7)) +
  scale_alpha_manual(values = c( 1, 0.6, 0.8, 0.8)) +
  scale_color_discrete(name = "NBA Champions", labels = c("Champions", "DNQ"))

champ_plot2

ggsave("champs_av_points.png", plot = champ_plot2, width = 300, height =175, units = "mm", dpi = 400)






