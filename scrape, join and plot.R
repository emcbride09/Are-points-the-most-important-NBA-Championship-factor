require(tidyverse)
require(httr)
require(rvest)
require(rlist)
require(varhandle)
require(ggrepel)


nba_all <- read.csv("C:/Users/EMCB-PC/Desktop/rdir/learning stuff/NBA/nba_all/nbaallelo.csv")

nba_all$date_game <- lubridate::mdy(nba_all$date_game)


point_wins <- nba_all %>% 
  select(year_id, date_game, win_equiv, pts, X_iscopy, fran_id, is_playoffs, game_location, game_result, year_id) %>% 
  filter(X_iscopy == 1, game_result == 'W', game_location == 'A') %>% 
  group_by(fran_id, year_id) %>% 
  mutate(av_point = sum(pts)/length(pts))

#levels(iris$Species) <- c(levels(iris$Species), "new.species") #add a new factor level and change wizards to bullets
#change factor wizards to bullets in 1973

levels(point_wins$fran_id) <- c(levels(point_wins$fran_id), c("Bullets", "SuperSonics"))

point_wins$fran_id[point_wins$year_id == 1978 & point_wins$fran_id == "Wizards"] <- "Bullets"

point_wins$fran_id[point_wins$year_id == 1979 & point_wins$fran_id == "Thunder"] <- "SuperSonics"


#####find champs

url <- "https://en.wikipedia.org/wiki/List_of_NBA_champions#Champions"

champs_list <- read_html(url) %>% 
  html_nodes(".hlist ul , #NBA_champions") %>% 
  html_text()

#creates a list, select long string with all data
champs <- champs_list[6]

champs <- champs %>% str_replace_all("Bullets", "Baltimore")
champs <- champs %>% str_replace_all("Trail Blazers", "Trailblazers")

# got this weird instruction using rbind as string...works from here
champs_df <- data.frame(do.call('rbind',(strsplit(champs, "\n", fixed = TRUE))))

champs_t <- as.data.frame(t(champs_df)) 

champs_t$V1 <- as.character(champs_t$V1)

year_champs <- as.data.frame(str_split_fixed(champs_t$V1, ":", 2)) 

year_champs <- year_champs %>% rename(year_id = V1,
                                      team = V2)

#new var with only last word of var2
year_champs <- year_champs %>% mutate(fran_id = word(year_champs$team, -1))

yr_ch_final <- year_champs %>% select(year_id, fran_id)

yr_ch_final$fran_id <- as.factor(as.character(yr_ch_final$fran_id))

yr_ch_final$year_id <- as.numeric(as.character(yr_ch_final$year_id))

yr_ch_final <- yr_ch_final %>% rename(champs = fran_id)

#two teams have been called the bullets, rename one that was in 78 which was the Washington bullets
#must add factor

levels(yr_ch_final$champs) <- c(levels(yr_ch_final$champs), c("Bullets", "SuperSonics", '76ers', 'Sixers'))

yr_ch_final$champs[yr_ch_final$year_id == 1978 & yr_ch_final$champs == "Baltimore"] <- "Bullets"

yr_ch_final$champs[yr_ch_final$year_id == 1983 & yr_ch_final$champs == "76ers"] <- "Sixers"

#####join data
  

points_wins_champs <- full_join(point_wins, yr_ch_final, by = "year_id")

points_wins_champs$fran_id <- as.character(points_wins_champs$fran_id)

points_wins_champs$year_id <- as.numeric(points_wins_champs$year_id)

champs_only <- points_wins_champs %>% mutate(result = ifelse(champs == fran_id, "champs", "dnq"))

#df %>% mutate(rank = dense_rank(desc(score)))

champs_rank <- champs_only %>% group_by(year_id) %>% 
  mutate(rank = dense_rank(desc(av_point)))

###plot
##remove doubles for a cleaner plot


clean_rank <- champs_rank %>% select(year_id, rank, fran_id, av_point, result, is_playoffs)

clean_rank$rank <- as.numeric(clean_rank$rank)

clean_rank <- clean_rank %>% filter(rank != 'NA')

clean_rank <- clean_rank %>% mutate(finish = ifelse(result == "champs", "champs", "DNQ"))

clean_rank <- clean_rank %>% unique()


#this plot took it out of me...only 4 lines....wtf
champ_plot <- ggplot(clean_rank %>% group_by(year_id), aes(year_id, av_point, color = finish, alpha = finish)) +
  labs(title = "Defence Wins Championships", 
       subtitle = "Times the NBA Champions have also averaged \nthe most points per season",
       x = "Season (year ending)",
       y = "Average Points Per Season") + 
  geom_point(aes(size = 12, shape = result)) +
  geom_text_repel(data = filter(clean_rank, result == "champs" & rank == 1), 
            aes(year_id, av_point, size = 12, label = paste("",fran_id,",",year_id,"")), 
            show.legend = FALSE, nudge_x = 3.5) +
  scale_shape_manual(values = c(20, 21, 6)) +
  scale_alpha_manual(values = c(0.8, 0.3)) +
  scale_color_discrete(name = "NBA Champions", labels = c("Champions", "DNQ", "Playoffs"))
  
champ_plot <- champ_plot + scale_x_continuous(breaks = seq(min(1940), max(2015), by = 5)) +
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
           label = "There has been one instance in the History of the NBA where the highest average scoring team has won the championship which was by the Boston Celtics in 1957",
           color = "grey60",
           fill = 'black') +
  scale_shape_manual(values = c(20, 21, 7)) +
  scale_alpha_manual(values = c( 1, 0.6, 0.8)) +
  scale_color_discrete(name = "NBA Champions", labels = c("Champions", "DNQ", "Playoffs"))

  

champ_plot
  
ggsave("champs_av_points.png", plot = champ_plot, width = 500, height = 285, units = "mm", dpi = 400)

getwd()  

