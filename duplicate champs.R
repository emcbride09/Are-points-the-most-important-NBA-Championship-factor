require(httr)
require(rvest)
require(rlist)
install.packages('varhandle')
require(varhandle)


url <- "https://en.wikipedia.org/wiki/List_of_NBA_champions#Champions"

champs_list <- read_html(url) %>% 
  html_nodes(".hlist ul , #NBA_champions") %>% 
  html_text()

#creates a list, select long string with all data
champs <- champs_list[6]

# got this weird instruction using rbind as string...works from here --- needs placing
champs_df <- data.frame(do.call('rbind',(strsplit(champs, "\n", fixed = TRUE))))

champs_t <- as.data.frame(t(champs_df)) 

champs_t$V1 <- as.character(champs_t$V1)

year_champs <- as.data.frame(str_split_fixed(champs_t$V1, ":", 2)) 

year_champs <- year_champs %>% rename(year_id = V1,
                      team = V2)

#new var with only last word of var2
year_champs <- year_champs %>% mutate(fran_id = word(year_champs$team, -1))

yr_ch_final <- year_champs %>% select(year_id, fran_id)

yr_ch_final$year_id <- as.numeric(as.character(yr_ch_final$year_id))

yr_ch_final <- yr_ch_final %>% rename(champs = fran_id)






