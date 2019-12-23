library("tidyverse")
library("cowplot")
library("gapminder")
library("ggridges") 
library("scales")
library("janitor")
library("lubridate")
library("stringr")

#https://raw.githubusercontent.com/vega/vega-datasets/master/data/movies.json
df_movies_raw_1980 <- read_csv("../data/movies_clean_data_python.csv")

#https://www.natoonline.org/data/ticket-price/
df_tix_price <- read_csv("../data/movie_ticket_prices.csv")

#https://www.in2013dollars.com/us/inflation/1980?amount=1
df_inf <-read_csv("../data/inflation_data_1975_to_2019.csv")

##https://www.kaggle.com/danofer/movies-data-clean/output Data from
df_movie_raw_2017 <- read_csv("../data/movies_tmdbMeta.csv")

#clean and select df_movie_raw_2017
df_movie_raw_2017 <- df_movie_raw_2017 %>% 
  select(budget, original_title, original_language,
         release_date, revenue, production_companies) %>% 
  drop_na(budget, original_title, original_language,
          release_date, revenue, production_companies) %>% 
  filter(original_language == "en") %>% 
  #select(-original_language) %>% 
  mutate(year = year(release_date))

#filter production companies down to the main one  
df_movie_raw_2017<- df_movie_raw_2017 %>% 
  mutate(splits = strsplit(production_companies, "'")) %>% 
  rowwise() %>% 
  mutate(Distributor = splits[2]) 


df_movie_raw_2017 <- df_movie_raw_2017 %>% 
  filter(year >= 1975) %>% 
  select(original_title, revenue, budget, Distributor, year) %>% 
  clean_names() %>% 
  rename(title = original_title, 
         worldwide_gross = revenue, 
         production_budget = budget)

#make it looke like df_movies_raw and filter movies that arent in df_movies_raw
#note in the 1980-2010 it was bottom 5% of production_budget and 1% of us_gross
#Becasue there is now no US Gross Worldwide gross will replace it. 
#Now for 2011 to 2017 it will be bottom 5% of wordwide_gross and production_budget

bot_5_pb <- quantile(df_movie_raw_2017$production_budget, .05)[[1]]
bot_5_wg <- quantile(df_movie_raw_2017$worldwide_gross, .05)[[1]]

df_movie_raw_2017 <- df_movie_raw_2017 %>% 
  filter(production_budget >= bot_5_pb, 
         worldwide_gross >= bot_5_wg)

#remove all the na from the production companies remove duplicates
df_movie_raw_2017 <- df_movie_raw_2017 %>% 
  distinct(title, .keep_all = TRUE) %>% 
  drop_na() 


#drop index and clean column names 
df_movies_raw_1980 <- df_movies_raw_1980 %>%
  select(-X1, -US_Gross) %>% 
  clean_names() %>% 
  rename(year = release_year) 

df_movies_raw <- rbind(df_movie_raw_2017, df_movies_raw_1980)

#df_movies_raw <- df_movies_raw %>% disin

#change the column types for tix price 
df_tix_price$year <- as.numeric(df_tix_price$year)
df_tix_price$price <- str_remove(df_tix_price$price, "[$]")
df_tix_price$price <- as.numeric(df_tix_price$price)


#join the three tables together
df_movies_raw <- left_join(df_movies_raw, df_tix_price, by = "year")
df_movies_raw <- left_join(df_movies_raw, df_inf, by ="year")

#check if all the movie titles are unique or if an additional col of 
#movie & title needs be added.

unique(length(df_movies_raw$title))
length(df_movies_raw$title)

#conclude that all titles in this data set are unique, no addiontal wrangling

#make an profit column, world and us. Box offit - production budget
df_movies_raw <- df_movies_raw %>% 
  mutate(worldwide_profit_gross = worldwide_gross - production_budget)

#butts in seats (bits) us and world.  Boxoffice/price per ticket 
df_movies_raw <- df_movies_raw %>% 
  mutate(worldwide_bits = worldwide_gross/price)

#adjust for inflation budget, boxoffice and profit. adj = gross/inflation  
df_movies_raw <- df_movies_raw %>% 
  mutate(production_budget_adj = production_budget/amount,
         worldwide_adj = worldwide_gross/amount, 
         worldwide_profit_adj = worldwide_profit_gross/amount)

duplicate_titles <- c("10,000 BC", "28 Days Later", "Adaptation", "Aeon Flux","	Alpha and Omega 3D", "Barbershop 2:  Back in Business",  "Battlefield Earth", "Beetlejuice", "Blade 2", "Borat", "Chéri", "	Cry_Wolf", "	Dead Poets Society", "DÈj‡ Vu", "Dude, Where's My Car?", "	Dumb & Dumber", "Dungeons and Dragons", "Fantasia 2000 (Theatrical Release)", "Halloween 3: Season of the Witch", "Hellboy 2: The Golden Army", "	Herbie Fully Loaded", "House of 1,000 Corpses", "I Now Pronounce You Chuck & Larry", "I'm Not There.", "	Interview with the Vampire", "Jackass Number Two", "Jane Austen's Mafia!", "Jeepers Creepers II", "Jurassic Park 3", "Kama Sutra - A Tale of Love","Kill Bill: Vol. 1", "Kill Bill: Vol. 2", "Kiss Kiss Bang Bang", "Lara Croft: Tomb Raider: The Cradle of Life", "Licence to Kill", "Life or Something Like It", "Love & Basketball", "Mad Max 2", "	Men in Black II", "Mission: Impossible 2", "Moulin Rouge!", "Mouse Hunt", "Mr. & Mrs. Smith", "National Lampoon's Van Wilder", "Resident Evil: Afterlife 3D", "Speed II: Cruise Control", "Star Wars: Episode I - The Phantom Menace", "Star Wars: Episode II - Attack of the Clones", "Star Wars: Episode III - Revenge of the Sith", "Step Up 2 the Streets", "	The 40 Year Old Virgin", "The Adventures of Sharkboy and Lavagirl in 3-D", "The Break Up", "The Empire Strikes Back", "The Forgotten", "	The Informant!", "The Taking of Pelham 1 2 3", "The Work and the Glory: American Zion", "	The X Files: Fight the Future", "Yours, Mine & Ours", "Yu-Gi-Oh! The Movie", "Про любоff", "포화 속으로", "美人鱼", "Zathura: A Space Adventure", "Who Framed Roger Rabbit", "Urban Legends: Final Cut", "ET: The Extra-Terrestrial", "Return of the Jedi", "Original Gangstas")

#reorder the data frame and drop some non essental info
df_movies_raw <- df_movies_raw %>% 
  select(title, year, distributor,
         worldwide_gross, worldwide_profit_gross, worldwide_adj, worldwide_profit_adj, worldwide_bits,        
         production_budget, production_budget_adj) %>% 
  mutate(norm_title = toupper(title)) %>% 
  distinct(norm_title, .keep_all = TRUE) %>% 
  select(-norm_title) %>% 
  filter(!(title %in% duplicate_titles))


df_movies_raw$distributor[df_movies_raw$distributor %in% ' Company\", '] <- "Warner Bros."

  

df_movies_clean <- df_movies_raw


write_csv(df_movies_clean, "../data/movies_data_clean_new.csv")

