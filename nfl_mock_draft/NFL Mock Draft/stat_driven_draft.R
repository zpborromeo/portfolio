# Author: Zach Borromeo
# Project Name: AI Final Project
# Date Created: March 20, 2022
# Last Modified: April 13, 2022


# importing all required libraries for this project, primarily to handle data,
# SQL tables, and statistics of tables
library(httr)
library(tidyverse)
library(DBI)
library(janitor)
library(fs)
library(pool)
library(RPostgres)
library(stringdist)
library(fuzzyjoin)
library(tm)
library(matrixStats)

#importing all data related to this project, files are located in the project folder
# of this assignment

#list of the current NFL draft order for 2022
current_draft_order <- readxl::read_excel("data/nfl_draft_order_2022.xlsx") %>%
  clean_names()

#list of combined team needs for 2022
team_needs <- readxl::read_excel("data/nfl_draft_team_needs.xlsx") %>%
  clean_names() %>%
  rename(team = team_name)

#list of combined top players for the 2022 draft
top_200_players <- readxl::read_excel("data/top_200_players.xlsx") %>%
  clean_names()

#merging of draft order with the team needs in order
#this is to ensure each pick has team needs associated with them
draft_order_pickless <- current_draft_order %>%
  select(team)

merge_order_needs <- draft_order_pickless %>%
  left_join(team_needs, by="team")

#using remaining big board as a varible to remove players from as they are picked
#this ensures the same plaer isn't selected twice
remaining_big_board <- top_200_players

#importing the data from previous drafts to read and perform analysis
historic_draft_data <- readxl::read_excel("data/historical_draft_data.xlsx")

#since we only care about the Round, the Pick number, and the Position,
#we select these columns only
stripped_HDD <- historic_draft_data %>% 
  select(Round, Overall, Position)

#Now we are building the dataframe to total the number of times each position is
#selected at each pick
HDD_df <- data.frame("Round" = numeric(),
                     "Pick" = numeric(),
                     "QB" = numeric(),
                     "RB" = numeric(),
                     "WR" = numeric(),
                     "TE" = numeric(),
                     "OT" = numeric(),
                     "OG" = numeric(),
                     "OC" = numeric(),
                     "EDGE" = numeric(),
                     "DL" = numeric(),
                     "CB" = numeric(),
                     "LB" = numeric(),
                     "S" = numeric(),
                     "K" = numeric(),
                     "P" = numeric())

round <- 1
pick <- 1

#here we are building rows for each pick as our dataframe from before is empty
for (i in 1:nrow(current_draft_order)){
  
  row_i = c(round, pick, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  pick <- pick + 1
  
  if (pick <= 32){
    round <- 1
  }else if (pick <= 64 && pick > 32){
    round <- 2
  }else{
    round <- 3
  }
  
  HDD_df[i, ] <- row_i
}

#this for loop is going through the historical data and updating each row with +1
#to the column for the position drafted
for (i in 1:nrow(stripped_HDD)){
  
  overall <- as.numeric(stripped_HDD[i, 2])
  
  pick_pos <- stripped_HDD[i, 3]
  
  temp_HDD <- HDD_df %>% 
    filter(Pick == as.numeric(overall))
  
  index <- which(colnames(temp_HDD) == as.character(pick_pos), arr.ind = TRUE)
  
  temp_var <- as.numeric(HDD_df[overall, index])
  temp_var <- as.numeric(temp_var + 1)
  
  HDD_df[overall, index] <- temp_var
  
}

#clearing out empty rows on a temporary dataframe to perform row level statistics
temp_HDD_df <- HDD_df %>% 
  filter(!is.na(Round)) %>% 
  select(-c(Round, Pick))

#mutating dataframe to find the max values/names of the columns with the highest
#values in each row and then removing all digits and punctuation in those columns
temp_HDD_df <- temp_HDD_df %>% 
  mutate(Position_Name1 = colnames(temp_HDD_df[max.col(temp_HDD_df, "first")])) %>% 
  mutate(Position_Name2 = colnames(temp_HDD_df[max.col(temp_HDD_df, "last")])) %>%
  mutate(Position_Name3 = colnames(temp_HDD_df[max.col(temp_HDD_df, "random")])) %>%
  mutate(max_val = as.numeric(rowMaxs(as.matrix(temp_HDD_df)))) %>% 
  mutate(Position_Name1 = str_replace_all(Position_Name1, "[[:punct:]]", "")) %>%
  mutate(Position_Name1 = str_replace_all(Position_Name1, "[[:digit:]]", "")) %>% 
  mutate(Position_Name2 = str_replace_all(Position_Name2, "[[:punct:]]", "")) %>%
  mutate(Position_Name2 = str_replace_all(Position_Name2, "[[:digit:]]", "")) %>% 
  mutate(Position_Name3 = str_replace_all(Position_Name3, "[[:punct:]]", "")) %>%
  mutate(Position_Name3 = str_replace_all(Position_Name3, "[[:digit:]]", ""))

#rejoining the two dataframes together
HDD_df <- HDD_df %>% 
  left_join(temp_HDD_df) %>% 
  filter(!is.na(Round))

round <- 1
pick <- 1

#building the potential draft order dataframe
potential_draft_order <- data.frame("Round" = character(),
                                    "Pick" = character(),
                                    "Team" = character(),
                                    "Name" = character(),
                                    "Position" = character(),
                                    "College" = character(),
                                    "Score" = numeric(),
                                    "Score Multiplier" = numeric(),
                                    "Combined Rank" = numeric(),
                                    "Score Multiplier Rank" = numeric(),
                                    "Average Rank" = numeric())

#for loop to perform drafting of players
for (i in 1:nrow(merge_order_needs)){
  
  #getting the team name
  team_name <- as.character(merge_order_needs[i, 1])
  
  #for this part of the draft, since we are using statistics of previous drafts,
  #we want to pick a position with the highest pick percentage per draft pick
  #for example, historically 16 of the past 22 number one picks have been QBs, 
  #so we would end up choosing a QB. However, in the case where multiple positions
  #are taken with the same frequency, say CB, EDGE, and OT were all taken 6 times 
  #each at pick 30, we need to randomly pick one of them to draft. Hence why we 
  #randomly generate a number between 17 and 19, the indexes of the columns where
  #we see the most occurred player position selected
  #this does not affect instances like the aforementioned pick one. All three of 
  #the columns have the value of QB in them
  
  random_val <- round(runif(1, min=17, max=19))
  
  #for this loop, we need to increment the round number based on the picks. Round
  #1 moves to Round 2 after 32 picks, and Round 2 to Round 3 after 64 picks
  if (pick <= 32){
    round <- 1
  }else if (pick <= 64 && pick > 32){
    round <- 2
  }else{
    round <- 3
  }

  #getting the position of the pick from our table
  pick_pos <- HDD_df[i, as.numeric(random_val)]
  
  #finding the best available player at that position
  pick_selection <- remaining_big_board %>% 
    filter(position == as.character(pick_pos)) %>% 
    head(n=1)
  
  #now, if we can't find a player at that position, we need to still draft a player
  #in these instances, taking the best player available is a feasible strategy,
  #which is what this if statement does
  if (nrow(pick_selection) == 0){
    pick_selection <- remaining_big_board %>% 
      filter(score_total == max(score_total)) %>% 
      head(n=1)
  }
  
  #adding the pick to the draft order
  row_i = c(round, pick, team_name, pick_selection)
  potential_draft_order[i, ] <- row_i
  
  #finding the name of the pick
  pick_name <- pick_selection %>% 
    select(name)
  
  #removing the player from the board
  remaining_big_board <- remaining_big_board %>% 
    filter(name != as.character(pick_name))
  
  
  #increasing the pick we are on
  pick <- pick + 1
  
  
  
}

#You will need to change your directory to see the output if you want to run the files yourself
save_document <- write.csv(potential_draft_order, "E:/UALR/Artificial Intelligence/borromeoz-mock-draft/NFL-Mock-Draft/outputs/SDD_PDO_V1.csv")
