# Author: Zach Borromeo
# Project Name: AI Final Project
# Date Created: March 18, 2022
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

draft_order_pickless <- current_draft_order %>% 
  select(team)

#merging of draft order with the team needs in order
#this is to ensure each pick has team needs associated with them
merge_order_needs <- draft_order_pickless %>% 
  left_join(team_needs, by="team")

remaining_big_board <- top_200_players

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
#in this for loop, we are going to be using the team needs of each team
#in order to select players, thus the need for need1, need2, and need3
for (i in 1:nrow(merge_order_needs)){
  team_name <- as.character(merge_order_needs[i, 1])
  
  if (pick <= 32){
    round <- 1
  }else if (pick <= 64 && pick > 32){
    round <- 2
  }else{
    round <- 3
  }
  
  need1 <- as.character(merge_order_needs[i, 3])
  need2 <- as.character(merge_order_needs[i, 4])
  need3 <- as.character(merge_order_needs[i, 5])
  
  #filtering potential picks based on the base player available at that position
  potential_pick1 <- remaining_big_board %>% 
    filter(position == need1) %>% 
    filter(score_total == max(score_total)) %>% 
    head(n=1)
  potential_pick2 <- remaining_big_board %>% 
    filter(position == need2) %>% 
    filter(score_total == max(score_total)) %>% 
    head(n=1)
  potential_pick3 <- remaining_big_board %>% 
    filter(position == need3) %>% 
    filter(score_total == max(score_total)) %>% 
    head(n=1)

  #here we need to compare scores of each pick, and then select the best available
  #player that fills a need for the team
  pp1_score <- potential_pick1 %>% 
    select(score_total)
  pp2_score <- potential_pick2 %>% 
    select(score_total)
  pp3_score <- potential_pick3 %>% 
    select(score_total)
  
  if(dim(pp1_score)[1] == 0){
    pp1_score[1, 1] = 0
  }
  if(dim(pp2_score)[1] == 0){
    pp2_score[1, 1] = 0
  }
  if(dim(pp3_score)[1] == 0){
    pp3_score[1, 1] = 0
  }
  
  temp_max <- pp1_score 
  pick_selection <- potential_pick1
  
  if(temp_max < pp2_score){
    if(pp2_score < pp3_score){
      temp_max <- pp3_score
      pick_selection <- potential_pick3
    }else{
      temp_max <- pp2_score
      pick_selection <- potential_pick2
    }
  }
  
  #after finding the best player that fills a need, we draft the player and add
  #them to the draft list
  row_i = c(round, pick, team_name, pick_selection)
  potential_draft_order[i, ] <- row_i
  
  pick_name <- pick_selection %>% 
    select(name)
  
  pick_pos <- pick_selection %>% 
    select(position)
  
  remaining_big_board <- remaining_big_board %>% 
    filter(name != as.character(pick_name))
  
  #updating team needs based on pick selections
  #we needed a way to make sure duplicate positions weren't picked more than once
  #and remove the team needs from that list. 
  #in order to do that, the need was stripped from the team needs table through a 
  #temporary variable, then the row was removed and rebuilt without the drafted player
  #and added back to the team_needs table, then merged with the draft order list
  #again
  
  temp_team_needs <- team_needs %>% 
    filter(team == team_name)
  
  index <- which(temp_team_needs == as.character(pick_pos), arr.ind = TRUE)
  
  col_index <- as.numeric(index[, 2])
  
  row_i <- c(temp_team_needs[1:as.numeric((col_index - 1))], temp_team_needs[as.numeric((col_index + 1)):11], "NA")
  
  team_needs <- team_needs %>% 
    filter(team != team_name)
  
  team_needs[32, ] <- row_i
  
  merge_order_needs <- draft_order_pickless %>% 
    left_join(team_needs, by="team")
  
  pick <- pick + 1
  
  
  
}

#You will need to change your directory to see the output if you want to run the files yourself
save_document <- write.csv(potential_draft_order, "E:/UALR/Artificial Intelligence/borromeoz-mock-draft/NFL-Mock-Draft/outputs/SOD_PDO_V1.csv")


