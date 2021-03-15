voting.df <- read.csv("mvp_voting.csv")

# split the Player column to two columns, their full name and Basketballreference playerID 
for (i in 1:length(voting.df$Player)){
  a <- unlist(strsplit(voting.df$Player[i], ".", fixed = TRUE))
  voting.df$Name[i] = a[1]
  voting.df$PlayerID[i] = a[2]
}

#now remove the Player column 
voting.df <- voting.df[,-c(2)]

# now we have a baseline 
# let's add in the season win totals for teams 
team_wins.df <- read.csv("nba_season_franchise_wins.csv")

team_win_percent.df <- team_wins.df

# need to convert original df to win % 

# iterate through each of the rows 
for( i in 1:length(team_win_percent.df$TotalGames)){
  totalGames <- team_win_percent.df$TotalGames[i]
  
  #iterate through each of the team columns 
  for(j in 4:34){
    team_win_percent.df[i,j] <- team_win_percent.df[i,j]/totalGames
  }
}

# cool, now we ignore NAs because they don't matter 
# they don't matter because those teams don't exist, so it's fine 

# three franchises changed names/cities, so need to makes sure their abbreviations are accounted for 
team_win_percent.df$CHA <- team_win_percent.df$CHO
team_win_percent.df$CHH <- team_win_percent.df$CHO
team_win_percent.df$NOH <- team_win_percent.df$NOP
team_win_percent.df$SEA <- team_win_percent.df$OKC
team_win_percent.df$NJN <- team_win_percent.df$BRK
team_win_percent.df$WSB <- team_win_percent.df$WAS
team_win_percent.df$SDC <- team_win_percent.df$LAC
team_win_percent.df$KCK <- team_win_percent.df$SAC

row_count <-c()
# some players have multiple teams, and are listed as TOT -> need to find the second team they played for players_tot <- c()
player_data.df <- read.csv("nba-data-historical.csv")
for(i in 1:length(voting.df$Tm)){
  if(voting.df$Tm[i] == "TOT"){
    row_count <- c(row_count, i)
    print(voting.df$Year[i])
    print(voting.df$Name[i])
  }
}

# those are all the players that have the team names of "total" 
# and we have a vector to keep track of those rows 
for( i in 1:length(row_count)){
  player_rows <- grep(voting.df$PlayerID[row_count[i]], player_data.df$player_id)
  
  for( j in 1:length(player_rows)){
    if ((player_data.df$year_id[player_rows[j]] == voting.df$Year[row_count[i]]) && (player_data.df$type[player_rows[j]] == "RS") )
    {
      if(voting.df$Tm[row_count[i]] == "TOT"){
      #print("changed ",voting.df$Name[i]," ", voting.df$Year[i], " ", voting.df$Tm[i], " to ", player_data.df$team_id[player_rows[j]])
      print(voting.df$Name[row_count[i]])
      print(voting.df$Year[row_count[i]])
      print(voting.df$Tm[row_count[i]])
      print(player_data.df$team_id[player_rows[j]])
      voting.df$Tm[row_count[i]] <- player_data.df$team_id[player_rows[j]]
      }
    }
  }
}

# now we need to add in the team win pct to each of those games 
for(i in 1:length(voting.df$PlayerID)){
  team <- voting.df[i,3]
  year <- voting.df[i,20]
  col <- grep(team, colnames(team_win_percent.df))
  row <- grep(year, team_win_percent.df$Season)
  
  voting.df$winPct[i] <- team_win_percent.df[row, col]
}

# add in percent of games played to each player 