################################################
# Logit function to predict based on year 
###############################################
library(tidyverse)
logit_predict_separate_years <- function(model, year_index, df){
  
  results.df <- data.frame(row.names = seq(1,length(df$PlayerID),1))
  for(season in year_index){
    # create a temporary dataframe to hold all players in a single year
    temp.df <- data.frame(matrix(0, ncol = length(colnames(df))))
    
    # assign all years to be the same 
    colnames(temp.df)<- colnames(df)
    
    # remove first row, because it is populated by default 
    temp.df <- temp.df[-c(1),]
    #print(season)
    
    # this loop will add in all plauer seasons who's years match
    for(index in 1:length(df$Name)){
      
      if(df$Year[index] == season){
        # add that datapoint into the temporary df if the years match 
        temp.df <- rbind(temp.df, df[index,]) 
      }
    }
    # based on out logit model, predict the year 
    temp.pred <- predict(model, temp.df, type = "prob")
    temp.pred <- temp.pred[,2]
    # create temporary dataframe with all the players from the temp df
    # will be adding in their name, whether they made MVP, or if they 
    temp.results.df <- data.frame(row.names = seq(1,length(temp.pred),1))
    temp.results.df$Name <- temp.df$Name
    temp.results.df$Year <- temp.df$Year
    temp.results.df$Actual <- temp.df$MVP
    temp.results.df$log_reg <- temp.pred # add in prediction probability 
    temp.results.df <- temp.results.df[order(temp.results.df$log_reg, decreasing = TRUE),]
    temp.results.df$log_reg_pred <- 0
    temp.results.df$log_reg_pred[1] <- 1 
    
    results.df <- rbind(results.df, temp.results.df)
    
    remove(temp.results.df)
    remove(temp.df)
  }
  return(results.df)
}

################################################################
# Logit Create df with readable output for results 
##################################################################
logit_readable_output <- function(df){
  cols <- c("Year", "Winner", "Winner.Score", "Pred.Winner", "Pred.Winner.Score")
  
  winner_eval.df <- data.frame(matrix(0, ncol = length(cols), nrow = length(unique(df$Year))))
  data.frame()
  # assign all years to be the same 
  colnames(winner_eval.df)<- cols
  
  years <- unique(df$Year)
  
  i = 1
  for(year in years){
    #print(year)
    
    # find row with the year matching the loop and it is the winner 
    winner_row <- df[which(df$Year == year & df$Actual == 1),] 
    #print(winner_row)
    # find row with the year matching the loop and it is the predicted winner 
    
    pred_winner_row <- df[which(df$Year == year & df$log_reg_pred == 1),] 
    #print(pred_winner_row)
    #print(winner_eval.df)
    winner_eval.df$Year[i] <- year
    winner_eval.df$Winner[i] <- winner_row$Name[1]
    winner_eval.df$Winner.Score[i] <- winner_row$log_reg[1]
    winner_eval.df$Pred.Winner[i] <- pred_winner_row$Name[1]
    winner_eval.df$Pred.Winner.Score[i] <- pred_winner_row$log_reg[1]
    #print("here")
    i = i + 1 
  }
  
  sort(winner_eval.df$Year)
  return(winner_eval.df)
  
}

##########################################
# Attempt to do cross Validation with logit
##########################################
voting.df <- read.csv("ExportedDataFrames\\total.csv")

voting.org.df <- voting.df 
voting.df[is.na(voting.df)] <- 0
voting.df$MVP <- as.factor(voting.df$MVP)

# create a dataframe where the only predictive value is MVP binary 
voting.df <- voting.df[,-c(25:29)]

#create sequence of all the possible years 
year <- seq(1980, 2020, 1)

# create train.validation index of 90/10 
year.train.index <- sample(year, 0.9*length(year))
year.valid.index <- setdiff(year, year.train.index)

# create empty train df with the columns of the original df 
train.df <- data.frame(matrix(0, ncol = length(colnames(voting.df))))
colnames(train.df)<- colnames(voting.df)
train.df <- train.df[-c(1),]

# create empty test df with the columns of the original df 
valid.df <- data.frame(matrix(0, ncol = length(colnames(voting.df))))
colnames(valid.df)<- colnames(voting.df)
valid.df <- valid.df[-c(1),]

# add in each row to the training or validation df depending on the year 
for(i in 1:length(voting.df$PlayerID)){
  # check if the year is a training or a validation year 
  if( voting.df$Year[i] %in% year.train.index ) {
    # add to train df 
    train.df <- rbind(train.df, voting.df[i,]) 
  }
  else {
    # add to validation df 
    valid.df <- rbind(valid.df, voting.df[i,])
  }
}

library(caret)

# define training control
train_control <- trainControl(method = "cv", number = 41)

# train the model on training set
model <- train(MVP ~ Age + G + winPct + pctGamesPlayed + MP +
                 PTS + TRB + AST + STL + BLK + FG. +
                 X3P. + FT. + WS + WS.48 + TS + raptorO +
                 raptorD + raptorPM + USG,
               data = voting.df,
               trControl = train_control,
               method = "glm",
               family=binomial())
# print cv scores
summary(model)

results.df <- logit_predict_separate_years(model, year, voting.df)
library(caret)
confusionMatrix(as.factor(results.df$log_reg_pred), as.factor(results.df$Actual))
results.readable.df <- logit_readable_output(results.df)


