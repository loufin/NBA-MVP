# Split Train test 
voting.df <- read.csv("ExportedDataFrames\\total.csv")

voting.org.df <- voting.df 
voting.df[is.na(voting.df)] <- 0
voting.df$MVP <- as.factor(voting.df$MVP)
# create a dataframe where the only predictive value is MVP binary 
voting.df <- voting.df[,-c(25:29)]

year <- seq(1980, 2020, 1)

year.train.index <- sample(year, 0.8*length(year))
year.valid.index <- setdiff(year, year.train.index)

train.df <- data.frame(matrix(0, ncol = length(colnames(voting.df))))
colnames(train.df)<- colnames(voting.df)
train.df <- train.df[-c(1),]

valid.df <- data.frame(matrix(0, ncol = length(colnames(voting.df))))
colnames(valid.df)<- colnames(voting.df)
valid.df <- valid.df[-c(1),]

# one training set for all selected years 
for(i in 1:length(voting.df$PlayerID)){
  if( voting.df$Year[i] %in% year.train.index ) {
    train.df <- rbind(train.df, voting.df[i,]) 
  }
  else {
    valid.df <- rbind(valid.df, voting.df[i,])
  }
}

# logistic regression model to predict for variable MVP 
logit.reg <- glm(MVP ~ Age + G + winPct + pctGamesPlayed + MP +
                   PTS + TRB + AST + STL + BLK + FG. +
                   X3P. + FT. + WS + WS.48 + TS + raptorO +
                   raptorD + raptorPM + USG, data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, train.df, type = "response")
library(caret)
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(train.df$MVP))

results.df <- data.frame(row.names = seq(1,length(logit.reg.pred),1))
results.df$Name <- train.df$Name
results.df$Year <- train.df$Year
results.df$Actual <- train.df$MVP
results.df$log_reg <- logit.reg.pred
results.df$log_red_pred <- as.factor(ifelse(logit.reg.pred > 0.5, 1, 0))

# now do it with the test set 
logit.reg.pred2 <- predict(logit.reg, valid.df, type = "response")
library(caret)
confusionMatrix(as.factor(ifelse(logit.reg.pred2 > 0.5, 1, 0)), as.factor(valid.df$MVP))

results2.df <- data.frame(row.names = seq(1,length(logit.reg.pred2),1))
results2.df$Name <- valid.df$Name
results2.df$Year <- valid.df$Year
results2.df$Actual <- valid.df$MVP
results2.df$log_reg <- logit.reg.pred2
results2.df$log_red_pred <- as.factor(ifelse(logit.reg.pred2 > 0.5, 1, 0))

# First model prediction 


# linear discriminant analysis to predict for variable MVP 


# test - needs to be done dynamically in order to separate each year into it's own dataframe 

# create empty df for test 


valid.results.df <- data.frame(row.names = seq(1,length(valid.df$PlayerID),1))
for(season in year.valid.index){
  
  temp.df <- data.frame(matrix(0, ncol = length(colnames(valid.df))))
  colnames(temp.df)<- colnames(valid.df)
  temp.df <- temp.df[-c(1),]
  print(season)
  for(index in 1:length(valid.df$Name)){
    
    if(valid.df$Year[index] == season){
      temp.df <- rbind(temp.df, valid.df[index,]) 
    }
  }
 # print("Season ", season)
  
  temp.pred <- predict(logit.reg, temp.df, type = "response")
  print("here")
  temp.results.df <- data.frame(row.names = seq(1,length(temp.pred),1))
  temp.results.df$Name <- temp.df$Name 
  temp.results.df$Year <- temp.df$Year
  temp.results.df$Actual <- temp.df$MVP
  temp.results.df$log_reg <- temp.pred
  print("here")
  temp.results.df <- temp.results.df[order(temp.results.df$log_reg, decreasing = TRUE),]
  temp.results.df$log_reg_pred <- 0
  temp.results.df$log_reg_pred[1] <- 1 
  
  valid.results.df <- rbind(valid.results.df, temp.results.df)
  remove(temp.results.df)
  remove(temp.df)
}
confusionMatrix(as.factor(valid.results.df$log_reg_pred), as.factor(valid.results.df$Actual))


train.results.df <- data.frame(row.names = seq(1,length(train.df$PlayerID),1))
for(season in year.train.index){
  
  temp.df <- data.frame(matrix(0, ncol = length(colnames(train.df))))
  colnames(temp.df)<- colnames(train.df)
  temp.df <- temp.df[-c(1),]
  print(season)
  for(index in 1:length(train.df$Name)){
    
    if(train.df$Year[index] == season){
      temp.df <- rbind(temp.df, train.df[index,]) 
    }
  }
  
  temp.pred <- predict(logit.reg, temp.df, type = "response")
  print("here")
  temp.results.df <- data.frame(row.names = seq(1,length(temp.pred),1))
  temp.results.df$Name <- temp.df$Name 
  temp.results.df$Year <- temp.df$Year
  temp.results.df$Actual <- temp.df$MVP
  temp.results.df$log_reg <- temp.pred
  print("here")
  temp.results.df <- temp.results.df[order(temp.results.df$log_reg, decreasing = TRUE),]
  temp.results.df$log_reg_pred <- 0
  temp.results.df$log_reg_pred[1] <- 1 
  
  train.results.df <- rbind(train.results.df, temp.results.df)
  
  remove(temp.results.df)
  remove(temp.df)
  
}

###############################################
# Simple Logistic Regression 
###############################################
logit.simple.reg <- glm(MVP ~ G + winPct + MP +
                   AST +
                   X3P. + FT. + WS + WS.48 + raptorO +
                   raptorD + raptorPM, data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.simple.reg)
