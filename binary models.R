# Split Train test 
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
# now we have our training and testing dfs 

# logistic regression model to predict for variable MVP 
logit.reg <- glm(MVP ~ Age + G + winPct + pctGamesPlayed + MP +
                   PTS + TRB + AST + STL + BLK + FG. +
                   X3P. + FT. + WS + WS.48 + TS + raptorO +
                   raptorD + raptorPM + USG, data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

# predict training 
logit.reg.pred <- predict(logit.reg, train.df, type = "response")
library(caret)
# confusion matrix with a 0.5 cutoff 
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(train.df$MVP))
# but this is useless! We don't want the model to simply say whether a player is MVP in a vaccum 


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


###############################################
# Simple Logistic Regression 
###############################################
logit.simple.reg <- glm(MVP ~ G + winPct + MP +
                   AST +
                   X3P. + FT. + WS + WS.48 + raptorO +
                   raptorD + raptorPM, data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.simple.reg)


