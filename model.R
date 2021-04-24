# Split Train test 
voting.df <- read.csv("ExportedDataFrames\\total.csv")

voting.org.df <- voting.df 

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
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.4, 1, 0)), as.factor(train.df$MVP))
# linear discriminant analysis to predict for variable MVP 


# test - needs to be done dynamically in order to separate each year into it's own dataframe 
for(season in 1:length(year.valid.index)){
  year <- grep(voting.df$Year[i], team_wins.df$Season)
  
}

voting.df$Year[0]%in% year.train.index
