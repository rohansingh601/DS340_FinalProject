---
title: "data processing"
author: "Rohan Singh"
date: "2023-04-01"
---

#Clean Environment
rm(list = ls())
# Attaching necessary libraries
library(data.table)
library(dplyr)
library(xgboost)

# Reading in the data
data <- fread('./nfl_team_stats_2002-2021.csv')

# Creating data for wins, losses, and ties
data$diff <- (data$score_away - data$score_home)
data$winner <- 1
data$winner[data$diff < 0] <- data$home[data$diff < 0]
data$winner[data$diff > 0] <- data$away[data$diff > 0]

data$loser <- 1
data$loser[data$diff > 0] <- data$home[data$diff > 0]
data$loser[data$diff < 0] <- data$away[data$diff < 0]

data$tied[data$diff == 0] <- 1
data$winner[data$winner == 1] <- NA
data$loser[data$loser == 1] <- NA

#Seperating data into each respective season
nfl_list <- list()
#last day for every regular season
end_date_dict<- c("2009" = '-01-08', "2010" = '-01-07', "2011" = '-01-06', 
                  "2012" = '-01-04', "2013" = '-01-03', "2014" = '-01-02',
                  "2015" = '-01-08', "2016" = '-01-06', "2017" = '-01-05', 
                  "2018" = '-01-04', "2019" = '-01-03', "2020" = '-01-08', 
                  "2021" = '-01-14')

for (year in 2009:2021) {
  start_date <- as.Date(paste(year, "-08-01", sep = ""))
  end_date <- as.Date(paste(year + 1, end_date_dict[as.character(year)], sep = ""))
  nfl_list[[year]] <- data[data$date %between% c(start_date, end_date)]
}

# extracting each season from  nfl_list
nfl_2009 <- nfl_list[[2009]]
nfl_2010 <- nfl_list[[2010]]
nfl_2011 <- nfl_list[[2011]]
nfl_2012 <- nfl_list[[2012]]
nfl_2013 <- nfl_list[[2013]]
nfl_2014 <- nfl_list[[2014]]
nfl_2015 <- nfl_list[[2015]]
nfl_2016 <- nfl_list[[2016]]
nfl_2017 <- nfl_list[[2017]]
nfl_2018 <- nfl_list[[2018]]
nfl_2019 <- nfl_list[[2019]]
nfl_2020 <- nfl_list[[2020]]
nfl_2021 <- nfl_list[[2021]]


#######################2009##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2009$winner))
loses <- as.data.table(table(nfl_2009$loser))

season_2009 <- cbind(wins, loses$N)
colnames(season_2009) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2009$Ties <- 0
x <- na.omit(nfl_2009$away[nfl_2009$tied == 1])
y <- na.omit(nfl_2009$home[nfl_2009$tied == 1])
tied_teams <- c(x,y)

season_2009$Ties[season_2009$Teams == tied_teams] <- 1

# Win percentages
season_2009$win_percent <- season_2009$Wins/(season_2009$Wins+season_2009$Loses+season_2009$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2009$Teams){
  arr1 <- append(arr1, sum(nfl_2009$score_away[which(nfl_2009$away == n)]))
}

for (n in season_2009$Teams){
  arr2 <- append(arr2, sum(nfl_2009$score_home[which(nfl_2009$home == n)]))
}

season_2009$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2009$Teams){
  arr1 <- append(arr1, sum(nfl_2009$rushing_yards_away[which(nfl_2009$away == n)]))
}

for (n in season_2009$Teams){
  arr2 <- append(arr2, sum(nfl_2009$rushing_yards_home[which(nfl_2009$home == n)]))
}

season_2009$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2009$Teams){
  arr1 <- append(arr1, sum(nfl_2009$passing_yards_away[which(nfl_2009$away == n)]))
}

for (n in season_2009$Teams){
  arr2 <- append(arr2, sum(nfl_2009$passing_yards_home[which(nfl_2009$home == n)]))
}

season_2009$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2009$total_yards <- season_2009$total_rushing_yards + season_2009$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2009$Teams){
  arr1 <- append(arr1, sum(nfl_2009$turnovers_away[which(nfl_2009$away == n)]))
}

for (n in season_2009$Teams){
  arr2 <- append(arr2, sum(nfl_2009$turnovers_home[which(nfl_2009$home == n)]))
}

season_2009$total_turnovers <- mapply("+", arr1, arr2)

#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2009$Teams){
  arr1 <- append(arr1, sum(nfl_2009$def_st_td_away[which(nfl_2009$away == n)]))
}

for (n in season_2009$Teams){
  arr2 <- append(arr2, sum(nfl_2009$def_st_td_home[which(nfl_2009$home == n)]))
}

season_2009$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2009$Teams){
  arr1 <- append(arr1, sum(nfl_2009$first_downs_away[which(nfl_2009$away == n)]))
}

for (n in season_2009$Teams){
  arr2 <- append(arr2, sum(nfl_2009$first_downs_home[which(nfl_2009$home == n)]))
}

season_2009$first_downs <- mapply("+", arr1, arr2)

#######################2010##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2010$winner))
loses <- as.data.table(table(nfl_2010$loser))

season_2010 <- cbind(wins, loses$N)
colnames(season_2010) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2010$Ties <- 0
x <- na.omit(nfl_2010$away[nfl_2010$tied == 1])
y <- na.omit(nfl_2010$home[nfl_2010$tied == 1])
tied_teams <- c(x,y)

season_2010$Ties[season_2010$Teams == tied_teams] <- 1

# Win percentages
season_2010$win_percent <- season_2010$Wins/(season_2010$Wins+season_2010$Loses+season_2010$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2010$Teams){
  arr1 <- append(arr1, sum(nfl_2010$score_away[which(nfl_2010$away == n)]))
}

for (n in season_2010$Teams){
  arr2 <- append(arr2, sum(nfl_2010$score_home[which(nfl_2010$home == n)]))
}

season_2010$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2010$Teams){
  arr1 <- append(arr1, sum(nfl_2010$rushing_yards_away[which(nfl_2010$away == n)]))
}

for (n in season_2010$Teams){
  arr2 <- append(arr2, sum(nfl_2010$rushing_yards_home[which(nfl_2010$home == n)]))
}

season_2010$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2010$Teams){
  arr1 <- append(arr1, sum(nfl_2010$passing_yards_away[which(nfl_2010$away == n)]))
}

for (n in season_2010$Teams){
  arr2 <- append(arr2, sum(nfl_2010$passing_yards_home[which(nfl_2010$home == n)]))
}

season_2010$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2010$total_yards <- season_2010$total_rushing_yards + season_2010$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2010$Teams){
  arr1 <- append(arr1, sum(nfl_2010$turnovers_away[which(nfl_2010$away == n)]))
}

for (n in season_2010$Teams){
  arr2 <- append(arr2, sum(nfl_2010$turnovers_home[which(nfl_2010$home == n)]))
}

season_2010$total_turnovers <- mapply("+", arr1, arr2)


#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2010$Teams){
  arr1 <- append(arr1, sum(nfl_2010$def_st_td_away[which(nfl_2010$away == n)]))
}

for (n in season_2010$Teams){
  arr2 <- append(arr2, sum(nfl_2010$def_st_td_home[which(nfl_2010$home == n)]))
}

season_2010$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2010$Teams){
  arr1 <- append(arr1, sum(nfl_2010$first_downs_away[which(nfl_2010$away == n)]))
}

for (n in season_2010$Teams){
  arr2 <- append(arr2, sum(nfl_2010$first_downs_home[which(nfl_2010$home == n)]))
}

season_2010$first_downs <- mapply("+", arr1, arr2)

#######################2011##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2011$winner))
loses <- as.data.table(table(nfl_2011$loser))

season_2011 <- cbind(wins, loses$N)
colnames(season_2011) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2011$Ties <- 0
x <- na.omit(nfl_2011$away[nfl_2011$tied == 1])
y <- na.omit(nfl_2011$home[nfl_2011$tied == 1])
tied_teams <- c(x,y)

season_2011$Ties[season_2011$Teams == tied_teams] <- 1

# Win percentages
season_2011$win_percent <- season_2011$Wins/(season_2011$Wins+season_2011$Loses+season_2011$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2011$Teams){
  arr1 <- append(arr1, sum(nfl_2011$score_away[which(nfl_2011$away == n)]))
}

for (n in season_2011$Teams){
  arr2 <- append(arr2, sum(nfl_2011$score_home[which(nfl_2011$home == n)]))
}

season_2011$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2011$Teams){
  arr1 <- append(arr1, sum(nfl_2011$rushing_yards_away[which(nfl_2011$away == n)]))
}

for (n in season_2011$Teams){
  arr2 <- append(arr2, sum(nfl_2011$rushing_yards_home[which(nfl_2011$home == n)]))
}

season_2011$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2011$Teams){
  arr1 <- append(arr1, sum(nfl_2011$passing_yards_away[which(nfl_2011$away == n)]))
}

for (n in season_2011$Teams){
  arr2 <- append(arr2, sum(nfl_2011$passing_yards_home[which(nfl_2011$home == n)]))
}

season_2011$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2011$total_yards <- season_2011$total_rushing_yards + season_2011$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2011$Teams){
  arr1 <- append(arr1, sum(nfl_2011$turnovers_away[which(nfl_2011$away == n)]))
}

for (n in season_2011$Teams){
  arr2 <- append(arr2, sum(nfl_2011$turnovers_home[which(nfl_2011$home == n)]))
}

season_2011$total_turnovers <- mapply("+", arr1, arr2)


#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2011$Teams){
  arr1 <- append(arr1, sum(nfl_2011$def_st_td_away[which(nfl_2011$away == n)]))
}

for (n in season_2011$Teams){
  arr2 <- append(arr2, sum(nfl_2011$def_st_td_home[which(nfl_2011$home == n)]))
}

season_2011$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2011$Teams){
  arr1 <- append(arr1, sum(nfl_2011$first_downs_away[which(nfl_2011$away == n)]))
}

for (n in season_2011$Teams){
  arr2 <- append(arr2, sum(nfl_2011$first_downs_home[which(nfl_2011$home == n)]))
}

season_2011$first_downs <- mapply("+", arr1, arr2)

#######################2012##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2012$winner))
loses <- as.data.table(table(nfl_2012$loser))

season_2012 <- cbind(wins, loses$N)
colnames(season_2012) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2012$Ties <- 0
x <- na.omit(nfl_2012$away[nfl_2012$tied == 1])
y <- na.omit(nfl_2012$home[nfl_2012$tied == 1])
tied_teams <- c(x,y)

season_2012$Ties[season_2012$Teams == tied_teams] <- 1

# Win percentages
season_2012$win_percent <- season_2012$Wins/(season_2012$Wins+season_2012$Loses+season_2012$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2012$Teams){
  arr1 <- append(arr1, sum(nfl_2012$score_away[which(nfl_2012$away == n)]))
}

for (n in season_2012$Teams){
  arr2 <- append(arr2, sum(nfl_2012$score_home[which(nfl_2012$home == n)]))
}

season_2012$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2012$Teams){
  arr1 <- append(arr1, sum(nfl_2012$rushing_yards_away[which(nfl_2012$away == n)]))
}

for (n in season_2012$Teams){
  arr2 <- append(arr2, sum(nfl_2012$rushing_yards_home[which(nfl_2012$home == n)]))
}

season_2012$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2012$Teams){
  arr1 <- append(arr1, sum(nfl_2012$passing_yards_away[which(nfl_2012$away == n)]))
}

for (n in season_2012$Teams){
  arr2 <- append(arr2, sum(nfl_2012$passing_yards_home[which(nfl_2012$home == n)]))
}

season_2012$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2012$total_yards <- season_2012$total_rushing_yards + season_2012$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2012$Teams){
  arr1 <- append(arr1, sum(nfl_2012$turnovers_away[which(nfl_2012$away == n)]))
}

for (n in season_2012$Teams){
  arr2 <- append(arr2, sum(nfl_2012$turnovers_home[which(nfl_2012$home == n)]))
}

season_2012$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2012$Teams){
  arr1 <- append(arr1, sum(nfl_2012$def_st_td_away[which(nfl_2012$away == n)]))
}

for (n in season_2012$Teams){
  arr2 <- append(arr2, sum(nfl_2012$def_st_td_home[which(nfl_2012$home == n)]))
}

season_2012$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2012$Teams){
  arr1 <- append(arr1, sum(nfl_2012$first_downs_away[which(nfl_2012$away == n)]))
}

for (n in season_2012$Teams){
  arr2 <- append(arr2, sum(nfl_2012$first_downs_home[which(nfl_2012$home == n)]))
}

season_2012$first_downs <- mapply("+", arr1, arr2)


#######################2013##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2013$winner))
loses <- as.data.table(table(nfl_2013$loser))

season_2013 <- cbind(wins, loses$N)
colnames(season_2013) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2013$Ties <- 0
x <- na.omit(nfl_2013$away[nfl_2013$tied == 1])
y <- na.omit(nfl_2013$home[nfl_2013$tied == 1])
tied_teams <- c(x,y)

season_2013$Ties[season_2013$Teams == tied_teams] <- 1

# Win percentages
season_2013$win_percent <- season_2013$Wins/(season_2013$Wins+season_2013$Loses+season_2013$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2013$Teams){
  arr1 <- append(arr1, sum(nfl_2013$score_away[which(nfl_2013$away == n)]))
}

for (n in season_2013$Teams){
  arr2 <- append(arr2, sum(nfl_2013$score_home[which(nfl_2013$home == n)]))
}

season_2013$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2013$Teams){
  arr1 <- append(arr1, sum(nfl_2013$rushing_yards_away[which(nfl_2013$away == n)]))
}

for (n in season_2013$Teams){
  arr2 <- append(arr2, sum(nfl_2013$rushing_yards_home[which(nfl_2013$home == n)]))
}

season_2013$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2013$Teams){
  arr1 <- append(arr1, sum(nfl_2013$passing_yards_away[which(nfl_2013$away == n)]))
}

for (n in season_2013$Teams){
  arr2 <- append(arr2, sum(nfl_2013$passing_yards_home[which(nfl_2013$home == n)]))
}

season_2013$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2013$total_yards <- season_2013$total_rushing_yards + season_2013$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2013$Teams){
  arr1 <- append(arr1, sum(nfl_2013$turnovers_away[which(nfl_2013$away == n)]))
}

for (n in season_2013$Teams){
  arr2 <- append(arr2, sum(nfl_2013$turnovers_home[which(nfl_2013$home == n)]))
}

season_2013$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2013$Teams){
  arr1 <- append(arr1, sum(nfl_2013$def_st_td_away[which(nfl_2013$away == n)]))
}

for (n in season_2013$Teams){
  arr2 <- append(arr2, sum(nfl_2013$def_st_td_home[which(nfl_2013$home == n)]))
}

season_2013$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2013$Teams){
  arr1 <- append(arr1, sum(nfl_2013$first_downs_away[which(nfl_2013$away == n)]))
}

for (n in season_2013$Teams){
  arr2 <- append(arr2, sum(nfl_2013$first_downs_home[which(nfl_2013$home == n)]))
}

season_2013$first_downs <- mapply("+", arr1, arr2)


#######################2014##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2014$winner))
loses <- as.data.table(table(nfl_2014$loser))

season_2014 <- cbind(wins, loses$N)
colnames(season_2014) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2014$Ties <- 0
x <- na.omit(nfl_2014$away[nfl_2014$tied == 1])
y <- na.omit(nfl_2014$home[nfl_2014$tied == 1])
tied_teams <- c(x,y)

season_2014$Ties[season_2014$Teams == tied_teams] <- 1

# Win percentages
season_2014$win_percent <- season_2014$Wins/(season_2014$Wins+season_2014$Loses+season_2014$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2014$Teams){
  arr1 <- append(arr1, sum(nfl_2014$score_away[which(nfl_2014$away == n)]))
}

for (n in season_2014$Teams){
  arr2 <- append(arr2, sum(nfl_2014$score_home[which(nfl_2014$home == n)]))
}

season_2014$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2014$Teams){
  arr1 <- append(arr1, sum(nfl_2014$rushing_yards_away[which(nfl_2014$away == n)]))
}

for (n in season_2014$Teams){
  arr2 <- append(arr2, sum(nfl_2014$rushing_yards_home[which(nfl_2014$home == n)]))
}

season_2014$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2014$Teams){
  arr1 <- append(arr1, sum(nfl_2014$passing_yards_away[which(nfl_2014$away == n)]))
}

for (n in season_2014$Teams){
  arr2 <- append(arr2, sum(nfl_2014$passing_yards_home[which(nfl_2014$home == n)]))
}

season_2014$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2014$total_yards <- season_2014$total_rushing_yards + season_2014$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2014$Teams){
  arr1 <- append(arr1, sum(nfl_2014$turnovers_away[which(nfl_2014$away == n)]))
}

for (n in season_2014$Teams){
  arr2 <- append(arr2, sum(nfl_2014$turnovers_home[which(nfl_2014$home == n)]))
}

season_2014$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2014$Teams){
  arr1 <- append(arr1, sum(nfl_2014$def_st_td_away[which(nfl_2014$away == n)]))
}

for (n in season_2014$Teams){
  arr2 <- append(arr2, sum(nfl_2014$def_st_td_home[which(nfl_2014$home == n)]))
}

season_2014$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2014$Teams){
  arr1 <- append(arr1, sum(nfl_2014$first_downs_away[which(nfl_2014$away == n)]))
}

for (n in season_2014$Teams){
  arr2 <- append(arr2, sum(nfl_2014$first_downs_home[which(nfl_2014$home == n)]))
}

season_2014$first_downs <- mapply("+", arr1, arr2)


#######################2015##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2015$winner))
loses <- as.data.table(table(nfl_2015$loser))

season_2015 <- cbind(wins, loses$N)
colnames(season_2015) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2015$Ties <- 0
x <- na.omit(nfl_2015$away[nfl_2015$tied == 1])
y <- na.omit(nfl_2015$home[nfl_2015$tied == 1])
tied_teams <- c(x,y)

season_2015$Ties[season_2015$Teams == tied_teams] <- 1

# Win percentages
season_2015$win_percent <- season_2015$Wins/(season_2015$Wins+season_2015$Loses+season_2015$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2015$Teams){
  arr1 <- append(arr1, sum(nfl_2015$score_away[which(nfl_2015$away == n)]))
}

for (n in season_2015$Teams){
  arr2 <- append(arr2, sum(nfl_2015$score_home[which(nfl_2015$home == n)]))
}

season_2015$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2015$Teams){
  arr1 <- append(arr1, sum(nfl_2015$rushing_yards_away[which(nfl_2015$away == n)]))
}

for (n in season_2015$Teams){
  arr2 <- append(arr2, sum(nfl_2015$rushing_yards_home[which(nfl_2015$home == n)]))
}

season_2015$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2015$Teams){
  arr1 <- append(arr1, sum(nfl_2015$passing_yards_away[which(nfl_2015$away == n)]))
}

for (n in season_2015$Teams){
  arr2 <- append(arr2, sum(nfl_2015$passing_yards_home[which(nfl_2015$home == n)]))
}

season_2015$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2015$total_yards <- season_2015$total_rushing_yards + season_2015$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2015$Teams){
  arr1 <- append(arr1, sum(nfl_2015$turnovers_away[which(nfl_2015$away == n)]))
}

for (n in season_2015$Teams){
  arr2 <- append(arr2, sum(nfl_2015$turnovers_home[which(nfl_2015$home == n)]))
}

season_2015$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2015$Teams){
  arr1 <- append(arr1, sum(nfl_2015$def_st_td_away[which(nfl_2015$away == n)]))
}

for (n in season_2015$Teams){
  arr2 <- append(arr2, sum(nfl_2015$def_st_td_home[which(nfl_2015$home == n)]))
}

season_2015$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2015$Teams){
  arr1 <- append(arr1, sum(nfl_2015$first_downs_away[which(nfl_2015$away == n)]))
}

for (n in season_2015$Teams){
  arr2 <- append(arr2, sum(nfl_2015$first_downs_home[which(nfl_2015$home == n)]))
}

season_2015$first_downs <- mapply("+", arr1, arr2)

#######################2016##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2016$winner))
loses <- as.data.table(table(nfl_2016$loser))

season_2016 <- cbind(wins, loses$N)
colnames(season_2016) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2016$Ties <- 0
x <- na.omit(nfl_2016$away[nfl_2016$tied == 1])
y <- na.omit(nfl_2016$home[nfl_2016$tied == 1])
tied_teams <- c(x,y)

season_2016$Ties[season_2016$Teams == tied_teams] <- 1

# Win percentages
season_2016$win_percent <- season_2016$Wins/(season_2016$Wins+season_2016$Loses+season_2016$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2016$Teams){
  arr1 <- append(arr1, sum(nfl_2016$score_away[which(nfl_2016$away == n)]))
}

for (n in season_2016$Teams){
  arr2 <- append(arr2, sum(nfl_2016$score_home[which(nfl_2016$home == n)]))
}

season_2016$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2016$Teams){
  arr1 <- append(arr1, sum(nfl_2016$rushing_yards_away[which(nfl_2016$away == n)]))
}

for (n in season_2016$Teams){
  arr2 <- append(arr2, sum(nfl_2016$rushing_yards_home[which(nfl_2016$home == n)]))
}

season_2016$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2016$Teams){
  arr1 <- append(arr1, sum(nfl_2016$passing_yards_away[which(nfl_2016$away == n)]))
}

for (n in season_2016$Teams){
  arr2 <- append(arr2, sum(nfl_2016$passing_yards_home[which(nfl_2016$home == n)]))
}

season_2016$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2016$total_yards <- season_2016$total_rushing_yards + season_2016$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2016$Teams){
  arr1 <- append(arr1, sum(nfl_2016$turnovers_away[which(nfl_2016$away == n)]))
}

for (n in season_2016$Teams){
  arr2 <- append(arr2, sum(nfl_2016$turnovers_home[which(nfl_2016$home == n)]))
}

season_2016$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2016$Teams){
  arr1 <- append(arr1, sum(nfl_2016$def_st_td_away[which(nfl_2016$away == n)]))
}

for (n in season_2016$Teams){
  arr2 <- append(arr2, sum(nfl_2016$def_st_td_home[which(nfl_2016$home == n)]))
}

season_2016$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2016$Teams){
  arr1 <- append(arr1, sum(nfl_2016$first_downs_away[which(nfl_2016$away == n)]))
}

for (n in season_2016$Teams){
  arr2 <- append(arr2, sum(nfl_2016$first_downs_home[which(nfl_2016$home == n)]))
}

season_2016$first_downs <- mapply("+", arr1, arr2)


#######################2017##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2017$winner))
loses <- as.data.table(table(nfl_2017$loser))

season_2017 <- cbind(wins, loses$N)
colnames(season_2017) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2017$Ties <- 0
x <- na.omit(nfl_2017$away[nfl_2017$tied == 1])
y <- na.omit(nfl_2017$home[nfl_2017$tied == 1])
tied_teams <- c(x,y)

season_2017$Ties[season_2017$Teams == tied_teams] <- 1

# Win percentages
season_2017$win_percent <- season_2017$Wins/(season_2017$Wins+season_2017$Loses+season_2017$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2017$Teams){
  arr1 <- append(arr1, sum(nfl_2017$score_away[which(nfl_2017$away == n)]))
}

for (n in season_2017$Teams){
  arr2 <- append(arr2, sum(nfl_2017$score_home[which(nfl_2017$home == n)]))
}

season_2017$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2017$Teams){
  arr1 <- append(arr1, sum(nfl_2017$rushing_yards_away[which(nfl_2017$away == n)]))
}

for (n in season_2017$Teams){
  arr2 <- append(arr2, sum(nfl_2017$rushing_yards_home[which(nfl_2017$home == n)]))
}

season_2017$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2017$Teams){
  arr1 <- append(arr1, sum(nfl_2017$passing_yards_away[which(nfl_2017$away == n)]))
}

for (n in season_2017$Teams){
  arr2 <- append(arr2, sum(nfl_2017$passing_yards_home[which(nfl_2017$home == n)]))
}

season_2017$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2017$total_yards <- season_2017$total_rushing_yards + season_2017$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2017$Teams){
  arr1 <- append(arr1, sum(nfl_2017$turnovers_away[which(nfl_2017$away == n)]))
}

for (n in season_2017$Teams){
  arr2 <- append(arr2, sum(nfl_2017$turnovers_home[which(nfl_2017$home == n)]))
}

season_2017$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2017$Teams){
  arr1 <- append(arr1, sum(nfl_2017$def_st_td_away[which(nfl_2017$away == n)]))
}

for (n in season_2017$Teams){
  arr2 <- append(arr2, sum(nfl_2017$def_st_td_home[which(nfl_2017$home == n)]))
}

season_2017$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2017$Teams){
  arr1 <- append(arr1, sum(nfl_2017$first_downs_away[which(nfl_2017$away == n)]))
}

for (n in season_2017$Teams){
  arr2 <- append(arr2, sum(nfl_2017$first_downs_home[which(nfl_2017$home == n)]))
}

season_2017$first_downs <- mapply("+", arr1, arr2)


#######################2018##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2018$winner))
loses <- as.data.table(table(nfl_2018$loser))

season_2018 <- cbind(wins, loses$N)
colnames(season_2018) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2018$Ties <- 0
x <- na.omit(nfl_2018$away[nfl_2018$tied == 1])
y <- na.omit(nfl_2018$home[nfl_2018$tied == 1])
tied_teams <- c(x,y)

season_2018$Ties[season_2018$Teams == tied_teams] <- 1

# Win percentages
season_2018$win_percent <- season_2018$Wins/(season_2018$Wins+season_2018$Loses+season_2018$Ties)

#Adding total points column 
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$score_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$score_home[which(nfl_2018$home == n)]))
}

season_2018$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$rushing_yards_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$rushing_yards_home[which(nfl_2018$home == n)]))
}

season_2018$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$passing_yards_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$passing_yards_home[which(nfl_2018$home == n)]))
}

season_2018$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column
season_2018$total_yards <- season_2018$total_rushing_yards + season_2018$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$turnovers_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$turnovers_home[which(nfl_2018$home == n)]))
}

season_2018$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$def_st_td_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$def_st_td_home[which(nfl_2018$home == n)]))
}

season_2018$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2018$Teams){
  arr1 <- append(arr1, sum(nfl_2018$first_downs_away[which(nfl_2018$away == n)]))
}

for (n in season_2018$Teams){
  arr2 <- append(arr2, sum(nfl_2018$first_downs_home[which(nfl_2018$home == n)]))
}

season_2018$first_downs <- mapply("+", arr1, arr2)


######################2019##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2019$winner))
loses <- as.data.table(table(nfl_2019$loser))

season_2019 <- cbind(wins, loses$N)
colnames(season_2019) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2019$Ties <- 0
x <- na.omit(nfl_2019$away[nfl_2019$tied == 1])
y <- na.omit(nfl_2019$home[nfl_2019$tied == 1])
tied_teams <- c(x,y)

season_2019$Ties[season_2019$Teams == tied_teams] <- 1

season_2019$win_percent <- season_2019$Wins/(season_2019$Wins+season_2019$Loses+season_2019$Ties)

arr1 <- c()
arr2 <- c()

# Calculating team records
for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$score_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$score_home[which(nfl_2019$home == n)]))
}

season_2019$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$rushing_yards_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$rushing_yards_home[which(nfl_2019$home == n)]))
}

season_2019$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$passing_yards_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$passing_yards_home[which(nfl_2019$home == n)]))
}

season_2019$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2019$total_yards <- season_2019$total_rushing_yards + season_2019$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$turnovers_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$turnovers_home[which(nfl_2019$home == n)]))
}

season_2019$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$def_st_td_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$def_st_td_home[which(nfl_2019$home == n)]))
}

season_2019$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2019$Teams){
  arr1 <- append(arr1, sum(nfl_2019$first_downs_away[which(nfl_2019$away == n)]))
}

for (n in season_2019$Teams){
  arr2 <- append(arr2, sum(nfl_2019$first_downs_home[which(nfl_2019$home == n)]))
}

season_2019$first_downs <- mapply("+", arr1, arr2)

######################2020##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2020$winner))
loses <- as.data.table(table(nfl_2020$loser))

season_2020 <- cbind(wins, loses$N)
colnames(season_2020) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2020$Ties <- 0
x <- na.omit(nfl_2020$away[nfl_2020$tied == 1])
y <- na.omit(nfl_2020$home[nfl_2020$tied == 1])
tied_teams <- c(x,y)

season_2020$Ties[season_2020$Teams == tied_teams] <- 1

season_2020$win_percent <- season_2020$Wins/(season_2020$Wins+season_2020$Loses+season_2020$Ties)

arr1 <- c()
arr2 <- c()

# Calculating team records
for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$score_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$score_home[which(nfl_2020$home == n)]))
}

season_2020$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$rushing_yards_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$rushing_yards_home[which(nfl_2020$home == n)]))
}

season_2020$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$passing_yards_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$passing_yards_home[which(nfl_2020$home == n)]))
}

season_2020$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2020$total_yards <- season_2020$total_rushing_yards + season_2020$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$turnovers_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$turnovers_home[which(nfl_2020$home == n)]))
}

season_2020$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$def_st_td_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$def_st_td_home[which(nfl_2020$home == n)]))
}

season_2020$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2020$Teams){
  arr1 <- append(arr1, sum(nfl_2020$first_downs_away[which(nfl_2020$away == n)]))
}

for (n in season_2020$Teams){
  arr2 <- append(arr2, sum(nfl_2020$first_downs_home[which(nfl_2020$home == n)]))
}

season_2020$first_downs <- mapply("+", arr1, arr2)

######################2021##########################
#Adding total wins and loses for each team
wins <- as.data.table(table(nfl_2021$winner))
loses <- as.data.table(table(nfl_2021$loser))

season_2021 <- cbind(wins, loses$N)
colnames(season_2021) <- c('Teams', 'Wins', 'Loses')

#adding in any ties during the season
season_2021$Ties <- 0
x <- na.omit(nfl_2021$away[nfl_2021$tied == 1])
y <- na.omit(nfl_2021$home[nfl_2021$tied == 1])
tied_teams <- c(x,y)

season_2021$Ties[season_2021$Teams == tied_teams] <- 1

season_2021$win_percent <- season_2021$Wins/(season_2021$Wins+season_2021$Loses+season_2021$Ties)

arr1 <- c()
arr2 <- c()

# Calculating team records
for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$score_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$score_home[which(nfl_2021$home == n)]))
}

season_2021$total_points <- mapply("+", arr1, arr2)

#Adding total rushing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$rushing_yards_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$rushing_yards_home[which(nfl_2021$home == n)]))
}

season_2021$total_rushing_yards <- mapply("+", arr1, arr2)

#Adding total passing yards column
arr1 <- c()
arr2 <- c()

for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$passing_yards_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$passing_yards_home[which(nfl_2021$home == n)]))
}

season_2021$total_passing_yards <- mapply("+", arr1, arr2)

#Adding total yards column

season_2021$total_yards <- season_2021$total_rushing_yards + season_2021$total_passing_yards

#Adding total turnovers column 
arr1 <- c()
arr2 <- c()

for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$turnovers_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$turnovers_home[which(nfl_2021$home == n)]))
}

season_2021$total_turnovers <- mapply("+", arr1, arr2)



#Adding total defensive touchdowns stopped column 
arr1 <- c()
arr2 <- c()

for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$def_st_td_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$def_st_td_home[which(nfl_2021$home == n)]))
}

season_2021$def_st_td <- mapply("+", arr1, arr2)

#Adding total first downs column 
arr1 <- c()
arr2 <- c()

for (n in season_2021$Teams){
  arr1 <- append(arr1, sum(nfl_2021$first_downs_away[which(nfl_2021$away == n)]))
}

for (n in season_2021$Teams){
  arr2 <- append(arr2, sum(nfl_2021$first_downs_home[which(nfl_2021$home == n)]))
}

season_2021$first_downs <- mapply("+", arr1, arr2)


#####################################
fwrite(season_2009, "season_2009.csv")
fwrite(season_2010, "season_2010.csv")
fwrite(season_2011, "season_2011.csv")
fwrite(season_2012, "season_2012.csv")
fwrite(season_2013, "season_2013.csv")
fwrite(season_2014, "season_2014.csv")
fwrite(season_2015, "season_2015.csv")
fwrite(season_2016, "season_2016.csv")
fwrite(season_2017, "season_2017.csv")
fwrite(season_2018, "season_2018.csv")
fwrite(season_2019, "season_2019.csv")
fwrite(season_2020, "season_2020.csv")
fwrite(season_2021, "season_2021.csv")

