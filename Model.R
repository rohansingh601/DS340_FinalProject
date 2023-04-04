library(ggplot2)

data <- fread('./season_2018.csv')
data <- fread('./season_2019.csv')
data <- fread('./season_2020.csv')
data <- fread('./season_2021.csv')

################## 2019 Prediction ###################
season_2018$super_bowl_winner <- 0
season_2018$super_bowl_winner[season_2018$Teams == "Eagles"] <- 1

droped <- season_2018$Teams

drops <- c("Teams")
season_2018 <- season_2018[, !drops, with = FALSE]

mylogit <- glm(formula = super_bowl_winner ~ Wins + Loses + Ties + win_percent + total_points + total_rushing_yards
               + total_passing_yards + total_yards, data = season_2018)

# Predicting the Super Bowl winner
season_2019$super_bowl_winner <- predict(mylogit, newdata = season_2018)

season_2018$Teams <- droped

# Creating plots
ggplot(season_2019, aes(x = reorder(Teams, super_bowl_winner), y = super_bowl_winner)) +
  geom_point(aes(colour = Teams)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none") + 
  xlab("NFL Teams") + 
  ylab("Predicted Winner") + 
  ggtitle("2019 Super Bowl Winner Prediction")
######################################################

################## 2020 Prediction ###################
season_2019$super_bowl_winner <- 0
season_2019$super_bowl_winner[season_2019$Teams == "Patriots"] <- 1

droped <- season_2019$Teams

drops <- c("Teams")
season_2019 <- season_2019[, !drops, with = FALSE]

mylogit <- glm(formula = super_bowl_winner ~ Wins + Loses + Ties + win_percent + total_points + total_rushing_yards
               + total_passing_yards + total_yards, data = season_2019)

# Predicting the Super Bowl winner
season_2020$super_bowl_winner <- predict(mylogit, newdata = season_2019)

season_2019$Teams <- dropped

# Creating plots
ggplot(season_2020, aes(x = reorder(Teams, super_bowl_winner), y = super_bowl_winner)) +
  geom_point(aes(colour = Teams)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none") + 
  xlab("NFL Teams") + 
  ylab("Predicted Winner") + 
  ggtitle("2020 Super Bowl Winner Prediction")

################## 2021 Prediction ###################
season_2020$super_bowl_winner <- 0
season_2020$super_bowl_winner[season_2020$Teams == "Buccaneers"] <- 1

droped <- season_2020$Teams
drops <- c("Teams")
season_2019 <- season_2020[, !drops, with = FALSE]

mylogit <- glm(formula = super_bowl_winner ~ Wins + Loses + Ties + win_percent + total_points + total_rushing_yards
               + total_passing_yards + total_yards, data = season_2020)

#Predicting the Super Bowl winner

season_2021$super_bowl_winner <- predict(mylogit, newdata = season_2020)

season_2020$Teams <- dropped

#Creating plots

ggplot(season_2021, aes(x = reorder(Teams, super_bowl_winner), y = super_bowl_winner)) +
  geom_point(aes(colour = Teams)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none") +
  xlab("NFL Teams") +
  ylab("Predicted Winner") +
  ggtitle("2021 Super Bowl Winner Prediction")

###################################################
