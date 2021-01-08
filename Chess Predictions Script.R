# This is the raw script of my Chess Predictions project

# Downloading the dataset from my github, since downloading it from kaggle requires an account or you will get a "Too much traffic" error
if(!require(readr)) install.packages("readr")
library(readr)

# Downloading the dataset from my github, since downloading it from kaggle requires an account or you will get a "Too much traffic" error
myfile <- 'https://raw.githubusercontent.com/jojoha1337/EDX-Chess-Predictions/main/games.csv'

dat<-read_csv(url(myfile))

# ranger is what we will be using to generate our models
if (!require('ranger')) install.packages('ranger'); library('ranger')
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# removing the draw class from the winner status
dat <- dat[dat$winner != 'draw',]

# looking at the winrates by opening_names
opening_name_winrates <- dat %>% group_by(opening_name) %>%
  summarise(white = mean(winner == 'white'),
            black = mean(winner == 'black'))


opening_name_winrates

# looking at the winrates by opening_eco
opening_eco_winrates <- dat %>% group_by(opening_eco) %>%
  summarise(white = mean(winner == 'white'),
            black = mean(winner == 'black'))

opening_eco_winrates

# Splitting the data into training and testing sets
ind <- createDataPartition(dat$winner, times = 1, p = 0.2, list = FALSE)

train_set <- dat[-ind,]
test_set <- dat[ind,]

# Explanation: If white's winrate is higher than black's, then predict white, else predict black
winrates_names <- ifelse(opening_name_winrates['white'] > opening_name_winrates['black'], 'white', 'black')

winrates_eco <- ifelse(opening_eco_winrates['white'] > opening_eco_winrates['black'], 'white', 'black')

# Creating the dataframes
wr_name_table <- data.frame(opening_name = opening_name_winrates$opening_name, winrate = winrates_names)

wr_eco_table <- data.frame(opening_eco = opening_eco_winrates$opening_eco, winrate = winrates_eco)

# renaming the second column, for some reason it is named "white", renaming it to something else.
names(wr_name_table)[names(wr_name_table) == "white"] <- "Side with Highest Winrate"
names(wr_eco_table)[names(wr_eco_table) == "white"] <- "Side with Highest Winrate"

# Explanation: For each row in test set, look at it's opening_name/eco column, if the opening_name/eco column is in the wr_name/eco_tables
# then return the element of wr_name/eco_table$`Side with Highest Winrate` else return an error (to warn that something is wrong about the code)

predictions <- ifelse(test_set$opening_name %in% wr_name_table$opening_name, wr_name_table$`Side with Highest Winrate`, 'error')
predictions_eco <- ifelse(test_set$opening_eco %in% wr_eco_table$opening_eco, wr_eco_table$`Side with Highest Winrate`, 'error')

results <- tibble()
results <- bind_rows(results,
                     tibble(Method = 'Guessing by opening_name winrates',
                            Accuracy = mean(predictions == test_set$winner)))
results <- bind_rows(results,
                     tibble(Method = 'Guessing by opening_eco winrates',
                            Accuracy = mean(predictions_eco == test_set$winner)))

results

# creating the difference between white and black rating
test_rating_diffs <- test_set$white_rating - test_set$black_rating

# creating the predictions
# If the rating difference is 0, then do a 50/50 guess, if the rating difference is positive, then predict white, else black
preds <- ifelse(test_rating_diffs == 0, sample(c('white', 'black'),1, replace = TRUE, prob = c(0.5,0.5)), 
                ifelse(test_rating_diffs > 0, 'white', 'black'))

# appending the results to the accuracy table
results <- bind_rows(results,
                     tibble(Method = 'Guessing by rating difference',
                            Accuracy = mean(preds == test_set$winner)))

results

# creating the rating diff variable and viewing the selected columns
dat %>% mutate(total_game_time = dat$last_move_at - dat$created_at) %>%
  select(total_game_time, winner, increment_code, turns, moves)

# We cannot trust these 'time' based columns, we will have to remove them.
# Also removing the id of the game and both of the player id's, those are clearly not useful for prediction and removing the
#'moves' column, because viewing how the game ended by looking at the moves is not really how we want our model to predict.
#And removing the increment_code, opening_eco and opening name columns, for some reason, when attempting
#a random forest with these variables present, the model takes way too long to train. 

dat <- subset(dat, select = -c(created_at, last_move_at, id, white_id, black_id, moves,increment_code, opening_eco, opening_name))


# adding the rating_difference variable, because it showed that it was a powerful predictor on the Analysis 
dat <- dat %>% mutate(rating_difference = dat$white_rating - dat$black_rating)

# Splitting the data into training and testing sets
ind <- createDataPartition(dat$winner, times = 1, p = 0.3, list = FALSE)

train_set <- dat[-ind,]
temp <- dat[ind,]

ind_2 <- createDataPartition(temp$winner, times = 1, p = 0.5, list = FALSE)

test_set <- temp[-ind_2,]
validation <- temp[ind_2,]

#  Creating the Decision Tree model and binding it's score to the results table
dtreet <- train(winner ~ ., data = train_set, method = 'rpart')

preds <- predict(dtreet, test_set)

results <- bind_rows(results,
                     tibble(Method = 'Decision Tree',
                            Accuracy = mean(preds == test_set$winner)))

rf <- train(winner ~ ., data = train_set, method = 'ranger')

preds <- predict(rf, test_set)

results <- bind_rows(results,
                     tibble(Method = 'Random Forest',
                            Accuracy = mean(preds == test_set$winner)))

results