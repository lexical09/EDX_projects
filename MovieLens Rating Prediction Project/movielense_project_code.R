#########################################################################################
#MOVIELENS PROJECT CODE
#########################################################################################
# Create edx set, validation set, and submission file
#########################################################################################
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens_10M <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens_10M$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens_10M[-test_index,]
temp <- movielens_10M[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#For checking
write.csv(validation,"checking.csv", na = "", row.names=FALSE)

# Learners will develop their algorithms on the edx set
# For grading, learners will run algorithm on validation set to generate ratings
validation <- validation %>% select(-rating)

# Ratings will go into the CSV submission file below:
#write.csv(validation %>% select(userId, movieId) %>% mutate(rating = NA),"submission.csv", na = "", row.names=FALSE)

#Remove data
rm(dl, ratings, movies, test_index, temp, removed, movielens_10M)

#########################################################################################
#Import packages
#########################################################################################
library(caret)
library(purrr)
library(tidyverse)
library(lattice)
library(ggplot2)
library(lubridate)
library(dplyr)

#########################################################################################
# Split edx data into train and test sets
#########################################################################################
set.seed(1)
test_index_edx <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index_edx,] #train set will be 90% of edx dataset
test_set <- edx[test_index_edx,] #test set will be 10% of edx dataset
#Remove unused data
rm(test_index_edx)

# Make sure userId and movieId in test set are also in train set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#########################################################################################
# Define The Residual Mean Squared Error (RMSE)
#########################################################################################
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2)) }

#########################################################################################
# Modeling Movie Effects
#########################################################################################
mu <- mean(train_set$rating) #Average rating

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mean(train_set$rating) ))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
# Calculate RMSE 
model_1_rmse <- RMSE(test_set$rating, predicted_ratings)
rmse_results <- tibble(method = "Movie Effect Model", RMSE = model_1_rmse) #SAVE TO RESULT TABLE

#########################################################################################
# Modeling User Effects
#########################################################################################
user_avgs <- train_set  %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE
model_2_rmse <- RMSE(predicted_ratings,test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                 RMSE = model_2_rmse ))
rmse_results %>% knitr::kable() #Show the result table

#########################################################################################
# FINAL PREDICTION AND SAVE FILE
#########################################################################################
predicted_rating <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

output <- cbind(validation, predicted_rating) #combine validation and predicted_rating
#Save the output as submission.csv without timestamp. title and genres columns
write.csv(output %>% select(-c(timestamp,title,genres)),"submission.csv", na = "", row.names=FALSE)
