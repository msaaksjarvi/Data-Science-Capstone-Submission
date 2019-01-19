#############################################################
# Create edx set, validation set, and submission file
#############################################################

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

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

library(caret)
library(dplyr)
library(ggplot2)
#check the number of rows in the edx file
nrow(edx)
#check the number of columns in the edx file
ncol(edx)
#check the number of distinct movies rated 
n_distinct(edx$movieId)
#check the number of distinct users that rated the movies 
n_distinct(edx$userId)
#plot the distribution of movies
a <- ggplot(edx, aes(x = movieId))
a + geom_histogram(bins = 30, color = "black", fill = "gray")
#plot the disribution of users
b <- ggplot(edx, aes(x = userId))
b + geom_histogram(bins = 30, color = "black", fill = "gray")
#convert rating and userId to numeric variables
edx$rating <- as.numeric(edx$rating)
edx$userId <- as.numeric(edx$userId)
#create a function that computes the rmse for a vector of ratings and their corresponding predictors
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#simplest possible model: average rating of all movies across all users
mu_hat <- mean(edx$rating)
mu_hat
#compute the rmse of the simplest possible model on the test set data
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse
#start optimizing model: include movie bias 
mu <- mean(edx$rating)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
#compare prediction in training set with validation set
predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by="movieId") %>%
  .$b_i
#calculate rmse for model
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
model_1_rmse
#continue optimizing model by including user bias
user_avgs <- validation %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#compare prediction in training set with validation set
predicted_ratings <- validation %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
#calculate rmse for model
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse








