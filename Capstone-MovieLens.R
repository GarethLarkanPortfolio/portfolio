## Gareth Larkan
## South Africa
## HarvardX : Capstone Project

#### INTRODUCTION ####

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

#### DATASET ####

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#Load the libraries
library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(dplyr)


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

#######################################################################################################

#### ANALYSIS ####

head(edx)

#General rating distribution
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating distribution") +
  theme_update()

#Number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "white") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie") +
  theme_update()

# Plot number of ratings given by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "white") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users") + 
  theme_update()

###########################################################################################################

#Extracting the premier date and then adding it to the table
premier <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric()
edx_with_title_dates <- edx %>% mutate(premier_date = premier)

#There might be errors, therefore we need to check for premier dates above 2020 and below a chosen data
edx_with_title_dates %>% filter(premier_date > 2018) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

#Based on trial and error, i chose date 1910
edx_with_title_dates %>% filter(premier_date < 1910) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

##16 dates are wrong, they need to be corrected
edx_with_title_dates[edx_with_title_dates$movieId == "27266", "premier_date"] <- 2004
edx_with_title_dates[edx_with_title_dates$movieId == "671", "premier_date"] <- 1996
edx_with_title_dates[edx_with_title_dates$movieId == "2308", "premier_date"] <- 1973
edx_with_title_dates[edx_with_title_dates$movieId == "4159", "premier_date"] <- 2001
edx_with_title_dates[edx_with_title_dates$movieId == "5310", "premier_date"] <- 1985
edx_with_title_dates[edx_with_title_dates$movieId == "8864", "premier_date"] <- 2004
edx_with_title_dates[edx_with_title_dates$movieId == "1422", "premier_date"] <- 1997
edx_with_title_dates[edx_with_title_dates$movieId == "4311", "premier_date"] <- 1998
edx_with_title_dates[edx_with_title_dates$movieId == "5472", "premier_date"] <- 1972
edx_with_title_dates[edx_with_title_dates$movieId == "6290", "premier_date"] <- 2003
edx_with_title_dates[edx_with_title_dates$movieId == "6645", "premier_date"] <- 1971
edx_with_title_dates[edx_with_title_dates$movieId == "8198", "premier_date"] <- 1960
edx_with_title_dates[edx_with_title_dates$movieId == "8905", "premier_date"] <- 1992
edx_with_title_dates[edx_with_title_dates$movieId == "53953", "premier_date"] <- 2007
edx_with_title_dates[edx_with_title_dates$movieId == "2691", "premier_date"] <- 1998
edx_with_title_dates[edx_with_title_dates$movieId == "26359", "premier_date"] <- 1976

#Working out the age of the movie
edx_with_title_dates <- edx_with_title_dates %>% mutate(age_of_movie = 2020 - premier_date)
head(edx_with_title_dates)

#Is there in correlation between the age and the rating
age_avgs <- edx_with_title_dates %>% group_by(age_of_movie) %>% summarize(avg_rating_by_age = mean(rating))
head(age_avgs)

#It can be difficult to see from the table, rather graph the results
age_avgs %>%
  ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point() +
  ggtitle("Age of Movie vs Average Movie Rating")

#Does genre have an effect on rating?
dat <- edx_with_title_dates %>% separate_rows(genres, sep ="\\|")

genre_avgs <- edx %>% group_by(genres) %>% summarize(avg_rating_by_genre = mean(rating))
head(genre_avgs)

temp <- dat %>%
  group_by(genres) %>%
  summarize(mean_rating_by_genre=mean(rating)) %>%
  arrange(-mean_rating_by_genre)

temp %>%
  ggplot(aes(reorder(genres, mean_rating_by_genre), mean_rating_by_genre, fill= mean_rating_by_genre)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "spectral") + labs(y = "Mean Rating", x = "Genre") +
  ggtitle("Average Rating of Genres") +
  theme_update()

temp <- dat %>%
  group_by(genres) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n/sumN) %>%
  arrange(-percentage)

temp %>%
  ggplot(aes(reorder(genres, percentage), percentage, fill= percentage)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "spectral") + labs(y = "Percentage", x = "Genre") +
  ggtitle("Distribution of Genres by Percent Rated")

#We can see film-Noir and Documentary have the highest average ratings, however, this could be scewed
#based on how many ratings they have, maybe genre's with fewer ratings are rated higher

#Is there correlation between the users and the ratings?
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "white") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_update()

#######################################################################################################

####MODELLING####

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Basic average rating
mu <-mean(edx$rating)
rmse <- RMSE(validation$rating, mu)

#Simple movie bias 
simple_bias <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu +  validation %>%
  left_join(simple_bias, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, validation$rating)

#User bias
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, validation$rating)

lambdas <- seq(0, 10, 0.25)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time  
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, rmses) 

lambda <- lambdas[which.min(rmses)]
lambda

#Getting the RMSE for the optimal lambda
rmses <- sapply(lambda, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

rmses
