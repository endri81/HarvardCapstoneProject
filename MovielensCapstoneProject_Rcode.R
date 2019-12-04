
# Packages needed (Code provided from edx)

if(!require(kableExtra)) install.packages('kableExtra', 
                                          repos = 'http://cran.us.r-project.org')
if(!require(dataCompareR)) install.packages('dataCompareR', 
                                            repos = 'http://cran.us.r-project.org')
if(!require(tidyverse)) install.packages('tidyverse', 
                                         repos = 'http://cran.us.r-project.org')
if(!require(caret)) install.packages('caret', 
                                     repos = 'http://cran.us.r-project.org')
if(!require(data.table)) install.packages('data.table', 
                                          repos = 'http://cran.us.r-project.org')

# Data download (Code provided from edx)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file('http://files.grouplens.org/datasets/movielens/ml-10m.zip', dl)


# Some arrangements (Code provided from edx)

ratings <- fread(text = gsub('::', '\t', 
readLines(unzip(dl, 'ml-10M100K/ratings.dat'))),
col.names = c('userId', 'movieId', 'rating', 'timestamp'))
movies <- str_split_fixed(readLines(unzip(dl, 'ml-10M100K/movies.dat')), '\\::', 3)
colnames(movies) <- c('movieId', 'title', 'genres')
movies <- as.data.frame(movies) %>% 
mutate(movieId = as.numeric(levels(movieId))[movieId],
title = as.character(title),
genres = as.character(genres))
movielens <- left_join(ratings, movies, by = 'movieId')


# Data partition (Code provided from edx)
# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind='Rounding')
test_index <- createDataPartition(y = movielens$rating, 
times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Final arrengements (Code provided from edx)
# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = 'movieId') %>%
  semi_join(edx, by = 'userId')

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Save our data as R objects
save(edx, file = './data/edx.RData')
save(validation, file = './data/validation.RData')


# Load data
load('./data/edx.RData')
load('./data/validation.RData')


# Check format
class(edx)
class(validation)


# Find out more about the structure of our **edx**:
  
as_tibble(edx) %>%
slice(1:5)


# Find out more about the structure of our **validation**:
as_tibble(validation) %>%
slice(1:5) 

# Print features of both data frames **edx** and **validation**

comp_edx_val <- rCompare(edx, validation)
comp_summ <- summary(comp_edx_val)
comp_summ[c('datasetSummary', 'ncolInAOnly', 'ncolInBOnly', 'ncolCommon', 'rowsInAOnly', 'rowsInBOnly', 'nrowCommon')] 


# Distinct users, movies, genres
dist_col <- edx %>% 
summarize(distinct_users = n_distinct(userId),
            distinct_movies = n_distinct(movieId),
            distinct_genres = n_distinct(genres))
dist_col



## Data Wrangling

tidydf <- function(df){
  df$genres <- as.factor(df$genres) #Convert genres to factor
  df$timestamp <- as.Date(as.POSIXct(df$timestamp, origin='1970-01-01'))
  #Convert timestamp
  names(df)[names(df) == 'timestamp'] <- 'rate_year' # Rename column timestamp to rate_year
  df <- df %>% 
    mutate(title = str_trim(title), rate_year = year(rate_year)) %>%  #Mutate title and rate_year
    extract(title, c('title', 'premier_year'), regex = '(.*)\\s\\((\\d+)\\)', convert = TRUE) 
#Separate title from year
return(df)
}
# Transform our dataframes
edx <- tidydf(edx)
validation <- tidydf(validation)


#Now our data frames look like this:
  
as_tibble(edx)
as_tibble(validation)


# Check edx dataframe for NA values
edx_na <- edx %>%
filter(is.na(title) | is.na(year))
glimpse(edx_na) 

# Check validation dataframe for NA values
validation_na <- validation %>%
filter(is.na(title) | is.na(year))
glimpse(validation_na) 



# Check frequencies of ratings unique values
table_rating <- as.data.frame(table(edx$rating))
colnames(table_rating) <- c('Rating', 'Frequencies')
table_rating


# Frequency plot of the ratings
table_rating %>% ggplot(aes(Rating, Frequencies)) +
  geom_bar(stat = 'identity') +
  labs(x='Ratings', y='Count') +
  ggtitle('Distribution of ratings')


# Top movies by number of views
tmovies <- edx %>% select(title) %>% 
group_by(title) %>% 
summarize(count=n()) %>% 
arrange(desc(count)) 

# Print top_movies
head(tmovies,10)


# Top movies by rating average
rating_avg <- edx %>%
  select(title, rating) %>%
  group_by(title) %>%
  summarise(count = n(), avg = mean(rating), min = min(rating), max = max(rating)) %>%
  arrange(desc(avg))
# Print top_movies
head(rating_avg,10)


# Top movies by rating average
rating_avg_200 <- edx %>%
  select(title, rating) %>%
  group_by(title) %>%
  summarise(count = n(), avg = mean(rating), min = min(rating), max = max(rating)) %>%
  filter(count > 200) %>%  
  arrange(desc(avg))

# Print top_movies
head(rating_avg_200,10)



# Distribution of average ratings

rating_avg_200 %>% 
  ggplot(aes(x= avg, fill = count)) +
  geom_histogram( binwidth = 0.2) +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  labs(x='rating average', y='count') +
  ggtitle('Distribution of average ratings ') 



# We create a copy of existing edx
edx_copy <-edx
# Sample of 100 users 
users <- sample(unique(edx_copy$userId), 100)
edx_copy %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,., col = 'blue', xlab='Movies', ylab='Users', main = 'Heatmap of the movie rates matrix')


# Distribution of movies

edx %>%  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = 'black') + 
  scale_x_log10() + 
  ggtitle('Distribution of movies')

# Distribution of users

edx %>%  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = 'black') + 
  scale_x_log10() + 
  ggtitle('Distribution of users')



## Building models


# Rating for all movies
mu_hat <- mean(edx$rating)
mu_hat

#RMSE function (prof. Rafael version)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# RMSE calculation
simple_model_rmse <- RMSE(validation$rating, mu_hat) 
simple_model_rmse

# Results table to store all RMSE 
rmse_values <- tibble(method = 'Simple model RMSE', RMSE = simple_model_rmse)
rmse_values


# Model 2 : Computing predicted ratings for all movies based on movie effects

#Compute the average of all ratings of the edx set
mu <- mean(edx$rating)
#Compute b_i
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


#Plot b_i distribution
movie_avgs %>% 
  ggplot(aes(b_i)) + 
  geom_histogram(bins = 30, color = 'black') + 
  ggtitle('Distribution of estimated b_i')


# Predict bi
model_2_pred <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
movie_effect_rmse <- RMSE(model_2_pred, validation$rating)

# Enter RMSE value in table 
rmse_values <- bind_rows(rmse_values,
                         tibble(method='Movie Effect Model',  
                                RMSE = movie_effect_rmse))
rmse_values

# Model 3 : Computing predicted ratings for all movies based on movie and user effects

# Compute average rating for user u who rated more than 100 movies
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = 'black') + 
  ggtitle('Distribution of estimated b_u')

#Compute b_u on edx 
user_avgs <- edx %>%  
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


# Predicted ratings
model3_pred <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
movie_user_effect <- RMSE(model3_pred, validation$rating)
rmse_values <- bind_rows(rmse_values,
                         tibble(method='Movie + User Effects Model',  
                                RMSE = movie_user_effect))
rmse_values


## Regularization



validation %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>% slice(1:10)


# merged database
merge_db <- edx %>% 
  select(movieId, title) %>%
  distinct()


# top 10 best movies based on b_i
movie_avgs %>% left_join(merge_db, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i) %>%
  slice(1:10) 
movie_avgs
validation %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(merge_db, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10)


# top 10 worse movies based on b_i
movie_avgs %>% left_join(merge_db, by="movieId") %>%
arrange(b_i) %>%
select(title, b_i) %>%
slice(1:10)
movie_avgs
validation %>% count(movieId) %>%
left_join(movie_avgs) %>%
left_join(merge_db, by="movieId") %>%
arrange(b_i) %>%
select(title, b_i, n) %>%
slice(1:10)


# Penalized least squares

#Regularized estimates of b_i using lambda 3

lambda <- 3
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 


#Plot of the regularized estimates versus the least squares estimates.

tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

#Top 10 best movies based on the penalized estimates
  

edx %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = 'movieId') %>%
  left_join(merge_db, by = 'movieId') %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)


#Top 10 worst movies:
  
edx %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = 'movieId') %>%
  left_join(merge_db, by='movieId') %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  pull(title)

# Check if results improved

predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by = 'movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, validation$rating)
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_values <- bind_rows(rmse_values,
                         tibble(method='Regularized Movie Effect Model',  
                                RMSE = model_3_rmse))
rmse_values 


# Choosing the penalty terms

lambdas <- seq(0, 10, 0.25)
mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})

# Plot lambdas and rmse
ggplot(data.frame(lambdas = lambdas, rmses = rmses ), aes(lambdas, rmses)) +
  geom_point()
lambdas[which.min(rmses)]



lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
ggplot(data.frame(lambdas = lambdas, rmses = rmses ), aes(lambdas, rmses)) +
  geom_point()  


# Value of lambda that minimizes  RMSE
lambda <- lambdas[which.min(rmses)]
lambda


# Add model with the minimal RMSE to the results data frame
rmse_values <- bind_rows(
  rmse_values,
  tibble(method='Regularized Movie + User Effect Model',  
         RMSE = min(rmses)))
rmse_values

