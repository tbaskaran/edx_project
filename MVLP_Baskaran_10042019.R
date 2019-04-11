#######################################################
#                MOvie Recommendation System          #
#                The Script file                      #     
#                MVLP_Baskaran_10042019.R             #
#######################################################

#############################################
#Initial settings
#############################################
#Load the required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

#############################################
# Data Preparation
#############################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


#download the data file
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#Read the ratings.dat file
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
class(ratings)
ratings[1:5,]
dim(ratings)
mean(ratings$rating)


#Read the movies.dat file
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% 
		mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
class(movies)
dim(movies)

#Combine the ratings and movies dataframe and create a new movielens dataframe
movielens <- left_join(ratings, movies, by = "movieId")
class(movielens)
str(movielens)


#Construct a function named RMSEto calculate RMSE
RMSE <- function(true_ratings, predicted_ratings){
  		sqrt(mean((true_ratings - predicted_ratings)^2))
  		}

# Saparate a Validation set of 10% from movieLens data
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

dim(edx)
dim(validation)

##############################################
#          Exploratory Data Analysis
##############################################

#Number of movies with 0 and 3 ratings 
edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()

#Number of distinct movies and users
edx %>% summarise(users =n_distinct(userId), movies= n_distinct(movieId))

#Number of ratings in different genres
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  head()

#Number of ratings for different movies
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  head()

#Count of ratings
edx %>% 
  group_by(rating) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) 
 
#Nature of half rating numerically
half_rating <- edx %>% 
  group_by(rating) %>% 
  summarize(count = n())
head(half_rating)

#Nature of half rating visually
half_rating %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()+
  ggtitle("Nature of half rating")

#Distribution of movies rating
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "blue", fill = "violet") + 
  scale_x_log10() + 
  ggtitle("Movies")


# Distribution of users rating 
edx %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "blue", fill = "violet") + 
  scale_x_log10() + 
  ggtitle("Users")


#Distribution of genres
edx %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number = n()) %>%
  arrange(desc(number)) %>% 
  ggplot(aes(x=genres, y=number))+
  geom_bar(stat="Identity")+
  ggtitle("Ratings in each genres") + 
  theme(axis.text.x = element_text( angle = 90)) 


#####################################################
#          Developing the model
#####################################################

###Partition edx data set into train and test sets
set.seed(500)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

### Adjusting_for_rows
test_set1 <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test_set set back into train_set set
removed <- anti_join(test_set, test_set1)
train_set <- rbind(train_set, removed)
test_set <- test_set1

########################
#Model 1: Guessing Model
########################

#Prediction by gussing
mu_hat <- as.numeric(sample(c("0.5", "1",   "1.5", "2",  "2.5", "3",   "3.5", "4",   "4.5", "5" ), length(test_set$rating), replace = TRUE)) 

Guess_RMSE <- RMSE(test_set$rating,mu_hat)
Guess_RMSE

#Create a results table 
rmse_results <- data_frame(method = "Guessing Model", RMSE = Guess_RMSE)

############################
#Model 2: Proportional Model
############################

#Define a sequence of ratings
s <- seq(0.5,5,0.5)
#Define a vector of 10 zeros
p <- rep(0,10)
# Populate p
for (k in 1:10) { 
  i <- s[k]
  p[k] <- mean(test_set$rating==i)
}
p
mu_hat <- as.numeric(sample(c("0.5", "1",   "1.5", "2",  "2.5", "3",   "3.5", "4",   "4.5", "5" ), length(test_set$rating), replace = TRUE, prob=p))

Prop_RMSE <- RMSE(test_set$rating,mu_hat)
Prop_RMSE

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Proportional Model", RMSE = Prop_RMSE))


################################
#Model 3: Same Mean rating Model
################################

#Same Mean rating Model
mu_hat <- mean(train_set$rating)
mu_hat

same_rating_RMSE <- RMSE(test_set$rating, mu_hat)
same_rating_RMSE

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Same Mean rating Model", RMSE = same_rating_RMSE))


################################
#Model 4: Movie Effect Model
################################

#Movie Effect Model Histogram
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(as.numeric(rating) - mu_hat))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("blue"), fill=I("purple"), main = "Movie average bias")


#Movie effect prediction
predicted_ratings <- mu_hat + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

movie_rmse <- RMSE(predicted_ratings, test_set$rating)
movie_rmse

rmse_results <- bind_rows(rmse_results,data_frame(method="Movie Effect Model",RMSE = movie_rmse))


###################################
#Model 5: Movie + User Effect Model
###################################

#Movie + User Effect Model Histogram
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

user_avgs %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("blue"), fill=I("purple"), main= "User Average bias")


#Movie+User Effect Predictions
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

movie_user_rmse <- RMSE(predicted_ratings, as.numeric(test_set$rating))
movie_user_rmse

rmse_results <- bind_rows(rmse_results,
data_frame(method="Movie + User Effects Model",  RMSE = movie_user_rmse))



########################################
#Model 6: Regularized Movie Effect Model
########################################


#Plot of regularized estimates versus the least squares estimates
lambda <- 2.25
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(as.numeric(rating) - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i,
           regularlized = movie_reg_avgs$b_i,
           n = movie_reg_avgs$n_i) %>%
  		ggplot(aes(original, regularlized, size=sqrt(n))) +
  		geom_point(shape=1, alpha=0.5)

#Cross validation for lambda for movie effect
lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  		group_by(movieId) %>% 
  		summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, as.numeric(test_set$rating)))
})

#Plot of rmses vs lambdas
qplot(lambdas, rmses)  

#Predictions
lambdas[which.min(rmses)]
min(rmses)

rmse_results <- bind_rows(rmse_results,
                    data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = min(rmses)))



##########################################
#Model 7: Regularized Movie + Effect Model
##########################################

###Cross validation for lambda for movie + user effect
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(as.numeric(train_set$rating))
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(as.numeric(rating) - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(as.numeric(rating) - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, as.numeric(test_set$rating)))
})

#Plot of rmses vs lambdas
qplot(lambdas, rmses)  

#Predictions
lambda <- lambdas[which.min(rmses)]
lambda
min(rmses)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))


################################
#Results
################################

#Print the models and their RMSEs
rmse_results



################################
#Validation
################################

####Predictions using the validation set
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(as.numeric(rating) - mu_hat))

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

movie_user_rmse_vali <- RMSE(predicted_ratings, validation$rating)
movie_user_rmse_vali


###################################################################
#Conclusion:  The Movie + User effect Model is the appropriate movie recommendation system
###################################################################

