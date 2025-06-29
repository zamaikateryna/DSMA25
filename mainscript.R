# first you need to install the libraries DBI and RSQlite
# please do so by clicking on the Packages tab on the right, then Install.
# Type the name of the packages: "DBI, RSQLite" without the " and hit Install

library(DBI)
library(jsonlite)
library(RPostgres)
library(readr)
library(ggplot2)
library(tidyverse)
library(VIM)
library(caret)

con <- dbConnect(RPostgres::Postgres(), dbname="remoteuser", host = "localhost", port = "5432",
                 user = "dsma_student", password = "DSMA_Stud23")

# now you should be connected. Please check by clicking on Connections tab on the upper right. You should see the 
# name of the database "remoteuser".

# exctraction of business data
fileName=paste0("~/dsma_project/businesstable.sql") # the name of the file containing the query is businesstable.sql
sqlquery=readChar(fileName, file.info(fileName)$size)
business=dbGetQuery(con, statement = sqlquery) # this stores the output in a variable called business

# exctraction of check-in data 
fileName1=paste0("~/dsma_project/checkin.sql") # the name of the file containing the query is checkin.sql
sqlquery1=readChar(fileName1, file.info(fileName1)$size)
checkin=dbGetQuery(con, statement = sqlquery1) # this stores the output in a variable called checkin

# exctraction of review data 
fileName2=paste0("~/dsma_project/review.sql") # the name of the file containing the query is review.sql
sqlquery2=readChar(fileName2, file.info(fileName2)$size)
review=dbGetQuery(con, statement = sqlquery2) # this stores the output in a variable called review

# exctraction of users data 
fileName3=paste0("~/dsma_project/users.sql") # the name of the file containing the query is users.sql
sqlquery3=readChar(fileName3, file.info(fileName3)$size)
users=dbGetQuery(con, statement = sqlquery3) # this stores the output in a variable called users

DBI::dbDisconnect(con)

write_csv(business, "~/dsma_project/business.csv")
write_csv(checkin, "~/dsma_project/checkin.csv")
write_csv(review, "~/dsma_project/review.csv")
write_csv(users, "~/dsma_project/users.csv")


business_df <- read_csv("~/dsma_project/business.csv")
checkin_df <- read_csv("~/dsma_project/checkin.csv")
review_df <- read_csv("~/dsma_project/review.csv")
users_df <- read_csv("~/dsma_project/users.csv")

# Dividing the BusinessParking column
business_df$businessparking <- as.character(business_df$businessparking)
business_df$businessparking_garage <- grepl("'garage':\\s*True", business_df$businessparking)

business_df$businessparking_street <- grepl("'street':\\s*True", business_df$businessparking)

business_df$businessparking <- NULL

# Overview of the internal data

str(business_df)

business_df %>%
  glimpse() 

business_df %>%
  ggplot(aes(stars)) +
  geom_histogram()

mean(business_df$review_count, na.rm = TRUE)
# On average traditional American restaurants have 120 reviews

business_df %>%
  ggplot(aes(review_count)) +
  geom_histogram() +
  scale_x_log10()

# We choose first top 3 US states to see, where there are the most traditional restaurants
# As we can see they're located in Pennsylvania, Florida, Missouri
top_states <- business_df %>%
  group_by(state) %>%
  summarise(restaurant_count = n()) %>%
  arrange(desc(restaurant_count)) %>%
  slice_head(n = 3)

price_distribution <- business_df %>%
  filter(state %in% top_states$state,
         !is.na(restaurantspricerange)) %>%
  group_by(state, restaurantspricerange) %>%
  summarise(count = n_distinct(business_id), .groups = "drop")

# Visualization of the results above
ggplot(price_distribution, aes(x = state, y = count, fill = factor(restaurantspricerange))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Restaurants by State and Price Range",
       x = "State",
       y = "Number of Restaurants",
       fill = "Price Range") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# We can clearly see that the common price range of the American traditional restaurants is 2 which means moderately priced. The price range in Yelp is based on users’ subjective ratings, and may indicate value more than absolute price.

business_df %>%
  group_by(name) %>%
  summarise(
    avg_stars = mean(stars),
    count = n()
  ) %>%
  arrange(desc(count), avg_stars) %>%
  head(10)
# Having many locations does not necessarily correlate with high customer ratings ->
# For instance, Popeyes and Applebee’s both have 112 entries but average around 2.1–2.5 stars

# Check-in overview
# Join business_df to checkin_df by "business_id"
checkin_df <- checkin_df %>%
  left_join(business_df %>% select(business_id, name, stars), by = "business_id")

checkin_df %>%
  arrange(desc(checkin_count)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(name, checkin_count), y = checkin_count)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 20 restaurants by check-ins", x = "Restaurant", y = "Check-in Count")

# Cleaning

business_df %>%
  mutate(
    wifi = recode(wifi, 
                  "u'free'" = "free",
                  "'free'" = "free",
                  "'no'" = "no",
                  "'paid'" = "paid",
                  "u'no'" = "no",
                  "u'paid'" = "paid"),
    wifi = na_if(wifi, "None"),
    restaurantsdelivery = na_if(restaurantsdelivery, "None"),
    restaurantsdelivery = replace_na(restaurantsdelivery, "False"),
    wifi = replace_na(wifi, "no")
  ) -> business_df

library(mice)
#imputed_data <- mice(business_df, method = "pmm", m = 3, maxit = 30, seed = 500)
#business_df <- complete(imputed_data, 1)
business_df <- business_df[!is.na(business_df$restaurantspricerange), ]


# Selecting only 3 states with the most number of traditional restaurants
business_df <- business_df %>%
  filter(state %in% c("PA","FL", "MO"))

# Adapting other df
review_df <- review_df %>%
  semi_join(business_df, by = "business_id")


# We can observe spikes at full stars (1, 2, 3, 4, 5), especially at 5.0
# Including all users could lead to outliers, signal noise from low-effort reviewers or spam.
users_df %>%
  ggplot(aes(x = average_stars)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white")

# Filter for reviews from users with more than 4 reviews and users with 0 usefulness 
filtered_users <- users_df %>%
  filter(review_count >= 5,
         useful >= 3)

# Results look already better
filtered_users %>%
  ggplot(aes(x = average_stars)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white")

mean(users_df$average_stars, na.rm = TRUE) # 3.69

# Adding external US Census dataset
library(readxl)
avg_income <- read_excel("income_states.xlsx")

avg_income <- avg_income %>%
  mutate(postal_code = as.character(postal_code))

business_df <- business_df %>%
  left_join(avg_income, by = "postal_code")

# Row 5 & 22177 were duplicated, thus we need to delete one of their copies
business_df <- business_df[-c(5, 22177), ]

# Deleting 2 dublicating columns
business_df <- business_df %>%
  select(-n_returns, -amount)

business_df <- business_df %>%
  rename(average_income = avvg_income_in1000)

any(is.na(business_df$average_income)) # We can observe no NAs in the column, meaning all postal codes are correctly joined

business_df <- business_df %>%
  mutate(
    income_category = case_when(
      average_income < 30.000 ~ "Low class",
      average_income >= 30.000 & average_income < 58.020 ~ "Lower-middle class",
      average_income >= 58.020 & average_income <= 94.000 ~ "Middle class",
      average_income >= 94.000 & average_income <= 153.000 ~ "Upper-middle class",
      average_income > 153.000 ~ "Upper class"
    )
  )

# Checking the income class distributions in the selected states
income_distribution <- business_df %>%
  filter(state %in% top_states$state,
         !is.na(income_category)) %>%
  group_by(state, income_category) %>%
  summarise(count = n_distinct(business_id), .groups = "drop")

ggplot(income_distribution, aes(x = state, y = count, fill = income_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
       x = "State",
       y = "Number of Restaurants",
       fill = "Income Category") +
  scale_fill_brewer(palette = "Pastel1") +  
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(plotly)

pricerange_income <- business_df %>%
  select( -businessparking_garage, -businessparking_street, -restaurantsdelivery, -restaurantsgoodforgroups, -name, -wifi, -stars)

incomecategory_colors <- c(
  "Low class" = "red",
  "Lower-middle class" = "orange",
  "Middle class" = "yellow",
  "Upper-middle class" = "lightgreen",
  "Upper class" = "darkgreen"
)

pricerange_income <- pricerange_income %>%
  dplyr::mutate(
    income_class = factor(income_category, levels = names(incomecategory_colors)),
    text_label = paste0("Income: ", income_category,
                        "<br>Price Level: ", restaurantspricerange)
  )

fig <- plot_ly(
  data = pricerange_income,
  type = 'scattermapbox',
  mode = 'markers',
  lat = ~latitude,
  lon = ~longitude,
  text = ~text_label,
  hoverinfo = "text",
  marker = list(
    size = ~restaurantspricerange * 4,  # size scaled by price level
    opacity = 0.6
  ),
  color = ~income_class,         
  colors = incomecategory_colors 
)


fig <- fig %>% layout(
  mapbox = list(
    style = "carto-positron",
    zoom = 10,
    center = list(
      lat = mean(pricerange_income$latitude, na.rm = TRUE),
      lon = mean(pricerange_income$longitude, na.rm = TRUE)
    )
  ),
  margin = list(t = 0, b = 0, l = 0, r = 0),
  legend = list(title = list(text = "<b>Income Class</b>"))
)

fig

# Preparing data (continuing)
business_sel <- business_df %>%
  select(income_category, restaurantspricerange, review_count, stars)

final_data <- business_sel

median(final_data$stars)

final_data$satisfaction <- ifelse(final_data$stars >= 3.5, "Yes", "No") # threshold 3.5 because of the median result
final_data$satisfaction=as.factor(final_data$satisfaction)

# We tried to filter the reviews by determining the 99th percentile threshold but the results were not satisfied
#review_threshold <- quantile(final_data$review_count, 0.99, na.rm = TRUE)
#final_data <- final_data %>%
#filter(review_count <= review_threshold)

final_data <- final_data[final_data$review_count >= 30, ]


#Analysis of missing values
aggr_plot <- aggr(final_data, col=c('lightgreen','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=0.5, gap=2, ylab=c("Histogram of missing data","Pattern"))
# Only income_category has 0.85% missing values

final_data <- final_data[!is.na(final_data$income_category), ]

# Issue with the low class category (only 10 observations)
final_data$income_category[final_data$income_category == "Low class"] <- "Lower-middle class"

final_data$restaurantspricerange[final_data$restaurantspricerange %in% c("3", "4")] <- "Expensive"
final_data$restaurantspricerange[final_data$restaurantspricerange == "1"] <- "Inexpensive"
final_data$restaurantspricerange[final_data$restaurantspricerange == "2"] <- "Mid range"


final_data$restaurantspricerange=as.factor(final_data$restaurantspricerange)
levels(final_data$income_category) <- c("Lower-middle class", "Middle class", "Upper-middle class", "Upper class")

final_data$restaurantspricerange <- factor(final_data$restaurantspricerange,
                                           levels = c("Mid range", "Inexpensive", "Expensive"))

final_data$satisfaction <- factor(final_data$satisfaction, levels = c("No", "Yes"))

final_data <- final_data %>%
  select(-stars)

# Checking class imbalance
table(final_data$satisfaction)

vars_to_use <- c("satisfaction", "income_category", "restaurantspricerange", "review_count")
data_subset <- subset(final_data, select = vars_to_use)

# Split into train/test
set.seed(66)
datasetsize <- nrow(data_subset)
x <- data_subset[sample(1:nrow(data_subset), datasetsize, replace = FALSE), ]
x.train <- x[1:floor(nrow(x) * 0.75), ]
x.test  <- x[(floor(nrow(x) * 0.75) + 1):nrow(x), ]

table(x.train$satisfaction)

# Split by class
yes_class <- x.train[x.train$satisfaction == "Yes", ]
no_class  <- x.train[x.train$satisfaction == "No", ]

set.seed(66)
no_oversampled <- no_class[sample(1:nrow(no_class), nrow(yes_class), replace = TRUE), ]

# Combine to get balanced train set
x.train_balanced <- bind_rows(yes_class, no_oversampled) %>%
  slice_sample(prop = 1)

# Check new balance
table(x.train_balanced$satisfaction)

# Logistic regression
library(rpart)
library(rattle)				
library(rpart.plot)
library(pROC)
model_logit <- glm(satisfaction ~ income_category + restaurantspricerange + review_count,
                   data = x.train_balanced,
                   family = "binomial")
summary(model_logit)

pred_probs <- predict(model_logit, newdata = x.test, type = "response")

# Converting probabilities to labels (threshold = 0.5)
pred_labels <- ifelse(pred_probs > 0.5, "Yes", "No")
pred_labels <- as.factor(pred_labels)
actual_labels <- x.test$satisfaction

# Confusion matrix
conf_matrix <- table(Predicted = pred_labels, Actual = actual_labels)
print(conf_matrix)

# Accuracy
log_accuracy <- mean(pred_labels == actual_labels)

# F1
library(yardstick)
library(MLmetrics)
truth <- factor(actual_labels, levels = c("No", "Yes"))
pred <- factor(pred_labels, levels = c("No", "Yes"))
log_f1 <- F1_Score(y_true = truth, y_pred = pred, positive = "Yes")

# ROC AUC
roc_obj <- roc(response = x.test$satisfaction, predictor = pred_probs, levels = c("No", "Yes"))
plot(roc_obj, main = "Logistic Regression ROC Curve")
log_roc <- auc(roc_obj)
log_gini <- 2 * log_roc - 1

# Decision tree

tree <- rpart(satisfaction ~ income_category + restaurantspricerange
                     + review_count,
                    data = x.train_balanced, method = "class")
fancyRpartPlot(tree)
tree_pred_class <- predict(tree, newdata = x.test, type = "class")
actual_labels <- x.test$satisfaction
# Accuracy
tree_accuracy <- mean(pred == x.test$satisfaction)
# F1
truth <- factor(actual_labels, levels = c("No", "Yes"))
pred <- factor(tree_pred_class, levels = c("No", "Yes"))
tree_f1 <- F1_Score(y_true = truth, y_pred = pred, positive = "Yes")

# Probabilities for class "Yes"
tree_probs <- predict(tree, newdata = x.test, type = "prob")[, "Yes"]

# ROC AUC
roc_obj <- roc(response = actual_labels, predictor = tree_probs, levels = c("No", "Yes"))
tree_roc <- auc(roc_obj)
tree_gini <- 2 * tree_roc - 1

# Random Forest 
library(randomForest)
library(tidymodels)

rec <- recipe(satisfaction ~ ., data = x.train_balanced) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

rf <- rand_forest(mode = "classification")
rf_wflow <- workflow() %>%
  add_model(rf) %>%
  add_recipe(rec)

rf_fit <- rf_wflow %>% fit(data = x.train_balanced)

rf_test_pred <- predict(rf_fit, new_data = x.test)
rf_test_class <- bind_cols(x.test, rf_test_pred)

# Accuracy and F1
rf_accuracy <- mean(rf_test_class$.pred_class == rf_test_class$satisfaction)
truth <- factor(rf_test_class$satisfaction, levels = c("No", "Yes"))
pred <- factor(rf_test_class$.pred_class, levels = c("No", "Yes"))
rf_f1 <- F1_Score(y_true = truth, y_pred = pred, positive = "Yes")

# ROC and AUC
rf_prob <- predict(rf_fit, new_data = x.test, type = "prob")
rf_eval <- bind_cols(x.test, rf_prob)
roc_obj <- roc(response = x.test$satisfaction, predictor = rf_eval$.pred_Yes, levels = c("No", "Yes"))
rf_roc <- auc(roc_obj)

rf_gini <- 2 * rf_roc - 1

# K-Nearest Neighbors
library(parsnip)
library(kknn)
library(workflows)

rec <- recipe(satisfaction ~ income_category + restaurantspricerange + review_count,
              data = x.train_balanced) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

knn <- nearest_neighbor(mode = "classification", neighbors = 5) %>%
  set_engine("kknn")

knn_wflow <- workflow() %>%
  add_model(knn) %>%
  add_recipe(rec)

knn_fit <- fit(knn_wflow, data = x.train_balanced)
knn_pred_class <- predict(knn_fit, new_data = x.test)
knn_pred_prob <- predict(knn_fit, new_data = x.test, type = "prob")

knn_results <- x.test %>%
  select(satisfaction) %>%
  bind_cols(knn_pred_class, knn_pred_prob)

# Accuracy
knn_accuracy <- mean(knn_results$.pred_class == knn_results$satisfaction)

# F1
knn_f1 <- F1_Score(knn_results$satisfaction, knn_results$.pred_class, positive = "Yes")

# ROC AUC
roc_obj <- roc(response = knn_results$satisfaction, predictor = knn_results$.pred_Yes, levels = c("No", "Yes"))
knn_roc <- auc(roc_obj)

knn_gini <- 2 * knn_roc - 1


# Support Vector Machine
library(e1071)
svm <- svm(satisfaction ~ income_category + restaurantspricerange + review_count,
                 data = x.train_balanced,
                 kernel = "radial",
                 cost = 1,           
                 gamma = 0.1,        
                 probability = TRUE)
svm_pred <- predict(svm, x.test, probability = TRUE)
#Accuracy
svm_acc <- mean(svm_pred == x.test$satisfaction)
# F1
svm_f1 <- F1_Score(y_pred = svm_pred, y_true = x.test$satisfaction, positive = "Yes")
# ROC AUC
prob_values <- attr(predict(svm, x.test, probability = TRUE), "probabilities")[, "Yes"]
svm_roc_obj <- roc(response = x.test$satisfaction, predictor = prob_values, levels = c("No", "Yes"))
svm_roc <- auc(svm_roc_obj)

svm_gini <- 2 * svm_roc - 1

# XGBoost (Boosting)
library(xgboost)
boost <- boost_tree(mode = "classification")

boost_wflow <- workflow() %>%
  add_model(boost) %>%
  add_recipe(rec)

boost_fit <- boost_wflow %>% fit(data = x.train_balanced)

# Predictions 
boost_test_pred <- predict(boost_fit, x.test)
boost_test_prob <- predict(boost_fit, x.test, type = "prob")

# Accuracy
boost_accuracy <- mean(boost_test_pred$.pred_class == x.test$satisfaction)

# F1 (assuming "Yes" is positive class)
boost_f1 <- F1_Score(x.test$satisfaction, boost_test_pred$.pred_class, positive = "Yes")

# ROC AUC
boost_roc <- roc_auc(
  data = bind_cols(x.test, boost_test_prob),
  truth = satisfaction,
  .pred_Yes,
  event_level = "second"
) %>% pull(.estimate)

boost_gini <- 2 * boost_roc - 1

# Boosting with xgboost algorithm
boost_tuned <- boost_tree(
  mode = "classification",
  trees = 1000,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>% set_engine("xgboost")

boost_wflow <- workflow() %>%
  add_model(boost_tuned) %>%
  add_recipe(rec)

set.seed(123) # Cross-validation setup
cv_folds <- vfold_cv(x.train_balanced, v = 5, strata = satisfaction) # Performs v-fold cross-validation, where the dataset is split into v groups, used to train and validate models during tuning
grid <- grid_space_filling( # A grid of hyperparameter combinations using space-filling design
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  size = 10 # Generate 10 combinations of the parameters
)

set.seed(123)
tuned_results <- tune_grid( # Tuning the model
  boost_wflow,
  resamples = cv_folds, # Cross-validation folds used to evaluate each parameter combination
  grid = grid,
  metrics = metric_set(accuracy, f_meas, roc_auc)
)
show_best(tuned_results, metric = "roc_auc")

best_parameters <- select_best(tuned_results, metric = "roc_auc")

final_boost <- finalize_workflow(boost_wflow, best_parameters)
final_fit <- final_boost %>% fit(data = x.train_balanced)

boost_pred_class <- predict(final_fit, x.test)
boost_pred_prob  <- predict(final_fit, x.test, type = "prob")

# Accuracy
boost_tuned_acc <- mean(boost_pred_class$.pred_class == x.test$satisfaction)

# F1 
boost_tuned_f1 <- F1_Score(x.test$satisfaction, boost_pred_class$.pred_class, positive = "Yes")
# ROC AUC
boost_tuned_roc <- roc_auc(
  data = bind_cols(x.test, boost_pred_prob),
  truth = satisfaction,
  .pred_Yes,
  event_level = "second"
) %>% pull(.estimate)

boost_tuned_gini <- 2 * boost_tuned_roc - 1


# Bagging
bagging <- rand_forest(
  mode = "classification",
  mtry = 3,          # All predictors used at each split
  trees = 200,       # N. of tress must be > 100
  min_n = 5          
) %>%
  set_engine("ranger", importance = "impurity")

bag_wflow_fixed <- workflow() %>%
  add_model(bagging) %>%
  add_recipe(rec)

bag_fit_fixed <- fit(bag_wflow_fixed, data = x.train_balanced)
bag_test_pred_class <- predict(bag_fit_fixed, new_data = x.test)
bag_test_pred_prob  <- predict(bag_fit_fixed, new_data = x.test, type = "prob")
bag_eval <- bind_cols(x.test, bag_test_pred_class, bag_test_pred_prob)


# Accuracy
bag_accuracy <- mean(bag_eval$.pred_class == bag_eval$satisfaction)
# F1
bag_f1 <- F1_Score(
  y_true = factor(bag_eval$satisfaction, levels = c("No", "Yes")),
  y_pred = factor(bag_eval$.pred_class, levels = c("No", "Yes")),
  positive = "Yes"
)

# ROC AUC
roc_obj <- roc(
  response = bag_eval$satisfaction,
  predictor = bag_eval$.pred_Yes,
  levels = c("No", "Yes")
)

bag_roc <- auc(roc_obj)
bag_gini <- 2 * bag_roc - 1


# Bagging (Tuned)
library(tune)
library(dials)

bag_tuned <- rand_forest(
  mode = "classification",
  trees = tune(),     
  mtry = tune(),      
  min_n = tune()      
) %>%
  set_engine("ranger", importance = "impurity")

bag_wflow <- workflow() %>%
  add_model(bag_tuned) %>%
  add_recipe(rec)

bag_grid <- grid_space_filling(
  mtry(range = c(1, 3)),          # Since we have 3 predictors
  trees(range = c(100, 500)),
  min_n(range = c(2, 10)),
  size = 15
)
tuned_bag_results <- tune_grid(
  bag_wflow,
  resamples = cv_folds,
  grid = bag_grid,
  metrics = metric_set(accuracy, f_meas, roc_auc)
)

best_bag <- select_best(tuned_bag_results, metric = "f_meas")
final_bag_wflow <- finalize_workflow(bag_wflow, best_bag)
final_bag_fit <- fit(final_bag_wflow, x.train_balanced) # Fitting on full training data

bag_test_pred <- predict(final_bag_fit, new_data = x.test)
bag_test_class <- bind_cols(x.test, bag_test_pred)

# Accuracy
bag_tuned_acc <- bag_test_class %>%
  accuracy(truth = satisfaction, estimate = .pred_class) %>%
  pull(.estimate)

# F1
bag_tuned_f1 <- bag_test_class %>%
  f_meas(truth = satisfaction, estimate = .pred_class, event_level = "second") %>%
  pull(.estimate)

# ROC AUC
bag_prob <- predict(final_bag_fit, new_data = x.test, type = "prob")
bag_eval <- bind_cols(x.test, bag_prob)
bag_tuned_auc <- bag_eval %>%
  roc_auc(truth = satisfaction, .pred_Yes, event_level = "second") %>%
  pull(.estimate)

bag_tuned_gini <- 2 * bag_tuned_auc - 1

# Neural network
library(nnet)

# The neural network requires normalized values for better prediction
rec_nn <- recipe(satisfaction ~ income_category + restaurantspricerange + review_count, data = x.train_balanced) %>%
  step_dummy(all_nominal_predictors()) %>%      # One-hot encoding for categorical predictors
  step_zv(all_predictors()) %>%                 
  step_normalize(all_predictors())    

nn <- mlp(
  mode = "classification",
  hidden_units = 5,
  penalty = 0.01,
  epochs = 100
) %>%
  set_engine("nnet")

nn_wflow <- workflow() %>%
  add_model(nn) %>%
  add_recipe(rec_nn)

nn_fit <- nn_wflow %>%
  fit(data = x.train_balanced)

nn_pred_class <- predict(nn_fit, x.test)
nn_pred_prob  <- predict(nn_fit, x.test, type = "prob")
nn_eval <- bind_cols(x.test, nn_pred_class, nn_pred_prob)

# Accuracy
nn_accuracy <- nn_eval %>%
  accuracy(truth = satisfaction, estimate = .pred_class) %>%
  pull(.estimate)

# F1 
nn_f1 <- nn_eval %>%
  f_meas(truth = satisfaction, estimate = .pred_class, event_level = "second") %>%
  pull(.estimate)
# ROC AUC
nn_roc <- nn_eval %>%
  roc_auc(truth = satisfaction, .pred_Yes, event_level = "second") %>%
  pull(.estimate)

nn_gini <- 2 * nn_roc - 1

# Neural network tuned

nn_tuned <- mlp(
  mode = "classification",
  hidden_units = tune(),   
  penalty = tune(),        # Weight decay (L2 regularization) penalizes large weights to help the model generalize better
  epochs = tune()         
) %>%
  set_engine("nnet")

nn_tuned_wflow <- workflow() %>%
  add_recipe(rec_nn) %>%
  add_model(nn_tuned)

set.seed(123)
cv_folds <- vfold_cv(x.train_balanced, v = 5) # 5-fold CV, as The model trains on 4 parts (80% of data) and validates on the remaining 1 part (20%) 
# 5-fold CV common default choice

nn_grid <- grid_regular(
  hidden_units(range = c(1, 10)),   # Small to moderate complexity
  penalty(range = c(-4, -1), trans = log10_trans()),  # L2 penalty from 1e-4 to 1e-1
  epochs(range = c(50, 200)),       # Sufficient iterations to learn
  levels = 3                        # 3 levels per parameter = 27 combinations
)

set.seed(123)
nn_tune_results <- tune_grid(
  nn_tuned_wflow,
  resamples = cv_folds,
  grid = nn_grid,
  metrics = metric_set(roc_auc, accuracy, f_meas)
)

best_params <- nn_tune_results %>%
  select_best(metric = "roc_auc") # Threshold-independent (unlike Accuracy or F1) and reflects ranking quality

final_nn_wflow <- nn_wflow %>%
  finalize_workflow(best_params)

final_nn_fit <- final_nn_wflow %>%
  fit(data = x.train_balanced)

final_nn_pred_class <- predict(final_nn_fit, x.test)
final_nn_pred_prob  <- predict(final_nn_fit, x.test, type = "prob")
final_nn_eval <- bind_cols(x.test, final_nn_pred_class, final_nn_pred_prob)

# Accuracy
nn_tuned_acc <- final_nn_eval %>%
  accuracy(truth = satisfaction, estimate = .pred_class) %>%
  pull(.estimate)

nn_tuned_f1 <- final_nn_eval %>%
  f_meas(truth = satisfaction, estimate = .pred_class, event_level = "second") %>%
  pull(.estimate)

nn_tuned_roc <- final_nn_eval %>%
  roc_auc(truth = satisfaction, .pred_Yes, event_level = "second") %>%
  pull(.estimate)

nn_tuned_gini <- 2 * nn_tuned_roc - 1


model_metrics <- tibble(
  model = c("log_reg", "decision tree", "rf", "bagging", "boosting", "knn", "svm", "neural network"),
  accuracy = c(log_accuracy, tree_accuracy, rf_accuracy, bag_accuracy, boost_accuracy, knn_accuracy, svm_acc, nn_accuracy),
  f1       = c(log_f1, tree_f1, rf_f1, bag_f1, boost_f1, knn_f1, svm_f1, nn_f1),
  roc      = c(log_roc, tree_roc, rf_roc, bag_roc, boost_roc, knn_roc, svm_roc, nn_roc),
  gini     = c(log_gini, tree_gini, rf_gini, bag_gini, boost_gini, knn_gini, svm_gini, nn_gini)
)

model_metrics

library(formattable)

formattable(model_metrics, 
            align =c("l","c","c","c")
)

model_metrics_tuned <- tibble(
  model = c("log_reg", "decision tree", "rf", "bagging (tuned)", "boosting (tuned)", "knn", "svm", "neural network (tuned)"),
  accuracy = c(log_accuracy, tree_accuracy, rf_accuracy, bag_tuned_acc, boost_tuned_acc, knn_accuracy, svm_acc, nn_tuned_acc),
  f1       = c(log_f1, tree_f1, rf_f1, bag_tuned_f1, boost_tuned_f1, knn_f1, svm_f1, nn_tuned_f1),
  roc      = c(log_roc, tree_roc, rf_roc, bag_tuned_auc, boost_tuned_roc, knn_roc, svm_roc, nn_tuned_roc),
  gini     = c(log_gini, tree_gini, rf_gini, bag_tuned_gini, boost_tuned_gini, knn_gini, svm_gini, nn_tuned_gini)
)

formattable(model_metrics_tuned, 
            align =c("l","c","c","c")
)

model_metrics_tuned %>%
  pivot_longer(cols = c(accuracy, f1, roc, gini), names_to = "metric") %>%
  ggplot(aes(model, value, fill = model)) +
  geom_col() +
  facet_wrap(~metric) +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values = c("#AEC6CF", 
                               "#B3D7E8",  
                               "#99CCFF",  
                               "#C6DBEF",  
                               "#ADD8E6",  
                               "#B0E0E6",  
                               "#AFDBF5",  
                               "#D0EFFF"   
  ))

model_metrics_tuned %>%
  pivot_longer(cols = c(accuracy, f1, roc, gini), names_to = "metric") %>%
  ggplot(aes(x = metric, y = model, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 3)), color = "black") +
  scale_fill_viridis_c(option = "C", direction = -1) +
  theme_light() 
