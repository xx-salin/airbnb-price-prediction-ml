
# DATASET

packages <- c("tidyverse", "skimr", "DataExplorer", "GGally", "factoextra", "readr", "dplyr", "caret", "tidymodels", "randomForest", "keras", "nnet", "kableExtra", "glmnet")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
library(tidyverse)   
library(skimr)        
library(DataExplorer)  
library(GGally)        
library(factoextra) 
library(knitr)
library(readr)
library(dplyr)
library(caret)
library(tidymodels)
library(randomForest)
library(keras)
library(nnet)
library(kableExtra)
library(glmnet)


LONDON <- read_csv("data/LONlistings.csv")
View(LONDON)





##### CLEANING / PREPROCESSING

str(LONDON)



# id / url variable cleaning
names(LONDON)[sapply(LONDON, function(col) length(unique(col)) == nrow(LONDON))]
LONDON <- LONDON[ , !(names(LONDON) %in% c("id", "listing_url"))] # drop variable if all unique values
LONDON <- LONDON[ , !(names(LONDON) %in% c("scrape_id", "last_scraped", "calendar_last_scraped", "source", "picture_url", "host_url", "host_thumbnail_url", "host_picture_url", "host_verifications", "host_id"))]
LONDON$has_availability <- NULL



# removing unnecessary variables with better alternatives

LONDON <- LONDON[ , !(names(LONDON) %in% c("neighbourhood", "neighborhood_overview", "host_neighbourhood", "host_location"))]  # neighbourhood_cleansed 

LONDON <- LONDON[ , !(names(LONDON) %in% c("maximum_maximum_nights", "maximum_nights_avg_ntm", "minimum_maximum_nights", "maximum_minimum_nights", "minimum_minimum_nights", "minimum_nights_avg_ntm"))] # maximum_nights / minimum_nights

LONDON <- LONDON[ , !(names(LONDON) %in% c("calculated_host_listings_count", 
                                           "calculated_host_listings_count_entire_homes", 
                                           "calculated_host_listings_count_private_rooms", 
                                           "calculated_host_listings_count_shared_rooms",
                                           "host_total_listings_count"))] # host_listings_count

LONDON$host_acceptance_rate <- NULL

LONDON %>%
  select(number_of_reviews, number_of_reviews_ltm,
         number_of_reviews_l30d, reviews_per_month) %>%
  cor(use = "complete.obs") # corr matrix for variables containing 'reviews'


LONDON$days_since_first_review <- NULL
LONDON$days_since_last_review <- NULL
LONDON <- LONDON[ , !(names(LONDON) %in% c("number_of_reviews",
                                           "number_of_reviews_ltm",
                                           "number_of_reviews_l30d"))] # keep reviews_per_month (highest corr with price)

LONDON$reviews_per_month[is.na(LONDON$reviews_per_month)] <- 0

LONDON %>%
  select(availability_30, availability_60, availability_90, availability_365) %>%
  cor(use = "complete.obs") # corr matrix for variables containing 'availability'
LONDON <- LONDON[ , !(names(LONDON) %in% c("availability_30",
                                           "availability_60",
                                           "availability_365"))]









# missing values cleaning
sort(colSums(is.na(LONDON)) / nrow(LONDON) * 100, decreasing = TRUE)
LONDON <- LONDON[!is.na(LONDON$price), ] # drop obs if NA price
LONDON <- LONDON[ , !(names(LONDON) %in% c("neighbourhood_group_cleansed", "license", "calendar_updated"))] # drop variable if only NA values
LONDON <- LONDON[ , !(names(LONDON) %in% c("neighbourhood", "neighborhood_overview", "host_neighbourhood", "host_location"))] 



# removing long text (not planning to do in depth text analysis)
names(LONDON)[sapply(LONDON, is.character)]
names(LONDON)[sapply(LONDON, function(col) {is.character(col) && mean(nchar(col), na.rm = TRUE) > 25 })]
LONDON <- LONDON[ , !(names(LONDON) %in% c("name", "description", "host_about"))] #drop variables with over 25 characters (except amenities because of later use)



# CONVERTING VARIABLE TYPES
table(sapply(LONDON, function(x) class(x)[1]))



## 1) characters / strings
names(LONDON)[sapply(LONDON, is.character)]

LONDON <- LONDON[ , !(names(LONDON) %in% c("host_name", "bathrooms_text"))] #drop since made redundant by other variables, host_id and bathrooms


# a) characters to numeric

LONDON$host_response_rate[LONDON$host_response_rate %in% c("N/A", "")] <- NA
LONDON$host_response_rate <- as.numeric(gsub("%", "", LONDON$host_response_rate))

LONDON$price <- as.numeric(gsub("[$,]", "", LONDON$price))

# b) characters to factors 

unique(LONDON$host_response_time)
LONDON$host_response_time[LONDON$host_response_time == "N/A"] <- NA
LONDON$host_response_time[is.na(LONDON$host_response_time)] <- "unknown"
LONDON$host_response_time <- factor(LONDON$host_response_time, levels = c("unknown", "a few days or more", "within a day", "within a few hours", "within an hour"), ordered = TRUE)

unique(LONDON$neighbourhood_cleansed)
LONDON$neighbourhood_cleansed <- as.factor(LONDON$neighbourhood_cleansed)

unique(LONDON$property_type)
sort(table(LONDON$property_type), decreasing = TRUE) # lowering levels in property_type, collapsing rare (if less than 100 obs) categories into other
prop_counts <- table(LONDON$property_type)
common_prop_types <- names(prop_counts[prop_counts >= 100])
LONDON$property_type_1 <- ifelse(
  LONDON$property_type %in% common_prop_types,
  LONDON$property_type,
  "Other"
)
LONDON$property_type_1 <- as.factor(LONDON$property_type_1)
LONDON$property_type <- NULL

unique(LONDON$room_type)
LONDON$room_type <- as.factor(LONDON$room_type)


# c) amenities special case -> occurrence range filtering and creating integer dummies (for non-linear models only)

amenities_sep <- strsplit(gsub('[\\[\\]"]', '', LONDON$amenities), ",\\s*") 
amenities_occur <- sort(table(unlist(amenities_sep)), decreasing = TRUE)
min_threshold <- 0.25 * nrow(LONDON)
max_threshold <- 0.75 * nrow(LONDON)
amenities_1 <- names(amenities_occur[amenities_occur >= min_threshold & amenities_occur <= max_threshold]) #amenities filtered by occurrence range (25% - 75%)
length(amenities_1)
amenities_occur[amenities_1]
for (amenity in amenities_1) {
  col_name <- paste0("has_", gsub("[^A-Za-z0-9]", "_", amenity))
  LONDON[[col_name]] <- sapply(amenities_sep, function(x) amenity %in% x)
} # creating dummy variable for each amenity selected by occurrence range filtering, turning from logical to integer
amenity_cols <- grep("^has_", names(LONDON), value = TRUE)
LONDON[amenity_cols] <- lapply(LONDON[amenity_cols], as.integer) 
LONDON$amenities <- NULL



## 2) logical
names(LONDON)[sapply(LONDON, is.logical)]

LONDON <- LONDON %>%
  mutate(
    host_is_superhost = as.integer(host_is_superhost),
    host_has_profile_pic = as.integer(host_has_profile_pic),
    host_identity_verified = as.integer(host_identity_verified),
    instant_bookable = as.integer(instant_bookable)
  ) # turning all logical into integer



## 3) Date
names(LONDON)[sapply(LONDON, inherits, what = "Date")]

reference_date <- as.Date("2024-12-12") # reference date is when data was scraped

LONDON <- LONDON %>%
  mutate(
    host_days_active = as.numeric(difftime(reference_date, host_since, units = "days")))
LONDON <- LONDON %>% select(-host_since, -first_review, -last_review)


table(sapply(LONDON, function(x) class(x)[1]))


# CLEANING NA DATAPOINTS: dropping, imputing, etc
sort(colSums(is.na(LONDON)), decreasing = TRUE)

numeric_vars <- names(LONDON)[sapply(LONDON, is.numeric)]
sort(colSums(is.na(LONDON[, numeric_vars])))

LONDON <- LONDON %>% 
  filter(!is.na(host_listings_count),
         !is.na(host_has_profile_pic),
         !is.na(host_identity_verified),
         !is.na(host_days_active)) # drop observations for variable with NA that is very low, 2 NA for each

LONDON$host_response_rate[is.na(LONDON$host_response_rate)] <- median(LONDON$host_response_rate, na.rm = TRUE)
LONDON$bathrooms[is.na(LONDON$bathrooms)] <- median(LONDON$bathrooms, na.rm = TRUE)
LONDON$bedrooms[is.na(LONDON$bedrooms)]   <- median(LONDON$bedrooms, na.rm = TRUE)
LONDON$beds[is.na(LONDON$beds)]           <- median(LONDON$beds, na.rm = TRUE) # median imputation (should be robust to outliars)


review_score_vars <- c(
  "review_scores_rating", "review_scores_accuracy", "review_scores_cleanliness",
  "review_scores_communication", "review_scores_checkin",
  "review_scores_location", "review_scores_value"
)

for (var in review_score_vars) {
  LONDON[[var]][is.na(LONDON[[var]])] <- mean(LONDON[[var]], na.rm = TRUE)
} # mean imputation for review score variables

LONDON$host_is_superhost[is.na(LONDON$host_is_superhost)] <- 0 # assume not superhost if not mentioned

sum(is.na(LONDON))



# REMOVING OUTLIERS from price and other numeric variables 
summary(LONDON$price)
quantile(LONDON$price, probs = c(0.01, 0.99), na.rm = TRUE)
q1_price <- quantile(LONDON$price, 0.01, na.rm = TRUE)
q99_price <- quantile(LONDON$price, 0.99, na.rm = TRUE)
LONDON <- LONDON %>%
  filter(price >= q1_price, price <= q99_price) #removing price outliers

summary(LONDON$host_listings_count)
quantile(LONDON$host_listings_count, probs = c(0.01, 0.99), na.rm = TRUE)
q1_hlc <- quantile(LONDON$host_listings_count, 0.01, na.rm = TRUE)
q99_hlc <- quantile(LONDON$host_listings_count, 0.99, na.rm = TRUE)
LONDON <- LONDON %>%
  filter(host_listings_count >= q1_hlc, host_listings_count <= q99_hlc)

LONDON$ln_price <- log(LONDON$price)

##### EDA


glimpse(LONDON)
introduce(LONDON)
skim(LONDON$price)
table(sapply(LONDON, function(x) class(x)[1]))


# picking top features for EDA from list of variables 

# 1) top continuous numeric variables

numeric_features <- LONDON %>% 
  select(where(is.numeric)) %>%
  select(-price, -ln_price) %>% 
  select(where(~ length(unique(.x[!is.na(.x)])) > 2))

var_table <- numeric_features %>% 
  summarise(across(everything(), ~ var(.x, na.rm = TRUE))) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "variance") %>%
  arrange(desc(variance)) # finding numeric features with highest variance

cor_table <- numeric_features %>% 
  summarise(across(everything(), ~ cor(.x, LONDON$ln_price, use = "complete.obs"))) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "cor_with_ln_price") # finding num features with highest corr with price

feature_table <- left_join(var_table, cor_table, by = "variable") %>%
  mutate(score = variance * abs(cor_with_ln_price)) %>%
  arrange(desc(score)) # ranked table of features with highest var * corr with price

top_eda_1 <- feature_table %>% filter(abs(cor_with_ln_price) >= 0.05) # list of top features based on core_table, ensuring at least 0.05 corr with price
top_eda_1 %>% arrange(desc(score)) %>% kable()










# 2) top binary variables 

binary_features <- LONDON %>% select(where(~ is.integer(.x) && all(na.omit(.x) %in% c(0, 1))))

binary_results <- data.frame(variable = character(), mean_diff = numeric(), p_value = numeric())
for (var in names(binary_features)) {
  group_means <- tapply(LONDON$ln_price, binary_features[[var]], mean, na.rm = TRUE)
  mean_diff <- abs(diff(group_means))
  p_val <- t.test(LONDON$ln_price ~ binary_features[[var]])$p.value
  binary_results <- rbind(binary_results, data.frame(variable = var, mean_diff = mean_diff, p_value = p_val))
} # mean difference in price combined with t-testing

top_eda_2 <- binary_results %>% filter(p_value < 0.05) %>% arrange(desc(mean_diff)) %>% slice_head(n = 10)
top_eda_2b <- binary_results %>% filter(p_value < 0.05) %>% arrange(desc(mean_diff)) %>% filter(mean_diff >= 0.2)
top_eda_2 %>% kable()



# 3) top categorical variables

multi_cat_features <- LONDON %>% select(where(is.factor)) %>% select(where(~ n_distinct(.x) > 2))

multi_cat_table <- map_dfr(names(multi_cat_features), function(var) {
  group_means <- aggregate(ln_price ~ ., data = LONDON[c(var, "ln_price")], mean, na.rm = TRUE)[, 2]
  spread <- sd(group_means, na.rm = TRUE)
  tibble(variable = var, group_ln_price_sd = spread, n_levels = n_distinct(LONDON[[var]]))
}) # compare price spread between categories 

top_eda_3 <- multi_cat_table %>% filter(group_ln_price_sd > 0.2) %>% arrange(desc(group_ln_price_sd))
top_eda_3 %>% kable()





### EDA VISUALIZATION


# price distribution

ggplot(LONDON, aes(x = price)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Price", x = "ln(Price)", y = "Count")

ggplot(LONDON, aes(x = ln_price)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "white") +
  labs(title = "Distribution of log Price", x = "Log Price", y = "Count")


## top_eda_1

top_eda_1 %>% arrange(desc(score)) %>% kable()

eda_1 <- c(
  "host_listings_count",
  "accommodates",
  "beds",
  "bedrooms",
  "bathrooms",
  "reviews_per_month",
  "review_scores_location",
  "review_scores_cleanliness",
  "longitude"
)

LONDON <- LONDON[LONDON$beds <= 10, ] # alterations to variables for skewness / outliers 
LONDON <- LONDON[LONDON$bedrooms <= 10, ]
LONDON <- LONDON[LONDON$bathrooms <= 10, ]
LONDON <- LONDON[LONDON$accommodates <= 13, ]
LONDON$host_listings_count <- log(LONDON$host_listings_count)
LONDON$reviews_per_month <- log1p(LONDON$reviews_per_month)


for (var in eda_1) {
  cat("Variable:", var, "\n")
  print(
    ggplot(LONDON, aes(x = .data[[var]])) +
      geom_histogram(bins = 50, fill = "skyblue", color = "white") +
      labs(title = paste("Distribution of", var), x = var, y = "Count")
  )
} # distributions of continuous variables


for (var in eda_1) {
  cat("Variable:", var, "\n")
  print(
    ggplot(LONDON, aes(x = .data[[var]], y = ln_price)) +
      geom_point(alpha = 0.2, color = "steelblue") +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Scatterplot of", var, "vs log price"),
           x = var,
           y = "log price")
  )
} # scatter plots of eda_1 vs log price



## top eda_2 and eda_3

eda_2 <- c(
  "has__Dishwasher_",
  "has__TV_",
  "has__Hair_dryer_",
  "has__Private_entrance_",
  "has__Freezer_",
  "has__Iron_",
  "has__Dining_table_",
  "has__Wine_glasses_",
  "has__Oven_",
  "has__Toaster_"
)

for (var in eda_2) {
  cat("Variable:", var, "\n")
  print(
    ggplot(LONDON, aes(x = factor(.data[[var]]), y = ln_price)) +
      geom_boxplot(fill = "skyblue") +
      labs(title = paste("ln(Price) by", var),
           x = var,
           y = "ln_price")
  )
}

eda_3 <- c("room_type", "property_type_1", "neighbourhood_cleansed")

for (var in eda_3) {
  cat("Variable:", var, "\n")
  print(
    ggplot(LONDON, aes(x = .data[[var]], y = ln_price)) +
      geom_boxplot(fill = "orange") +
      labs(title = paste("ln(Price) by", var),
           x = var,
           y = "ln_price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}



### EDA correlation matrix

corr_max_data <- LONDON %>% select(all_of(eda_1))

ggcorr(corr_max_data,
       label = TRUE,
       label_round = 2,
       label_size = 2.5,
       size = 2,
       hjust = 0.75,
       layout.exp = 2,
       low = "red", mid = "white", high = "blue",
       midpoint = 0, nbreaks = 5, limits = c(-1, 1)) +
  ggtitle("Correlation Matrix of Continuous Feature Variables")

cor(LONDON$accommodates, LONDON$ln_price)
cor(LONDON$bedrooms, LONDON$ln_price)
cor(LONDON$beds, LONDON$ln_price)





### PCA

pre_proc <- preProcess(LONDON[, eda_1], method = c("center", "scale")) # standardizing PCA variables 
scaled_data <- predict(pre_proc, LONDON[, eda_1])
names(scaled_data) <- paste0(eda_1, "_scaled")
LONDON <- bind_cols(LONDON, scaled_data)

scaled_vars <- paste0(eda_1, "_scaled")
pca_result <- prcomp(LONDON[, scaled_vars], center = FALSE, scale. = FALSE) #PCA on standardized features

summary(pca_result)


# Scree Plot with cumulative variance sum of PCA results
pca_var <- pca_result$sdev^2
pca_var_exp <- pca_var / sum(pca_var)
pca_cumvar <- cumsum(pca_var_exp)

scree_df <- data.frame(
  PC = factor(1:length(pca_var_exp)),
  Variance = pca_var_exp,
  Cumulative = pca_cumvar
)

ggplot(scree_df, aes(x = PC, y = Variance, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), limits = c(0, max(pca_var_exp) + 0.05)) +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained") +
  theme_minimal()

ggplot(scree_df, aes(x = PC)) +
  geom_line(aes(y = Variance, group = 1), color = "steelblue", size = 1) +
  geom_point(aes(y = Variance), size = 2, color = "steelblue") +
  geom_line(aes(y = Cumulative, group = 1), color = "orange", size = 1) +
  geom_point(aes(y = Cumulative), size = 2, color = "orange") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), limits = c(0, 1.05)) +
  labs(title = "Scree Plot with Cumulative Variance",
       x = "Principal Component",
       y = "Variance Explained") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = as.character(1:length(pca_var_exp)))


pc_scores <- as.data.frame(pca_result$x[, 1:6])
colnames(pc_scores) <- paste0("PC", 1:6)
LONDON <- bind_cols(LONDON, pc_scores)



##### Modeling

# splitting data in train and test sets (no validation -> k-fold crossvalidation)
set.seed(349)
data_split <- initial_split(LONDON, prop = 0.8)
train_data <- training(data_split)
test_data  <- testing(data_split)

train_data$price <- NULL
test_data$price <- NULL

pca_vars <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")

##### LINEAR
### OLS with PCA (top 6 PC as that is 90% of cum prop var)

pca_vars <- paste0("PC", 1:6)

pca_recipe <- recipe(ln_price ~ ., data = train_data %>% select(ln_price, all_of(pca_vars)))
ols_spec_pca <- linear_reg() %>% set_engine("lm")
ols_pca_workflow <- workflow() %>% add_model(ols_spec_pca) %>% add_recipe(pca_recipe)
ols_pca_fit <- fit(ols_pca_workflow, data = train_data)

ols_pca_preds <- predict(ols_pca_fit, new_data = train_data) %>%
  bind_cols(train_data %>% select(ln_price))
rsq_ols_pca <- rsq_trad_vec(truth = ols_pca_preds$ln_price, estimate = ols_pca_preds$.pred)
rsq_ols_pca

tidy(ols_pca_fit) %>%
  arrange(desc(abs(estimate))) %>%
  kable(digits = 4, caption = "OLS Coefficients Using Top 6 Principal Components")
glance(ols_pca_fit$fit$fit)   



train_data <- train_data[ , !(names(train_data) %in% pca_vars) ] # drop pca_vars from train and test data
test_data  <- test_data[ , !(names(test_data) %in% pca_vars) ]

### OLS with interpretable variables 

OLS_feature_vars <- c(eda_1, eda_2, "room_type", "property_type_1")

price_recipe <- recipe(ln_price ~ ., data = train_data %>% select(ln_price, all_of(OLS_feature_vars))) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

ols_spec <- linear_reg() %>% set_engine("lm")
ols_workflow <- workflow() %>% add_model(ols_spec) %>% add_recipe(price_recipe)
ols_fit <- fit(ols_workflow, data = train_data)

tidy(ols_fit) %>% arrange(desc(abs(estimate))) %>% kable(digits = 4, caption = "OLS Coefficient Estimates (Sorted by Effect Size)")


ols_preds <- predict(ols_fit, new_data = train_data) %>% bind_cols(train_data %>% select(ln_price))

rsq_ols <- rsq_trad_vec(truth = ols_preds$ln_price, estimate = ols_preds$.pred)
rsq_ols

tibble(
  Model = c("OLS with PCA", "OLS with Interpretable Vars"),
  R_squared = c(rsq_ols_pca, rsq_ols)
) %>%
  kable(digits = 4, caption = "Comparison of R-squared: OLS with PCA vs Interpretable Variables")

metrics(ols_preds, truth = ln_price, estimate = .pred) # OLS goodness of fit


ggplot(ols_preds, aes(.pred, ln_price)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "OLS: Predicted vs Actual (ln_price)", x = "Predicted", y = "Actual") # OLS Predicted vs Actual Price





### Elastic Net

elastic_feature_vars <- c(OLS_feature_vars, "neighbourhood_cleansed")

elastic_recipe <- recipe(ln_price ~ ., data = train_data %>% select(ln_price, all_of(elastic_feature_vars))) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

elastic_spec <- linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")  # penalty = λ and mixture = α
elastic_workflow <- workflow() %>% add_model(elastic_spec) %>% add_recipe(elastic_recipe)

set.seed(349)
folds <- vfold_cv(train_data, v = 5) # Cross-validation folds

elastic_grid <- grid_regular(
  penalty(range = c(-4, 0)),
  mixture(range = c(0, 1)),  # 0 = ridge, 1 = lasso
  levels = 5
)

# Tune the model
elastic_tuned <- tune_grid(
  elastic_workflow,
  resamples = folds,
  grid = elastic_grid,
  metrics = metric_set(rmse, rsq)
) # tune model

best_elastic <- select_best(elastic_tuned, metric = "rmse") # best model from RMSE
final_elastic_workflow <- finalize_workflow(elastic_workflow, best_elastic)


elastic_fit <- fit(final_elastic_workflow, data = train_data) # fit on training data
elastic_preds <- predict(elastic_fit, new_data = train_data) %>%
  bind_cols(train_data %>% select(ln_price))

tidy(elastic_fit) %>%
  arrange(desc(abs(estimate))) %>%
  kable(digits = 4, caption = "Elastic Net Coefficient Estimates (Sorted by Effect Size)")

top_elastic_features <- tidy(elastic_fit) %>%
  filter(term != "(Intercept)") %>%
  mutate(strength = abs(estimate)) %>%
  arrange(desc(strength)) %>%
  slice_head(n = 10) %>%
  select(term, strength)

top_elastic_features %>%
  kable(
    digits = 4,
    col.names = c("Feature", "Strength"),
    caption = "Top 10 Most Predictive Features – Elastic Net"
  )

rsq_elastic <- rsq_trad_vec(truth = elastic_preds$ln_price, estimate = elastic_preds$.pred)
rsq_elastic

metrics(elastic_preds, truth = ln_price, estimate = .pred) # elastic net goodness of fit

# Plot predicted vs actual
ggplot(elastic_preds, aes(.pred, ln_price)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  labs(title = "Elastic Net: Predicted vs Actual (ln_price)", x = "Predicted", y = "Actual")


## table comparing OLS and Elastic net goodness of fit

elastic_metrics <- metrics(elastic_preds, truth = ln_price, estimate = .pred) %>%
  mutate(model = "Elastic Net")
ols_metrics <- metrics(ols_preds, truth = ln_price, estimate = .pred) %>%
  mutate(model = "OLS")
linear_metrics <- bind_rows(elastic_metrics, ols_metrics) %>%
  select(model, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

linear_metrics %>%
  kable(digits = 4, caption = "Comparison of Goodness-of-Fit Metrics: Elastic Net vs OLS")





##### NON-LINEAR
### Random Forest

train_data <- train_data[ , !grepl("_scaled$", names(train_data)) ]
test_data  <- test_data[ , !grepl("_scaled$", names(test_data)) ]

set.seed(349)

rf_model <- randomForest(
  ln_price ~ .,
  data = train_data,
  ntree = 300,
  mtry = 16,
  nodesize = 5,       # Minimum terminal node size
  maxnodes = 80,      # Limit number of leaf nodes
  importance = TRUE
)

print(rf_model) 

varImpPlot(rf_model, cex = 0.4) # RF Variable Importance Plot
importance(rf_model)

top_rf_features <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column(var = "Feature") %>%
  arrange(desc(`%IncMSE`)) %>%
  slice_head(n = 10) %>%
  select(Feature, `%IncMSE`)

top_rf_features %>%
  kable(
    digits = 2,
    col.names = c("Feature", "Importance (%IncMSE)"),
    caption = "Top 10 Most Predictive Features – Random Forest (%IncMSE)"
  )

rf_preds <- predict(rf_model, newdata = test_data)
rf_results <- tibble(ln_price = test_data$ln_price, .pred = rf_preds)

ggplot(rf_results, aes(x = .pred, y = ln_price)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "green") +
  labs(title = "Figure 16 - Random Forest Predicted vs Actual Log Price",
       x = "Predicted",
       y = "Actual") +
  theme_minimal() # RF Prediction vs Actual Plot

rf_metrics <- metrics(rf_results, truth = ln_price, estimate = .pred)




### FeedForward Neural Network

set.seed(349)

fnn_vars <- c(OLS_feature_vars, "neighbourhood_cleansed")

fnn_recipe <- recipe(ln_price ~ ., data = train_data %>% select(ln_price, all_of(fnn_vars))) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) # dummy encoding and scaling

prepped_fnn <- prep(fnn_recipe)
train_fnn <- bake(prepped_fnn, new_data = NULL)
test_fnn <- bake(prepped_fnn, new_data = test_data)

fnn_model <- nnet(
  ln_price ~ .,
  data = train_fnn,
  size = 5,        # number of hidden units
  linout = TRUE,   # regression mode
  decay = 0.01,    # weight decay
  maxit = 500      # max iterations
)

fnn_preds <- as.vector(predict(fnn_model, newdata = test_fnn))

fnn_results <- tibble(
  ln_price = test_fnn$ln_price,
  .pred = fnn_preds
)

ggplot(fnn_results, aes(x = .pred, y = ln_price)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "purple") +
  labs(title = "FNN: Predicted vs Actual ln(Price)",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()

fnn_metrics <- metrics(fnn_results, truth = ln_price, estimate = .pred)
fnn_metrics


### table comparing RF and FNN Goodness of Fit Tests

nonlinear_metrics <- bind_rows(
  rf_metrics %>%
    select(.metric, .estimate) %>%
    mutate(model = "Random Forest"),
  
  fnn_metrics %>%
    select(.metric, .estimate) %>%
    mutate(model = "FNN")
) %>%
  select(model, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

nonlinear_metrics %>% kable(digits = 4, caption = "Comparison of Goodness-of-Fit Metrics: Random Forest vs FNN")


### table comparing OLS, Elastic Net, RF, and FNN Goodness of Fit Tests

all_models_metrics <- bind_rows(
  linear_metrics,      
  nonlinear_metrics    
)

all_models_metrics %>%
  arrange(desc(rsq)) %>%
  kable(digits = 4, caption = "Comparison of Goodness-of-Fit Metrics (Sorted by R-squared)")

