#Installing necessary packages
install.packages("ggplot2")
install.packages("caret")
install.packages("randonForest")
install.packages("xgboost")
install.packages("Metrics")
install.packages("corrplot")
install.packages("GGally")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")
install.packages("tidyr")

#Loading packages
library(ggplot2)
library(caret)
library(randomForest)
library(xgboost)
library(Metrics)
library(corrplot)
library(GGally)
library(dplyr)
library(readr)
library(tidyverse)
library(tidyr)

music_dataset <- read.csv("Music Dataset.csv", header = TRUE)


# Setting values for start and end date for filtering dataset for 10 years    
start_date <- as.Date("2008-01-01")
end_date <- as.Date("2018-12-31")   


#Filtering dataset to 10 years(from 2008 to 2018)
filtered_dataset <- music_dataset [music_dataset$release_date >= start_date & 
                                     music_dataset$release_date <= end_date,]
# Checking missing values
colSums(is.na(filtered_dataset))

# Removing missing values
dataset_cleaned <- na.omit(filtered_dataset)
View(dataset_cleaned)


########################################################################################

#Exploratory Data Analysis

#Inspecting Data features
str(dataset_cleaned)
summary_data <- summary(dataset_cleaned)
summary_table <- as.data.frame(summary_data)
print(summary_table)
glimpse(summary_table)

feature_audio <- names(dataset_cleaned)[c(19:31)]
feature_popularity <- names(dataset_cleaned)[c(6, 7, 16, 32:34)]


dataset_cleaned %>%
  select(all_of(feature_audio)) %>%
  gather(feature, value) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "darkorange") +
  facet_wrap(~ feature, scales = "free") +
  labs(title = "Distribution of Audio Features")

dataset_cleaned %>%
  select(all_of(feature_popularity)) %>%
  gather(feature, value) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "darkorange") +
  facet_wrap(~ feature, scales = "free") +
  labs(title = "Distribution of Popularity")


#Correlation

ggpairs(dataset_cleaned[feature_audio])

#ggcorr(dataset_cleaned[feature_audio], label = TRUE)

cor_matrix <- cor(dataset_cleaned %>% select(all_of(feature_popularity), all_of(feature_audio)),
                  use = "complete.obs")

# Correlation plot between audio features and popularity
corrplot(cor_matrix, method = "color", tl.cex = 0.8)

#Correlation plot for just  audio features
corrplot(cor(dataset_cleaned[feature_audio]), method = "ellipse")

#Scatterplots for audio features vs popularity
ggplot(dataset_cleaned, aes(audio_features.danceability, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Danceability vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.energy, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Energy vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.duration_ms, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Duration vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.key, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Key vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.tempo, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Tempo vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.instrumentalness, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Intrumentalness vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.liveness, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Liveness vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.speechiness, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Speechiness vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.acousticness, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Acousticness vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.loudness, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Loudness vs Popularity")

ggplot(dataset_cleaned, aes(audio_features.valence, songs.popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Valence vs Popularity")



########################################################################################

set.seed(42)

selected_data <- dataset_cleaned %>%
  select(
    songs.popularity,  
    audio_features.duration_ms, 
    audio_features.key, 
    audio_features.mode,
    audio_features.time_signature, 
    audio_features.acousticness, 
    audio_features.danceability,
    audio_features.energy, 
    audio_features.instrumentalness, 
    audio_features.liveness,
    audio_features.loudness,
    audio_features.speechiness,
    audio_features.valence,
    audio_features.tempo
  )

#Splitting data into training and test sets
train_index <- createDataPartition(selected_data$songs.popularity, p = 0.8, list = FALSE)
train_data <- selected_data[train_index, ]
test_data  <- selected_data[-train_index, ]

#Normalisation of numerical features
preprocess <- preProcess(train_data[, -1], method = c("center", "scale"))

normalised_train_data <- train_data
normalised_train_data[, -1] <- predict(preprocess, train_data[, -1])

normalised_test_data <- test_data
normalised_test_data[, -1] <- predict(preprocess, test_data[, -1])

########################################################################################

# 1. Linear Regression

lr_model <- lm(songs.popularity ~ ., data = normalised_train_data)
summary(lr_model)   

# Predictions + Root Mean Squared Error (RMSE)
lr_preds <- predict(lr_model, normalised_test_data)
lr_rmse <- rmse(normalised_test_data$songs.popularity, lr_preds)
cat("Linear Regression RMSE:", lr_rmse, "\n")

# R-squared function
calculate_r2 <- function(actual, predicted) {
  sum_of_squares_residual <- sum((actual - predicted)^2)
  sum_of_squares_total <- sum((actual - mean(actual))^2)
  1 - (sum_of_squares_residual / sum_of_squares_total)
}

lr_r2 <- calculate_r2(normalised_test_data$songs.popularity, lr_preds)
cat("Linear Regression R-squared:", lr_r2, "\n")


#Plotting feature importance based on the linear regresssion model
lr_imp <- data.frame(
  Feature = names(lr_model$coefficients)[-1],
  Coefficient = abs(lr_model$coefficients[-1])
)

ggplot(lr_imp, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  labs(
    title = "Linear Regression Feature Importance",
    x = "Feature",
    y = "Absolute Coefficient"
  )


########################################################################################

# 2. Random Forest

rf_model <- randomForest(
  songs.popularity ~ .,
  data = normalised_train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

rf_preds <- predict(rf_model, normalised_test_data)
rf_rmse <- rmse(normalised_test_data$songs.popularity, rf_preds)
cat("Random Forest RMSE:", rf_rmse, "\n")

rf_r2 <- calc_r2(normalised_test_data$songs.popularity, rf_preds)
cat("Random Forest R-squared:", rf_r2, "\n")

# Feature importance plot
varImpPlot(rf_model)

# Extracting feature importance from Random Forest model
rf_importance <- importance(rf_model)  

# Converting to a data frame for ggplot
rf_imp_df <- as.data.frame(rf_importance) %>%
  rownames_to_column(var = "Feature") %>%
  dplyr::rename(Importance = `%IncMSE`) %>%
  arrange(desc(Importance))

# Plotting feature importance with ggplot
ggplot(rf_imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    x = "Feature",
    y = "Importance (%IncMSE)"
  ) +
  theme_minimal()


########################################################################################

# 3. XGBoost

# Convert to DMatrix
train_matrix <- xgb.DMatrix(
  data = as.matrix(normalised_train_data[, -1]),
  label = as.numeric(normalised_train_data$songs.popularity)
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(normalised_test_data[, -1]),
  label = as.numeric(normalised_test_data$songs.popularity)
)

# Train XGBoost model
xgb_model <- xgboost::xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = 4,
    eta = 0.1
  ),
  data = train_matrix,
  nrounds = 200
)

# Predictions + RMSE
xgb_preds <- predict(xgb_model, test_matrix)
xgb_rmse <- rmse(normalised_test_data$songs.popularity, xgb_preds)
cat("XGBoost RMSE:", xgb_rmse, "\n")

xgb_r2 <- calc_r2(normalised_test_data$songs.popularity, xgb_preds)
cat("XGBoost R-squared:", xgb_r2, "\n")

# XGBoost feature importance
xgb_imp <- xgb.importance(model = xgb_model)
# Using XGBoost's plot function to plot feature importance
xgb.plot.importance(xgb_imp)

# Using Gain as the importance metric
xgb_imp_df <- xgb_imp %>%
  arrange(desc(Gain))

#Plotting feature importance with ggplot
ggplot(xgb_imp_df, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "grey") +
  coord_flip() +
  labs(
    title = "XGBoost Feature Importance",
    x = "Feature",
    y = "Gain"
  ) +
  theme_minimal()

# creating data frame with actual and predicted values of song popularity for the 3 models
results <- data.frame(
  Actual = normalised_test_data$songs.popularity,
  Linear_Regression = lr_preds,
  Random_Forest = rf_preds,
  XGBoost = xgb_preds
)

results_long <- results %>%
  pivot_longer(-Actual, names_to = "Model", values_to = "Predicted")

#Plotting the results
ggplot(results_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Song Popularity",
    x = "Actual Popularity",
    y = "Predicted Popularity"
  ) +
  theme_bw()



# Additional code used to create a faceted scatterplot for audio features vs song popularity used in the report
#dataset_long <- dataset_cleaned %>%
# select(
#    songs.popularity,
#    audio_features.danceability,
#    audio_features.energy,
#    audio_features.duration_ms,
#    audio_features.key,
#   audio_features.tempo,
#    audio_features.instrumentalness,
#    audio_features.liveness,
#    audio_features.speechiness,
#    audio_features.acousticness,
#    audio_features.loudness,
#    audio_features.valence
#  ) %>%
#  pivot_longer(
#    cols = starts_with("audio_features"),
#    names_to = "feature",
#    values_to = "value"
#  )
#dataset_long$feature <- gsub("audio_features\\.", "", dataset_long$feature)
#Faceted scatter plot
#ggplot(dataset_long, aes(x = value, y = songs.popularity)) +
#  geom_point(alpha = 0.3) +
#  geom_smooth(method = "loess", se = FALSE, color = "blue") +
#  facet_wrap(~feature, scales = "free_x") +
#  labs(
#    title = "Audio Features vs Popularity",
#    x = "Audio Feature Value",
#    y = "Popularity"
#  ) +
#  theme_minimal()

