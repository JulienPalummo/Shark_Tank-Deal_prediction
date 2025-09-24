# Loading necessary library
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("stringr")
library(stringr)
install.packages("caret")
library(caret)


# Read the Shark Tank dataset
shark_tank_data <- read.csv('C:/Users/julie/Downloads/shark_tank.csv')

##############################
########Exploration###########
##############################

summary(shark_tank_data)

# Function to determine plot type based on data type
plot_column <- function(data, column_name) {
  if (is.numeric(data[[column_name]])) {
    p <- ggplot(data, aes_string(x = column_name)) +
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      labs(title = paste("Distribution of", column_name),
           x = column_name,
           y = "Frequency")
  } else {
    p <- ggplot(data, aes_string(x = column_name)) +
      geom_bar(fill = "steelblue") +
      labs(title = paste("Frequency of", column_name),
           x = column_name,
           y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  print(p)
}

# Loop over each column in the dataset
for (column_name in names(shark_tank_data)) {
  plot_column(shark_tank_data, column_name)
}

# Deal Distribution by Category
ggplot(shark_tank_data, aes(x = category, fill = as.factor(deal))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Deal Distribution by Category",
       x = "Category",
       y = "Percentage",
       fill = "Deal Made") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
library(gridExtra)

data <- shark_tank_data

# Scatter Plot of Valuation vs. Deal Outcome
plot1 <- ggplot(data, aes(x=valuation, y=deal)) +
  geom_point(aes(color=as.factor(deal)), alpha=0.5) +
  ggtitle("Valuation vs. Deal Outcome") +
  theme_minimal()

# Bar Plot of Number of Deals per Season
plot2 <- ggplot(data, aes(x=as.factor(season))) +
  geom_bar(aes(fill=as.factor(deal))) +
  ggtitle("Number of Deals per Season") +
  theme_minimal()

# Scatter Plot of Asked Amount vs. Exchange for Stake
plot3 <- ggplot(data, aes(x=askedFor, y=exchangeForStake)) +
  geom_point(aes(color=as.factor(deal)), alpha=0.5) +
  ggtitle("Asked Amount vs. Exchange for Stake") +
  theme_minimal()

# Boxplot of Valuation by Deal Outcome
plot4 <- ggplot(data, aes(x=as.factor(deal), y=valuation, fill=as.factor(deal))) +
  geom_boxplot() +
  ggtitle("Valuation by Deal Outcome") +
  theme_minimal()

# Histogram of Exchange for Stake
plot5 <- ggplot(data, aes(x=exchangeForStake)) +
  geom_histogram(bins=30, fill="steelblue", color="black") +
  ggtitle("Distribution of Exchange for Stake") +
  theme_minimal()

# Scatter Plot of Episode vs. Valuation with Deal Outcome
plot6 <- ggplot(data, aes(x=episode, y=valuation, color=as.factor(deal))) +
  geom_point(alpha=0.5) +
  ggtitle("Episode vs. Valuation by Deal Outcome") +
  theme_minimal()

# Histogram of Valuation Distribution
plot7 <- ggplot(shark_tank_data, aes(x=valuation)) +
  geom_histogram(bins=30, fill="blue", color="black") +
  ggtitle("Distribution of Valuations") +
  theme_minimal()

# Boxplot of Asked Amount by Deal Outcome
plot8 <- ggplot(data, aes(x=as.factor(deal), y=askedFor, fill=as.factor(deal))) +
  geom_boxplot() +
  ggtitle("Asked Amount by Deal Outcome") +
  theme_minimal()

# Set the size of the plotting window to 8.5 x 11 inches
options(repr.plot.width=8.5, repr.plot.height=11)

# Combine all plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol=2, heights=c(1, 1, 1, 1))


##############################
########Pre-processing########
##############################

# Convert 'deal' to a binary variable (0 for no deal, 1 for deal)
shark_tank_data$deal <- as.integer(shark_tank_data$deal == 'TRUE')

# Convert 'Multiple_entrepreneurs' to a binary variable (0 for no, 1 for yes)
shark_tank_data$Multiple.Entreprenuers <- as.integer(shark_tank_data$Multiple.Entreprenuers == 'TRUE')

# Identifying all unique sharks
unique_sharks <- unique(unlist(shark_tank_data[, c('shark1', 'shark2', 'shark3', 'shark4', 'shark5')]))

# First, let's create binary columns for shark presence
unique_sharks <- unique(unlist(shark_tank_data[, c('shark1', 'shark2', 'shark3', 'shark4', 'shark5')]))
for (shark in unique_sharks) {
  if (!is.na(shark)) {
    shark_col_name <- paste0('shark_', gsub(" ", ".", shark))
    shark_col_name <- paste0('shark_', gsub(" |'", ".", shark))
    shark_tank_data[[shark_col_name]] <- apply(shark_tank_data[, c('shark1', 'shark2', 'shark3', 'shark4', 'shark5')], 1, function(x) as.integer(shark %in% x))
  }
}


# Define a mapping from original categories to the new groups
category_to_group_mapping <- list(
  'Technology' = c('Electronics', 'Mobile Apps', 'Productivity Tools','Online Services'),
  'Food.Beverage' = c('Alcoholic Beverages', 'Baby and Children\'s Food', 'Non-Alcoholic Beverages', 'Specialty Food', 'Wine Accessories'),
  'Fashion' = c('Baby and Children\'s Apparel and Accessories', 'Fashion Accessories', 'Fitness Apparel and Accessories', 'Maternity', 'Men\'s Accessories', 'Men and Women\'s Accessories', 'Men and Women\'s Apparel', 'Men and Women\'s Shoes', 'Women\'s Accessories', 'Women\'s Apparel', 'Women\'s Shoes', 'Undergarments and Basics', 'Costumes'),
  'Health.Wellness' = c('Baby and Child Care','Cycling','Golf Products', 'Fitness Equipment', 'Fitness Programs','Outdoor Recreation', 'Health and Well-Being', 'Homeopathic Remedies', 'Personal Care and Cosmetics'),
  'Entertainment' = c('Baby and Children\'s Entertainment', 'Entertainment', 'Music', 'Toys and Games','Holiday Cheer','Party Supplies' ),
  'Home' = c('Baby and Children\'s Bedding','Furniture','Gardening','Home Accessories','Home Improvement','Home Security Solutions', 'Kitchen Tools','Storage and Cleaning Products'),
  'Other' = c('Automotive', 'Consumer Services', 'Education',  'Novelties' ,  'Pest Control', 'Pet Products', 'Professional Services', 'Water Bottles', 'Weddings')
)

# Function to map original category to new group
map_category_to_group <- function(category) {
  for (group in names(category_to_group_mapping)) {
    if (category %in% category_to_group_mapping[[group]]) {
      return(group)
    }
  }
  return('Other') # Default group if no match is found
}

# Apply this mapping to the dataset
shark_tank_data$grouped_category <- sapply(shark_tank_data$category, map_category_to_group)

# one-hot encode these grouped categories
grouped_category_columns <- unique(shark_tank_data$grouped_category)
shark_tank_data<- cbind(shark_tank_data, sapply(grouped_category_columns, function(x) as.integer(shark_tank_data$grouped_category == x)))
colnames(shark_tank_data)[(ncol(shark_tank_data) - length(grouped_category_columns) + 1):ncol(shark_tank_data)] <- paste0("grouped_category_", grouped_category_columns)

# Count the number of values in each grouped category
category_counts <- table(shark_tank_data$grouped_category)

# Print the counts
print(category_counts)


library(dplyr)
library(stringr)
# Keeping only the state from the Location and extracting two-letter state codes
shark_tank_data <- shark_tank_data %>%
  mutate(State = str_extract(str_split(location, pattern = ",", simplify = TRUE)[,2], "\\b[A-Z]{2}\\b")) %>%
  select(-location) # remove the original Location column
# Get the state counts
state_counts <- table(shark_tank_data$State)

# Find the top 5 states with the highest counts
top_5_states <- head(sort(state_counts, decreasing = TRUE), 5)

# Create a new column "State_Grouped" to store the grouped states
shark_tank_data$State_Grouped <- ifelse(shark_tank_data$State %in% names(top_5_states), shark_tank_data$State, "Other")

# One-Hot Encoding for States
state_columns <- unique(na.omit(shark_tank_data$State_Grouped))
shark_tank_data <- cbind(shark_tank_data, sapply(state_columns, function(x) as.integer(shark_tank_data$State_Grouped == x)))
colnames(shark_tank_data)[(ncol(shark_tank_data) - length(state_columns) + 1):ncol(shark_tank_data)] <- paste0("state_", state_columns)


# Dropping unnecessary columns
columns_to_drop <- c('category','description', 'entrepreneurs', 'website', 'title', 'episode_season', 'shark1', 'shark2', 'shark3', 'shark4', 'shark5','episode.season','grouped_category','State_Grouped','State')
shark_tank_data_processed <- shark_tank_data[, !(names(shark_tank_data) %in% columns_to_drop)]


remove_outliers_percentile <- function(data, column, lower_perc = 0.01, upper_perc = 0.99) {
  lower_bound <- quantile(data[[column]], lower_perc, na.rm = TRUE)
  upper_bound <- quantile(data[[column]], upper_perc, na.rm = TRUE)
  
  data <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  return(data)
}

numeric_columns <- sapply(shark_tank_data_processed, is.numeric)
for (column_name in names(shark_tank_data_processed)[numeric_columns]) {
  shark_tank_data_processed <- remove_outliers_percentile(shark_tank_data_processed, column_name)
}

##### Correlation between variables #####
# Load necessary libraries
library(corrplot)
library(dplyr)

# Identify constant columns
constant_columns <- sapply(shark_tank_data_processed, function(x) length(unique(x)) == 1)

# Remove constant columns from the data
shark_tank_data_no_constants <- shark_tank_data_processed[, !constant_columns]

# Calculate the correlation matrix using numeric columns
numeric_data <- shark_tank_data_no_constants %>% select_if(is.numeric)
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Load the necessary libraries
library(ggplot2)
library(reshape2)

# Create a data frame from the correlation matrix
correlation_data <- melt(correlation_matrix)

# Set the title for the correlation matrix
matrix_title <- "Correlation Matrix of Numeric Data"

# Create a custom correlation heatmap with ggplot2 using a simpler color palette
ggplot(correlation_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = matrix_title) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()

attach(shark_tank_data_processed)
library(car)
install.packages('Metrics')
library(Metrics)

# Fit your linear regression model with your specific dataset
lm_model <- lm(deal ~ askedFor + episode + exchangeForStake + grouped_category_Entertainment
               + grouped_category_Fashion + grouped_category_Food.Beverage + grouped_category_Health.Wellness
               + grouped_category_Home + grouped_category_Other + grouped_category_Technology
               + Multiple.Entreprenuers + season + shark_Barbara.Corcoran + shark_Daymond.John
               + shark_Jeff.Foxworthy + shark_John.Paul.DeJoria + shark_Kevin.Harrington + shark_Kevin.O.Leary
               + shark_Lori.Greiner + shark_Mark.Cuban + shark_Nick.Woodman + shark_Robert.Herjavec
               + shark_Steve.Tisch + state_CA + state_FL + state_IL + state_NY + state_Other + state_TX + valuation, 
               data = shark_tank_data_processed)

lm_stepwise <- step(lm_model, direction = "both", trace = 0)

# Calculate VIF scores
vif(lm_stepwise)




##############################
########Model Building########
##############################

## Random Forest ##

install.packages("tree")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("gbm")
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
install.packages("caret")
library(caret)

# Attach data
attach(shark_tank_data_processed)


# Splitting data into training and testing sets
set.seed(123) 
train_index <- sample(1:nrow(shark_tank_data_processed), 0.8 * nrow(shark_tank_data_processed))
train_data <- shark_tank_data_processed[train_index, ]
test_data <- shark_tank_data_processed[-train_index, ]


# Initial tree
mytree <- rpart(deal ~ ., data = train_data, control = rpart.control(cp = 0.01))
rpart.plot(mytree)
summary(mytree)

# Overfitted tree
myoverfittedtree <- rpart(deal ~ ., data = train_data, control = rpart.control(cp = 0.00003))
rpart.plot(myoverfittedtree)
printcp(myoverfittedtree)
plotcp(myoverfittedtree)

# Optimal cp
opt_cp <- myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]), "CP"]

# Best tree
myoptimaltree <- rpart(deal ~ ., data = train_data, control = rpart.control(cp = opt_cp))
rpart.plot(myoptimaltree)

train_data <- data.frame(lapply(train_data, as.numeric))

# Building the Random Forest
myforest <- randomForest(deal ~ ., data = train_data, ntree = 500, importance = TRUE, na.action = na.omit)
print(myforest)
importance(myforest)
varImpPlot(myforest)

# Extracting importance metrics
importance_metrics <- importance(myforest)

# Sort and rank variables based on %IncMSE
rank_incMSE <- sort(importance_metrics[,"%IncMSE"], decreasing = TRUE)
top_incMSE <- head(rank_incMSE, 10)

# Sort and rank variables based on IncNodePurity
rank_incNodePurity <- sort(importance_metrics[,"IncNodePurity"], decreasing = TRUE)
top_incNodePurity <- head(rank_incNodePurity, 10)

# Print the top 10 variables based on %IncMSE
cat("Top 10 Variables by %IncMSE:\n")
print(top_incMSE)

# Print the top 10 variables based on IncNodePurity
cat("\nTop 10 Variables by IncNodePurity:\n")
print(top_incNodePurity)



### Generalized boosted models (GBM)

install.packages("gbm")
library(gbm)
set.seed (1)
boosted=gbm(deal~.,data=train_data,distribution= "gaussian",n.trees=10000, interaction.depth=4)
summary(boosted) 

library(knitr)

# Create a data frame with the summary data
boosted_summary <- data.frame(
  Variable = c("episode", "valuation", "askedFor", "exchangeForStake", "season", 
               "state_Other", "shark_Barbara.Corcoran", "Multiple.Entreprenuers", 
               "grouped_category_Other", "grouped_category_Home", "shark_Lori.Greiner", 
               "state_CA", "grouped_category_Food.Beverage", 
               "grouped_category_Health.Wellness", "grouped_category_Entertainment", 
               "grouped_category_Fashion", "grouped_category_Technology", 
               "shark_Daymond.John", "state_NY", "shark_Kevin.Harrington", 
               "shark_Mark.Cuban", "state_FL", "state_TX", "state_IL", 
               "shark_Robert.Herjavec", "shark_Kevin.O.Leary", "shark_Steve.Tisch", 
               "shark_Jeff.Foxworthy", "shark_John.Paul.DeJoria", "shark_Nick.Woodman"),
  RelativeInfluence = c(21.5593323, 19.7448793, 13.5145671, 9.1762258, 6.8994012,
                        3.6793542, 2.6346201, 2.4138544, 2.2862727, 1.8968071, 
                        1.8653465, 1.6893675, 1.6840116, 1.6084552, 1.6051649, 
                        1.5842428, 1.5018153, 1.3654692, 0.9548347, 0.6014441, 
                        0.5983239, 0.5695098, 0.3566185, 0.2100817, 0.0000000, 
                        0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000)
)

# Generate the table using kable
kable(boosted_summary, format = "markdown", caption = "Summary of Generalized Boosted Model")

# Load necessary libraries
library(randomForest)
library(gbm)

# Predict on the test data
predictions <- predict(myforest, newdata = test_data, type = "response")

# Convert to binary predictions based on a threshold (0.5)
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)

# Ensure that both binary predictions and actual deal values are integers
binary_predictions <- as.integer(binary_predictions)
deal_values <- as.integer(test_data$deal)

# Convert both binary predictions and actual deal values to factors
binary_predictions_factor <- factor(binary_predictions, levels = c(0, 1))
deal_factor <- factor(deal_values, levels = c(0, 1))

# Load necessary library for confusionMatrix
library(caret)

# Create the confusion matrix
confusion_matrix <- confusionMatrix(binary_predictions_factor, deal_factor)

# Print the confusion matrix
print(confusion_matrix)

# Calculate performance metrics
accuracy <- confusion_matrix$overall['Accuracy']
f1_score <- confusion_matrix$byClass['F1']

# Output the model performance
cat("Accuracy:", accuracy, "\n")
cat("F1 Score:", f1_score, "\n")


## Obtain MSE
predicted_score=predict(myforest, newdata=test_data, n.trees=10000)
mean((predicted_score - train_data$deal)^2) 

## Prediction Example
example_data <- data.frame(
  askedFor = 50000,  # Example value
  episode = 10,      # Example value
  exchangeForStake = 15, # Example value
  valuation = 200000, # Example value
  season = 4,        # Example value
  Multiple.Entreprenuers = 1, # Example value (1 for Yes, 0 for No)
  shark_Barbara.Corcoran = 1, # Example value (1 if present, 0 if not)
  shark_Lori.Greiner = 1,     # Example value (1 if present, 0 if not)
  shark_Robert.Herjavec = 0,  # Example value (1 if present, 0 if not)
  shark_Kevin.O.Leary = 0,    # Example value (1 if present, 0 if not)
  shark_Steve.Tisch = 1,      # Example value (1 if present, 0 if not)
  shark_Daymond.John = 0,     # Example value (1 if present, 0 if not)
  shark_Jeff.Foxworthy = 0,   # Example value (1 if present, 0 if not)
  shark_Mark.Cuban = 1,       # Example value (1 if present, 0 if not)
  shark_Kevin.Harrington = 0, # Example value (1 if present, 0 if not)
  shark_John.Paul.DeJoria = 1, # Example value (1 if present, 0 if not)
  shark_Nick.Woodman = 0,     # Example value (1 if present, 0 if not)
  grouped_category_Other = 0, # Example value (1 if present, 0 if not)
  grouped_category_Food.Beverage = 0, # Example value (1 if present, 0 if not)
  grouped_category_Health.Wellness = 0, # Example value (1 if present, 0 if not)
  grouped_category_Fashion = 0, # Example value (1 if present, 0 if not)
  grouped_category_Technology = 0, # Example value (1 if present, 0 if not)
  grouped_category_Home = 1, # Example value (1 if present, 0 if not)
  grouped_category_Entertainment = 0, # Example value (1 if present, 0 if not)
  state_Other = 0,   # Example value (1 if from Other, 0 if not)
  state_CA = 1,      # Example value (1 if from CA, 0 if not)
  state_NY = 0,      # Example value (1 if from NY, 0 if not)
  state_TX = 0,      # Example value (1 if from TX, 0 if not)
  state_FL = 0,      # Example value (1 if from FL, 0 if not)
  state_IL = 0       # Example value (1 if from IL, 0 if not)
)


# Prediction using Random Forest
predicted_deal_rf <- predict(myforest, newdata = example_data)
print(predicted_deal_rf)

# Prediction using GBM
predicted_deal_gbm <- predict(boosted, newdata = example_data, n.trees = 10000)
print(predicted_deal_gbm)


##############################
#####2nd Model Building#######
##############################
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Attach data
attach(shark_tank_data_processed)

# Splitting the data into training and testing sets
set.seed(123) 
training_indices <- sample(1:nrow(shark_tank_data_processed), 0.8 * nrow(shark_tank_data_processed))
train_data <- shark_tank_data_processed[training_indices, ]
test_data <- shark_tank_data_processed[-training_indices, ]


# Filter the dataset based on your input conditions
input_category <- "Food & Beverages"  # Example category
input_multiple_entrepreneurs <- 0  # 0 for solo, 1 for multiple
input_state <- "CA"  # Example state (California)

filtered_data <- shark_tank_data_processed %>%
  filter(grouped_category_Food.Beverage == 1, 
         Multiple.Entreprenuers == input_multiple_entrepreneurs,
         state_CA == 1)

# Calculate the acceptance rate for each shark
shark_acceptance_rates <- sapply(unique_sharks, function(shark) {
  shark_col_name <- paste0('shark_', gsub(" ", ".", gsub("'", "", shark)))  
  shark_presence <- sum(filtered_data[[shark_col_name]], na.rm = TRUE)
  shark_deals <- sum(filtered_data[[shark_col_name]] & filtered_data$deal, na.rm = TRUE)
  
  if (shark_presence > 0) {
    return(shark_deals / shark_presence)
  } else {
    return(NA)
  }
})

# Create a dataframe for shark acceptance rates
shark_rates_df <- data.frame(shark = unique_sharks, acceptance_rate = shark_acceptance_rates)

# Remove NA values and sort sharks by their acceptance rates
top_sharks <- shark_rates_df %>%
  filter(!is.na(acceptance_rate)) %>%
  arrange(desc(acceptance_rate)) %>%
  head(5)  # Select the top 5 sharks

# Output the top sharks
print(top_sharks)

# Perform clustering on the filtered dataset
km_result <- kmeans(filtered_data$episode, centers = 3)

# Add cluster assignments to the filtered data
filtered_data$cluster <- km_result$cluster

# Calculate deal success rate for each cluster
deal_success_rates <- filtered_data %>%
  group_by(cluster) %>%
  summarize(deal_rate = mean(deal))

# Identify the cluster with the highest deal success rate
best_cluster <- deal_success_rates[which.max(deal_success_rates$deal_rate), ]$cluster

# Output the best cluster
print(best_cluster)

# Merge the deal rates back into the original filtered dataset
filtered_data <- merge(filtered_data, deal_success_rates, by = "cluster")

# Plotting all clusters with deal rate on the Y-axis
ggplot(filtered_data, aes(x = episode, y = deal_rate, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "Cluster Visualization with Deal Rate",
       x = "Episode Number",
       y = "Deal Rate",
       color = "Cluster") +
  theme_minimal()

# Filter data to only include the best cluster
best_cluster_data <- filtered_data %>%
  filter(cluster == best_cluster)

# Find the range of episodes in the best cluster
min_episode <- min(best_cluster_data$episode, na.rm = TRUE)
max_episode <- max(best_cluster_data$episode, na.rm = TRUE)

# Output the episode range
paste("The best cluster (Cluster", best_cluster, ") ranges from episode", min_episode, "to episode", max_episode)

# Determining the best time in the season to present
best_time_to_present <- if (best_cluster == 1) "beginning" else if (best_cluster == 2) "middle" else "end"

# Output
cat("Top 5 Sharks to Target: ", paste(top_sharks$shark, collapse = ", "), "\n")
cat("Best Time in the Season to Present: ", best_time_to_present, "\n")


#### Using the Random Forest Model to predict probability of a deal with these values

# Creating a new data frame for prediction
prediction_data <- data.frame(matrix(ncol = ncol(shark_tank_data_processed), nrow = 1))
colnames(prediction_data) <- colnames(shark_tank_data_processed)

# Resetting all columns to 0 or their mean for numerical columns
for (col_name in colnames(prediction_data)) {
  if (is.numeric(shark_tank_data_processed[[col_name]])) {
    prediction_data[[col_name]] <- mean(shark_tank_data_processed[[col_name]], na.rm = TRUE)
  } else {
    prediction_data[[col_name]] <- 0
  }
}

# Set the top sharks to 1 (present)
prediction_data$shark_DBarbara.Corcoran <- 1
prediction_data$shark_Lori.Greiner <- 1
prediction_data$shark_Mark.Cuban <- 1
prediction_data$shark_Robert.Herjavec <- 1
prediction_data$shark_Daymond.John <- 1

# Set other conditions
prediction_data$grouped_category_Food.Beverage <- 1  # Assuming interest in the "Food & Beverage" category
prediction_data$Multiple.Entreprenuers <- 0       # Assuming a single entrepreneur
prediction_data$state_CA <- 1                     # Assuming the entrepreneur is from California
prediction_data$episode <- 23                      # Assuming the presentation is in the beginning of the season

# Compute expected deal value
predicted_deal_rf <- predict(myforest, newdata = prediction_data)

# Output the predicted deal value
cat("Expected Deal Probability: ", predicted_deal_rf, "\n")
