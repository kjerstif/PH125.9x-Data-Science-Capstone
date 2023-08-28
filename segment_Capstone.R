## ----setup, include=TRUE,results='hide',message=FALSE------------------------------------------------------------------------------



if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(kknn)) install.packages("kknn", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("gt", repos = "http://cran.us.r-project.org")

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(kknn)
library(e1071)
library(tidyverse)
library(formatR)
library(RColorBrewer)
library(tidyr)
library(gt)

knitr::opts_chunk$set(echo = TRUE,warning = FALSE,size="tiny", tidy = TRUE,message=FALSE)


# Required Libraries
library(readxl)

# Download Excel file from GitHub to a temporary local path
url <- "https://raw.githubusercontent.com/kjerstif/PH125.9x-Data-Science-Capstone/main/Capstone_data.xlsx"
local_path <- tempfile(fileext = ".xlsx")
download.file(url, local_path, mode = "wb")

# Read the downloaded Excel file
data <- read_excel(local_path)






## ----data-cleaning,include=TRUE----------------------------------------------------------------------------------------------------

# Display the first few rows
head(data)

# Summary of data
summary(data)

# how many respondents
length(unique(data$RespondentID))


#see unique responses for one q
unique(data$`Q9c_1 A high tax level is necessary to maintain important public services`)


# Convert Segment to factor and inspect it
data$Segment <- as.factor(data$Segment)
table(data$Segment)


#splitting the data
set.seed(123)
splitIndex <- createDataPartition(data$Segment, p = 0.8, list = TRUE, times = 1)
train_data <- data[splitIndex$Resample1, ]
test_data <- data[-splitIndex$Resample1, ]



## ----segment,include=TRUE,tidy = TRUE----------------------------------------------------------------------------------------------


ggplot(train_data, aes(x = Segment, fill = Segment)) +
 geom_bar(width=0.7) +
 geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
 scale_fill_brewer(palette="Pastel1") +
 theme_minimal() +
 labs(title = "Distribution of Segment in Training Data", x = "Segment", y = NULL) +
 theme(legend.position = "none", axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())




## ----Q1,include=FALSE,out.width="50%", fig.align='left'----------------------------------------------------------------------------
# Define the desired order
desired_order <- c("Completely disagree", "Partially disagree", "Absolutely impossible to answer", "Partially agree", "Completely agree")

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)

# Reshape the data to long format
long_data <- train_data %>%
  gather(key = "Question", value = "Response", 5:16)

# Define a function to plot for a specific question
plot_question <- function(data, question_name) {
  filtered_data <- data %>% filter(Question == question_name)
  
  ggplot(filtered_data, aes(x = Response, fill = Response)) +
    geom_bar(width=0.7) +
    geom_text(stat='count', aes(label=..count..), vjust=-0.5, position=position_dodge(width=0.7)) +
    scale_fill_brewer(palette="Pastel1") +
    scale_x_discrete(limits = desired_order) +
    theme_minimal() +
    labs(title =  question_name, 
         x = "Response Alternatives", y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none", axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
}

# Loop through unique questions in the long data and plot them
for (question_name in unique(long_data$Question)) {
  print(plot_question(long_data, question_name))
}




## ----questions, include=TRUE,out.width="70%", fig.align='left'---------------------------------------------------------------------


# First, make sure we use the original dataset's column names
question_cols_original <- colnames(data)[5:16]

# Reshape the data to long format with original column names
long_data_original <- data %>%
  gather(key = "Question", value = "Response", 5:16)

# Create a data frame with percentages
long_data_percent <- long_data_original %>%
  group_by(Question, Segment, Response) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count), Percentage = (Count/Total)*100) %>%
  ungroup()

# Define the desired order
desired_order <- c("Completely disagree", "Partially disagree", "Absolutely impossible to answer", "Partially agree", "Completely agree")

# Plotting function with blank legend title
plot_question_percent <- function(data, question_name) {
  
  filtered_data <- data %>% filter(Question == question_name)
  
  ggplot(filtered_data, aes(x = Segment, y = Percentage, fill = factor(Response, levels = desired_order))) +
    geom_bar(stat="identity", width=0.7, position = "fill") +
    geom_text(aes(label = sprintf("%.0f%%", Percentage)), position = position_fill(vjust = 0.5)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_fill_brewer(palette="Pastel1", name="") + 
    scale_x_discrete(limits = unique(data$Segment)) +
    theme_minimal() +
    labs(title = question_name, 
         x = "Segment", y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
}

# Loop through unique questions in the long data and plot them
for (question_name in question_cols_original) {
  print(plot_question_percent(long_data_percent, question_name))
}





## ----spider, include=TRUE,out.width="70%", fig.align='left'------------------------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(fmsb)
library(gridExtra)
library(grid)


# Aggregate data for spider chart
spider_data <- long_data_percent %>%
  filter(Response %in% c("Partially agree", "Completely agree")) %>%
  group_by(Segment, Question) %>%
  summarise(Total_Percentage = sum(Percentage)) %>%
  spread(key = Question, value = Total_Percentage)

# Store the full question names for the table
full_question_names <- data.frame(Question = colnames(spider_data)[-1])

# Truncate question names to the first 6 letters for the spider chart
colnames(spider_data)[-1] <- substr(colnames(spider_data)[-1], 1, 6)

# Prepare data for spider chart
max_values <- apply(spider_data[, -1], 2, max)
min_values <- rep(0, length(max_values))

# Set the segments as row names
row.names(spider_data) <- spider_data$Segment

# Bind the min and max values, and remove the Segment column
spider_data_final <- rbind(min_values, spider_data[, -1], max_values)

# Define colors for each segment
colors <- rainbow(n = nrow(spider_data))



# Create the spider chart with defined colors
spider_plot <- radarchart(spider_data_final, pcol=colors, plwd=2, cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0, 100, 20), cglwd=0.8, title="Partially agree, Completely agree", show.legend = FALSE)


# Create the table with full question names
table_plot <- tableGrob(full_question_names, 
                        theme = ttheme_default(base_size = 8, 
                                               core = list(fg_params=list(hjust=0, x=0))))
# Create a custom legend and grab it as a grob
legend("topright", legend = spider_data$Segment, fill = colors, title = "Segment", bty = "n")
legend_grob <- grid::grid.grab()

# Arrange the spider chart, custom legend, and table side by side
grid.arrange(spider_plot, legend_grob, table_plot, ncol=3)


## ----Model, include=TRUE,out.width="100%", fig.align='left'------------------------------------------------------------------------
   
  
  # Ensure column names are the simplified versions
  colnames(train_data)[5:16] <- paste("Q", 1:12, sep="")
  colnames(test_data)[5:16] <- paste("Q", 1:12, sep="")
  
  # Construct the formula with the new column names
  model_formula <- as.formula(paste("Segment ~", paste(paste("Q", 1:12, sep=""), collapse = " + ")))
  
  # Random Forest model
  set.seed(123)
  rf_model <- randomForest(model_formula, data = train_data, importance = TRUE, ntree = 100)
  
  # Predict using the Random Forest model
  rf_predictions <- predict(rf_model, test_data)
  
  # Convert character columns to factors
  train_data[,5:16] <- lapply(train_data[,5:16], factor)
  test_data[,5:16] <- lapply(test_data[,5:16], factor)
  # Create the dummy variables again
  train_data_dummies <- model.matrix(~ . - 1, data = train_data[,5:16])
  test_data_dummies <- model.matrix(~ . - 1, data = test_data[,5:16])
  
  # Add the Segment column back to the datasets
  train_data_dummies <- as.data.frame(train_data_dummies)
  train_data_dummies$Segment <- train_data$Segment
  
  test_data_dummies <- as.data.frame(test_data_dummies)
  test_data_dummies$Segment <- test_data$Segment
  
  # k-Nearest Neighbors model with dummy variables
  set.seed(123)
  knn_predictions <- kknn(Segment ~ ., train_data_dummies, test_data_dummies, k = 5, distance = 1, kernel = "optimal")
  
  
  # Naive Bayes model
  set.seed(123)
  nb_model <- naiveBayes(model_formula, data = train_data)
  nb_predictions <- predict(nb_model, test_data[, 5:16])
  
  # Evaluate model performance
  rf_accuracy <- sum(diag(confusionMatrix(rf_predictions, test_data$Segment)$table)) / sum(confusionMatrix(rf_predictions, test_data$Segment)$table)
  knn_accuracy <- sum(diag(confusionMatrix(knn_predictions$fit, test_data$Segment)$table)) / sum(confusionMatrix(knn_predictions$fit, test_data$Segment)$table)
  nb_accuracy <- sum(diag(confusionMatrix(nb_predictions, test_data$Segment)$table)) / sum(confusionMatrix(nb_predictions, test_data$Segment)$table)
  
 
  # Create the data frame
  results_df <- data.frame(
    Model = c("Random Forest", "k-Nearest Neighbors", "Naive Bayes"),
    Accuracy = sprintf("%.2f%%", c(rf_accuracy, knn_accuracy, nb_accuracy) * 100)  # Convert to percentage with a % sign
  )
  
  # Display it as a table
  results_df %>%
    gt()%>%
  tab_header(title = "Model Accuracy")

 



## ----model_extended, include=FALSE,out.width="70%", fig.align='left'---------------------------------------------------------------
# Rename the datasets
train_data_extended <- train_data
test_data_extended <- test_data

# Include the background variables in the column names simplification
colnames(train_data_extended)[c(5:16)] <- c(paste("Q", 1:12, sep=""))
colnames(test_data_extended)[c(5:16)] <- c(paste("Q", 1:12, sep=""))

# Update the model formula to include the background variables
model_formula <- as.formula(paste("Segment ~", paste(c(paste("Q", 1:12, sep=""), "Age", "Gender", "County"), collapse = " + ")))

# Random Forest model
set.seed(123)
rf_model_extended <- randomForest(model_formula, data = train_data_extended, importance = TRUE, ntree = 100)
rf_predictions_extended <- predict(rf_model_extended, test_data_extended)

# Convert character columns to factors
train_data_extended[,2:16] <- lapply(train_data_extended[,2:16], factor)
test_data_extended[,2:16] <- lapply(test_data_extended[,2:16], factor)

# Create the dummy variables including the background variables
train_data_dummies_extended <- model.matrix(~ . - 1, data = train_data_extended[,2:16])
test_data_dummies_extended <- model.matrix(~ . - 1, data = test_data_extended[,2:16])

# Add the Segment column back to the datasets
train_data_dummies_extended <- as.data.frame(train_data_dummies_extended)
train_data_dummies_extended$Segment <- train_data_extended$Segment

test_data_dummies_extended <- as.data.frame(test_data_dummies_extended)
test_data_dummies_extended$Segment <- test_data_extended$Segment

# Ensure both datasets have the same dummy columns
missing_cols <- setdiff(names(train_data_dummies_extended), names(test_data_dummies_extended))
for (col in missing_cols) {
  test_data_dummies_extended[[col]] <- 0
}

# Similarly, ensure train_data_dummies has columns that might only appear in test_data_dummies
missing_cols <- setdiff(names(test_data_dummies_extended), names(train_data_dummies_extended))
for (col in missing_cols) {
  train_data_dummies_extended[[col]] <- 0
}

# Order the columns to ensure they're in the same order
train_data_dummies_extended <- train_data_dummies_extended[, names(test_data_dummies_extended)]

# k-Nearest Neighbors model with dummy variables
set.seed(123)
knn_predictions_extended <- kknn(Segment ~ ., train_data_dummies_extended, test_data_dummies_extended, k = 5, distance = 1, kernel = "optimal")

# Naive Bayes model
set.seed(123)
nb_model_extended <- naiveBayes(model_formula, data = train_data_extended)
nb_predictions_extended <- predict(nb_model_extended, test_data_extended[, 2:16])  # Ensure this encompasses the correct columns

# Evaluate model performance
rf_accuracy_extended <- sum(diag(confusionMatrix(rf_predictions_extended, test_data_extended$Segment)$table)) / sum(confusionMatrix(rf_predictions_extended, test_data_extended$Segment)$table)
knn_accuracy_extended <- sum(diag(confusionMatrix(knn_predictions_extended$fit, test_data_extended$Segment)$table)) / sum(confusionMatrix(knn_predictions_extended$fit, test_data_extended$Segment)$table)
nb_accuracy_extended <- sum(diag(confusionMatrix(nb_predictions_extended, test_data_extended$Segment)$table)) / sum(confusionMatrix(nb_predictions_extended, test_data_extended$Segment)$table)

# Display the results in a table
results_df_extended <- data.frame(
  Model = c("Random Forest (Extended)", "k-Nearest Neighbors (Extended)", "Naive Bayes (Extended)"),
  Accuracy = sprintf("%.2f%%", c(rf_accuracy_extended, knn_accuracy_extended, nb_accuracy_extended) * 100)  # Convert to percentage with a % sign
)

results_df_extended %>%
  gt() %>%
  tab_header(title = "Model Accuracy with Extended Variables")




## ----model_results, include=TRUE,out.width="100%", fig.align='left'----------------------------------------------------------------
# Create the data frames
results_df <- data.frame(
  Model = c("Random Forest", "k-Nearest Neighbors", "Naive Bayes"),
  Accuracy_Standard = sprintf("%.2f%%", c(rf_accuracy, knn_accuracy, nb_accuracy) * 100)  # Convert to percentage with a % sign
)

results_df_extended <- data.frame(
  Model = c("Random Forest", "k-Nearest Neighbors", "Naive Bayes"),
  Accuracy_Extended = sprintf("%.2f%%", c(rf_accuracy_extended, knn_accuracy_extended, nb_accuracy_extended) * 100)  # Convert to percentage with a % sign
)
# Merge the data frames
combined_df <- merge(results_df, results_df_extended, by = "Model", all = TRUE, sort = FALSE)

# Reorder the Model column to maintain desired order
desired_order <- c("Random Forest", "k-Nearest Neighbors", "Naive Bayes",
                   "Random Forest (Extended)", "k-Nearest Neighbors (Extended)", "Naive Bayes (Extended)")
combined_df$Model <- factor(combined_df$Model, levels = desired_order)

# Display the combined results in a table
combined_df %>%
  gt() %>%
  tab_header(title = "Model Accuracy Comparison")


