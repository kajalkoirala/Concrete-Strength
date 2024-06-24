
# import data
concrete_train <- read.csv('C:\\Users\\kajal\\OneDrive\\Desktop\\kanchan Koirala- 77356758\\concrete_strength_train.csv')
concrete_test <- read.csv('C:\\Users\\kajal\\OneDrive\\Desktop\\kanchan Koirala- 77356758\\concrete_strength_test.csv')

# Load required libraries
library(ggplot2)
library(mice)
library(gridExtra)
library(VIM)


summary(concrete_test)
summary(concrete_train)

dim(concrete_test)
dim(concrete_train)

colnames(concrete_test)
colnames(concrete_train)

str(concrete_test)
str(concrete_train)

View(head(concrete_test, 10))
View(head(concrete_train, 10))
View(tail(concrete_test, 10))
View(tail(concrete_train, 10))


missing_data_test <- is.na(concrete_test)
missing_data_train <- is.na(concrete_train)

sum(missing_data_test)
sum(missing_data_train)

missing_values_test <- colMeans(is.na(concrete_test)) * 100
missing_values_train <- colMeans(is.na(concrete_train)) * 100

missing_values_test
missing_values_train

md.pattern(concrete_test)
md.pattern(concrete_train)



# Histograms plot for Strength of train and test
g1<-ggplot(concrete_train, aes(x = Strength)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Strength (concrete_train)", x = "Strength", y = "Frequency")

g2<-ggplot(concrete_test, aes(x = Strength)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Strength (concrete_test)", x = "Strength", y = "Frequency")


# Create histogram for "Cement" column in train dataset
g1<-ggplot(concrete_train, aes(x = Cement)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Cement (concrete_train)", x = "Cement", y = "Frequency")

# Create histogram for "Cement" column in test dataset
g2<-ggplot(concrete_test, aes(x = Cement)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Cement (concrete_test)", x = "Cement", y = "Frequency")

grid.arrange(g1,g2, ncol = 2)

# Create histogram for "Blast.Furnace.Slag" column in train dataset
g1<-ggplot(concrete_train, aes(x = Blast.Furnace.Slag)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Blast Furnace Slag (concrete_train)", x = "Blast Furnace Slag", y = "Frequency")

# Create histogram for "Blast.Furnace.Slag" column in test dataset
g2<-ggplot(concrete_test, aes(x = Blast.Furnace.Slag)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Blast Furnace Slag (concrete_test)", x = "Blast Furnace Slag", y = "Frequency")
grid.arrange(g1,g2, ncol = 2)

# Create histogram for "Fly.Ash" column in train dataset
g1<-ggplot(concrete_train, aes(x = Fly.Ash)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Fly Ash (concrete_train)", x = "Fly Ash", y = "Frequency")

# Create histogram for "Fly.Ash" column in test dataset
g2<-ggplot(concrete_test, aes(x = Fly.Ash)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Fly Ash (concrete_test)", x = "Fly Ash", y = "Frequency")
grid.arrange(g1,g2, ncol = 2)

# Create histogram for "Water" column in train dataset
g1<-ggplot(concrete_train, aes(x = Water)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Water (concrete_train)", x = "Water", y = "Frequency")

# Create histogram for "Water" column in test dataset
g2<-ggplot(concrete_test, aes(x = Water)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Water (concrete_test)", x = "Water", y = "Frequency")
grid.arrange(g1,g2, ncol = 2)

# Create histogram for "Superplasticizer" column in train dataset
g1<-ggplot(concrete_train, aes(x = Superplasticizer)) +
  geom_histogram(fill = "gold", color = "blue", bins = 20) +
  labs(title = "Distribution of Superplasticizer (concrete_train)", x = "Superplasticizer", y = "Frequency")

# Create histogram for "Superplasticizer" column in test dataset
g2<-ggplot(concrete_test, aes(x = Superplasticizer)) +
  geom_histogram(fill = "gold", color = "blue", bins = 20) +
  labs(title = "Distribution of Superplasticizer (concrete_test)", x = "Superplasticizer", y = "Frequency")
grid.arrange(g1,g2, ncol = 2)

# Create histogram for "Coarse.Aggregate" column in train dataset
g1<-ggplot(concrete_train, aes(x = Coarse.Aggregate)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Coarse Aggregate (concrete_train)", x = "Coarse Aggregate", y = "Frequency")

# Create histogram for "Coarse.Aggregate" column in test dataset
g2<-ggplot(concrete_test, aes(x = Coarse.Aggregate)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Coarse Aggregate (concrete_test)", x = "Coarse Aggregate", y = "Frequency")
grid.arrange(g1,g2, ncol = 2)

# Create histogram for "Fine.Aggregate" column in train dataset
g1<-ggplot(concrete_train, aes(x = Fine.Aggregate)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Fine Aggregate (concrete_train)", x = "Fine Aggregate", y = "Frequency")

# Create histogram for "Fine.Aggregate" column in test dataset
g2<-ggplot(concrete_test, aes(x = Fine.Aggregate)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Fine Aggregate (concrete_test)", x = "Fine Aggregate", y = "Frequency")
grid.arrange(g1,g2, ncol = 2)

# Create histogram for "Age" column in train dataset
g1<-ggplot(concrete_train, aes(x = Age)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Age (concrete_train)", x = "Age", y = "Frequency")

# Create histogram for "Age" column in test dataset
g2<-ggplot(concrete_test, aes(x = Age)) +
  geom_histogram(fill = "gold", color = "blue", bins = 30) +
  labs(title = "Distribution of Age (concrete_test)", x = "Age", y = "Frequency")
grid.arrange(g1,g2, ncol = 2)


#density for input variable
dens_train<- ggplot(concrete_train,aes(x=Cement))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Cement")


dens_test<- ggplot(concrete_test,aes(x=Cement))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Cement")

grid.arrange(dens_train,dens_test, ncol = 2)


dens_train<- ggplot(concrete_train,aes(x=Blast.Furnace.Slag))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Blast Furnance Slag")


dens_test<- ggplot(concrete_test,aes(x=Blast.Furnace.Slag))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Blast Furnance Slag")

grid.arrange(dens_train,dens_test, ncol = 2)

dens_train<- ggplot(concrete_train,aes(x=Fly.Ash))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Fly.Ash")


dens_test<- ggplot(concrete_test,aes(x=Fly.Ash))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Fly.Ash")
grid.arrange(dens_train,dens_test, ncol = 2)

dens_train<- ggplot(concrete_train,aes(x=Coarse.Aggregate))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Coarse.Aggregate")

dens_test<- ggplot(concrete_test,aes(x=Coarse.Aggregate))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Coarse.Aggregate")
grid.arrange(dens_train,dens_test, ncol = 2)

dens_train<- ggplot(concrete_train,aes(x=Water))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Water ")

dens_test<- ggplot(concrete_test,aes(x=Water))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Water ")

grid.arrange(dens_train,dens_test, ncol = 2)

dens_train<- ggplot(concrete_train,aes(x= Strength))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Strength ")
dens_train

dens_test<- ggplot(concrete_test,aes(x=Strength))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Strength ")
dens_test
grid.arrange(dens_train,dens_test, ncol = 2)


dens_train<- ggplot(concrete_train,aes(x= Age))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Age ")
dens_train

dens_test<- ggplot(concrete_test,aes(x=Age))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Age ")
dens_test
grid.arrange(dens_train,dens_test, ncol = 2)


dens_train<- ggplot(concrete_train,aes(x= Fine.Aggregate))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Fine Aggregate ")
dens_train

dens_test<- ggplot(concrete_test,aes(x=Fine.Aggregate))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Fine Aggregate ")
dens_test
grid.arrange(dens_train,dens_test, ncol = 2)


dens_train<- ggplot(concrete_train,aes(x= Superplasticizer))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of train Superplasticizer ")
dens_train

dens_test<- ggplot(concrete_test,aes(x=Superplasticizer))+
  geom_histogram(aes(y= ..density..), color='black',fill = "skyblue",binwidth = 10)+
  geom_density(color='red',  fill = 'yellow', alpha=0.5)+
  ggtitle("Density of test Superplasticizer ")
dens_test


#distrbution and density of input varible
grid.arrange(dens_train,dens_test,g1,g2, ncol = 2)



# Scatter plots
scatter_plot1_test <- ggplot(concrete_test, aes(x = Superplasticizer, y = Strength)) +
  geom_point(color = "blue") +  # Set the color of the points to blue for the strength variable
  labs(title = "Superplasticizer vs. Strength (concrete_test)", x = "Superplasticizer", y = "Strength")

scatter_plot1_train <- ggplot(concrete_train, aes(x = Superplasticizer, y = Strength)) +
  geom_point(color = "black", size = 2) +
  labs(title = "Superplasticizer vs. Strength (concrete_train)", x = "Superplasticizer", y = "Strength")

scatter_plot2_test <- ggplot(concrete_test, aes(x = Fly.Ash, y = Strength)) +
  geom_point() +
  labs(title = "Fly Ash vs. Strength(concrete_test)", x = "Fly Ash", y = "Strength")

scatter_plot2_train <- ggplot(concrete_train, aes(x = Fly.Ash, y = Strength)) +
  geom_point(color = "purple", size = 2) +
  labs(title = "Fly Ash vs. Strength(concrete_train)", x = "Fly Ash", y = "Strength")

# Create scatter plot for "Water" vs. "Strength" in test dataset
scatter_plot3_test <- ggplot(concrete_test, aes(x = Water, y = Strength)) +
  geom_point() +
  labs(title = "Water vs. Strength (concrete_test)", x = "Water", y = "Strength")

# Create scatter plot for "Water" vs. "Strength" in train dataset
scatter_plot3_train <- ggplot(concrete_train, aes(x = Water, y = Strength)) +
  geom_point(color = "purple", size = 2) +
  labs(title = "Water vs. Strength (concrete_train)", x = "Water", y = "Strength")



# Arrange scatter plots
grid.arrange(scatter_plot1_test, scatter_plot1_train, ncol = 2)
grid.arrange(scatter_plot2_test, scatter_plot2_train, ncol = 2)
grid.arrange(scatter_plot3_test, scatter_plot3_train, ncol = 2)



# Imputation using mice
imputed_data_train <- mice(concrete_train, m = 5, method = "mean", maxit = 10, na.rm = TRUE)
imputed_data_test <- mice(concrete_test, m = 5, method = "mean", maxit = 10, na.rm = TRUE)

# Complete imputed datasets
concretetrain_Imputed_train <- complete(imputed_data_train)
concretetrain_Imputed_test <- complete(imputed_data_test)

# Summary of imputed datasets
summary(concretetrain_Imputed_test)
summary(concretetrain_Imputed_train)

# Boxplots with outliers
boxplot(concretetrain_Imputed_test)
boxplot(concretetrain_Imputed_train)

# Function to remove outliers
remove_outliers <- function(data) {
  outliers <- numeric()
  for (col in names(data)) {
    Q1 <- quantile(data[[col]], 0.25)
    Q3 <- quantile(data[[col]], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- c(outliers, which(data[[col]] < lower_bound | data[[col]] > upper_bound))
  }
  data[-outliers, ]
}

# Remove outliers from the imputed training and test datasets
concretetrain_Imputed_train_no_outliers <- remove_outliers(concretetrain_Imputed_train)
concretetrain_Imputed_test_no_outliers <- remove_outliers(concretetrain_Imputed_test)

# Boxplots without outliers
boxplot(concretetrain_Imputed_test_no_outliers)
boxplot(concretetrain_Imputed_train_no_outliers)


dim(concretetrain_Imputed_test_no_outliers)
dim(concretetrain_Imputed_train_no_outliers)

# Scale the data
scaled_concrete_train <- scale(concretetrain_Imputed_train_no_outliers)
scaled_concrete_test <- scale(concretetrain_Imputed_test_no_outliers)

concrete_test <- as.data.frame(scaled_concrete_test)
concrete_train<- as.data.frame(scaled_concrete_train)



# Summary of scaled datasets
summary(scaled_concrete_train)
summary(scaled_concrete_test)
View(scaled_concrete_train)

#------------------------part-2 -----------------------------------------------------
library(lattice)
library(caret)
library(gridExtra)
library(ggplot2)
library(grid)
library(rpart)



# Check for missing values in scaled_concrete_train
if (anyNA(concrete_train)) {
  # If any missing values are found, print the column-wise count
  print("Missing values found:")
  print(colSums(is.naconcrete_train))
} else {
  # If no missing values are found, print a message
  print("No missing values found in concrete_train.")
}

#---------------Decision Tree-Assuming target variable - "speicies"

# Fit the decision tree model
model_dt <- rpart(Strength ~ ., data = concrete_train)
summary(model_dt)

# Make predictions on the test data
predictions_dt <- predict(model_dt, newdata = concrete_test)

# Print predictions
print(predictions_dt)

# Plot the decision tree
plot(model_dt)
text(model_dt)

# Get actual values from the test data
actual_values <- concrete_test$Strength

# R-squared 
SSE <- sum((predictions_dt - actual_values)^2)  # Sum of Squared Errors
SST <- sum((actual_values - mean(actual_values))^2)  # Total Sum of Squares
R2 <- 1 - (SSE / SST)
print(paste("R-squared:", R2))

# Adjusted R-squared calculation
n_test <- nrow(concrete_test)
p_test <- ncol(concrete_test) - 1  # Number of predictors in the model
Adjusted_R2 <- 1 - ((1 - R2) * ((n_test - 1) / (n_test - p_test - 1)))
print(paste("Adjusted R-squared:", Adjusted_R2))

# Root mean squared error (RMSE) calculation
RMSE <- sqrt(mean((predictions_dt - actual_values)^2))
print(paste("RMSE:", RMSE))

# Mean square error (MSE) calculation
MSE <- mean((predictions_dt - actual_values)^2)
print(paste("MSE:", MSE))

# Mean absolute error (MAE) calculation
MAE <- mean(abs(predictions_dt - actual_values))
print(paste("MAE:", MAE))

print(paste("R-squared:", R2))
print(paste("Adjusted R-squared:", Adjusted_R2))
print(paste("RMSE:", RMSE))
print(paste("MSE:", MSE))
print(paste("MAE:", MAE))

# Plot actual vs. predicted values
plot(actual_values, predictions_dt, 
     xlab = "Actual Strength", ylab = "Predicted Strength", 
     main = "Actual vs. Predicted Strength of Decision Tree Model")



#----------------Linear Regression

# Perform linear regression
model_lr <- lm(Strength ~ ., data = concrete_train)
plot(model_lr)
# Summarize the linear regression model


predictions_lr <- predict(model_lr, newdata = concrete_test)
predictions_lr

summary(model_lr)
summary_lm <- summary(model_lr)
r_squared_lr <- summary_lm$r.squared


mse_lr <- mean((predictions_lr - concrete_test$Strength)^2)
mse_lr

n_test <- nrow(concrete_test)
p_test <- ncol(concrete_test) - 1  # Number of predictors in the model

#adjusted_R2
adjusted_R2_lr <- 1 - ((1 - r_squared) * ((n_test - 1) / (n_test - p_test - 1)))
adjusted_R2_lr

#mean square error 
MAE_lr <- MAE(predictions_lr, concrete_test$Strength) 
MAE_lr

#root mean square error
rmse_lr <- sqrt(mean((concrete_test$Strength - predictions_lr)^2))
rmse_lr

print(paste("R2 :", r_squared_lr))
print(paste("Adjusted R2 :", adjusted_R2_lr))
print(paste("Mean Squared Error :", mse_lr))
print(paste(" Root Mean Squared Error:", rmse_lr))
print(paste("  Mean Absoulte Error:", MAE_lr))

plot(concrete_test$Strength, predictions_value3, 
     xlab = "Actual Strength", ylab = "Predicted Strength", 
     main = "Actual vs. Predicted Strength in Linear Regression")

#---------------Random Forest Model

set.seed(921)
concretetrain_rf <-train(Strength ~., data =scaled_concrete_train, method = "rf") 
concretetrain_rf
summary(concretetrain_rf)

concretetrain_rf$finalModel 
plot(concretetrain_rf) 
varImp(concretetrain_rf) 

actualtest_rf <- concrete_test$Strength

#RootSquare
predictedtest <- unname(predict(concretetrain_rf, concrete_test))
R2test_rf <- 1 - (sum((actualtest_rf-predictedtest)^2)/sum((actualtest_rf-mean(actualtest_rf))^2))
R2test_rf

#adjustedR2
n_test <- nrow(concrete_test)
p_test <- ncol(concrete_test) - 1  # Number of predictors in the model
Adjusted_R2_test_rf <- 1 - ((1 - R2test_rf) * (n_test - 1) / (n_test - p_test - 1))
Adjusted_R2_test_rf

#mean Sqauare Error
MSE_test_rf <- mean((concrete_test$Strength - predictedtest)^2)
MSE_test_rf

#Root mean Square error 
RMSE_test_rf <- sqrt(MSE_test_rf)
RMSE_test_rf

#mean absoulte error
predictions_rf <- predict(object = concrete_rf,newdata = concrete_test, interval = "confidence", level = 0.95)
MAE_test_rf<-MAE(pred = predictions_rf, obs = concrete_test$Strength)
MAE_test_rf

print(paste("R-squared (R2):", R2test_rf))
print(paste("Adjusted R-squared:", Adjusted_R2_test_rf))
print(paste("Mean Squared Error (MSE):", MSE_test_rf))
print(paste("Root Mean Squared Error (RMSE):", RMSE_test_rf))
print(paste("Mean Absolute Error (MAE):", MAE_test_rf))

plot(concrete_test$Strength, predictedtest, 
     xlab = "Actual Strength", ylab = "Predicted Strength", 
     main = "Actual vs. Predicted Strength of Random Forest Model") 



#---------KNN(K-Nearest Neighbour)
model <- train(
  Strength~., data = concrete_train, method = "knn",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

# Plot model error RMSE vs different values of k
plot(model)
predictions_knn <- predict(model, newdata=concrete_test)
predictions_knn


# Calculate R-squared 
#SSE - Sum of Squared Errors
#SST - Total Sum of Squares
SSE <- sum((predictions_knn - concrete_test$Strength)^2)  
SST <- sum((concrete_test$Strength - mean(concrete_test$Strength))^2)  
R2 <- 1 - (SSE / SST)
R2

# Adjusted R-squared
n_test <- nrow(concrete_test)
p_test <- ncol(concrete_test) - 1  # Number of predictors in the model
Adjusted_R2 <- 1 - ((1 - R2) * ((n_test - 1) / (n_test - p_test - 1)))
Adjusted_R2

#calculate root mean square error
RMSE(predictions_knn, concrete_test$Strength)


#MEAN SQUARE ERROR 
MSE_test <- mean((concrete_test$Strength - predictions_knn)^2)
MSE_test

#mean absoulte error 
MAE(predictions_knn, concrete_test$Strength) 

print("R-squared:")
print(R2)
print("Adjusted R-squared:")
print(Adjusted_R2)
print("Mean Square Error:")
print(MSE_test)
print("Mean Absolute Error:")
print(MAE(predictions_knn, concrete_test$Strength))



plot(concrete_test$Strength, predictions_knn, 
     xlab = "Actual Strength", ylab = "Predicted Strength", 
     main = "Actual vs. Predicted Strength of KNN") 



#---------------KNN(different values)

# Split the data into training and testing sets (80% train, 20% test)
set.seed(123) # for reproducibility
# Define a range of k values to try
k_values <- c(1,2)

# Loop over each k value
for (k in k_values) {
  # Create a KNN regression model
  model <- train(Strength ~ ., data = concrete_train, method = "knn",
                 trControl = trainControl(method = "cv", number = 5),
                 tuneGrid = data.frame(k = k))
  
  # Make predictions on the testing data
  predictions_d_knn <- predict(model, newdata = concrete_test)
  
  # Evaluate the model
  mse_knn <- mean((predictions_d_knn - concrete_test$Strength)^2)
  SSE_knn <- sum((predictions_d_knn - concrete_test$Strength)^2)  # Sum of Squared Errors
  SST_knn <- sum((concrete_test$Strength - mean(concrete_test$Strength))^2)  # Total Sum of Squares
  r2_knn <- 1 - (SSE_knn / SST_knn)
  n_test <- nrow(concrete_test)
  p_test <- ncol(concrete_test) - 1  # Number of predictors in the model
  adjusted_r2_knn <- 1 - ((1 - r2_knn) * ((n_test - 1) / (n_test - p_test - 1)))
  rmse_knn <- RMSE(predictions_d_knn, concrete_test$Strength)
  mae_knn <- MAE(predictions_d_knn, concrete_test$Strength) 
  
  # Print evaluation metrics for current k value
  print(paste("R2 (k =", k, "):", r2_knn))
  print(paste("Adjusted R2 (k =", k, "):", adjusted_r2_knn))
  print(paste("Mean Squared Error (k =", k, "):", mse_knn))
  print(paste("Root Mean Squared Error (k =", k, "):", rmse_knn))
  print(paste("Mean Absolute Error (k =", k, "):", mae_knn))
}

# Plot actual vs. predicted strength
plot(concrete_test$Strength, predictions_d_knn, 
     xlab = "Actual Strength", ylab = "Predicted Strength", 
     main = "Actual vs. Predicted Strength of KNN with Different Values of k")


# Create the fitted vs original plot for Decison Tree
plot_residuals_dt <- ggplot(data = concrete_test, aes(x =  predictions_dt, y = concrete_test$Strength - predictions_dt)) +
  geom_point() +  # Plot the points
  geom_smooth(method = "loess", color = "blue") +  
  ggtitle("Decison Tree Residuals") +  # Add a title
  xlab("Predicted Strength") +  # Label for the x-axis
  ylab("Residuals")  # Label for the y-axis

plot_fitted_dt <- ggplot(data = concrete_test, aes(x = concrete_test$Strength, y =  predictions_dt)) +
  geom_point() +  
  geom_line(aes(y = concrete_test$Strength), color = "red", linetype = "dashed") + 
  ggtitle("Decision Tree Fitted vs Original") +  # Add a title
  xlab("Actual Strength") +  # Label for the x-axis
  ylab("Predicted Strength")  # Label for the y-axis

grid.arrange(plot_residuals_dt, plot_fitted_dt,ncol=2)



# Create the fitted vs original plot for KNN
plot_residuals_knn <- ggplot(data = concrete_test, aes(x =  predictions_value1, y = concrete_test$Strength - predictions_knn)) +
  geom_point() +  # Plot the points
  geom_smooth(method = "loess", color = "purple") +  
  ggtitle(" KNN Residuals") +  # Add a title
  xlab("Predicted Strength") +  # Label for the x-axis
  ylab("Residuals")  # Label for the y-axis


plot_fitted_knn <- ggplot(data = concrete_test, aes(x = concrete_test$Strength, y =predictions_knn)) +
  geom_point() +  
  geom_line(aes(y = concrete_test$Strength), color = "red", linetype = "dashed") + 
  ggtitle("KNN Fitted vs Original") +  # Add a title
  xlab("Actual Strength") +  # Label for the x-axis
  ylab("Predicted Strength")  # Label for the y-axis

grid.arrange(plot_residuals_knn,plot_fitted_knn,ncol=2)


# Create the fitted vs original plot for Random Forest
plot_residuals_rf <- ggplot(data = concrete_test, aes(x = predictedtest, y = concrete_test$Strength - predictedtest)) +
  geom_point() +  # Plot the points
  geom_smooth(method = "loess", color = "black") +  
  ggtitle("Random Forest Residuals") +  # Add a title
  xlab("Predicted Strength") +  # Label for the x-axis
  ylab("Residuals")  # Label for the y-axis

plot_fitted_rf <- ggplot(data = concrete_test, aes(x = concrete_test$Strength, y = predictedtest)) +
  geom_point() +  
  geom_line(aes(y = concrete_test$Strength), color = "gold", linetype = "dashed") + 
  ggtitle("Random Forest Fitted vs Original") +  # Add a title
  xlab("Actual Strength") +  # Label for the x-axis
  ylab("Predicted Strength")  # Label for the y-axis

grid.arrange(plot_residuals_rf, plot_fitted_rf,ncol=2)


# Create the fitted vs original plot for knn with Different Value
plot_residuals_knn_d <- ggplot(data = concrete_test, aes(x =  predictions_value2, y = concrete_test$Strength -  predictions_d_knn)) +
  geom_point() +  # Plot the points
  geom_smooth(method = "loess", color = "blue") +  
  ggtitle(" KNN with Different Value of K Residuals") +  # Add a title
  xlab("Predicted Strength") +  # Label for the x-axis
  ylab("Residuals")  # Label for the y-axis


plot_fitted_knn_d <- ggplot(data = concrete_test, aes(x = concrete_test$Strength, y =   predictions_d_knn)) +
  geom_point() +  
  geom_line(aes(y = concrete_test$Strength), color = "red", linetype = "dashed") + 
  ggtitle("KNN with different values of K  Fitted vs Original") +  # Add a title
  xlab("Actual Strength") +  # Label for the x-axis
  ylab("Predicted Strength")  # Label for the y-axis

grid.arrange(plot_residuals_knn_d,plot_fitted_knn_d,ncol=2)



# Create the fitted vs original plot for Linear Regression
plot_residuals_lr <- ggplot(data = concrete_test, aes(x =  predictions_lr, y = concrete_test$Strength -  predictions_lr)) +
  geom_point() +  # Plot the points
  geom_smooth(method = "loess", color = "yellow") +  
  ggtitle(" Linear Regression  Residuals") +  # Add a title
  xlab("Predicted Strength") +  # Label for the x-axis
  ylab("Residuals")  # Label for the y-axis

plot_fitted_lr <- ggplot(data = concrete_test, aes(x = concrete_test$Strength, y =   predictions_lr)) +
  geom_point() +  
  geom_line(aes(y = concrete_test$Strength), color = "gold", linetype = "dashed") + 
  ggtitle("Linear Regression Fitted vs Original") +  # Add a title
  xlab("Actual Strength") +  # Label for the x-axis
  ylab("Predicted Strength")  # Label for the y-axis

grid.arrange(plot_residuals_lr,plot_fitted_lr,ncol=2)




