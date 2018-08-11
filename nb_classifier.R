#### Naive Bayes Classifier ####

# install.packages("e1071")
library(e1071)
source("preprocessor.R")
source("functions.R")

# Preprocess data using specified DTM sparsity
nb.data <- preprocess(sparsity = 0.9999)


#### Classification ####

# Train and test model using k-fold cross-validation
set.seed(100)
data.rand <- nb.data[sample(1:nrow(nb.data)),]  # Randomize data set
k <- 5  # Number of folds
fold <- round(nrow(data.rand) / k, 0)  # Size of fold
idx.head <- 0
idx.tail <- fold

# Initialize confusion matrix
category <- factor(c(), levels = levels(nb.data$category))
nb.cm <- table(category, category, dnn = c("Actual","Predicted"))

# Set fold in iteration i as test set and remaining folds as training set
time.start <- Sys.time()
for (i in 1:k) {
  idx.head <- idx.head + 1
  idx.tail <- fold * i
  idx <- idx.head:idx.tail
  
  # Split data for training and testing
  data.test <- na.omit(data.rand[idx,])
  data.train <- data.rand[-idx,]
  
  # Build model
  nb.model <- naiveBayes(category ~ ., data.train[, -1])
  
  # Predict class labels
  nb.pred <- predict(nb.model, data.test[, -c(1, ncol(data.test))])
  
  
  #### Evaluation ####
  
  # Create data frame of predicted and actual class labels
  nb.pred.df <- data.frame(name=data.test[, 1])
  nb.pred.df <- cbind(nb.pred.df, actual = data.test$category)
  nb.pred.df <- cbind(nb.pred.df, predict = nb.pred)
  
  # Create data frame of misclassified objects
  # nb.misclass <- nb.pred.df[which(nb.pred.df$predict != nb.pred.df$actual),]
  
  # Sum confusion matrices
  nb.cm <- nb.cm + table(nb.pred.df$actual, nb.pred.df$predict, dnn = c("Actual","Predicted"))
  
  # Set current fold's tail as next fold's head 
  idx.head <- idx.tail
}
print(time.end <- Sys.time() - time.start)

# Plot confusion matrix
plot.cm(nb.cm, title = "Confusion Matrix of Naive Bayes")

# Calculate accuracy
nb.accu <- sum(diag(nb.cm))/sum(nb.cm)
cat("Accuracy:", nb.accu)


# Build final model using all data
nb.model <- naiveBayes(category ~ ., nb.data[, -1])