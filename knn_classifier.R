#### k-Nearest Neighbours Classifier ####

library(class)
source("preprocessor.R")
source("functions.R")

# Preprocess data using specified DTM sparsity
data <- preprocess(sparsity = 0.999)


#### Classification ####

# Train and test model using k-fold cross-validation
set.seed(100)
data.rand <- data[sample(1:nrow(data)),]  # Randomize data set
k <- 5  # Number of folds
fold <- round(nrow(data.rand) / k, 0)  # Size of fold
idx.head <- 0
idx.tail <- fold

# Initialize confusion matrix
category <- factor(c(), levels = levels(data$category))
knn.cm <- table(category, category, dnn = c("Actual","Predicted"))

# Set fold in iteration i as test set and remaining folds as training set
time.start <- Sys.time()
for (i in 1:k) {
  idx.head <- idx.head + 1
  idx.tail <- fold * i
  idx <- idx.head:idx.tail
  
  # Split data for training and testing
  data.test <- na.omit(data.rand[idx,])
  data.train <- data.rand[-idx,]
  
  # Predict class labels
  knn.pred <- knn(data.train[, -c(1, ncol(data.train))], data.test[, -c(1, ncol(data.test))],
                  data.train$category, k=3)
  
  
  #### Evaluation ####
  
  # Create data frame of predicted and actual class labels
  knn.pred.df <- data.frame(name=data.test[, 1])
  knn.pred.df <- cbind(knn.pred.df, actual = data.test$category)
  knn.pred.df <- cbind(knn.pred.df, predict = knn.pred)
  
  # Create data frame of misclassified objects
  # knn.misclass <- knn.pred.df[which(knn.pred.df$predict != knn.pred.df$actual),]
  
  # Sum confusion matrices
  knn.cm <- knn.cm + table(knn.pred.df$actual, knn.pred.df$predict, dnn = c("Actual","Predicted"))
  
  # Set current fold's tail as next fold's head 
  idx.head <- idx.tail
}
print(time.end <- Sys.time() - time.start)

# Plot confusion matrix
plot.cm(knn.cm, title = "Confusion Matrix of k-Nearest Neighbours")

# Calculate accuracy
knn.accu <- sum(diag(knn.cm))/sum(knn.cm)
cat("Accuracy:", knn.accu)