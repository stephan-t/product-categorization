#### Decision Tree Classifier ####

# install.packages("rpart")
library(rpart)
source("preprocessor.R")
source("functions.R")

# Preprocess data using specified sparsity
data <- preprocess(0.99999)


#### Classification ####

# Train and test model using k-fold cross-validation
set.seed(100)
data.rand <- data[sample(1:nrow(data)),]  # Randomize data set
k <- 5  # Number of folds
fold <- round(nrow(data.rand) / k, 0)  # Size of fold
idx.head <- 0
idx.tail <- fold

# Initialize confusion matrix
category <- factor(c(), levels = sort(unique(data$category)))
dt.cm <- table(category, category, dnn = c("Actual","Predicted"))

# Set fold in iteration i as test set and remaining folds as training set
time.start <- Sys.time()
for (i in 1:k) {
  idx.head <- idx.head + 1
  idx.tail <- fold * i
  idx <- idx.head:idx.tail
  
  # Split data for training and testing
  data.test <- data.rand[idx,]
  data.train <- data.rand[-idx,]

  # Build model
  dt.ctrl <- rpart.control(cp = 0.001, minsplit = 20)
  dt.model <- rpart(category ~ ., data.train[, -1], method="class", 
                    parms = list(split="information"), control = dt.ctrl)
  
  # Predict class labels
  dt.pred <- predict(dt.model, data.test[, -c(1, ncol(data.test))], type="class")
  
  
  #### Evaluation ####
  
  # Create data frame of predicted and actual class labels
  dt.pred.df <- data.frame(name=data.test[, 1])
  dt.pred.df <- cbind(dt.pred.df, actual = data.test$category)
  dt.pred.df <- cbind(dt.pred.df, predict = dt.pred)
  
  # Create data frame of misclassified objects
  # dt.misclass <- dt.pred.df[which(dt.pred.df$predict != dt.pred.df$actual),]
  
  # Sum confusion matrix results
  dt.cm <- dt.cm + table(dt.pred.df$actual, dt.pred.df$predict, dnn = c("Actual","Predicted"))
  
  # Set current fold's tail as next fold's head 
  idx.head <- idx.tail
}
print(time.end <- Sys.time() - time.start)

# Plot confusion matrix
plot.cm(dt.cm, title = "Confusion Matrix of Decision Tree")

# Calculate accuracy
cat("Accuracy:", sum(diag(dt.cm))/sum(dt.cm))


# Build final model using all data
dt.model <- rpart(category ~ ., data[, -1], method="class",
                  parms = list(split="information"), control = dt.ctrl)