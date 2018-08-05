#### Decision Tree Classifier ####

# install.packages("rpart")
library(rpart)
source("preprocessor.R")
source("functions.R")

# Preprocess data using specified sparsity
data <- preprocess(0.99999)


#### Classification ####

# Set training sample size
set.seed(100)
idx <- sample(1:nrow(data), floor(nrow(data) * (2/3)))

# Create training and testing sets
data.train <- data[idx,]
data.test <- data[-idx,]

# Create model
time.start <- Sys.time()
dt.ctrl <- rpart.control(cp = 0.001, minsplit = 20)
dt.model <- rpart(category ~ ., data.train[, -1], method="class", 
                  parms = list(split="information"), control = dt.ctrl)
(time.end <- Sys.time() - time.start)

# Predict class labels
time.start <- Sys.time()
dt.pred <- predict(dt.model, data.test[, -c(1, ncol(data.test))], type="class")
(time.end <- Sys.time() - time.start)


#### Evaluation ####

# Create data frame of predicted and actual class labels
dt.pred.df <- data.frame(name=data.test[, 1])
dt.pred.df <- cbind(dt.pred.df, actual=data.test$category)
dt.pred.df <- cbind(dt.pred.df, predict=dt.pred)

# Create data frame of misclassified objects
dt.misclass <- dt.pred.df[which(dt.pred.df$predict != dt.pred.df$actual),]

# Create confusion matrix
dt.cm <- table(dt.pred.df$actual, dt.pred.df$predict, dnn=c("Actual","Predicted"))
plot.cm(dt.cm, title = "Confusion Matrix of Decision Tree")

# Calculate accuracy
cat("Accuracy:", sum(diag(dt.cm))/sum(dt.cm))


# Build final model using all data
# dt.model <- rpart(category ~ ., data[, -1], method="class", 
#                   parms = list(split="information"), control = dt.ctrl)