#---------------------------------------------
# Naive Bayes Classifier
#---------------------------------------------

# install.packages("e1071")
library(e1071)
source("preprocessor.R")

# Run preprocessor (param: random seed, train size, sparsity)
preprocess(100, .8, 0.99999)


# Create model
time.start <- Sys.time()
nb.model <- naiveBayes(category ~ ., dtm.train.df)
(time.end <- Sys.time() - time.start)

# Predict class labels
time.start <- Sys.time()
nb.pred <- predict(nb.model, dtm.test.df[,names(dtm.test.df) != "category"])
(time.end <- Sys.time() - time.start)

# Create data frame of predicted and actual class labels
nb.pred.df <- data.frame(name=prod.test$name)
nb.pred.df <- cbind(nb.pred.df, actual=as.character(dtm.test.df$category), stringsAsFactors = FALSE)
nb.pred.df <- cbind(nb.pred.df, predict=as.character(nb.pred), stringsAsFactors = FALSE)

# Create data frame of misclassified objects
nb.misclass <- nb.pred.df[which(nb.pred.df$predict != nb.pred.df$actual),]

# Show confusion matrix
table(nb.pred.df$actual, factor(nb.pred), dnn=c("Actual","Predicted"))

# Calculate accuracy
nrow(nb.pred.df[which(nb.pred.df$predict == nb.pred.df$actual),]) / nrow(nb.pred.df)