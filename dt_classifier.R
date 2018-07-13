#---------------------------------------------
# Decision Tree Classifier
#---------------------------------------------

# install.packages(c('rpart', 'rpart.plot'))
library(rpart)
library(rpart.plot)
source("preprocessor.R")

# Run preprocessor (param: random seed, train size, sparsity)
preprocess(100, .8, 0.99999)


# Create model
time.start <- Sys.time()
dt.model <- rpart(category ~ ., dtm.train.df, method="class", parms = list(split="information"))
(time.end <- Sys.time() - time.start)
# prp(dt.model)

# Predict class labels
time.start <- Sys.time()
dt.pred <- predict(dt.model, dtm.test.df[,names(dtm.test.df) != "category"], type="class")
(time.end <- Sys.time() - time.start)

# Create data frame of predicted and actual class labels
dt.pred.df <- data.frame(name=prod.test$name)
dt.pred.df <- cbind(dt.pred.df, actual=as.character(dtm.test.df$category), stringsAsFactors = FALSE)
dt.pred.df <- cbind(dt.pred.df, predict=as.character(dt.pred), stringsAsFactors = FALSE)

# Create data frame of misclassified objects
dt.misclass <- dt.pred.df[which(dt.pred.df$predict != dt.pred.df$actual),]

# Show confusion matrix
table(dt.pred.df$actual, factor(dt.pred), dnn=c("Actual","Predicted"))

# Calculate accuracy
nrow(dt.pred.df[which(dt.pred.df$predict == dt.pred.df$actual),]) / nrow(dt.pred.df)
