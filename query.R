library(SnowballC)
library(e1071)
library(rpart)
library(class)


predict.query <- function(query, model) {
  # Split query into seperate elements
  query <- strsplit(query, " ")
  
  # Stem query elements
  query.v <- character()
  for (i in query) {
    i <- wordStem(i)  
    query.v <- c(query.v, i)
  }
  
  # Convert query to document term matrix
  query.df <- dtm.train.df[0,]  # Requires same attribute length as training set
  query.df[1, 1:(ncol(query.df)-1)] <- 0
  query.df[1, names(query.df) %in% query.v] <- 1
  
  
  # Return predicted category according to model
  if (model == "dt") {
    result <- predict(dt.model, query.df[,names(query.df) != "category"], type="class")
  } else if (model == "nb") {
    result <- predict(nb.model, query.df[,names(query.df) != "category"])
  } else if (model == "knn") {
    k <- as.numeric(readline("Enter number of nearest neighbours (k): "))
    result <- knn(dtm.train.df[,names(dtm.train.df) != "category"],
                  query.df[,names(query.df) != "category"], 
                  dtm.train.df$category, k=k)
  }
  cat("Recommended category: ", as.character(result))
  cat("\n")
  cat("Terms in vocabulary:", colnames(query.df)[which(query.df == 1)])
}


# Request query
query <- readline(prompt = "Enter product name: ")
model <- readline(prompt = "Enter predictive model (nb/dt/knn): ")
predict.query(query, model)