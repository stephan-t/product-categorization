#---------------------------------------------
# Preprocessor
#---------------------------------------------

install.packages("tm")
install.packages("SnowballC")
library(tm)        # Text mining
library(SnowballC) # Stemming


# Import data
products <- read.csv("flipkart_com-ecommerce_sample.csv")

# Create data frame with name and category columns
prod <- products[,4:5]
colnames(prod) <- c("name","category")

# Remove duplicates
prod <- unique(prod)

# Extract top category
prod$category <- gsub('(\\["| >>.*)', "", prod$category)
prod$category <- as.factor(prod$category)

# Rename unknown categories
# prod$category <- gsub('.*"]', "Unknown", prod$category)

# Remove unknown categories
prod <- prod[(grep('.*"]', prod$category, invert = TRUE)),]
rownames(prod) <- 1:nrow(prod)

# Function to run from each classifier
preprocess <<- function(rand, spar) { # <------------ Re-run after updates

  # Randomize dataset
  if (rand == TRUE) {
    prod <<- prod[sample(nrow(prod)),]
  }
    
  # Create training and testing sets
  prod.train <<- prod[1:8000,]
  prod.test <<- prod[8001:10000,]
  
  #---------------------------------------------
  # Perform text processing & feature extraction
  #---------------------------------------------
  
  # Create corpora
  corp.train <- VCorpus(VectorSource(prod.train$name))
  corp.test <- VCorpus(VectorSource(prod.test$name))
  
  # Function to count words once
  uniqueWords <- function(word) {
    return(paste(unique(strsplit(word, " ")[[1]]), collapse = ' '))
  }
  
  # Process text
  process.text <- function(corp) {
    corp <- tm_map(corp, content_transformer(tolower))
    corp <- tm_map(corp, removePunctuation)
    corp <- tm_map(corp, removeNumbers)
    corp <- tm_map(corp, stripWhitespace)
    corp <- tm_map(corp, removeWords, stopwords("english"))
    corp <- tm_map(corp, stemDocument, language = "english")
    corp <- tm_map(corp, removeWords, c("black","blue","white","yellow","men","women","boy","girl"))
    corp <- tm_map(corp, content_transformer(uniqueWords))
    return(corp)
  }
  corp.train <- process.text(corp.train)
  corp.test <- process.text(corp.test)
    
  # Create document term matrix
  dtm.train <- DocumentTermMatrix(corp.train)
  
  # Reduce size of DTM by removing less frequent terms
  dtm.train <- removeSparseTerms(dtm.train, spar)
  
  # Make test set have same columns as training set
  dtm.test <- DocumentTermMatrix(corp.test, control = list
                                (dictionary=Terms(dtm.train), wordLengths = c(3,10)))
  
  # Create data frame and add class label
  dtm.train.df <<- data.frame(as.matrix(dtm.train))
  dtm.test.df <<- data.frame(as.matrix(dtm.test))
  dtm.train.df <<- cbind(dtm.train.df, category=prod.train[, "category"])
  dtm.test.df <<- cbind(dtm.test.df, category=prod.test[, "category"])
  
  # Change columns from numeric to binary factor
  dtm.train.df[,names(dtm.train.df) != "category"] <<-
    lapply(dtm.train.df[,names(dtm.train.df) != "category"], factor)
  dtm.test.df[,names(dtm.test.df) != "category"] <<-
    lapply(dtm.test.df[,names(dtm.test.df) != "category"], factor, levels = c("0","1"))
  
  # Update class label factor levels
  dtm.train.df$category <<- factor(dtm.train.df$category)
  dtm.test.df$category <<- factor(dtm.test.df$category)

}
