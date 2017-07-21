#-----------------------------------------------
#       Words analysis
#-----------------------------------------------

data <- as.data.frame(read.csv("../alphaClean.csv"))

library(stringr)

# Pick up every words 

words.title.list <- str_split(data$title, boundary("word"))
#words.brief.list <- str_split(data$brief, boundary("word"))
#words.ad.list <- str_split(data$title, boundary("word"))
words.cat.list <- str_split(data$category, "/")
word.cat.vector <- as.vector(unlist(words.cat.list))

#word.cat.vector <- sort(table(word.cat.vector), decreasing = TRUE)

words <- c(unlist(words.title.list), str_to_lower(word.cat.vector))

tableWords <- sort(table(words), decreasing = TRUE)
tableWords <- as.data.frame(tableWords)
tableWords$words <- as.character(tableWords$words)

# Removing all words containing less than 2 letters
tableWords <- tableWords[str_count(tableWords[,1], "")>2,]

# Removing conjonction, preposition, ??numbers??
commonWords <- read.csv("./More Stats/commonWords.csv")
commonWords <- as.vector(commonWords$Word)
commonWords <- str_trim(commonWords)
tableWords <-subset(tableWords, !( words %in% commonWords))

tableWords <- tableWords[!str_detect(tableWords$words, "^[0-9]*$"),]

#Removing small frequences

tableWords <- tableWords[which(tableWords$Freq > nrow(data)/5000), ]
