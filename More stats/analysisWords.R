#-----------------------------------------------
#       Words analysis
#-----------------------------------------------

#data <- as.data.frame(read.csv("../alphaClean.csv"))

library(stringr)

# Pick up every words 

words.title.list <- str_split(data$title, boundary("word"))
words.brief.list <- str_split(data$brief, boundary("word"))
words.ad.list <- str_split(data$title, boundary("word"))


tableWords <- sort(table(unlist(words.ad.list)), decreasing = TRUE)
tableWords <- as.data.frame(tableWords)
tableWords$Var1 <- as.character(tableWords$Var1)

# Removing all words containing less than 2 letters
tableWords <- tableWords[str_count(tableWords[,1], "")>2,]

# Removing conjonction, preposition, ??numbers??
commonWords <- read.csv("./More Stats/commonWords.csv")
commonWords <- as.vector(commonWords$Word)
commonWords <- str_trim(commonWords)
tableWords <-subset(tableWords, !( Var1 %in% commonWords))


#Removing small frequences

#tableWords <- tableWords[which(tableWords$Freq > nrow(data)/1000), ]
