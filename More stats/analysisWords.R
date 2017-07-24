#-----------------------------------------------
#       Words analysis
#-----------------------------------------------

#data <- as.data.frame(read.csv("../alphaClean.csv"))

library(stringr)

# Pick up every words in column category and title

words.title.list <- str_split(data$title, boundary("word"))

#In category keep only the 2nd and 3rd sub-category
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(data$category, regex)
words.cat.vector <- c(cat[,3], cat[,4])
words.cat.vector <- unlist(str_split(words.cat.vector, boundary("word")))

#words.cat.list <- str_split(data$category, "/")
#word.cat.vector <- as.vector(unlist(words.cat.list))

#word.cat.vector <- sort(table(word.cat.vector), decreasing = TRUE)

words <- c(unlist(words.title.list), str_to_lower(words.cat.vector))

# calculate occurences
tableWords <- sort(table(words), decreasing = TRUE)
tableWords <- as.data.frame(tableWords)
tableWords$words <- as.character(tableWords$words)

# Removing conjonction, preposition, ??numbers??
commonWords <- read.csv("./More Stats/5000CommonWords.csv")
commonWords <- as.vector(commonWords$Word)
commonWords <- str_trim(commonWords)
tableWords <-subset(tableWords, !( words %in% commonWords))

tableWords <- tableWords[!str_detect(tableWords$words, "^[0-9.,]*$"),]

#Removing small frequences

tableWords <- tableWords[which(tableWords$Freq > nrow(data)/5000), ]

#Removing "number + unit"

# Vector with all the unit that are allowed (add unit if needed)
unit_allowed <- c("mg","kg", "ug","lb","oz","ounce","g","gr","gram") 

# Construct a regular expression matching with digits + units allowed
regex_unit <- str_c("([0-9]+\\.?[0-9]*)((?:(\\s|)((",unit_allowed[1],")")
for(i in 2:length(unit_allowed)){
  regex_unit <- str_c(regex_unit,"|(",unit_allowed[i],")")
}
regex_unit <- str_c(regex_unit,")))")
# regex_unit = regular expression for dose and unit

# Extraction from the title :
toRemove<- !str_detect(tableWords$words, regex_unit)

tableWords <- tableWords[toRemove,]

# Removing all words containing less than 2 letters
tableWords <- tableWords[str_count(tableWords[,1], "")>2,]