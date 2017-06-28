
# Cleaning Function :

cleaningData <- function(database) {
  for(i in 2:4)
    { database[,i] <- iconv(database[,i], from="UTF-8", to="latin9", sub=" ")   # conversion UTF in ISO/IEC 8859-15  
      database[,i] <- gsub(pattern="<.*?>|\n", replacement=" ", database[,i])   # HTML tags and \n
      database[,i] <- tolower(database[,i])                                     # put in lowercase
      database[,i] <- gsub(pattern="\\s\\s*", replacement=" ", database[,i])    # remove spaces
    }
  
  return (database)
  
}
