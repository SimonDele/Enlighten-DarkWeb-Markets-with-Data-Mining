
  #####################################################################
  #                            Library
  #####################################################################
  
  library (stringr)
  library(plotrix)
  
  #####################################################################
  #                         Initialization
  #####################################################################
  
  #data <- as.data.frame(read.csv("data.csv"))[2:21]
  
  country <- "France"
  num <- 0
  
  # Importation / Exportation :
  if (num == 0) { 
    way <- "origin"
    txt <- "- Exportation"
  } else if (num == 1) {
    way <- "destination"
    txt <- "- Importation"
  }
  
  #####################################################################
  #                           Analysis
  #####################################################################
  
  # Country as destination
  matching_vector <- str_detect(data[,way], country)
  
  # list of the categories (among the line that have "Country" as origin)
  # -> Products (categories) exporting by the country 
  country_cat <- data[matching_vector,"category"] 
  
  # Handling of this categories
  # Regular expression for spliting the categories
  regex <- "/(.*)/(.*)/(.*)"
  cat <- str_match(country_cat, regex)
  
  # Counting this categories 
  tab <- table(cat[,3])   #cat[,3] : 2nd category 
  tab <- sort(tab, decreasing = TRUE)  # Sorting (biggest in first) 
  tab <- tab[1:10] # Taking only the most important
  
  #############

  # Country as destination
  matching_vector2 <- str_detect(data[,"destination"], country)
  
  # list of the categories (among the line that have "Country" as origin)
  # -> Products (categories) exporting by the country 
  country_cat2 <- data[matching_vector2,"category"] 
  
  # Handling of this categories
  # Regular expression for spliting the categories
  regex <- "/(.*)/(.*)/(.*)"
  cat2 <- str_match(country_cat2, regex)
  
  # Counting this categories 
  tab2 <- table(cat2[,3])   #cat[,3] : 2nd category 
  tab2 <- sort(tab2, decreasing = TRUE)  # Sorting (biggest in first) 
  tab2 <- tab2[1:10] # Taking only the most important
  
  tab <- data.frame(tab)
  tab2 <- data.frame(tab2)
  
  v <- merge(tab,tab2,by.x="Var1",by.y="Var1",all = TRUE)
  
  
  #####################################################################
  #                           Pie Chart     
  #####################################################################
  
  #par(mfrow = c(1,2))
  par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=TRUE)
  
  
  # 2- Title :
  title <- "exp"
  
  # 3- Colors :
  c <- rainbow(length())
  
  # 4- Plot :
  pie(v[1:10,2],labels=v[,1], main = title ,col=c)
  
  
  ####
  
  # 2- Title :
  title <- "imp"
  
  # 3- Colors :
  c <- rainbow(length(tab2))
  
  # 4- Plot :
<<<<<<< HEAD
  pie(piepercent,labels = lab, main = title ,col=c)
  
  # 5- Legend :
  legend("topright", legend = (piepercent), cex = 0.8, fill = c ,  inset=c(-2,-5))
  
}


#####################################################################
#                           Function  
#####################################################################

# 1st argument : Country
# 2nd : 
#   0 -> Exportation
#   1 -> Importation


#par(mfrow = c(1,2))

#country_Import_Export("United Kingdom",0)
country_Import_Export("United Kingdom",0)
=======
  pie(tab2, main = title ,col=c)
>>>>>>> dfb269def6a0a231a5977d2764c43b4cd0d95781
