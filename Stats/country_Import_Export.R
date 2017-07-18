
#data <- as.data.frame(read.csv("data.csv"))
  
  library(stringr)

  #-----------------------------------------------
  #   Importation / Exportation of a country
  #-----------------------------------------------
  
    #-------------------
    #  Initialization
    #-------------------
    
    country <- "Canada"
    
    #---------------------------
    #    Analysis - Exportation
    #---------------------------
    
    # Country as origin
    matching_vector <- str_detect(data[,"origin"], country)
    
    # list of the categories (among the line that have "Country" as origin)
    # -> Products (categories) exporting by the country 
    country_cat <- data[matching_vector,"category"] 
    
    # Handling of this categories
    # Regular expression for spliting the categories
    regex <- "/(.*)/(.*)/(.*)"
    cat <- str_match(country_cat, regex)
    
    # Counting this categories 
    tab_exp <- table(cat[,3])   #cat[,3] : 2nd category 
    tab_exp <- sort(tab_exp, decreasing = TRUE)  # Sorting (biggest in first) 
    tab_exp <- tab_exp[1:10] # Taking only the most important
    
    #---------------------------
    #    Analysis - Importation
    #---------------------------
    
    # Country as destination
    matching_vector <- str_detect(data[,"destination"], country)
    
    # list of the categories (among the line that have "Country" as destination)
    # -> Products (categories) importing by the country 
    country_cat <- data[matching_vector,"category"] 
    
    # Handling of this categories
    # Regular expression for spliting the categories
    regex <- "/(.*)/(.*)/(.*)"
    cat <- str_match(country_cat, regex)
    
    # Counting this categories 
    tab_imp <- table(cat[,3])   #cat[,3] : 2nd category 
    tab_imp <- sort(tab_imp, decreasing = TRUE)  # Sorting (biggest in first) 
    tab_imp <- tab_imp[1:10] # Taking only the most important
    
    #-------------------------
    #    Analysis - Fusion
    #-------------------------
    
    # Transformation in data frame
    tab_exp <- as.data.frame(tab_exp)
    tab_imp <- as.data.frame(tab_imp)
    
    # Merger of the 2 data frame in order to have the same labels 
    tab <- merge(tab_exp,tab_imp,by.x="Var1",by.y="Var1",all = TRUE)
    
    # Handling of the "NA" value (substitution by 0)
    for (j in 2:3) {
      for(i in 1:length(tab[,j])){
        if(is.na(tab[i,j])) {tab[i,j] <-0}
      }
    }  
    
    #---------------------------
    #    Pie Chart - Exporation     
    #---------------------------
    
    # ploting 2 graphics om the same picture
    par(mfrow = c(1,2))
    
    # 1- Labels :
    
    # Calculation in percentage
    piepercent <- round(100*tab[,2]/sum(tab[,2]), 1)
    # round(a,1) : one digit after the comma
    
    lab <- c()
    
    for(i in 1:length(piepercent)) {
      if(piepercent[[i]] == 0) {lab[i] <- ""} 
      else {lab[i] <- paste(piepercent[[i]], "%", sep=" ")}
    }

    # 2- Colors :
    c <- rainbow(length(tab[,1]))
    
    # 3- Plot :
    pie(piepercent,labels=lab,main="Exportation",col=c)
    
    #---------------------------
    #    Pie Chart - Importation     
    #---------------------------
    
    # 1- Labels :
    
    # Calculation in percentage
    piepercent <- round(100*tab[,3]/sum(tab[,3]), 1)
    # round(a,1) : one digit after the comma
    
    lab <- c()
    
    for(i in 1:length(piepercent)) {
      if(piepercent[[i]] == 0) {lab[i] <- ""}
      else {lab[i] <- paste(piepercent[[i]], "%", sep=" ")}
    }

    # 2- Plot :
    pie(piepercent,labels=lab,main="Importation",col=c)
    
    #------------------
    #   General - Plot     
    #-----------------
    
    par(oma=c(0,0,1,0))
    title("France",outer=TRUE)
    legend(x=-4.3,y=-1,tab[,1], cex = 0.8, fill=c,ncol=4,border=NA, xpd=NA)
    
    