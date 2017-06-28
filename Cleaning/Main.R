# Files / More / Set As Working Directory

#####################################################################
#                             Library
#####################################################################

#install.packages("stringr")
#install.packages("units")

library(stringr)
library(units)


#####################################################################
#                         Using files :
#####################################################################

source("cleaning.R")
source("computerReadable.R")


#####################################################################
#                            Main :
#####################################################################

  # 1- Importation of the database :
data <- as.data.frame(read.csv("alphaspider.csv"))

  # 2- Cleaning :
data <- cleaningData(data)

  # 3- Making data readable in a computering way :
data <- computerReadable(data)



#####################################################################
#                  exportation of the new data
#####################################################################

#write.csv(data, "data.csv")
