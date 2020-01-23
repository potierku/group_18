ameslist <- read.table("ames.csv",header = TRUE,sep = ",")

#Run the above, but instead specifying header = FALSE. What data type are 
#the various columns? Now try ommitting the line altogether. 
#What is the default behavior of the read.table function?1

names(ameslist)

#Go through the variables in the dataset and make a note about your 
#interpretation for each. 
#Many will be obvious, but some require additional thought.

typeof(ameslist)
