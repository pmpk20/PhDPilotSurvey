# Script to analyse pilot survey data


#Section One: Import Data
## Import from CSV and convert to data frame with relevant columns
Pilot <- data.frame(read.csv("PhD Survey_ Sample A.csv"))
Pilot <- Pilot[ -c(1:2,8,27)]
Pilot2 <- Pilot
#Section Two: Pre-processing.
## To do: Convert to factors
##        Change column names
for (i in colnames(Pilot)){
  if (is.factor(Pilot[[i]]) == TRUE){
    Pilot2[[i]] <- as.numeric(Pilot[[i]])-1
  }
}

## Factor levels:
# Male: 1, Female: 0
# Age: increases in number
# Coast: WRONG as it doesn't order 100+ properly
# CE: Sets SQ as 1, Alt =0 so need to fix
# What do we do with the responsibility question
# Understanding question should be weak, average, strong not 2,1,0
# Education, employment and income all need changing







