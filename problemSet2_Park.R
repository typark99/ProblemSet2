###############
#Problem Set 2#
#Taeyong Park #   
###############

#####1. CALCULATING VIOLATIONS#####

## Create a function named test.benfords to calculate Leemis' statistic and Cho-Gains' statistic
test.benfords <- test.benfords <- function (data, methods) { #data can be either matrix or vector; the methods option chooses either/both Leemis or/and Cho-Gains.
  require(stringr)  #We will use the str_count function available from the stringr package.
  
  ## We want to deal with the first digit values for the entire matrix.
  if (class(data)=="matrix") { #This is for the case where the input data have the matrix form.
    firstDigit <- vector("list") #Create an empty list that will store the the first digit values for the entire matrix.
    for (i in 1:ncol(data)){ 
      firstDigit[[i]] <-substr(as.character(data[,i]), start=1, stop=1) #Use the substr function to extract only the first digit for each datum.
    }
    firstDigit <- unlist(firstDigit) #Unlist the list to store the first values in the form of vector.
  } else { #If the input data have the vector form, we can directly extract the first digit values from the vector.
    firstDigit <-substr(as.character(data), start=1, stop=1)
  }
  
  xi<-rep(NA, 9)  #Create a vector storage that will contain the Xi part of the statistics
  integers<-as.character(1:9)  #Treat 1-9 integers as a character so that the str_count function can work below.
  benfordDiff<-rep(NA, 9) #Create a vector storage that will contain the "xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)" part. 
  for (i in 1:9){   #This for loop calculates the common part of the two statistics.
    xi[i] <- sum(str_count(firstDigit, integers[i]))/length(firstDigit)
    benfordDiff[i] <- xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)
  }  
  if (methods=="Leemis") {
    mStat <- sqrt(length(firstDigit))*max(benfordDiff) 
    output <- list(mStat, xi)
    names(output) <- c("Leemis' m statistic", "Full digit disribution (1~9)")
  }
  if (methods=="Cho-Gains"){
    dStat <- sqrt(length(firstDigit))*sqrt(sum(benfordDiff^2))
    output <- list(dStat, xi)
    names(output) <- c("Cho-Gains' d", "Full digit disribution (1~9)")
  }
  if(methods=="Both"){
    mStat <- sqrt(length(firstDigit))*max(benfordDiff)
    dStat <- sqrt(length(firstDigit))*sqrt(sum(benfordDiff^2))
    output <- list(mStat, dStat, xi)
    names(output) <- c("Leemis' m statistic", "Cho-Gains' d", "Full digit disribution (1~9)")
  }
  return(output)
}

#To check if the function works let's apply the function to expendData$ID.
library(foreign)
expendData<-read.table("C:/Users/Taeyong/Documents/GitHub/ProblemSet2/Expends2002.txt",  
                       header=T,          
                       sep=",") 
positiveAmount <- ifelse(expendData[, "Amount"]>0, expendData[, "Amount"], -expendData[, "Amount"])
a<-cbind(expendData[, "ID"][1:500], positiveAmount[1:500]) #matrix
#a<-expendData[, "ID"][1:500] #vector
which(is.na(a)) #No missing cases
test.benfords(data=a, methods="Leemis")
test.benfords(data=a, methods="Cho-Gains")
test.benfords(data=a, methods="Both")


#####2. CRITICAL VALUES#####
print.benfords <- function (data, methods) { #data can be either matrix or vector; the methods option chooses either/both Leemis or/and Cho-Gains.
  require(stringr)  #We will use the str_count function available from the stringr package.
  
  ## We want to deal with the first digit values for the entire matrix.
  if (class(data)=="matrix") { #This is for the case where the input data have the matrix form.
    firstDigit <- vector("list") #Create an empty list that will store the the first digit values for the entire matrix.
    for (i in 1:ncol(data)){ 
      firstDigit[[i]] <-substr(as.character(data[,i]), start=1, stop=1) #Use the substr function to extract only the first digit for each datum.
    }
    firstDigit <- unlist(firstDigit) #Unlist the list to store the first values in the form of vector.
  } else { #If the input data have the vector form, we can directly extract the first digit values from the vector.
    firstDigit <-substr(as.character(data), start=1, stop=1)
  }
  
  xi<-rep(NA, 9) #Create a vector storage that will contain the Xi part of the statistics
  integers<-as.character(1:9)   #Treat 1-9 integers as a character so that the str_count function can work below.
  benfordDiff<-rep(NA, 9)  #Create a vector storage that will contain the "xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)" part. 
  for (i in 1:9){  #This for loop calculates the common part of the two statistics.
    xi[i] <- sum(str_count(firstDigit,integers[i]))/length(firstDigit)
    benfordDiff[i] <- xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)
  } 
  mStat <- round(sqrt(length(firstDigit))*max(benfordDiff), 3) #Calculate Leemis' m statistic
  dStat <- round(sqrt(length(firstDigit))*sqrt(sum(benfordDiff^2)), 3) #Calculate Cho-Gains' d statistic
  criticalValueM <- c(0.851, 0.967, 1.212) # a vector for the critical values of m statistic
  criticalValueD <- c(1.212, 1.330, 1.569) # a vector for the critical values of d statistic
  astericks <- c(" ", "*", "**", "***") # a vector for the asterisks that represent the significance levels (0.1, 0.05, 0.01).
 
  ## Create a 4 by 2 matrix for the output that will return the two statistics and the significance tests.
  output <- matrix(NA, ncol=2, nrow=4) 
  colnames(output) <- c("Statistic", "Significance")  #The column names
  rownames(output) <- c("Leemis' m", "Cho-Gains' d", "-----", "Signif.codes") #The row names; The name of third row returns "-----" so that it can separate the result part and the legend. 
  output[,1] <- c(mStat, dStat, "", "")  #The first two rows of the first column return mStat and dStat, respectively; The third row should be empty since this row is just to separate the result part and the legend; The fourth row is for the legend.
  if (output[1,1] >= criticalValueM[3]){ #If the value of the first row and the first column, which is mStat, is greater than or equal to 1.212,
    output[1,2] <- astericks[4] #return "***" for the first row of the second column.
  } else if (output[1,1] >= criticalValueM[2]){ #Otherwise, compare mStat with 0.967, and if mStat is greater than or equal to 0.967,
    output[1,2] <- astericks[3] #return "**" for the first row of the second column.
  } else if (output[1,1] >= criticalValueM[1]){ #Otherwise, compare mStat with 0.851, and if mStat is greater than or equal to 0.851,
    output[1,2] <- astericks[2] #return "*" for the first row of the second column.
  } else { #Otherwise,
    output[1,2] <- astericks[1] #return blank for the first row of the second column.
  } 
  # The same applies to the d statistic, which is stored in the second row and the first colum of the matrix. If it is greater than or equal to 1.569, the second row and the second column element gets "***", and so on.
  if (output[2,1] >= criticalValueD[3]){ #If the value of the second row and the first column, which is dStat, is greater than or equal to 1.569,
    output[2,2] <- astericks[4] #return "***" for the second row of the second column.
  } else if (output[2,1] >= criticalValueD[2]){ #Otherwise, compare dStat with 1.330, and if dStat is greater than or equal to 1.330,
    output[2,2] <- astericks[3] #return "**" for the second row of the second column.
  } else if (output[2,1] >= criticalValueD[1]){ #Otherwise, compare dStat with 1.212, and if dStat is greater than or equal to 1.212,
    output[2,2] <- astericks[2] #return "*" for the second row of the second column.
  } else { #Otherwise,
    output[2,2] <- astericks[1] #return blank for the second row of the second column.
  } 
  output[3,] <- c(" ", " ")  #The two columns for the third row are empty, since this row is just for separating the result part and the legend.
  output[4,] <-  c("'*' 0.1,", "'**' 0.05,'***' 0.01") # The fourth row contains the legend that explains what the asterisks mean.
  return(as.table(output)) #Treat the output as a table to remove quotation marks.
} 

##Run the function using the example data set we created earlier.
print.benfords(data=a)


#####3. TESTING#####
unit.testing <- function () { 
  ## Start with generating two data sets such that one data set fits Benford's law, while the other does not.
  dataFitBenford <- rep(1:9, round(log(1+(1/(1:9)), base=10)*99)) #Create a vector of 100 values that meet Benford's law.
  dataNotFitBenford <- c(rep(1:9, 11), 9) #Create a vector of 100 values that does not meet Benford's law.
  
  ## Generate true values based on each of the data sets.
  # True values for dataFitBenford:
  firstDigitFit <-substr(as.character(dataFitBenford), start=1, stop=1) #Our data set is a vector.
  xiFit<-rep(NA, 9)  #Create a vector storage that will contain the Xi part of the statistics
  integers<-as.character(1:9)  #Treat 1-9 integers as a character so that the str_count function can work below.
  benfordDiffFit<-rep(NA, 9) #Create a vector storage that will contain the "xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)" part. 
  for (i in 1:9){   #This for loop calculates the common part of the two statistics.
    xiFit[i] <- sum(str_count(firstDigitFit, integers[i]))/length(firstDigitFit)
    benfordDiffFit[i] <- xiFit[i] - log(1 + 1/as.numeric(integers[i]), base=10)
  }
  mStatFit <- sqrt(length(firstDigitFit))*max(benfordDiffFit)
  dStatFit <- sqrt(length(firstDigitFit))*sqrt(sum(benfordDiffFit^2))
  truthFit <- list(mStatFit, dStatFit, xiFit)
  #True values for dataNotFitBenford:
  firstDigitNotFit <-substr(as.character(dataNotFitBenford), start=1, stop=1) #Our data set is a vector.
  xiNotFit<-rep(NA, 9)  #Create a vector storage that will contain the Xi part of the statistics
  benfordDiffNotFit<-rep(NA, 9) #Create a vector storage that will contain the "xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)" part. 
  for (i in 1:9){   #This for loop calculates the common part of the two statistics.
    xiNotFit[i] <- sum(str_count(firstDigitNotFit, integers[i]))/length(firstDigitNotFit)
    benfordDiffNotFit[i] <- xiNotFit[i] - log(1 + 1/as.numeric(integers[i]), base=10)
  }
  mStatNotFit <- sqrt(length(firstDigitNotFit))*max(benfordDiffNotFit)
  dStatNotFit <- sqrt(length(firstDigitNotFit))*sqrt(sum(benfordDiffNotFit^2))
  truthNotFit <- list(mStatNotFit, dStatNotFit, xiNotFit)
  
  #Include my function, test.benfords(), to conduct unit tests.
  test.benfords <- function (data, methods="Both") { #data can be either matrix or vector; the methods option is set to be "Both" as default.
    require(stringr) 
    if (class(data)=="matrix") { #This is for the case where the input data have a matrix form.
      firstDigit <- vector("list") #Create an empty list that will store the the first digit values for the entire matrix.
      for (i in 1:ncol(data)){ 
        firstDigit[[i]] <-substr(as.character(data[,i]), start=1, stop=1) #Use the substr function to extract only the first digit for each datum.
      }
      firstDigit <- unlist(firstDigit) #Unlist the list to store the first values in the form of vector.
    } else { #If the input data have the vector form, we can directly extract the first digit values from the vector.
      firstDigit <-substr(as.character(data), start=1, stop=1)
    } 
    xi<-rep(NA, 9)  #Create a vector storage that will contain the Xi part of the statistics
    integers<-as.character(1:9)  #Treat 1-9 integers as a character so that the str_count function can work below.
    benfordDiff<-rep(NA, 9) #Create a vector storage that will contain the "xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)" part. 
    for (i in 1:9){   #This for loop calculates the common part of the two statistics.
      xi[i] <- sum(str_count(firstDigit, integers[i]))/length(firstDigit)
      benfordDiff[i] <- xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)
    }  
    if (methods=="Leemis") {
      mStat <- sqrt(length(firstDigit))*max(benfordDiff) 
      output <- list(mStat, xi)
      names(output) <- c("Leemis' m statistic", "Full digit disribution (1~9)")
    }
    if (methods=="Cho-Gains"){
      dStat <- sqrt(length(firstDigit))*sqrt(sum(benfordDiff^2))
      output <- list(dStat, xi)
      names(output) <- c("Cho-Gains' d", "Full digit disribution (1~9)")
    }
    if(methods=="Both"){
      mStat <- sqrt(length(firstDigit))*max(benfordDiff)
      dStat <- sqrt(length(firstDigit))*sqrt(sum(benfordDiff^2))
      output <- list(mStat, dStat, xi)
      names(output) <- c("Leemis' m statistic", "Cho-Gains' d", "Full digit disribution (1~9)")
    }
    return(output)
  }
  
  ## Compare the results from my test.benfords function with the true values. 
  #Unit tests for the data that fit Benford's law
  unitTestFit <- c(test.benfords(dataFitBenford)[[1]] == truthFit[[1]],
                   test.benfords(dataFitBenford)[[2]] == truthFit[[2]],
                   test.benfords(dataFitBenford)[[3]] == truthFit[[3]])
  #Unit tests for the data that do not fit Benford's law
  unitTestNotFit <- c(test.benfords(dataNotFitBenford)[[1]] == truthNotFit[[1]],
                      test.benfords(dataNotFitBenford)[[2]] == truthNotFit[[2]],
                      test.benfords(dataNotFitBenford)[[3]] == truthNotFit[[3]])
  
  ## Conduct unit tests for my function through the following steps: 
  if (FALSE %in% unitTestFit[3:11]){
    print("FALSE: The function calculates the wrong Benford¡¯s distribution for dataset 1 that fit Benford's law")
  } else if (FALSE %in% unitTestNotFit[3:11]){
    print("FALSE: The function calculates the wrong Benford¡¯s distribution for dataset 2 that do not fit Benford's law")
  } else if (FALSE %in% unitTestFit[1:2]){
    print("FALSE: The function calculates the wrong m or D statistic for dataset 1 that fit Benford's law")
  } else if (FALSE %in% unitTestNotFit[1:2]){
    print("The function calculates the wrong m or D statistic for dataset 2 that do not fit Benford's law")
  } else {
    print("TRUE")
  }
}

unit.testing()