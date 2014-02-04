###############
#Problem Set 2#
#Taeyong Park #   
###############

##1. Calculating violations##

#A function to calculate the two statistics
test.benfords <- function (mat, methods) { #mat represents a matrix of data; the option methods chooses either/both Leemis or/and Cho-Gains.
  require(stringr)  #We will use the str_count function available from the stringr package.
  # We want to deal with the first digit values for the entire matrix.
  firstDigit <- vector("list") #Create an empty list that will store the the first digit values for the entire matrix.
  for (i in 1:ncol(mat)){ 
    firstDigit[[i]] <-substr(as.character(mat[,i]), start=1, stop=1) 
  }
  firstDigit <- unlist(firstDigit) # Unlist the list and store that values into a numeric vector named first.
  xi<-rep(NA, 9) 
  integers<-as.character(1:9)
  benfordDiff<-rep(NA, 9)
  for (i in 1:9){   #This for loop calculates the common part of the two statistics.
    xi[i] <- sum(str_count(firstDigit,integers[i]))/length(firstDigit)
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
expendData<-read.table("Expends2002.txt",  
                       header=T,          
                       sep=",") 
positiveAmount <- ifelse(expendData[, "Amount"]>0, expendData[, "Amount"], -expendData[, "Amount"])
a<-cbind(expendData[, "ID"][1:500], positiveAmount[1:500])
which(is.na(a)) #No missing cases
test.benfords(mat=a, methods="Leemis")
test.benfords(mat=a, methods="Cho-Gains")
test.benfords(mat=a, methods="Both")


##2. Critical values##
print.benfords <- function(mat){ 
  require(stringr)  #We will use the str_count function available from the stringr package.
  # We want to deal with the first digit values for the entire matrix.
  firstDigit <- vector("list") #Create an empty list that will store the the first digit values for the entire matrix.
  for (i in 1:ncol(mat)){ 
    firstDigit[[i]] <-substr(as.character(mat[,i]), start=1, stop=1) 
  }
  firstDigit <- unlist(firstDigit) # Unlist the list and store that values into a numeric vector named first.
  xi<-rep(NA, 9) 
  integers<-as.character(1:9)
  benfordDiff<-rep(NA, 9)
  for (i in 1:9){   #This for loop calculates the common part of the two statistics.
    xi[i] <- sum(str_count(firstDigit,integers[i]))/length(firstDigit)
    benfordDiff[i] <- xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)
  } 
  mStat <- round(sqrt(length(firstDigit))*max(benfordDiff), 3)
  dStat <- round(sqrt(length(firstDigit))*sqrt(sum(benfordDiff^2)), 3)
  criticalValueM <- c(0.851, 0.967, 1.212) # a vector for the critical values of m statistic
  criticalValueD <- c(1.212, 1.330, 1.569) # a vector for the critical values of d statistic
  astericks <- c(" ", "*", "**", "***") # a vector for the asterisks that represent the significance levels (0.1, 0.05, 0.01).
  # Create a 3 by 2 matrix for the output that will present the two statistics and the significance tests.
  output <- matrix(NA, ncol=2, nrow=4)
  colnames(output) <- c("Statistic", "Significance")
  rownames(output) <- c("Leemis' m", "Cho-Gains' d", "", "Signif.codes") # The row names contain the name of each statistic and a legend explaining the asterisks.
  output[,1] <- c(    mStat,    dStat, "", "")  # The top two entries in the first column store the m and d statistics. I leave the third row of the third column blank, since it is for a legend.
  if (output[1,1] >= criticalValueM[3]){ # If the value of the first row and the first column, which represents the vector's m statistic is greater or equal to the third element in the vector critval.m (=1.212),
    output[1,2] <- astericks[4] # store the 4th element, which is "***", to the first row and the second column of the matrix output.
  } else if (output[1,1] >= criticalValueM[2]){ # If the value of m statistic is greater or equal to 0.967, and less than 1.212,
    output[1,2] <- astericks[3] # store the third element of the vector alpha to the first row and the second column element of the matrix.
  } else if (output[1,1] >= criticalValueM[1]){ # If the value of m statistic greater or equal to 0.851 and less than 0.967,
    output[1,2] <- astericks[2] # the first row and the second column element of the matrix gets the value of the second element of the alpha.
  } else { # If the value of m statistic is less than 0.851,
    output[1,2] <- astericks[1] # the first row and the second column element gets the value of the first element of the alpha, which is blank.
  } 
  # The same applies to the d statistic, which is stored in the second row and the first colum of the matrix. If it is greater than or equal to 1.569, the second row and the second column element gets "***", and so on.
  if (output[2,1] >= criticalValueD[3]){ # If the value of the first row and the first column, which represents the vector's m statistic is greater or equal to the third element in the vector critval.m (=1.212),
    output[2,2] <- astericks[4] # store the 4th element, which is "***", to the first row and the second column of the matrix output.
  } else if (output[2,1] >= criticalValueD[2]){ # If the value of m statistic is greater or equal to 0.967, and less than 1.212,
    output[2,2] <- astericks[3] # store the third element of the vector alpha to the first row and the second column element of the matrix.
  } else if (output[2,1] >= criticalValueD[1]){ # If the value of m statistic greater or equal to 0.851 and less than 0.967,
    output[2,2] <- astericks[2] # the first row and the second column element of the matrix gets the value of the second element of the alpha.
  } else { # If the value of m statistic is less than 0.851,
    output[2,2] <- astericks[1] # the first row and the second column element gets the value of the first element of the alpha, which is blank.
  } 
  output[3,] <- c("-------", " ") 
  output[4,] <-  c("'*' 0.1,", "'**' 0.05,'***' 0.01") # The third row of the matrix contains a legend explaining the asterisks.
  return(as.table(output)) # Change the output into a table format, and return it. If we do not format into a table, the output looks messy with quotation marks.
} # close function print.benfords()

print.benfords(mat=a)



###3. Testing##
dataFitBenford <- c(rep(1,30), rep(2,18), rep(3,12),
                    rep(4,10), rep(5,8), rep(6,7),
                    rep(7,6), rep(8,5), rep(9,4))
dataFitBenford <- rep(dataFitBenford, 10)

dataNotFitBenford <- c(rep(1,11), rep(2,11), rep(3,11),
                       rep(4,11), rep(5,11), rep(6,11),
                       rep(7,11), rep(8,11), rep(9,12))
dataNotFitBenford <- rep(dataNotFitBenford, 10)


test.times.3.fn <- function(){
  times.3.fn <- function(thenumber){
    thenumber*3
  }
  if (times.3.fn(thenumber=4) == 12 &
        times.3.fn(thenumber=-4) == -12){
    print("It works!")
  } else {print("It does not work!")          
  }
}


if (vec == "fit"){
  statTestsBenford <- function (vec, methods) { #vec represents a vector of data; the option methods can be chose between Leemis and Cho-Gains
    require(stringr)  #We will use the str_count function available from the stringr package.                          
    xi<-rep(NA, 9)
    integers<-as.character(1:9)
    benfordDiff<-rep(NA, 9)
    for (i in 1:9){   #This for loop calculates the common part of the two statistics.
      xi[i] <- sum(str_count(substr(as.character(vec),start=1, stop=1),integers[i]))/length(vec)
      benfordDiff[i] <- xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)
    }  
    if (methods=="Leemis") {
      mStat <- max(benfordDiff) 
      output <- list(mStat, xi)
      names(output) <- c("Leemis' m statistic", "Full digit disribution (1~9)")
    }
    if (methods=="Cho-Gains"){
      dStat <- sqrt(sum(benfordDiff^2))
      output <- list(dStat, xi)
      names(output) <- c("Cho-Gains' d", "Full digit disribution (1~9)")
    }
    if(methods=="Both"){
      mStat <- max(benfordDiff)
      dStat <- sqrt(sum(benfordDiff^2))
      output <- list(mStat, dStat, xi)
      names(output) <- c("Leemis' m statistic", "Cho-Gains' d", "Full digit disribution (1~9)")
    }
    return(output)
  }
} else {
  statTestsBenford <- function (vec, methods) { #vec represents a vector of data; the option methods can be chose between Leemis and Cho-Gains
    require(stringr)  #We will use the str_count function available from the stringr package.                          
    xi<-rep(NA, 9)
    integers<-as.character(1:9)
    benfordDiff<-rep(NA, 9)
    for (i in 1:9){   #This for loop calculates the common part of the two statistics.
      xi[i] <- sum(str_count(substr(as.character(vec),start=1, stop=1),integers[i]))/length(vec)
      benfordDiff[i] <- xi[i] - log(1 + 1/as.numeric(integers[i]), base=10)
    }  
    if (methods=="Leemis") {
      mStat <- max(benfordDiff) 
      output <- list(mStat, xi)
      names(output) <- c("Leemis' m statistic", "Full digit disribution (1~9)")
    }
    if (methods=="Cho-Gains"){
      dStat <- sqrt(sum(benfordDiff^2))
      output <- list(dStat, xi)
      names(output) <- c("Cho-Gains' d", "Full digit disribution (1~9)")
    }
    if(methods=="Both"){
      mStat <- max(benfordDiff)
      dStat <- sqrt(sum(benfordDiff^2))
      output <- list(mStat, dStat, xi)
      names(output) <- c("Leemis' m statistic", "Cho-Gains' d", "Full digit disribution (1~9)")
    }
    return(output)
  }
}


if (times.3.fn(thenumber=4) == 12 &
      times.3.fn(thenumber=-4) == -12){
  print("It works!")
} else {print("It does not work!")          
}

