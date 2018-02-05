#Kalen Davison
#Jacob Montgomery
#PS2
#Due 2/6/18

data <- c(1:100)  #test vector used throughout the PS to understand if the code is working

#Leemis function takes in vectors or matrices and returns the Leemis critical value.

Leemis <- function(data){  #set up function for inputted data called Leemis
  temp <- as.numeric(substr(data, 1, 1)) #defined a vector, "temp," within the function. Temp is the first digit of each element of the inputted vector
  temp.table <- as.data.frame(table(temp)) #creates "temp.table," which is "temp" in the form a frequency table.
  freq <- temp.table$Freq/sum(temp.table$Freq) #defines freq as the proportion of occurences for each digit out of the whole population
  out <- numeric() #ouput will be numberic data
  for(j in 1:length(levels(temp.table$temp))){ #for loop that defines j as a number 1-9. Takes into account the possibility of a first digit 1-9 missing in the inputted vector.
    for(i in 1:9){ #i=1, i=2, etc
      if(as.numeric(levels(temp.table$temp)[j])==i){ #if there are no missing first digits 1-9, then continue
        out[i] <- freq[j] - log10(1+1/i) #Given Leemis equation
      }
    }
  }
  return(max(out)) #returns max value of the outputted vector
}

(Leemis(data)) #test is successful

# The function ChoGains takes a vector or matrix in @data and returns the ChoGains critical value
ChoGains <- function(data){ #defines function as ChoGains
  temp <- as.numeric(substr(data, 1, 1)) 
  temp.table <- as.data.frame(table(temp))
  freq <- temp.table$Freq/sum(temp.table$Freq)
  out <- numeric()
  for(j in 1:length(levels(temp.table$temp))){
    for(i in 1:9){
      if(as.numeric(levels(temp.table$temp)[j])==i){
        out[i] <- (freq[j] - log10(1+1/i))^2 #ChoGains function is the same as the Leemis equation except for this line, different equation
      }
    }
  }
  return(sqrt(sum(out))) #returns the critical value
}

ChoGains(data)

#choose.function takes in a specification for the kind of statistical test desired and a vector or matrix. It can return one critical value or both critical values.
#@input can be "Leemis" which tells the function to run the Leemis function, "ChoGains" which tells the function to run the ChoGains function, or any character input which runs both functions
#@data can be in vector or matrix form
choose.function = function(input, data){ 
  temp <- as.numeric(substr(data, 1, 1)) 
  temp.table <- as.data.frame(table(temp))
  print(temp.table)
  if(input == "Leemis") {
    print(Leemis(data)) #if the input is "Leemis," then it will print the output of the Leemis function with the given data
  } else if (input == "ChoGains") {
    print(ChoGains(data)) #if the input is "ChoGains," then it will print the output of the ChoGain function with the given data
     } else{ (input == "Both") 
        print(c(Leemis(data), ChoGains(data))) #if the input is both or any other word, the outputs of both function will be printed
     }
  return()
}

choose.function("Leemis", data)
choose.function("ChoGains", data)
choose.function("Both", data) #tests successful

#2)
### Creating a function that takes in a matrix or vector and returns a table including
### the statistic, the statistic name, significance level, and legend.
 

# Function can return the critical value from Leemis or ChoGains along with asterisks that indicate the significance of the critical value
# @Input can be "ChoGains" or "Leemis," signals which critical value to return
# @Data can be in vector or matrix form
asterisk.function = function(input, data){
    if (input == "ChoGains" & ChoGains(data) >=1.212 & ChoGains(data) < 1.33) { 
      print(paste(ChoGains(data), "**", sep=" "))
    } else if (input == "ChoGains" & ChoGains(data) >= 1.33 & ChoGains(data) < 1.569) { 
      print(paste(ChoGains(data), "***", sep=" "))
    } else if (input == "ChoGains" & ChoGains(data) >= 1.569) {
      print(paste(ChoGains(data), "****", sep=" ")) 
   } else if (input == "ChoGains" & ChoGains(data) < 1.212){
    print(paste(ChoGains(data), "*", sep=" "))
     } else if (input == "Leemis" & Leemis(data) >=.851 & Leemis(data) < .967) { 
        print(paste(Leemis(data), "**", sep=" "))
      } else if (input == "Leemis" & Leemis(data) >= .967 & Leemis(data) < 1.212) { 
        print(paste(Leemis(data), "***", sep=" "))
      } else if (input =="Leemis" & Leemis(data) >= 1.212) {
        print(paste(Leemis(data), "****", sep=" ")) 
      } else if (input=="Leemis" & Leemis(data) < .851) 
      print(paste(Leemis(data), "*", sep=" "))
}

asterisk.function("ChoGains", data)

#the combined function basically adds a "both" option to the asterisk.function
combined = function(input, data){
  if (input == "ChoGains") {
      asterisk.function("ChoGains", data)
    } else if (input == "Leemis") {
      asterisk.function("Leemis", data)
    } else if (input == "Both") {
      asterisk.function("Leemis", data)
      asterisk.function("ChoGains", data)
    }
}

combined("ChoGains", data)
combined("Leemis", data)
combined("Both", data) #successful except it returns the output twice for some reason.


###
print.benfords = function(data){ 
  Stat_name = as.character(c("Leemis", "ChoGains"))
  Statistic = c(combined("Leemis", data), combined("ChoGains", data))
  stat.table = data.frame(Stat_name, Statistic)
  print(stat.table)
  cat("* = Fail to reject null hypothesis at 10% significance level", "** = Reject null hypothesis at 10% significance level",
                       "*** = Reject null hypothesis at 5% significance level", "**** = Reject null hypothesis at 1% significance level")
}

print.benfords(data)

benfords.writeCSV <- function(data){
  sink(file="Benford.csv")
  print.benfords(data)
  sink()
}

benfords.writeCSV(data)

test.data1 = c(1:49, 58:121)
benfords.writeCSV(test.data1)

library(readr) #vector data converted into benford table and read in as csv successfully
Benfords.Data <- read_csv("/Users/kalendavison/Desktop/Applied Statistical Programming/Benford.csv")
View(Benfords.Data)


test.data2 = matrix(1:100) #matrix data converted into benford table and read in as csv successfully
benfords.writeCSV(test.data2)
library(readr)
BenfordsData <- read_csv("~/Desktop/Applied Statistical Programming/Benford.csv")
View(BenfordsData)





