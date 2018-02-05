data <- c(1:100)  #test dataset used to understand if the code is working

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

ChoGains <- function(data){ #defines function as ChoGains, data is input)
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
  return(sqrt(sum(out))) #returns one number
}

ChoGains(data)

choose.function = function(input, data){ #choose.function has to two inputs, "input" and "data"
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

#making a function for activity four that returns significance

Leemis.sig= function(data) { #will return if leemis critical value is significant
   if (Leemis(data) >=.851 & Leemis(data) < .967) { #if critical value is between these .851 and .967, it's significant at .1
      print("Critical value is significant at .10")
    } else if (Leemis(data) >= .967 & Leemis(data) < 1.212) { 
      print("Critical value is significant at .05")
       } else if (Leemis(data) >= 1.212) 
         print("Critical value is significant at .01") 
            else (Leemis(data) < .851) #if critical value is less than .851, no significance
            print ("Critical value is not significant")
}
    

Leemis.sig(data) #test works



ChoGains.sig= function(data) { #will return if ChoGain critical value is significant
  if (ChoGains(data) >=1.212 & ChoGains(data) < 1.33) { #if critical value is between these 1.212 and 1.33, it's significant at .1
    print("Critical value is significant at .10")
  } else if (ChoGains(data) >= 1.33 & ChoGains(data) < 1.569) { 
    print("Critical value is significant at .05")
  } else if (ChoGains(data) >= 1.569) 
    print("Critical value is significant at .01") 
  else (ChoGains(data) < 1.212) #if critical value is less than  1.212, no significance
  print ("Critical value is not significant")
}

ChoGains.sig(data) #test works



# make function that unifies ChoGains and Leemis

sig.function = function(input, data = 1:100){ #must specify data or returns error 
  if (input == "Leemis"){
    print(Leemis.sig(data)) #returns Leemis critical value significance if input is Leemis
  }  else if (input == "ChoGains"){
      print(ChoGains.sig(data)) #returns ChoGains sig level if input is ChoGains
    } else {
      print(c(ChoGains.sig(data), Leemis.sig(data))) #returns both sig levels if input is anything else
    }
  }

sig.function("ChoGains") #test works but for some reason returns the print twice.


#Master function

Benford = function (input, data){ 
  output = c(choose.function(input, data), sig.function(input, data)) #combines the functions. Will return the critical value of whichever function is specified along with significance.
}

Benford("Leemis", data)
Benford("ChoGains", data)
Benford("Both", data)  #all tests are successful except that it prints the critical value's significance twice, but the results are still correct.
  

### Creating a function that takes in a matrix or vector and returns a table including
### the statistic, the statistic name, significance level, and legend

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

combined = function(input, data){
  if (input == "ChoGains") {
      print(asterisk.function("ChoGains", data))
    } else if (input == "Leemis") {
      print(asterisk.function("Leemis", data))
    } else {
      print(c(asterisk.function("Leemis", data), asterisk.function("ChoGains", data)))
    }
}
print.benfords("Leemis", data)
print.benfords("d", data)

print.benfords = function(data){
  Stat_name = as.character(c("Leemis", "ChoGains"))
  Statistic = combined("both", data)
  stat.table = data.frame(Stat_name, Statistic)
  legend = rbind("* = Fail to reject null hypothesis at 10% significance level", "** = Reject null hypothesis at 10% significance level",
                  "*** = Reject null hypothesis at 5% significance level", "**** = Reject null hypothesis at 1% significance level")
  return(list(stat.table, legend))
}

print.benfords(data)

benfords.writeCSV = function (x, Benfords.file.path){ #function saves table as a csv in specified working directory
  setwd(Benfords.file.path)
  write.as = file("BenfordsData.csv")
  stat.table = print.benfords(x)
  sink(write.as)
  write.csv(stat.table, write.as)
  close(write.as)
  }

test.data1 = c(1:49, 58:121)
benfords.writeCSV(test.data, "/Users/kalendavison/Desktop/Applied Statistical Programming")

library(readr) #vector data converted into benford table and read in as csv successfully
BenfordsData <- read_csv("~/Desktop/Applied Statistical Programming/BenfordsData.csv")
View(BenfordsData)


test.data2 = matrix(1:100) #matrix data converted into benford table and read in as csv successfully
benfords.writeCSV(test.data2, "/Users/kalendavison/Desktop/Applied Statistical Programming")
library(readr)
BenfordsData <- read_csv("~/Desktop/Applied Statistical Programming/BenfordsData.csv")
View(BenfordsData)

  




