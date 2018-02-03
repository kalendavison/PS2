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

ChoGains <- function(data){
  temp <- as.numeric(substr(data, 1, 1))
  temp.table <- as.data.frame(table(temp))
  freq <- temp.table$Freq/sum(temp.table$Freq)
  out <- numeric()
  for(j in 1:length(levels(temp.table$temp))){
    for(i in 1:9){
      if(as.numeric(levels(temp.table$temp)[j])==i){
        out[i] <- (freq[j] - log10(1+1/i))^2
      }
    }
  }
  return(sqrt(sum(out)))
}

ChoGains(data)

choose.function = function(input, data){
  if(input == "Leemis") {
    print(Leemis(data))
  } else if (input == "ChoGains") {
    print(ChoGains(data))
     } else{ (input == "Both")
        print(c(Leemis(data), ChoGains(data)))
     }
  return()
}

choose.function("Both", data)
