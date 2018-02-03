data <- c(1:100)

Leemis <- function(data){
  temp <- as.numeric(substr(data, 1, 1))
  temp.table <- as.data.frame(table(temp))
  freq <- temp.table$Freq/sum(temp.table$Freq)
  out <- numeric()
  for(j in 1:length(levels(temp.table$temp))){
    for(i in 1:9){
      if(as.numeric(levels(temp.table$temp)[j])==i){
        out[i] <- freq[j] - log10(1+1/i)
      }
    }
  }
  return(max(out))
}

(Leemis(data))

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
