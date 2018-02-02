input <- c(1:100)

Leemis <- function(input){
  temp <- as.numeric(substr(input, 1, 1))
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
  return(out)
}

Leemis(input)


ChoGains <- function(input){
  temp <- as.numeric(substr(input, 1, 1))
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
  return(out)
}

sqrt(sum(ChoGains(input)))



