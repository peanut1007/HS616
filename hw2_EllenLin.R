#1
z <- matrix(10:19, nrow=5, ncol=2)

z_tr = matrix(z[1,1]:length(z),nrow = ncol(z), ncol = nrow(z))
for(i in 1: nrow(z)){
  for(j in 1: ncol(z)){
    z_tr[j,i] <- z[i,j]
  }
}
z_tr

#2
dat <- data.frame(radius=rep(NA),volume=rep(NA))
for (i in 3:20){
  dat[i,] <- c(i, i**3*(pi)*4/3)
  
}
dat
#3
library(MASS)
attach(ChickWeight)
table(Diet)
for(i in 1:4){
  weight1 <-subset(ChickWeight, Diet ==i)
  print(mean(weight1$weight))
}

for(i in length(ChickWeight$Time)){
  Time[i] <- round(Time[i]/356, 5)
  
}

