#generate table for criteria
#s <- c(0.00238,0.00548,0.00655,0.00300)
#u <- c(0.00043,0.0008,0.0019,0.0021)
#m <- c(0.00225,0.00359,0.0071,0.0101)
#range <- c('age45','age55','age65','age75')
#male <-data.frame(range,s,u,m)

angina_simulation<-function(gender,age){
  if (gender=='male') 
    {s <- c(0.00238,0.00548,0.00655,0.00300)
    u <- c(0.00043,0.0008,0.0019,0.0021)
    m <- c(0.00225,0.00359,0.0071,0.0101)} else{
      s <- c(0.00098,0.00357,0.00333,0.006)
      u <- c(0.00029,0.00030,0.00039,0.00048)
      m <- c(0.0003,0.00165,0.00236,0.0059)
      }
  event <- 0
  
  counter <-1
  
  
  while(age<85 && event==0)
  {
    if (age<55) {counter <- 1} else if (age<65) {counter <- 2
      }else if (age<75) {counter <- 3}else {counter <-4 }
  
      if (runif(1,0,1) < s[counter]) {event <- 1} else if (runif(1,0,1) < u[counter]) {event<-2
      } else if (runif(1,0,1) < m[counter]) {event<-3}
    
    if (event==0) {age <- age+1}
  }
  
  return (c(age,event))
}

set.seed(22)
i=0

a<- c(0,0)

s_vector <-c()
u_vector<-c()
m_vector<-c()
healthy_vector<-c()

while(i<1000){
  a<-angina_simulation('female',45)
  if (a[2]==0) {healthy_vector <- c(healthy_vector,a[1])}
  if (a[2]==1) {s_vector <- c(s_vector,a[1])}
  if (a[2]==2) {u_vector <- c(u_vector,a[1])}
  if (a[2]==3) {m_vector <- c(m_vector,a[1])}
  i<- i+1
}
survive = length(healthy_vector)/1000
#62.3% male health till 85 years old.
#change gender to female and simulate the data again.
#78.5% female stays health till 85 years old. 
1-survive

library("reshape2")
library("ggplot2")

occurance <- list(s_vector,u_vector,m_vector)
L <-melt(occurance)
g <- ggplot(L,aes(x=value, fill = as.factor(L1)))
g+geom_histogram(binwidth=2, position="dodge",title(xlab = "age", ylab = "count of events"))
#g+geom_histogram(binwidth = 1, position = "identity", alpha =0.5)
g+geom_density(position="dodge",alpha=0.5)
par(mfrow=c(1,2))
#female
occurance2 <- list(s_vector,u_vector,m_vector)
L <-melt(occurance2)
g2 <- ggplot(L,aes(x=value, fill = as.factor(L1)))
g2+geom_histogram(binwidth=2, position="dodge",title(xlab = "age", ylab = "count of events"))
#g+geom_histogram(binwidth = 1, position = "identity", alpha =0.5)
g2+geom_density(position="dodge",alpha=0.5)
par(mfrow=c(1,2))
g+geom_histogram(binwidth=2, position="dodge",title(xlab = "age", ylab = "count of events"))
g2+geom_histogram(binwidth=2, position="dodge",title(xlab = "age", ylab = "count of events"))

#statistical analysis
#mean of male and female having MI at the same age?
male <-lapply(occurance, mean)
female <- lapply(occurance2, mean)
t.test(as.numeric(occurance[[3]]),as.numeric(occurance2[[3]]))

