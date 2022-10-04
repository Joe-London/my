library(fmsb)
library(tidyverse)
library(Hmisc)
library(scatterplot3d)
library(plotly)

#For question a
mid_square_rng <- function(seed,N){
  mid<-NULL
  for (i in 1:N) {
    square <- seed * seed
    y <- nchar(square, type = 'bytes')
    if(y <8){
      k <- 8-y
      for (j in 1:k) {
        square <- paste0("0",as.character(square) )
      }
    }
    else{}
    Y <- as.numeric(substr(square, start = 3, stop = 6))
    seed <- as.numeric(paste(Y,collapse= ""))
    mid <- c(mid, seed/10000)
  }
  return(mid)
}

#For question b. we got a random serise but it become really small. 
mid_square_rng(1010, 20)

#For question c. we even got a loop which is 0.0009-0.0008-0.0006-0.0003-0.0009.....
mid_square_rng(6100,20)

#For question d. it stays 0.3792. since 3792 *3792 = 14379264. so it stays.
mid_square_rng(3792,20)

# For question e. 9999 * 9999 = 99980001. it is 8 digit.
#and it is the biggest number you can have. 
#so they would not have more than 8 digits. 

# For question f. Based on what I got from question b. the number become really
#small. so it is not going to cover every thing. So it is not a reliable way. 

#For question g. It have a loop. It state to repeat. 






#For question a
lehmer_rng <- function(N, m, a, b, X0){
  rng <- vector(length = N)
  X <- NULL
  for (i in 1:N) {
    X <- (a * X0 + b) %% m
    X0 <- X
    P <- X0 / m
    rng[i] <- P
  }
  print(rng)
  return(rng)
}


#For question b.
A <- matrix(data = NA, nrow = 13, ncol = 16, byrow = FALSE )
k <- 1
for (i in 3:15) {
  row_i <- lehmer_rng(16, 16, i, 1, 1)
  A[k,] <- row_i
  k <- k+1
}

dfA <- data.frame(rbind(rep(1,16), rep(0,16),A))
df1 <- data.frame(rbind(rep(1,16), rep(0,16),A[1,]))
df2 <- data.frame(rbind(rep(1,16), rep(0,16),A[2,]))
df3 <- data.frame(rbind(rep(1,16), rep(0,16),A[3,]))
df4 <- data.frame(rbind(rep(1,16), rep(0,16),A[4,]))
df5 <- data.frame(rbind(rep(1,16), rep(0,16),A[5,]))
df6 <- data.frame(rbind(rep(1,16), rep(0,16),A[6,]))
df7 <- data.frame(rbind(rep(1,16), rep(0,16),A[7,]))
df8 <- data.frame(rbind(rep(1,16), rep(0,16),A[8,]))
df9 <- data.frame(rbind(rep(1,16), rep(0,16),A[9,]))
df10 <- data.frame(rbind(rep(1,16), rep(0,16),A[10,]))
df11 <- data.frame(rbind(rep(1,16), rep(0,16),A[11,]))
df12 <- data.frame(rbind(rep(1,16), rep(0,16),A[12,]))
df13 <- data.frame(rbind(rep(1,16), rep(0,16),A[13,]))

radarchart(dfA, title = "A total", axistype = 1, axislabcol = "grey", cglty = 1, cglcol = "gray", plwd = 1.5, plty = 1)
radarchart(df1,title = "a=3")
radarchart(df2,title = "a=4")
radarchart(df3,title = "a=5")
radarchart(df4,title = "a=6")
radarchart(df5,title = "a=7")
radarchart(df6,title = "a=8")
radarchart(df7,title = "a=9")
radarchart(df8,title = "a=10")
radarchart(df9,title = "a=11")
radarchart(df10,title = "a=12")
radarchart(df11,title = "a=13")
radarchart(df12,title = "a=14")
radarchart(df13,title = "a=15")
#For question c
B <- matrix(data = NA, nrow = 7, ncol = 16, byrow = FALSE )
l <- 1
for (h in 2:8) {
  row_j <- lehmer_rng(16, 16, 5, h, 1)
  B[l,] <- row_j
  l <- l+1
}

dfB <- data.frame(rbind(rep(1,16), rep(0,16),B))
dfb1 <- data.frame(rbind(rep(1,16), rep(0,16),B[1,]))
dfb2 <- data.frame(rbind(rep(1,16), rep(0,16),B[2,]))
dfb3 <- data.frame(rbind(rep(1,16), rep(0,16),B[3,]))
dfb4 <- data.frame(rbind(rep(1,16), rep(0,16),B[4,]))
dfb5 <- data.frame(rbind(rep(1,16), rep(0,16),B[5,]))
dfb6 <- data.frame(rbind(rep(1,16), rep(0,16),B[6,]))
dfb7 <- data.frame(rbind(rep(1,16), rep(0,16),B[7,]))



radarchart(dfB,title = "B total", axistype = 1, axislabcol = "grey", cglty = 1, cglcol = "gray", plwd = 1.5, plty = 1)
radarchart(dfb1,title = "b=2")
radarchart(dfb2,title = "b=3")
radarchart(dfb3,title = "b=4")
radarchart(dfb4,title = "b=5")
radarchart(dfb5,title = "b=6")
radarchart(dfb6,title = "b=7")
radarchart(dfb7,title = "b=8")

#For question d, it is not random, it more likely to have small numbers.
D <- lehmer_rng(20, 100, 21, 1, 6)
hist(D)

#For question e， the frequency is nearly the same. so it is random. 
E <- lehmer_rng(5000,2^11,1229,1,1)
hist(E, breaks = 50)

#For question f
E_p <- Lag(E,1)
E_p[1] <- 1
plot(E_p,E ,xlab="E_p ", ylab="E")

#For question g. It is better since the next one do not depend on previous one. 
G <- lehmer_rng(5000, 244944, 1597, 51749, 1)
hist(G, breaks = 50)

G_p <- Lag(G, 1)
G_p[1] <- 1
plot(G_p,G,xlab="G_p ", ylab="G")


#For question h
H <- lehmer_rng(5000, 2^31, 2^16+3, 0, 1)
hist(H, breaks = 50)

H_p <- Lag(H, 1)
H_p[1] <- 1
plot(H_p,H,xlab="H_p ", ylab="H")
#chi square test

#For question i
I <- H
I_p <- Lag(I,1)
I_p[1] <- 1
I_pp <- Lag(I_p,1)
I_pp[1] <- 0
scatterplot3d(I_p, I, I_pp)
k<-data_frame(I_p,I,I_pp)
g <- plot_ly(k,x=~I_p,y=~I,z=~I_pp,size=1)
g

