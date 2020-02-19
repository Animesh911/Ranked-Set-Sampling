setwd("E:/Ranked_Set_Sample")
install.packages("MASS")
library('MASS')

#Data sets are generated from a normal super population with μ = 10, σ = 4 and population size N = 1500

mean <- 12
sdv <- 5
N <- 1500
r <- 1  #0.5, 0.75

h<- 2 # size/H also for 3,4,5
d<- 5 # cycles
n<- d*h # no of samples

num_sims <- 1000
#set.seed(12345)
sim_output <- rep(NA, times = num_sims)
for (sim_number in 1:num_sims){

#set.seed(9000)
dataY <- mean+sdv*scale(rnorm(N))
#var(dataY)
mu = c(4,mean)
Sigma <- matrix(c(1, r, r, 1), nrow=2)

data = mvrnorm(N, mu, Sigma, empirical=TRUE)
X = data[, 1]
Y = data[, 2]
#cor(X, Y)
#mean(X)
#mean(Y)

#### RSS #################################
#set.seed(1000)
sam <- sample(1:N,n*h)

SRS.index<-matrix(sam,nrow=h)
selected.X<-matrix(X[SRS.index], nrow=h)
new<-matrix(paste0(selected.X,"_",SRS.index), nrow=h)

new1=matrix(data=NA, nrow=h, ncol=n)
for(col in 1:ncol(new)){
	new1[,col]<-new[,col][order(as.numeric(gsub("_.*", "", new[,col])))]
}

RSS.sampl <- NULL
for(col in seq(1,n,h)){
	a <- (diag(new1[c(1:h),c(col:(col+(h-1)))]))
	RSS.sampl <- rbind(a,RSS.sampl )
}
rownames(RSS.sampl) <- NULL

index.x <- as.numeric(sub(".*\\_","", RSS.sampl))
index.y<-matrix(index.x,nrow=d)
selected.Y<-matrix(Y[index.y], nrow=d)

sa <- data.frame(selected.Y)

#############################
t1 <-NULL
for(row in 1:nrow(sa)){
	for(col in 1:ncol(sa)){
	p <- sa[row,col]
	q <- sum((p - sa[,-col])^2)
	t1 <- sum(t1,q)
	}
}
#t1
T1 <- t1/(2*(h^2)*(d^2))

t2 <-NULL
for(col in 1:ncol(sa)){
test <- sa[,col]
test1 <- outer(test, test, `-`)
test2 <- 2*sum((test1[upper.tri(test1)])^2)
t2 <- sum(t2,test2)
}
#t2
T2 <- t2/(2*d*(d-1)*(h^2))

sigma.RSS <- T2*(h/n) - (T1+T2)/N

sim_output[sim_number] <- sigma.RSS
}

#sim_output
mean(sim_output) #UE estimates

#### SRS #################################

num_sims <- 1000
#set.seed(12345)
sim_output1 <- rep(NA, times = num_sims)
for (sim_number in 1:num_sims){

SRS.index <- sample(1:N, n)
selected.SRS <- Y[SRS.index]
#sum of squares
ss.SRS <- sum( (selected.SRS - mean(selected.SRS) )^2 ) / (length(selected.SRS) - 1)
var.SRS <- ((1/n)-(1/N))* ss.SRS
sim_output1[sim_number] <- var.SRS
}

mean(sim_output1) # Est. from equation 

closeAllConnections() 
rm(list=ls())
