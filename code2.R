setwd("E:/Ranked_Set_Sample")
install.packages("MASS")
library('MASS')

mean <- 10
sdv <- 4
N <- 1500
r <- 0.5 # 0.75, 1

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
mu = c(4,10)
Sigma <- matrix(c(1, r, r, 1), nrow=2)

data = data.frame(mvrnorm(N, mu, Sigma, empirical=TRUE))
X = data[, 1]
Y = data[, 2]
#cor(X, Y)
#mean(X)
#mean(Y)

######## Generated data #############################

mu <- c(as.numeric(colMeans(data)))
stddev <- c(as.numeric(sapply(data, sd)))
corMat <- matrix(c(as.numeric(cor(data))), nrow=2)
covMat <- stddev %*% t(stddev) * corMat

data.generated = mvrnorm(N, mu, covMat, empirical=TRUE)

XX = data.generated [, 1]  
YY = data.generated [, 2]
#cor(data.generated)
#mean(XX)
#var(YY)

#### RSS #################################
sam <- sample(1:N,n*h)

SRS.index <- matrix(sam,nrow=h)
selected.XX <- matrix(XX[SRS.index], nrow=h)
new <- matrix(paste0(selected.XX,"_",SRS.index), nrow=h)

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
index.y <- matrix(index.x,nrow=d)
selected.Y <- matrix(YY[index.y], nrow=d)

average <- mean(selected.Y)
sim_output[sim_number] <- average
}

sim_output
var(sim_output)

closeAllConnections() 
rm(list=ls())
