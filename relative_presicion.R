setwd("E:/animesh/RSS")
df <- read.csv("Sheep Weight.csv", header = TRUE)
X1 <- df[,1]
X2 <- df[,2]
Y <- df[,3]

#cor(X1, Y, method="pearson")
#cor(X2, Y, method="pearson")

Y <- sapply(Y, as.numeric)

auxillary<- X2
dependent<- Y
N<-length(auxillary);

h<- 2 # size/H
d<- 5 # cycles
n<- d*h # no of samples

sam <- sample(1:N,n*h)

SRS.index<-matrix(sam,nrow=h)
selected.auxillary<-matrix(auxillary[SRS.index], nrow=h)
new<-matrix(paste0(selected.auxillary,"_",SRS.index), nrow=h)

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
selected.dependent<-matrix(dependent[index.y], nrow=d)

sa <- data.frame(selected.dependent)
sb <- mean(sapply(sa, mean))

md <- 0
for(col in 1:h){
#col <- 2
	col.mean <- (mean(sa[,col]) - sb)^2
	md <- md + col.mean
}	

md1 <- md/h

D <- 1 - (md1/(var(Y)))
RP <- 1/(1-D)
print(RP)

num_sims <- 1000
sim_output <- rep(NA, times = num_sims)
for (sim_number in 1:num_sims){

average <- mean(RP)
sim_output[sim_number] <- average
}
mean(sim_output)

closeAllConnections() 
rm(list=ls())
