library(foreach)
library(doParallel)
library(rlecuyer)
library(Rcpp)
library(microbenchmark)
library(ggplot2)


#set up stuff
#working.dir1 <- file.path("?")
#working.dir2 <- file.path("?")
load("lingBinary.Rdata")
#sourceCPP(file.path(working.dir, "??.cpp"))

nCores <- 4
registerDoParallel(nCores)
#ensure independent streams of random numbers with foreach
RNGkind("L'Ecuyer-CMRG")


#look at dataset and extract the columns for clustering
dim(lingBinary)
ling.qdata <- lingBinary[7:474]
rm(lingBinary)
num.obs <- nrow(ling.qdata)


CMatrix <- function(clust, k) {
  #clust is a vector encoding which cluster the data point belongs to
  #k is number of clusters, which should be equal to max(clust)
  #output is a square symmetric matrix with dimension length(clust)
  clust.len <- length(clust)
  #initialize
  c.mat <- matrix(rep(NA, clust.len^2), nrow=clust.len, ncol=clust.len)
  for (i in 1:clust.len) {
    for (j in 1:clust.len) {
      if (clust[i] == clust[j]) {
        c.mat[i,j] <- 1
        c.mat[j,i] <- 1
      } else {
        c.mat[i,j] <- 0
        c.mat[j,i] <- 0
      }
    }
  }
  diag(c.mat) <- 0
  return(c.mat)
}


ComputeSimSlow <- function(clust1, clust2, k) {
  #computes correlation similarity between two clusters
  #assumed to be ordered and on the same set of data points
  #this is slow
  c.mat1 <- CMatrix(clust1, k)
  c.mat2 <- CMatrix(clust2, k)
  l1.l2 <- sum(c.mat1 * c.mat2)
  l1.l1 <- sum(c.mat1)    #or sum(table(clust1)^2) - length(clust1)
  l2.l2 <- sum(c.mat2)    #or sum(table(clust2)^2) - length(clust2)
  corr <- l1.l2/sqrt(l1.l1*l2.l2)
  return(corr)
}


ComputeSim <- function(clust1, clust2, k) {
  #computes correlation similarity between two clusters
  #assumed to be ordered and on the same set of data points
  #so that length(clust1) = length(clust2)
  #the following method of calculating the fowlkes-mallows index 
  #is due to wikipedia
  clust.len <- length(clust1)
  k.mat <- matrix(rep(NA, k^2), nrow=k, ncol=k)
  for (row in 1:k) {
    for (col in 1:k) {
      k.mat[row,col] <- length(intersect(which(clust1==row), which(clust2==col)))
    }
  }
  tk <- sum(k.mat^2) - clust.len
  pk <- sum(colSums(k.mat)^2) - clust.len
  qk <- sum(rowSums(k.mat)^2) - clust.len
  return(tk/sqrt(pk*qk))
}


cppFunction('double SimC(NumericVector x, NumericVector y) {
  
  int n = x.size();
  int t, r;
  double a, b, c, z;
  a = 0.0; b = 0.0; c = 0.0;

  for(t = 0; t < n; t ++){
      for(r = t + 1; r < n; r ++){
          if((x[t] == x[r]) && (y[t] == y[r])){
              a += 1.0;
          } else if((x[t] == x[r]) && (y[t] != y[r])){
              b += 1.0;
          } else if((x[t] != x[r]) && (y[t] == y[r])){
              c += 1.0;
          }
      }
  }

  z = a / sqrt((a + b) * (a + c));
  return z;
}')

set.seed(828)
foo1 <- sample(1:6, size=19000, replace=T)
foo2 <- sample(1:6, size=19000, replace=T)
ddd <- ComputeSim(foo1, foo2, 7)
fff <- SimC(foo1, foo2)


k.similarities <- foreach(k = 2:10, .combine = rbind) %dopar% {
  #k.similarities will be a matrix with 9 rows and 100 columns
  #where each row contains the 100 similarity values calculated for each value of k
  sim <- rep(NA, 100)
  for (i in 1:100) {
    #set.seed(53)   #set.seed(i)
    #sample and sort for easy comparison later
    #no need to worry about size = m*num.obs
    #since it is rounded down in the sample function
    #start.time1 <- Sys.time()
    sub1.index <- sort(sample(1:num.obs, size=0.51*num.obs, replace=F))
    sub2.index <- sort(sample(1:num.obs, size=0.51*num.obs, replace=F))
    #find intersection, which is already sorted
    #note that the sampling is done without replacement and m > 0.5 
    #so we don't have to worry about the intersection being empty
    intersection <- intersect(sub1.index, sub2.index)
    #cluster and immediately extract the clusters since we are only interested in that
    kclust1 <- kmeans(ling.qdata[sub1.index, ], centers=k, iter.max=35)$cluster
    kclust2 <- kmeans(ling.qdata[sub2.index, ], centers=k, iter.max=35)$cluster
    kclust1.int <- unname(kclust1[row.names(ling.qdata[intersection, ])])
    kclust2.int <- unname(kclust2[row.names(ling.qdata[intersection, ])])
    #all(names(kclust1.int) %in% names(kclust2.int)) should return true
    #start.time2 <- Sys.time()
    sim[i] <- ComputeSim(kclust1.int, kclust2.int, k)
    #fooxx <- adjustedRand(kclust1.int, kclust2.int, randMethod="FM")
    #end.time <- Sys.time()
    #duration1 <- end.time - start.time1
    #duration2 <- end.time - start.time2
  }
}


#plot.ecdf(sim)
#qplot(sim, stat="ecdf", geom="step")


save(k.similarities, file="sim.RData")

#TODO: wrap above in a function and throw an error for an empty intersection
#      figure out how to use ggplot to plot multiple ecdfs on one plot





####some tests and other weird stuff
# foo <- kmeans(ling.qdata, centers = 4)
# foo1.index <- sub1.index[1:100]
# foo2.index <- sub2.index[1:100]
# foo.inters <- intersect(foo1.index, foo2.index)
# foo1 <- kclust1$cluster[1:100]
# foo2 <- kclust2$cluster[1:100]
# #use row.names and as.integer
# #names of clusters are row.names
# #names(foo1) gives foo1.index
# fooxx <- cbind(foo1,foo2) #does not work
# #try lingBinary[names(foo1),]
# 
# kclust1[names(kclust2)] #extracts the elements of kclust1 where their 
# fooomit1 <- na.omit(kclust1[names(kclust2)])
# fooomit2 <- na.omit(kclust2[names(kclust1)])
# all(names(fooomit2) %in% names(fooomit1))    #TRUE!!!!
# fooxx <- adjustedRand(kclust1.int, kclust2.int, randMethod="FM")