library(foreach)
library(doParallel)
library(rlecuyer)
library(Rcpp)
library(microbenchmark)
library(ggplot2)


# set up stuff
#working.dir <- file.path("?")
load("lingBinary.Rdata")


nCores <- 4
registerDoParallel(nCores)
# ensure independent streams of random numbers with foreach
RNGkind("L'Ecuyer-CMRG")


# look at dataset and extract the columns for clustering
dim(lingBinary)
ling.qdata <- lingBinary[7:474]
rm(lingBinary)


CMatrix <- function(clust, k) {
  # clust is a vector encoding which cluster the data point belongs to
  # k is number of clusters, which should be equal to max(clust)
  # output is a square symmetric matrix with dimension length(clust)
  clust.len <- length(clust)
  # initialize
  c.mat <- matrix(rep(NA, clust.len ^ 2), nrow=clust.len, ncol=clust.len)
  for (i in 1:clust.len) {
    for (j in 1:clust.len) {
      if (clust[i] == clust[j]) {
        c.mat[i, j] <- c.mat[j, i] <- 1
      } else {
        c.mat[i, j] <- c.mat[j, i] <- 0
      }
    }
  }
  diag(c.mat) <- 0
  return(c.mat)
}


ComputeSimSlow <- function(clust1, clust2, k) {
  # computes correlation similarity between two clusters
  # assumed to be ordered and on the same set of data points
  # this is very slow
  c.mat1 <- CMatrix(clust1, k)
  c.mat2 <- CMatrix(clust2, k)
  l1.l2 <- sum(c.mat1 * c.mat2)
  l1.l1 <- sum(c.mat1)    # or sum(table(clust1)^2) - length(clust1)
  l2.l2 <- sum(c.mat2)    # or sum(table(clust2)^2) - length(clust2)
  corr <- l1.l2 / sqrt(l1.l1 * l2.l2)
  return(corr)
}


ComputeSim <- function(clust1, clust2, k) {
  # computes correlation similarity between two clusters
  # assumed to be ordered and on the same set of data points
  # so that length(clust1) = length(clust2)
  # the following method of calculating the fowlkes-mallows index 
  # is due to wikipedia
  clust.len <- length(clust1)
  k.mat <- matrix(rep(NA, k ^ 2), nrow=k, ncol=k)
  for (rw in 1:k) {
    for (cl in 1:k) {
      k.mat[rw, cl] <- length(intersect(which(clust1==rw), which(clust2==cl)))
    }
  }
  tk <- sum(k.mat ^ 2) - clust.len
  pk <- sum(colSums(k.mat) ^ 2) - clust.len
  qk <- sum(rowSums(k.mat) ^ 2) - clust.len
  return(tk / sqrt(pk * qk))
}


cppFunction('double SimC(NumericVector clust1, NumericVector clust2) { 
  int i, j, n;
  double x, y, z, sim;

  n = clust1.size();
  x = 0.0; 
  y = 0.0; 
  z = 0.0;

  for(i = 0; i < n; i ++){
      for(j = i + 1; j < n; j ++){
          if((clust1[i] == clust1[j]) && (clust2[i] == clust2[j])){
              x += 1.0;
          } else if((clust1[i] == clust1[j]) && (clust2[i] != clust2[j])){
              y += 1.0;
          } else if((clust1[i] != clust1[j]) && (clust2[i] == clust2[j])){
              z += 1.0;
          }
      }
  }

  sim = x / sqrt((x + y) * (x + z));
  return sim;
}')


ClustStability <- function(m) {
  # some more comments about this function
  num.obs <- nrow(ling.qdata)
  k.similarities <- foreach(k=2:10, .combine='cbind') %dopar% {
    # k.similarities will be a matrix with 100 rows and 9 columns
    # where each column contains the 100 similarity values 
    # calculated for each value of k
    sim <- rep(NA, 100)
    for (i in 1:100) {
      # sample and sort for easy comparison later
      # no need to worry about size = m*num.obs not being an integer
      # since it is rounded down in the sample function
      sub1.index <- sort(sample(1:num.obs, size=m*num.obs, replace=FALSE))
      sub2.index <- sort(sample(1:num.obs, size=m*num.obs, replace=FALSE))
      # find intersection, which is already sorted
      intersection <- intersect(sub1.index, sub2.index)
      # error handling in the very unlikely case of an empty intersection
      if (m <= 0.5 && length(intersection) == 0) {
        # no similarity value is inserted into the sim vector
        # leaving the option for the NA to be subsequently removed
        # if this warning is thrown
        warning("An intersection is empty. Similarity between clusters cannot be computed.")
      } else {
        # cluster and immediately extract the clusters 
        # since we are only interested in that
        clust1 <- kmeans(ling.qdata[sub1.index, ], centers=k, iter.max=35)$cluster
        clust2 <- kmeans(ling.qdata[sub2.index, ], centers=k, iter.max=35)$cluster
        # restrict ourselves to looking at the clusterings on the intersection
        clust1.int <- unname(clust1[row.names(ling.qdata[intersection, ])])
        clust2.int <- unname(clust2[row.names(ling.qdata[intersection, ])])
        sim[i] <- ComputeSim(clust1.int, clust2.int, k)
      }
    }
    sim
  }
  return(k.similarities)
}


print("Finished defining functions")


similarities  <- ClustStability(m = 0.6)
#plot.ecdf(similarities)
#qplot(similarities, stat="ecdf", geom="step")


save(similarities, file="sim.RData")


# testing running times
set.seed(387)
testclust1 <- sample(1:6, size=45000, replace=TRUE)
testclust2 <- sample(1:6, size=45000, replace=TRUE)
microbenchmark(ComputeSim(testclust1, testclust2, 6), 
               SimC(testclust1, testclust2), times=5)


#TODO: figure out how to use ggplot to plot multiple ecdfs on one plot