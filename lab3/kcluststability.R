library(foreach)
library(doParallel)
library(rlecuyer)
library(Rcpp)
library(microbenchmark)
library(ggplot2)


# set up working directory and load data
working.dir <- file.path("C:/Users/rrruss/Desktop/stat215a/stat215af14/lab3")
load(file.path(working.dir, "lingBinary.RData"))


# set up parallelization
nCores <- 9
registerDoParallel(nCores)
# ensure independent streams of random numbers with foreach
RNGkind("L'Ecuyer-CMRG")


# look at dataset and extract the columns for clustering
dim(lingBinary)
ling.qdata <- lingBinary[7:474]
# if memory is an issue, lingBinary can be removed 
# since it is not needed any more
#rm(lingBinary) 


CMatrix <- function(clust, k) {
  # Creates the C matrix for a given clustering partition,
  # as defined at the top of page 3 in Ben-Hur et al.
  # 
  # Args:
  #    clust: a vector encoding which cluster the data point belongs to
  #    k    : number of clusters, which should be equal to max(clust)
  #
  # Returns:
  #    C matrix, which is a square symmetric matrix
  #    with dimension equal to length(clust)
  #
  clust.len <- length(clust)
  # initialize the matrix with NAs
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
  # force the diagonal to be 0s, as specified in Ben-Hur et al.
  diag(c.mat) <- 0
  return(c.mat)
}


ComputeSimSlow <- function(clust1, clust2, k) {
  # computes correlation similarity between two clusters (Fowlkes-Mallows index)
  # the algorithm implemented here is the one specified in Ben-Hur et al.
  #
  # this function runs very slowly and should only be used for sanity checking
  # the results of subsequent implementations of calculating the FM index
  #
  #
  # Requires: CMatrix()
  #
  # Args:
  #    clust1: a numeric vector encoding which cluster the data point belongs to, 
  #            for partition 1
  #    clust2: a numeric vector encoding which cluster the data point belongs to, 
  #            for partition 2
  # clust1 and clust2 are assumed to be ordered, on the same set of data points
  # so that clust1[i] and clust2[i] both refer to the same data point i
  # 
  #    k     : number of clusters, 
  #            which should be equal to max(clust1) and max(clust2)
  #
  # Returns:
  #    FM index for a pair of clustering partitions 
  #    (a real number returned as a numerical value)
  #
  c.mat1 <- CMatrix(clust1, k)
  c.mat2 <- CMatrix(clust2, k)
  l1.l2 <- sum(c.mat1 * c.mat2)
  l1.l1 <- sum(c.mat1)    # or sum(table(clust1)^2) - length(clust1)
  l2.l2 <- sum(c.mat2)    # or sum(table(clust2)^2) - length(clust2)
  corr <- l1.l2 / sqrt(l1.l1 * l2.l2)
  return(corr)
}


ComputeSim <- function(clust1, clust2, k) {
  # computes correlation similarity between two clusters (Fowlkes-Mallows index)
  # the algorithm implemented here is due to wikipedia
  #
  # Args:
  #    clust1: a numeric vector encoding which cluster the data point belongs to, 
  #            for partition 1
  #    clust2: a numeric vector encoding which cluster the data point belongs to, 
  #            for partition 2
  # clust1 and clust2 are assumed to be ordered, on the same set of data points
  # so that clust1[i] and clust2[i] both refer to the same data point i
  # 
  #    k     : number of clusters, 
  #            which should be equal to max(clust1) and max(clust2)
  #
  # Returns:
  #    FM index for the pair of clustering partitions 
  #    (a real number returned as a numerical value)
  #
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
  /* calculates the Fowlkes-Mallows index for two clustering partitions
     the algorithm implemented here is described in Ben-Hur et al.

     Args:
        clust1: a numeric vector encoding which cluster the data point 
                belongs to, for partition 1
        clust2: a numeric vector encoding which cluster the data point 
                belongs to, for partition 2
     clust1 and clust2 are assumed to be ordered, on the same set of data points
     so that clust1[i] and clust2[i] both refer to the same data point i

     Returns:
        FM-index for the pair of clustering partitions
        (a real number returned as a double) */

  //initialize and define variables
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
  # implements the model explorer algorithm outlined in Ben-Hur et al.
  # for k-means clustering of the binary-coded linguistic data provided
  # for 9 values of k between 2 and 10 inclusive
  #
  # Requires: libarary(foreach), library(doParallel), ComputeSim()
  # 
  # Args:
  #     m: fraction of data to be subsampled
  #
  #
  # Returns:
  #    a matrix with 100 rows and 9 columns, where each column contains 
  #    the 100 similarity values calculated for each value of k
  #
  num.obs <- nrow(ling.qdata)
  k.similarities <- foreach(k=2:10, .combine='cbind') %dopar% {
    # initialize the vector of similarities with NAs
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
        # leaving the option for the NA to be subsequently removed or dealt with
        # in another manner if this warning is thrown
        warning("An intersection is empty. Similarity between clusters cannot be computed.")
      } else {
        # cluster and immediately extract the clusters 
        # since we are only interested in that
        clust1 <- kmeans(ling.qdata[sub1.index, ], centers=k, iter.max=40)$cluster
        clust2 <- kmeans(ling.qdata[sub2.index, ], centers=k, iter.max=40)$cluster
        # restrict ourselves to looking at the clusterings on the intersection
        # followed by computing the similarity for this pair of partitions
        clust1.int <- unname(clust1[row.names(ling.qdata[intersection, ])])
        clust2.int <- unname(clust2[row.names(ling.qdata[intersection, ])])
        sim[i] <- ComputeSim(clust1.int, clust2.int, k)
      }
    }
    # output sim so that foreach will combine the vectors using cbind()
    sim
  }
  return(k.similarities)
}


print("Finished defining functions")


# run and save so that plotting can be done separately
similarities08  <- ClustStability(m = 0.8)
save(similarities08, file="sim08.RData")


# testing running times
set.seed(387)
testclust1 <- sample(1:6, size=45000, replace=TRUE)
testclust2 <- sample(1:6, size=45000, replace=TRUE)
microbenchmark(ComputeSim(testclust1, testclust2, 6), 
               SimC(testclust1, testclust2), times=5)
