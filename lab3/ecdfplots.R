library(ggplot2)


working.dir <- file.path("C:/Users/rrruss/Desktop/stat215a/stat215af14/lab3")

# load data
# sim06 corresponds to m=0.6, inner loop 100
# sim08 corresponds to m=0.8, inner loop 100
# sim308 corresponds to m=0.8, inner loop 300
load(file.path(working.dir, "sim06.Rdata"))
load(file.path(working.dir, "sim08.RData"))
load(file.path(working.dir, "sim308.RData"))

# generate data frames to feed into ggplot
# together with levels (corresponding to k) for easy plotting
sim06df <- data.frame(sim = as.vector(similarities), k=gl(10,100)[101:1000])
sim08df <- data.frame(sim = as.vector(similarities08), k=gl(10,100)[101:1000])
sim308df <- data.frame(sim = as.vector(similarities308), k =gl(10,300)[301:3000])

ggplot(sim06df, aes(sim, colour=k)) + stat_ecdf() + 
  xlab("Fowlkes-Mallows similarity values") + ylab("Empirical CDF")

p1 <- qplot(similarities08[,1], geom="histogram", xlab="k = 2")
p2 <- qplot(similarities08[,2], geom="histogram", xlab="k = 3")
p3 <- qplot(similarities08[,3], geom="histogram", xlab="k = 4")
p4 <- qplot(similarities08[,4], geom="histogram", xlab="k = 5")
p5 <- qplot(similarities08[,5], geom="histogram", xlab="k = 6")
p6 <- qplot(similarities08[,6], geom="histogram", xlab="k = 7")
p7 <- qplot(similarities08[,7], geom="histogram", xlab="k = 8")
p8 <- qplot(similarities08[,8], geom="histogram", xlab="k = 9")
p9 <- qplot(similarities08[,9], geom="histogram", xlab="k = 10")
ggplot(sim08df, aes(sim, colour=k)) + stat_ecdf() + 
  xlab("Fowlkes-Mallows similarity values") + ylab("Empirical CDF")

ggplot(sim308df, aes(sim, colour=k)) + stat_ecdf() + 
  xlab("Fowlkes-Mallows similarity values") + ylab("Empirical CDF")
