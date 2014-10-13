#have a look at all.ans, which is a list of data frames, one for each question
all.ans[[50]]
sum(all.ans[[50]]$per)

#read data in
lingloc <- read.table("lingLocation.txt", header=T)
lingdata <- read.table("lingData.txt", header=T)
load("question_data.Rdata")

sum(!is.numeric(lingdata$ZIP))
sum(!is.numeric(lingdata$lat))
sum(!is.numeric(lingdata$long))
sum(is.na(lingdata$lat))
sum(is.na(lingdata$long))
head(which(is.na(lingdata$long))) 
#have town, state. could scrape web to fill in but that would take too much time
all(which(is.na(lingdata$lat)) == which(is.na(lingdata$long))) #TRUE
#just remove NAs
lingdata <- lingdata[-which(is.na(lingdata$lat)),]
#46451 rows left

qcols <- 5:71


#now check for rows which have no response at all
noresponse <- rep(NA,46451)
for (i in 1:46451) {
  noresponse[i] <- all(lingdata[i,qcols]==0)
}
length(which(noresponse))   #1015. best to remove all
lingdata <- lingdata[-which(noresponse),]
#45436 rows left

all(!is.na(lingdata[,qcols]))            #TRUE
all(!is.numeric(lingdata[,qcols]))       #TRUE
#question columns seem to be working fine

all(!duplicated(lingdata$ID))            #TRUE
#no duplicates


#bad states:
unique(lingdata$STATE)                   #lots of crap like XX, 94, c), !L
head(filter(lingdata, STATE == "XX"))
#according to the zip codes, they're from different states
#actually just ignore the state column
lingdata <- select(lingdata, -STATE)
#question columns are now
qcols <- 4:70


questions <- quest.use$qnum
length(questions) == length(qcols)   #TRUE
ans <- all.ans[questions]
length(ans) == length(questions)     #TRUE


#see if any questions have options which were not selected at all
findempty <- rep(NA, length(qcols))
sum100 <- rep(NA, length(qcols))
j <- 1
for (i in questions) {
  findempty[j] <- all(all.ans[[i]]$per > 0)
  sum100[j] <- abs(sum(all.ans[[i]]$per) - 100) < 0.1
  j <- j+1
}
rm(i,j)
which(sum100 == FALSE)      #None
emptyq <- questions[findempty == FALSE]     
#58,59,63,67,76,81
#actually the percentages could just be very small and were rounded down to 0.00 
#we'll check the extended matrix later



#this is dumb but let's try it to make sure it works
j <- 1
num_options <- rep(NA,length(questions))
for (i in questions) {
  num_options[j] <- dim(all.ans[[i]])[1] 
  j <- j+1
}
rm(i,j)
sum(num_options)     #468 = 471 - 3 columns of non questions


#alright so i'm going to do the stupid thing and loop over each question
ling_extended <- lingdata$ID
for (i in qcols) {
  num_options_i <- max(lingdata[,i])
  mat_i <- matrix(data=NA, nrow=45436, ncol=num_options_i)
  for (j in 1:45436) {
    ans <- lingdata[j,i]
    mat_i[j,ans] <- 1
  }
  ling_extended <- cbind(ling_extended,mat_i)
}

dim(ling_extended)
ling_extended[is.na(ling_extended)] <- 0
rownames(ling_extended) <- ling_extended[,1]
ling_extended <- ling_extended[,-1]
#so ling_extended is now the expanded binary form of lingdata (but not binned)


load("clean.RData")
load("lingext.RData")
library(ggplot2)
#45436 observations of 468 variables
which(colSums(ling_extended) == 0)  #excellent, none of the columns are all zero

table(rowSums(ling_extended))
#shows that the majority (39218 out of 45436) answered all the 67 questions
#but a few people only answered a small fraction
#let's remove people who answered less than 60 questions since they don't
#contain much information and may mess with the clustering/dimension reduction
slackers <- which(rowSums(ling_extended) < 60)
length(slackers)          #there are 552 of them
ling_extended <- ling_extended[-slackers,]
lingdata <- lingdata[-slackers,]
dim(ling_extended)        #nrows = 44884 = 45436 - 552, ncols = 468
dim(lingdata)             #nrows = 44884, ncols = 468
save.image(file="clean2.RData")


#brief look at data:
ggplot(lingdata) + geom_point(aes(x=long,y=lat))
#looks good. no obvious location outliers


########something strange i tried that's probably silly############
#brief attempt at finding questions where people disagree the most
#now find entropy again
qentr2 <- rep(NA,length(qcols))
for (i in qcols) {
  p <- all.ans[[i+45]]$per/100
  qentr2[i-4] <- (-1)*sum(p*log2(p))
}
#these look interesting:
which(qentr2 > 2)     #1, 10, 12, 20, 21, 25, 30, 33, 35, 36, 39, 45, 47, 50, 53
###################################################################


#insectsQ(num_options): 16(6), 25(14), 53(11)
#roadsQ(num_options): 12(7), 30(10), 35(7), 50(8)
#completely unrelated(num_options): 36(6), 39(7), 45(6), 47(7)


library(vcd)
str(lingdata)     #the answers coded in lingdata are integers not factors

insect1 <- factor(lingdata[,16+3])
insect2 <- factor(lingdata[,25+3])
insect3 <- factor(lingdata[,53+3])

road1 <- factor(lingdata[,12+3])
road2 <- factor(lingdata[,30+3])
road3 <- factor(lingdata[,35+3])
road4 <- factor(lingdata[,50+3])

other1 <- factor(lingdata[,36+3])
other2 <- factor(lingdata[,39+3])
other3 <- factor(lingdata[,45+3])
other4 <- factor(lingdata[,47+3])

mosaic(~ road1 + road3, shade=TRUE)
mosaic(~ road1 + road4, shade=TRUE)
mosaic(~ road3 + road4, shade=TRUE)

mosaic(~ other1 + other2, shade=TRUE)
mosaic(~ other1 + other3, shade=TRUE)
mosaic(~ other1 + other4, shade=TRUE)
mosaic(~ other2 + other3, shade=TRUE)
mosaic(~ other2 + other4, shade=TRUE)
mosaic(~ other3 + other4, shade=TRUE)

mosaic(~ road4 + other4, shade=TRUE)


library(iplots)
iplot(lingdata$long, lingdata$lat)
ibar(road3)    #option 1 closely related to new england
all.ans[[84]]
ibar(road4)    #option 4 related to south texas
all.ans[[99]]
ibar(insect1)
ibar(other1)   #option 1 possibly related to ny/new england
all.ans[[85]]
ibar(other3)   #option 1 north, option 2 south
all.ans[[94]]
ibar(other4)   #option 1 midwest?
all.ans[[96]]
ibar(road1)
ibar(insect2)




#lingdist1 <- dist(ling_extended, method="manhattan")
#takes forever. tried on arwen and eventually finished after 4 hours 
#but too large to do mds/isomap with
#i suppose i'll have to do pca first

ling_pca1 <- prcomp(ling_extended, scale=TRUE)
save(ling_pca1, file="lingpca1.RData")
summary(ling_pca1)
screeplot(ling_pca1)
biplot(ling_pca1)

lingpca1_2 <- ling_pca1$x[,1:2]
lingpca1_4 <- ling_pca1$x[,1:4]
lingpca1_6 <- ling_pca1$x[,1:6]
lingpca1_8 <- ling_pca1$x[,1:8]
#explore these PCs in some way
cor(lingpca1_2,c(lingdata$lat,lingdata$long))


####bootstrap to evaluate stability
set.seed(529)
lingboot1 <- ling_extended[sample(1:44884,replace=TRUE),]
lingboot2 <- ling_extended[sample(1:44884,replace=TRUE),]

lingboot1_pca <- prcomp(lingboot1, scale=TRUE)
lingboot2_pca <- prcomp(lingboot2, scale=TRUE)
save.image(file="aftersomePCA.RData")

summary(lingboot1_pca)
screeplot(lingboot1_pca)
biplot(lingboot1_pca)

summary(lingboot2_pca)
screeplot(lingboot2_pca)
biplot(lingboot2_pca)



####notes from class####
#check stability, some predictive power
#distance metric for categorical data?
#pca on categorical data?
#need to transform data before doing pca
#build binary matrix - difference between rows makes sense
#
#hamming distance for clustering people
#
#how to measure difference between 2 questions with different numbers of options?
#need this to do spectral clustering or MDS
#some kind of correlation but not ordinary
#try mutual information
#difference in entropy of the actual joint and 
#entropy of joint constructed by multiplying marginals
#large distance should be high degree of dependence (and the converse)
