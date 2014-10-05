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
head(which(is.na(lingdata$long))) #have town, state. could scrape web to fill in
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

questions <- c(50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,
               68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,
               86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,
               103,104,105,106,107,109,110,111,115,117,118,119,120,121)
length(qcols) == length(questions)   #TRUE

#hmm there are options nobody selected for 6 of the questions
#seems like these can be removed harmlessly

#this doesnt work since some questions in the 50-121 range are removed:
findempty <- rep(NA, length(qcols))
j <- 1
for (i in questions) {
  findempty[j] <- all(all.ans[[i]]$per > 0)
  j <- j+1
}
rm(i,j)
emptyq <- questions[findempty == FALSE]     #58,59,63,67,76,81

for (i in questions) {
  if (all(all.ans[[i]]$per > 0) == FALSE) {      #if some options are empty
    nonempty <- which(all.ans[[i]]$per > 0)      #find the nonempty ones
    all.ans[[i]] <- all.ans[[i]][nonempty,]   #discard the empty options
  }
}

#now find entropy again
qentr2 <- rep(NA,length(qcols))
for (i in qcols) {
  p <- all.ans[[i+45]]$per/100
  qentr2[i-4] <- (-1)*sum(p*log2(p))
}
#these look interesting:
which(qentr2 > 2)     #1, 10, 12, 20, 21, 25, 30, 33, 35, 36, 39, 45, 47, 50, 53


#this is dumb as fuck but let's try it to make sure it works
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






####notes from class
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
#try mutual information - find out what this is
#difference in entropy of the actual joint and 
#entropy of joint constructed by multiplying marginals
#large distance should be high degree of dependence (and the converse)
