#have a look at all.ans, which is a list of data frames, one for each question
all.ans[[50]]
sum(all.ans[[50]]$per)

#read data in
lingloc <- read.table("lingLocation.txt", header=T)
lingdata <- read.table("lingData.txt", header=T)

sum(!is.numeric(lingdata$ZIP))
sum(!is.numeric(lingdata$lat))
sum(!is.numeric(lingdata$long))
sum(is.na(lingdata$lat))
sum(is.na(lingdata$long))
head(which(is.na(lingdata$long))) #have town, state. could scrape web to fill in
all(which(is.na(lingdata$lat)) == which(is.na(lingdata$long))) #TRUE
#just remove NAs
lingdata <- lingdata[-which(is.na(lingdata$lat)),]

qcols <- 5:71
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

qentr = rep(NA,length(qcols))
for (i in qcols) {
  p <- all.ans[[i+45]]$per/100
  qentr[i-4] <- (-1)*sum(p*log2(p))
}
#hmm there are options nobody selected for 6 of the questions
#seems like these can be removed harmlessly