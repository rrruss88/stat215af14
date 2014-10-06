library(dplyr)
log <- read.csv('sonoma-data-log.csv',header=T)
net <- read.csv('sonoma-data-net.csv',header=T)
all <- read.csv('sonoma-data-all.csv', header=T)
mote.locations <- read.delim("mote-location-data.txt", sep="")

#brief look at metadata
hist(mote.locations$Height)
unique(mote.locations$ID)
table(mote.locations$Direc)
table(mote.locations$Dist)
table(mote.locations$Tree)

#remove irrelevant columns - parent, depth and humid_adj(what is that?)
all <- all[,-c(4,6,9)]

#check for bad columns
sum(!is.numeric(all$nodeid))
sum(!is.numeric(all$epoch))
sum(!is.numeric(all$voltage))
sum(!is.numeric(all$humidity))
sum(!is.numeric(all$humid_temp))
sum(!is.numeric(all$hamatop))
sum(!is.numeric(all$hamabot))
#all the above are 0


# How many are repeated?
all.multiplicity <- group_by(all, nodeid, epoch) %>% summarise(n=n()) %>%
  ungroup() %>% group_by(n) %>% summarise(total=n())
# Sanity check.
nrow(all)                                          #416036
sum(all.multiplicity$total * all.multiplicity$n)   #416036
#also note that 241104+67208+10544+175 = 319031 (see 2 lines below)

#should i look at only duplicates of nodeid and epoch or duplicates of all columns?
nrow(unique(all[,c("nodeid","epoch")]))                  #319031
nrow(unique(all))                                        #407852
length(which(duplicated(all)))                           #8184 = 416036 - 407852
length(which(duplicated(all[,c("nodeid","epoch")])))     #97005
#8184 is very small compared to 97005 because of slight differences in time


#remove duplicate node/epoch combinations
dup_index <- which(duplicated(all[,c("nodeid","epoch")]))
all <- all[-dup_index,]
nrow(all)                                                #319031



#look for NAs
sum(is.na(all$nodeid))       #0
sum(is.na(all$epoch))        #0
sum(is.na(all$voltage))      #0
sum(is.na(all$humidity))     #8852
sum(is.na(all$humid_temp))   #8852
sum(is.na(all$hamatop))      #8852
sum(is.na(all$hamabot))      #8852

all(which(is.na(all$humidity)) == which(is.na(all$humid_temp)))
all(which(is.na(all$humidity)) == which(is.na(all$hamatop)))
all(which(is.na(all$humidity)) == which(is.na(all$hamabot)))
all.equal(which(is.na(all$humidity)), which(is.na(all$hamabot)), 
          which(is.na(all$hamatop)), which(is.na(all$humid_temp)))
#all the above are TRUE so all the NAs are in the same 8852 rows

#look at time/date distribution of missing values
#if generally uniform over time/date then discard
#what else?? possible to just take the average but that's a lot of scripting
na_index <- which(is.na(all$humidity)) 
#8852 NAs out of 319031 obs. = 0.0277 - small enough to consider removing
#now let's look at the date/time distribution of the NAs
hist(all[na_index,]$epoch)
hist(all$epoch)


#remove all NAs:
all <- all[-na_index,]
nrow(all)                         #310179 = 319031 - 8852

#now look at nodeids and epochs
unique(all$nodeid)
length(unique(all$epoch))    #12634

#rogue node IDs. there are 3 False values:
unique(all$nodeid) %in% mote.locations$ID
#they are 100, 135 and 65535. 65535 is probably completely off
#what about 100, 135? take a look:
ddd <- filter(all, nodeid==100)       #result_time all the same. PAR is off.
ddd1 <- filter(all, nodeid==135)      #
ddd2 <- filter(all, nodeid==65535)    #clearly bogus
#actually, i'm not sure what's going on with result_time


sum(all$humidity > 100, na.rm=TRUE)
table(all[which(all$humidity > 100),]$nodeid)
sum(all$humidity < 0, na.rm=TRUE)
table(all[which(all$humidity < 0),]$nodeid)

min(all$humid_temp, na.rm=TRUE)
max(all$humid_temp, na.rm=TRUE)
sum(all$humid_temp > 40, na.rm=TRUE)
table(all[which(all$humid_temp>40),]$nodeid)
sum(all$humid_temp < 0, na.rm=TRUE)
table(all[which(all$humid_temp<0),]$nodeid)

length(which(all$humid_temp > 40))

sum(all$hamabot < 0, na.rm=TRUE)
sum(all$hamatop < 0, na.rm=TRUE)


####lab2####
alldf <- tbl_df(all)
clean <- filter(all, humid_temp > 5 & humid_temp < 35 & voltage > 2.5)
same_time <- filter(clean, epoch %% 288 == 87)
save(list=c("clean","same_time","m"),file="wood.RData")
library(KernSmooth)
foo1 <- bkde(clean$humid_temp)
foo2 <- bkde(clean$humid_temp,bandwidth=5)
foo3 <- bkde(clean$humid_temp,bandwidth=2)
foo4 <- bkde(clean$humid_temp,bandwidth=0.8)
foo5 <- bkde(clean$humid_temp,bandwidth=0.5)
foo6 <- bkde(clean$humid_temp,bandwidth=0.2)
foo7 <- bkde(clean$humid_temp,bandwidth=0.08)
plot(foo1,type="l")
library(ggplot2)
library(gridExtra)
foo <- data.frame(foo7)


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scale_colour_manual(values=cbbPalette)

m <- ggplot(clean, aes(x=humid_temp))

m + geom_histogram(aes(y = ..density..), color="black", fill="white") +
  geom_density(adjust=2, kernel="gaussian", size=1, color=1) + 
  geom_density(adjust=0.5, kernel="gaussian", size=1, color=2) +
  geom_density(adjust=1, kernel="gaussian", size=1, color=3) +
  xlab("Temperature")

m + geom_histogram(aes(y = ..density..), color="black", fill="white") + 
  geom_density(adjust=2, kernel="epanechnikov", size=1, color=1) + 
  geom_density(adjust=0.5, kernel="epanechnikov", size=1, color=2) +
  geom_density(adjust=1, kernel="epanechnikov", size=1, color=3) +
  xlab("Temperature")

m + geom_histogram(aes(y = ..density..), color="black", fill="white") + 
  geom_density(adjust=2, kernel="rectangular", size=1, color=1) + 
  geom_density(adjust=0.5, kernel="rectangular", size=1, color=2) +
  geom_density(adjust=1, kernel="rectangular", size=1, color=3) +
  xlab("Temperature")

m + geom_histogram(aes(y = ..density..), color="black", fill="white") + 
  geom_density(adjust=2, kernel="triangular", size=1, color=1) + 
  geom_density(adjust=0.5, kernel="triangular", size=1, color=2) +
  geom_density(adjust=1, kernel="triangular", size=1, color=3) +
  xlab("Temperature")

m + geom_histogram(aes(y = ..density..), color="black", fill="white") + 
  geom_density(adjust=2, kernel="biweight", size=1, color=1) + 
  geom_density(adjust=0.5, kernel="biweight", size=1, color=2) +
  geom_density(adjust=1, kernel="biweight", size=1, color=3) +
  xlab("Temperature")
