# Rossier-huckleberry

## Branch scale 
How to pool data into a mixed model with effects at plot and branch level? 

DV = ordered, zero-inflated data ("berryness score")

Branch-scale IV = canopy cover (hemispheric photos) & stem diameter (at base)
Plot-scale IV = canopy cover (photos & densiometer) & aspect & soil type & elevation & understory & tree & fuel loading

##___PLOT SCALE_DATA EXAMINATION__________________________________________________________
## Merge 3 datasets: 1) Photo/PlotName (photoid); 2) Photo/Pixels (photopix); 3) PlotData (huck)

 library(MASS)
 
 library(boot)

 huckpatch<-read.csv("huckfruitcanopy4.csv")
 
 plotnum <- huckpatch[,1:3]
 
 photoid <- read.csv("HuckHemi3.6.15plotcenter.csv")
 
 photopix2<-read.csv("allphotosv3.csv")

 photopc2<- merge(photoid,photopix2,by=c("Folder","PhotoID"))
 
 photopc3<- merge(photopc2, plotnum, by=c("PlotNum"))

#Add 2 more columns... 1 total pixels and 1 with % black = canopy closure.. Check successful.

photopc3$totalpix <- photopc3$TotalWhite + photopc3$TotalBlack

photopc3$cc <- photopc3$TotalBlack / photopc3$totalpix

#Examine Spread of pixel/canopy cover data. All seems to be rather tight, except for: 26A and 28A -- removed DSC_0859... need to examine 28A further.
plot(photopc3$PlotNum,photopc3$cc)

#Aggregate data by plotnumber
photoagg<-aggregate(photopc3, list(photopc3$Number),mean)

# final merge - a question here...
#huckpatchmast has all the photos split out individually
huckpatchmast<-merge(huckpatch, photopc3,by=c("Number"))
#huckpatchmast has all the photos aggregated for each plot
huckpatchmast2<-merge(huckpatch, photoagg,by=c("Number"))

## Need to see how data look within each plot - are the values close? Sd? And then how to average across - question for Richard McElreath?
pairs(huckpatchmast2[,c(6:11,16,23, 30,36, 37,59)])

pairs(huckpatchmast2[,c(6, 11,16,36, 37,59)])

##BRANCH SCALE

branchphotoid <- read.csv("HuckHemi4.4.15.csv")
branchphotopix<- read.csv("allphotosv3.csv")
huckbranchdata<-read.csv("Huckbranch3.8.15.csv")

plotnum <- huckpatch[,1:3]
hucknum<-huckbranchdata[,1:4]

photobranch<- merge(branchphotopix,branchphotoid,by=c("Folder","PhotoID"))
##To examine: 887 photos (branchphotopix) drops to 653 (photobranch) when merged with branchphotoid (656 obs)
huckplotnum<- merge(hucknum, plotnum, by=c("PlotNum"))
photobranch2<- merge(photobranch, huckplotnum, by=c("PlotNum", "BranchNum"))
##To examine: 653 photos (photobranch) drops to 587 (photobranch2) when merged with hucknum (198 obs)

photobranch2$totalpix <- photobranch2$TotalWhite + photobranch2$TotalBlack
photobranch2$cc <- photobranch2$TotalBlack / photobranch2$totalpix

#Examine:

##           Spread of pixel/canopy cover data.
plot(photobranch2$HuckID, photobranch2$cc)
boxplot(cc~factor(HuckID), varwidth=T, xlab = "HuckID", main="Boxplot of Canopy Cover conditional on HuckID", ylab="Canopy Cover", data =photobranch2)

##           Branchs/plot
plot(photobranch2$Number, photobranch2$BranchNum)

##          Range of light per plot
plot(photobranch2$PlotNum, photobranch2$cc)
plot(photobranch2$PlotName, photobranch2$cc)
plot(photobranch2$Number, photobranch2$cc)
unique(photobranch2$HuckID)
sd(photobranch2$cc)
# 195 branches measured... with total of 591 measurements

#####SD for each huck id number -- which ones are larger than 10%, 1%, 0.1% canopy cover different?
photobranch2sd<-aggregate(photobranch2$cc, list(photobranch2$HuckID),sd)
fiveper<-photobranch2sd[which(photobranch2sd$x>0.05),]
oneper<-photobranch2sd[which(photobranch2sd$x>0.01),]
colnames(oneper) <- "HuckID"
oneper

twoper <-photobranch2sd[which(photobranch2sd$x>0.025),]
colnames(twoper) <- "HuckID"
twoper

photo12<-photobranch2[which(photobranch2$HuckID==12),]
hist(photo12$cc, breaks=20)

photo112<-photobranch2[which(photobranch2$HuckID==112),]
hist(photo112$cc, breaks=20)


#To get a list of the questionable photos... look at huckbranchmast
PhotoIDQs <- merge(huckbranchmast, oneper,by=c("HuckID"), all.x=F)
check <- unique(PhotoIDQs$HuckID)
write.csv(PhotoIDQs, "/PhotoIDQs.csv")

#Aggregate = though still need to address 112, 181, 60, 63

#####Mean data for each huck id number
photobranch2agg<-aggregate(photobranch2, list(photobranch2$HuckID),mean)

# final merge
#huckbranchmast has all the photos split out individually - 587 obs
huckbranchmast<-merge(huckbranchdata, photobranch2,by=c("HuckID"))
#huckbranchmast has all the photos aggregated for each branch - 195 obs (lost 3)
huckbranchmast2<-merge(huckbranchdata, photobranch2agg,by=c("HuckID"))

# Examine without aggregating
hist(huckbranchmast$Berryness, breaks=10)

# Ordered data... turn it into a cumulative probability
pr_k <- table( huckbranchmast$Berryness ) / nrow(huckbranchmast)
pr_k
# cumsum converts to cumulative proportions
cum_pr_k <- cumsum( pr_k )
cum_pr_k
# plot
plot(names(cum_pr_k) , cum_pr_k , type="b" , xlab="Berryness",ylab="cumulative proportion" , ylim=c(0,1) )

# create a logit function
logit <- function(x) log(x/(1-x)) # convenience function 
( lco <- logit( cum_pr_k ) )  # run the logit of the cumulative probabilities
plot(names(lco) , lco , type="b" , xlab="Berryness",ylab="logit of cumulative proportion" , ylim=c(-1,3) )


pairs(huckbranchmast[,c(8, 9, 10, 12, 14, 15, 16, 18, 19, 22, 23, 41)])
hist(huckbranchmast$cc)

# Examine with aggregating
hist(huckbranchmast2$Berryness, breaks=10)
# Zero (1) inflated
hist(huckbranchmast2$Diameter, breaks=10)
#Gaussian.. avg around 1.5
hist(huckbranchmast2$PrcntClustered, breaks=10)
#Most around 0 and 1, few in the middle
hist(huckbranchmast2$BerrySize, breaks=10)
#zero-inflated, gaussian
hist(huckbranchmast2$PrcntRipe, breaks=10)
#Zero-inflated
hist(huckbranchmast2$cc, breaks=10)
# High between 80 and 90%
hist(huckbranchmast2$cc, breaks=20)
#Gaussian... peak at 85%
hist(huckbranchmast2$NumClumps, breaks=10)
#Zero-inflated
hist(huckbranchmast2$Age, breaks=10)
#Gaussian

huckB<-huckbranchmast

# Goal: Berryness ~ CanopyCover * StemDiam* TreeStuff* UnderstoryStuff * LitterDuff * SoilType * Elev * Aspect
#1st Try unfinished
library(glmmML)
practice <- glm(logit(cumsum(huckbranchmast$Berryness/nrow(huckbranchmast)))) ~ cc * Diameter, data=huckB)
summary(practice)
plot(logit(cumsum(huckbranchmast$Berryness/nrow(huckbranchmast)))~cc, data=huckB)
abline(practice)

#2nd Try...
## Richard suggestion:
# Fit a logistic model for "had berries" and an ordered logit model for "ripeness". 
#Berryness... 1-5 --> 0-4
huckbranchmast2$Berryness2<-huckbranchmast2$Berryness-1

#Berry presence/absense ... if 0 vs if 1-4
huckbranchmast2$Berry <- ifelse(huckbranchmast2$Berryness2==0, 0, 1)

#Model for flowering:
m1<-glm(Berry~(cc+I(cc^2))*Diameter, data= huckbranchmast2,family = binomial)
summary(m1)

m2<-glm(Berry~(cc+I(cc^2)), data= huckbranchmast2,family = binomial)
summary(m2)
plot(Berry~cc, data= huckbranchmast2)
lines(predict(m2))

library(nlme)
logBerry <- log(cumsum(huckbranchmast2$Berryness/nrow(huckbranchmast2)))
plot(logBerry)

glmer1 <- glmer(Berryness ~ fixed = (cc+I(cc^2)) * Diameter, random = PlotName data=huckpatchmast2)
summary(practice)
plot(logit(cumsum(huckbranchmast$Berryness/nrow(huckbranchmast)))~cc, data=huckB)
abline(practice)

