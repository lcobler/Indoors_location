#Evaluate techniques for wifi location
#Lara Cobler Moncunill
#January 9th, 2019

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(rpart.plot)
library(randomForest)

#compare the two datasets WIFI and TEST to check if both are equal (column wise)
columncompar <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
    }
    else if(i==tail(names(y),n=1)) {
      print('Names are identical')
    }
  }
}

#download data set
wifilocation <- read.csv("trainingData.csv",header=TRUE)
wifilocation_validation <- read.csv("validationData.csv",header=TRUE)
str(wifilocation[,521:529])
str(wifilocation_validation[,521:529])

#change floor, building, space, relative position, user id, phone id to cathegorical for training set
wifilocation$FLOOR <- factor(wifilocation$FLOOR,
                             levels=c(0,1,2,3,4), labels=c("Ground","First","Second","Third","Fourth"))
wifilocation$BUILDINGID <- factor(wifilocation$BUILDINGID,
                                  levels=c(0,1,2), labels=c("TI","TD","TC"))
wifilocation$SPACEID <- factor(wifilocation$SPACEID)
wifilocation$RELATIVEPOSITION <- factor(wifilocation$RELATIVEPOSITION,
                                        levels=c(1,2),labels=c("Inside","Outside"))
wifilocation$USERID <- factor(wifilocation$USERID, levels=0:18)
wifilocation$PHONEID <- factor(wifilocation$PHONEID, levels= 0:24)
#change UNIX timestamp to datetime
wifilocation$TIMESTAMP <- as.POSIXct(wifilocation$TIMESTAMP, origin="1970-01-01")
summary(wifilocation[,521:529])

#change floor, building, space, relative position, user id, phone id to cathegorical for validation set
wifilocation_validation$FLOOR <- factor(wifilocation_validation$FLOOR,
                                        levels=c(0,1,2,3,4), labels=c("Ground","First","Second","Third","Fourth"))
wifilocation_validation$BUILDINGID <- factor(wifilocation_validation$BUILDINGID,
                                             levels=c(0,1,2), labels=c("TI","TD","TC"))
wifilocation_validation$SPACEID <- "1"
wifilocation_validation$SPACEID <- factor(wifilocation_validation$SPACEID,levels=levels(wifilocation$SPACEID))
wifilocation_validation$RELATIVEPOSITION <- 1
wifilocation_validation$RELATIVEPOSITION <- factor(wifilocation_validation$RELATIVEPOSITION,
                                                   levels=c(1,2),labels=c("Inside","Outside"))
wifilocation_validation$USERID <- factor(wifilocation_validation$USERID,levels=0:18)
wifilocation_validation$PHONEID <- factor(wifilocation_validation$PHONEID,levels= 0:24)
#change UNIX timestamp to datetime
wifilocation_validation$TIMESTAMP <- as.POSIXct(wifilocation_validation$TIMESTAMP, origin="1970-01-01")
summary(wifilocation_validation[,521:529])

#-----------------------------------------Exploratory analysis----------------------------------------------------
#Explore WAPS:
#number WAPS vs records
WAPS <- grep("WAP",names(wifilocation),value=TRUE) #value=TRUE vector containing the elements
WAPSreceived <- apply(wifilocation[,WAPS],MARGIN=1,function(x) sum(x <=0)) #margin=1 indicates apply to rows, margin=2 to columns
hist(WAPSreceived)
WAPSreceived_validation <- apply(wifilocation_validation[,WAPS],1,function(x) sum(x <=0))
hist(WAPSreceived_validation)
# overlaping Histogram Colored (blue and red)
hist(WAPSreceived, col=rgb(1,0,0,0.5))
hist(WAPSreceived_validation, col=rgb(0,0,1,0.5), add=T)
box()

#explore longitude and latitude
summary(wifilocation$LONGITUDE)
summary(wifilocation$LATITUDE)

summary(wifilocation_validation$LONGITUDE)
summary(wifilocation_validation$LATITUDE)

plot(wifilocation$LONGITUDE,wifilocation$LATITUDE)
plot(wifilocation_validation$LONGITUDE,wifilocation_validation$LATITUDE)

#explore building
summary(wifilocation$BUILDINGID)
summary(wifilocation_validation$BUILDINGID)

#floor by building
table(wifilocation$BUILDINGID,wifilocation$FLOOR)
table(wifilocation_validation$BUILDINGID,wifilocation_validation$FLOOR)

ggplot(wifilocation, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Training set")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
ggplot(wifilocation_validation, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Validation set")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#3D plots??

#relative position
table(wifilocation$RELATIVEPOSITION, wifilocation$BUILDINGID)

#timestamp
#see days the the records were taken
table(date(wifilocation$TIMESTAMP))
table(date(wifilocation_validation$TIMESTAMP))

#---------------------------------------------Data cleaning----------------------------------------------------
#Remove WAPS only are in training or testing
WAPS <- grep("WAP",names(wifilocation),value=TRUE)
total_col_training <- sapply(wifilocation[,WAPS],sum) #vector with total sum of columns, if =1993700 all are 100, no detected 
total_col_validation <- sapply(wifilocation_validation[,WAPS],sum)
total_col <- as.data.frame(cbind(total_col_training,total_col_validation))
indices <- which((total_col$total_col_validation ==111100) | (total_col$total_col_training == 1993700))
wifilocation <- wifilocation[,- indices]
wifilocation_validation <- wifilocation_validation[,- indices]
#which(apply(wifilocation, 2, var) == 0) easiest for next time, find columns with 0 variance

#Remove rows that no detected any WAP
WAPS <- grep("WAP",names(wifilocation),value=TRUE) 
wifilocation <- filter(wifilocation, rowSums(wifilocation[,WAPS])!=31200) #remove 76 rows
sum(rowSums(wifilocation_validation[,WAPS])==31200) #0 no remove anything

#Remove duplicated values
wifilocation <- wifilocation[!duplicated(wifilocation), ] #637 duplicates
sum(duplicated(wifilocation_validation)) #0, no duplicates

#change 100 to -110
wifilocation[,WAPS] <- (sapply(wifilocation[,WAPS], function(x) ifelse(x==100,-110,x)))
wifilocation_validation[,WAPS] <- (sapply(wifilocation_validation[,WAPS], function(x) ifelse(x==100,-110,x)))

#Low signals per fingerprint
wifilocation_low <- wifilocation %>% filter_at(vars(starts_with("WAP")), all_vars(. < -80)) #601 all values are <-80
wifilocation_validation_low <- wifilocation_validation %>% filter_at(vars(starts_with("WAP")), all_vars(. >= -80)) #14 all values are <-80
#location:
ggplot(wifilocation, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Low siganl fingerprints")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(data=wifilocation_low,colour="red")+
  geom_point(data=wifilocation_validation_low,colour="green")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#do it further with grouped by location

#frequency each WAP
WAPSdetected <- apply(wifilocation[,WAPS],MARGIN=2,function(x) sum(x >=-80)) #39 WAPS all signals are lower than -80, 
plot(sort(WAPSdetected))
sum(WAPSdetected==0) #39 never detected
WAPSdetected_validation <- apply(wifilocation_validation[,WAPS],MARGIN=2,function(x) sum(x >=-80))
sum(WAPSdetected_validation==0) #53 never detected, those are not important because may be that for chance the spots were not chacked
#remove WAPS not detected in the training

wifilocation <- wifilocation[,- which(WAPSdetected==0)]
wifilocation_validation <- wifilocation_validation[,-which(WAPSdetected==0)]

#Rows with > 4 signals >-80
WAPS <- grep("WAP",names(wifilocation),value=TRUE)
sum(apply(wifilocation[,WAPS],MARGIN=1,function(x) (sum(x >=-80))>=4)) #16327 at least 4 points for 3D location
sum(apply(wifilocation[,WAPS],MARGIN=1,function(x) (sum(x >=-80))>=3)) #16944 if we have the floor 3 points needed for location
sum(apply(wifilocation_validation[,WAPS],MARGIN=1,function(x) (sum(x >=-80))>=4)) #995
sum(apply(wifilocation_validation[,WAPS],MARGIN=1,function(x) (sum(x >=-80))>=3)) #1026
#don't touch, don't remove anything because although the signal is weak may help locate in some place with poor reception

#Fingerprints with values >=-30
high30 <- wifilocation %>% filter_at(vars(starts_with("WAP")), any_vars(. >= -30)) #472
nrow(wifilocation_validation %>% filter_at(vars(starts_with("WAP")), any_vars(. >= -30))) #0
#phone ids
table(high30$PHONEID) #1:1, 7:52, 8:2, 11:2, 14:3, 16:5, 19:397.
table(high30$PHONEID) / table(wifilocation$PHONEID) #1:0%, 7:3%, 8:0%, 11:0%, 14:0%, 16:2%, 19:40%
    #Phone 19! 40% lectures with errors! a lot!, remove all this phone.
#WAPS with >= -30
table(apply (high30[,WAPS],2,function(x) sum(x>=-30) )) #   1 with 83 values >-30
#locate
ggplot(wifilocation, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Locations with WAPS >-30")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  geom_point(data=wifilocation[wifilocation$PHONEID=="19",],colour="green")+
  geom_point(data=wifilocation[wifilocation$PHONEID=="7",],colour="blue")+
  geom_point(data=high30,colour="red")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
  #all over phone19 zone.
#inspect phone 19
summary(wifilocation_validation$PHONEID) #Phone 19 not in the validation
#Remove phone19, 977 rows
wifilocation <- wifilocation[wifilocation$PHONEID!="19",]
high30 <- wifilocation %>% filter_at(vars(starts_with("WAP")), any_vars(. >= -30)) #75
table(apply (high30[,WAPS],2,function(x) sum(x>=-30) )) #1 wap with 26, the other less, ok.

#Exploratory
# check that maintains values everywhere
ggplot(wifilocation, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Training set")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#a corner of the 4th floor from the training set no values, whatch if predicts well.
ggplot(wifilocation_validation, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Validation set")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()

#10 different reads from the same user at the same location, also 10 more from another user in the same location.
#group by location and user/phone.
#Example 1 position
pos1 <- wifilocation %>% filter(LONGITUDE==wifilocation$LONGITUDE[1] & LATITUDE==wifilocation$LATITUDE[1] & FLOOR==wifilocation$FLOOR[1])
summary(pos1$WAP090)
summary(pos1$WAP172) #wide range, different signal all the WAPS.

wifilocation_grouped <- wifilocation %>% group_by(FLOOR,LONGITUDE,LATITUDE,BUILDINGID,SPACEID,RELATIVEPOSITION,USERID,PHONEID) %>% 
  summarise_at(vars(starts_with("WAP")),median)
ggplot(wifilocation_grouped, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Training set grouped")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#It's still the same
#validation don't need to be grouped because all are individual locations
wifilocation_validation_grouped <- wifilocation_validation %>% group_by(FLOOR,LONGITUDE,LATITUDE,BUILDINGID,SPACEID,RELATIVEPOSITION,USERID,PHONEID) %>% 
  summarise_at(vars(starts_with("WAP")),median)
#18 locations repeated.

#remove first values >-30
higher30 <- wifilocation_grouped %>% filter_at(vars(starts_with("WAP")), any_vars(. >= -30))
#3 locations phone7 and user14 
#locate
ggplot(wifilocation_grouped, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Locations with WAPS >-30")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  geom_point(data=wifilocation_grouped[wifilocation_grouped$PHONEID=="7",],colour="blue")+
  geom_point(data=higher30,colour="red")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#inspect phone 7
wifilocation_grouped %>% filter (PHONEID=="7") #129 locations (2/129 = 1.5% locations bad signall)

#Low signals per fingerprint
wifilocation_grouped %>% filter_at(vars(starts_with("WAP")), all_vars(. < -80)) #52 locations all values are <-80
wifilocation_validation %>% filter_at(vars(starts_with("WAP")), all_vars(. < -80)) #14 all values are <-80
#location:
ggplot(wifilocation_grouped, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Low siganl fingerprints")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(data=wifilocation_grouped_low,colour="red")+
  #  geom_point(data=wifilocation_validation_low,colour="green")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#---------add validation, suggest that they need more WAPs to cover the building

#new variable with max WAP, only 1 is necessary to know the building and the floor.
WAPS <- grep("WAP",names(wifilocation),value=TRUE)
wifilocation_grouped$max_val <- apply(wifilocation_grouped[,WAPS], 1, function(x) max(x[x < -30]))
hist(wifilocation_grouped$max_val) #most around -60
wifilocation_grouped$max_wap <- colnames(wifilocation_grouped[,WAPS])[apply(wifilocation_grouped[,WAPS],1,
                                                                            function(x) which.max(x[x < -30]))]
wifilocation_grouped$max_wap <- factor(wifilocation_grouped$max_wap,levels=WAPS) #to have the same levels with validation
table(wifilocation_grouped$max_wap)
table(wifilocation_grouped$max_wap,wifilocation_grouped$BUILDINGID) #WAP173 and WAP046 in 2 buildings, WAP248 in the 3 buildings
#validation
wifilocation_validation_grouped$max_val <- apply(wifilocation_validation_grouped[,WAPS], 1, max)
hist(wifilocation_validation_grouped$max_val)
wifilocation_validation_grouped$max_wap <- colnames(wifilocation_validation_grouped[,WAPS])[apply(wifilocation_validation_grouped[,WAPS],1,which.max)]
wifilocation_validation_grouped$max_wap <- factor(wifilocation_validation_grouped$max_wap,levels=WAPS) #define the same levels
hist(table(wifilocation_validation_grouped$max_wap))
table(wifilocation_validation_grouped$max_wap,wifilocation_validation_grouped$BUILDINGID) #no repeated,
length(setdiff(wifilocation_grouped$max_wap,wifilocation_validation_grouped$max_wap))#32
length(setdiff(wifilocation_validation_grouped$max_wap,wifilocation_grouped$max_wap)) #13
#13 that appear in the validation that are not in the training, take the max value of the validation from the training max values!
#plot these WAPS
ggplot(wifilocation_grouped, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Training set grouped")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  geom_point(data=wifilocation_grouped[wifilocation_grouped$max_wap=="WAP173",],colour="red")+
  geom_point(data=wifilocation_validation[wifilocation_validation$max_wap=="WAP173",],colour="green")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#In validation and 2 training in the ESTCE-TD floor2 -> check the one that is in the ESTCE-TI
x <- wifilocation_grouped %>% filter (BUILDINGID=="TI",max_wap=="WAP173")
wifilocation_grouped %>% filter(BUILDINGID==x$BUILDINGID & FLOOR==x$FLOOR & LONGITUDE==x$LONGITUDE) %>% select(max_wap, max_val)
#the other at the same position is WAP057
ggplot(wifilocation_grouped, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Training set grouped")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  geom_point(data=wifilocation_grouped[wifilocation_grouped$max_wap=="WAP046",],colour="red")+
  geom_point(data=wifilocation_validation[wifilocation_validation$max_wap=="WAP046",],colour="green")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#In training one in each position ESTCE-TI floor3 and ESTCE-TD floor1, validation ESTCE-TI floor3
x <- wifilocation_grouped %>% filter (max_wap=="WAP046", BUILDINGID=="TI")
wifilocation_grouped %>% filter(BUILDINGID==x$BUILDINGID & FLOOR==x$FLOOR & LONGITUDE==x$LONGITUDE) %>% select(max_wap, max_val)
#Partner of building TI is WAP029 -63dbm
x <- wifilocation_grouped %>% filter (max_wap=="WAP046", BUILDINGID=="TD")
wifilocation_grouped %>% filter(BUILDINGID==x$BUILDINGID & FLOOR==x$FLOOR & LONGITUDE==x$LONGITUDE) %>% select(max_wap, max_val)
#2 Partners of building TD is WAP103 and WAP105 -63dbm
#the other at the same position is WAP248
ggplot(wifilocation_grouped, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Training set grouped")+
  labs(x='Longitude', y='Latitude') +
  geom_point()+
  geom_point(data=wifilocation_grouped[wifilocation_grouped$max_wap=="WAP248",],colour="red")+
  geom_point(data=wifilocation_validation[wifilocation_validation$max_wap=="WAP248",],colour="green")+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()

##New max_wap. Not take WAP248,WAP173 and WAP046
 #For validation take only the variables used in the training.
WAPS <- grep("WAP",names(wifilocation),value=TRUE)
WAPS <- WAPS [!WAPS %in% c("WAP248","WAP173","WAP046")] #don't use these WAPS
wifilocation_grouped$max_val <- apply(wifilocation_grouped[,WAPS], 1, function(x) max(x[x < -30]))
hist(wifilocation_grouped$max_val) #most around -60
wifilocation_grouped$max_wap <- colnames(wifilocation_grouped[,WAPS])[apply(wifilocation_grouped[,WAPS],1,
                                                                            function(x) which.max(x[x < -30]))]
wifilocation_grouped$max_wap <- factor(wifilocation_grouped$max_wap) #to have the same levels with validation
table(wifilocation_grouped$max_wap)
table(wifilocation_grouped$max_wap,wifilocation_grouped$BUILDINGID) #WAP172 1 in 1 building and 17 to another.
#validation
WAPSvalidation <- levels(wifilocation_grouped$max_wap)
wifilocation_validation_grouped$max_val <- apply(wifilocation_validation_grouped[,WAPSvalidation], 1, max)
hist(wifilocation_validation_grouped$max_val)
wifilocation_validation_grouped$max_wap <- colnames(wifilocation_validation_grouped[,WAPSvalidation])[apply(wifilocation_validation_grouped[,WAPSvalidation],1,which.max)]
wifilocation_validation_grouped$max_wap <- factor(wifilocation_validation_grouped$max_wap,levels=WAPSvalidation) #define the same levels
hist(table(wifilocation_validation_grouped$max_wap))
table(wifilocation_validation_grouped$max_wap,wifilocation_validation_grouped$BUILDINGID) #no repeated,
length(setdiff(wifilocation_grouped$max_wap,wifilocation_validation_grouped$max_wap))#29 in the training not in the validation
length(setdiff(wifilocation_validation_grouped$max_wap,wifilocation_grouped$max_wap)) #0 in the validation not in the training

#user and phoneid?
#plot user by location
ggplot(wifilocation_grouped, aes(x=LONGITUDE,y=LATITUDE,color=USERID)) +
  ggtitle("Training set grouped")+
  labs(x='Longitude', y='Latitude') +
  geom_point(aes(colour=USERID))+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#very clear pattern where each user took sasmples , check overlay of point, make them translucid?...
#phone id by location
ggplot(wifilocation_grouped, aes(x=LONGITUDE,y=LATITUDE,color=PHONEID)) +
  ggtitle("Training set grouped")+
  labs(x='Longitude', y='Latitude') +
  geom_point(aes(colour=PHONEID))+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
ggplot(wifilocation_validation, aes(x=LONGITUDE,y=LATITUDE,color=PHONEID)) +
  ggtitle("Training set grouped")+
  labs(x='Longitude', y='Latitude') +
  geom_point(aes(colour=PHONEID))+
  facet_grid(FLOOR~BUILDINGID)+
  theme_dark()
#also the same phone with the same zone in the training set and in the validation different phones all over the places
table(wifilocation_grouped$USERID,wifilocation_grouped$PHONEID) #same user with the same phone


#-------------------------------Predicting building,--------------------------------------------- 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                           classProbs=TRUE)
## Random forest prediction, tunning parameter mtry, use 50 trees.
set.seed(123)
rf_building_max<- train(BUILDINGID~max_wap, data=wifilocation_grouped, method="rf",
                    trControl=fitControl,
                    ntree= 50)
rf_building_max

#random forest with all waps
set.seed(123)
rf_building_all<- train(BUILDINGID~.-FLOOR-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap, 
                        data=wifilocation_grouped, method="rf",
                        trControl=fitControl,
                        ntree= 50)
rf_building_all

#save the models
save(rf_building, file = "rf_building.rda")
save(rf_building_max, file = "rf_building_max.rda")
save(rf_building_all, file = "rf_building_all.rda")

#Predictions with rf using all WAPS
load("rf_building_all.rda")
predictions_building_rf_all<-predict(rf_building_all, newdata=wifilocation_validation_grouped) 
confusionMatrix(predictions_building_rf_all, wifilocation_validation_grouped$BUILDINGID) 
#1 error. Accuracy:0.9991, kappa:0.9986

#Predictions with rf
load("rf_building.rda")
predictions_building_rf<-predict(rf_building, wifilocation_validation_grouped)
confusionMatrix(predictions_building_rf, wifilocation_validation_grouped$BUILDINGID) 
#9 error. Accuracy:0.9918, kappa:.987
#Probably the 9 WAPS that appear in the validation and not in the training

#Predictions with rf groomed max
load("rf_building_max.rda")
predictions_building_rf_max<-predict(rf_building_max, wifilocation_validation_grouped)
confusionMatrix(predictions_building_rf_max, wifilocation_validation_grouped$BUILDINGID) 
#0 error. Accuracy:1, kappa:1

#------------------------Locate WAPS of each building-------------------------------------------
#locate building and floor of each WAP
WAPS <- grep("WAP",names(wifilocation_grouped),value=TRUE)

max_loc_val <- apply(wifilocation_grouped[,WAPS], 2, function(x) max(x[x < -30]))
hist(max_loc_val) #most around -40, second peak after -80
wap_loc <- as.data.frame(max_loc_val)
indices_wap <- apply(wifilocation_grouped[,WAPS],2,function(x) which.max(x[x < -30]))
wap_build <- (wifilocation_grouped[indices_wap,"BUILDINGID"])
wap_floor <- (wifilocation_grouped[indices_wap,"FLOOR"])
wap_loc <- cbind(wap_loc,wap_build)
wap_loc <- cbind(wap_loc,wap_floor)
table(wap_loc$BUILDINGID,wap_loc$FLOOR)
#vectors with WAPS for each building
WAPS_loc_TI <- row.names(wap_loc[wap_loc$BUILDINGID=="TI",])
WAPS_loc_TD <- row.names(wap_loc[wap_loc$BUILDINGID=="TD",])
WAPS_loc_TC <- row.names(wap_loc[wap_loc$BUILDINGID=="TC",])

#-------------------------------------PCA----------------------------------------------------------
WAPS <- grep("WAP",names(wifilocation_grouped),value=TRUE)
pca <- prcomp(wifilocation_grouped[,WAPS],scale=TRUE) #normalize
 #pca$center, pca$scale: used for normalization
 #pca$rotation: principal component loading
 #pca$x: principal component score vectors
biplot(pca,scale=0) #plot
std_dev <- pca$sdev #compute SD
pca_var <- std_dev^2 #compute variance
prop_varex <- pca_var/sum(pca_var) #proportion of variance explained
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
cumsum(prop_varex) #first 50 PCs explain 80%, firsts 62 PCs explain 85%, first 81PCs explain 90%
sum(prop_varex[1:50]) #first 50 principal components explains 80% of the variation

#PCA by building
wifilocation_grouped_TI <- wifilocation_grouped %>% filter(BUILDINGID=="TI")
wifilocation_grouped_TD <- wifilocation_grouped %>% filter(BUILDINGID=="TD")
wifilocation_grouped_TC <- wifilocation_grouped %>% filter(BUILDINGID=="TC")
wifilocation_validation_grouped_TI <- wifilocation_validation_grouped %>% filter(BUILDINGID=="TI")
wifilocation_validation_grouped_TD <- wifilocation_validation_grouped %>% filter(BUILDINGID=="TD")
wifilocation_validation_grouped_TC <- wifilocation_validation_grouped %>% filter(BUILDINGID=="TC")

#PCA for TI
pca_ti <- prcomp(wifilocation_grouped_TI[,WAPS])
biplot(pca_ti,scale=0) #plot
std_dev_ti <- pca_ti$sdev #compute SD
pca_var_ti <- std_dev_ti^2 #compute variance
prop_varex_ti <- pca_var_ti/sum(pca_var_ti) #proportion of variance explained
plot(prop_varex_ti, xlab = "Principal Component TI",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex_ti), xlab = "Principal Component TI",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
cumsum(prop_varex_ti)
sum(prop_varex_ti[1:18]) #first 18 principal components explains 89.45% of the variation

#PCA for TD
pca_td <- prcomp(wifilocation_grouped_TD[,WAPS])
biplot(pca_td,scale=0) #plot
std_dev_td <- pca_td$sdev #compute SD
pca_var_td <- std_dev_td^2 #compute variance
prop_varex_td <- pca_var_td/sum(pca_var_td) #proportion of variance explained
plot(prop_varex_td, xlab = "Principal Component TD",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex_td), xlab = "Principal Component TD",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
cumsum(prop_varex_td)
sum(prop_varex_td[1:19]) #first 19 principal components explains 89.67% of the variation

#PCA for TC
pca_tc <- prcomp(wifilocation_grouped_TC[,WAPS])
biplot(pca_tc,scale=0) #plot
std_dev_tc <- pca_tc$sdev #compute SD
pca_var_tc <- std_dev_tc^2 #compute variance
prop_varex_tc <- pca_var_tc/sum(pca_var_tc) #proportion of variance explained
plot(prop_varex_tc, xlab = "Principal Component TC",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex_tc), xlab = "Principal Component TC",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
cumsum(prop_varex_tc)
sum(prop_varex_tc[1:19]) #first 19 principal components explains 89.60% of the variation

#nexts steps are to use principal components x to variables that are not WAPS in the datasets and use it for modeling

#--------------------------------------FLOOR----------------------------------------------------
#check max wap for floor
table(wifilocation_grouped$max_wap,wifilocation_grouped$FLOOR) #each one predominates in 1 floor, some in more than one floor
wifilocation_grouped %>% 
  group_by(max_wap,BUILDINGID,FLOOR) %>% 
  tally() #counts

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                           classProbs=TRUE)
#floor_rf_max<- train(FLOOR~max_wap, 
#                        data=wifilocation_grouped, method="rf",
#                        trControl=fitControl,
#                        ntree= 10)
#floor_rf_max #Accuracy:0.8991, #kappa:0.8682
#floor_rf_max_build<- train(FLOOR~max_wap+BUILDINGID, 
#                     data=wifilocation_grouped, method="rf",
#                     trControl=fitControl,
#                     ntree= 10)
#floor_rf_max_build #0.8964536  0.8645903
#floor_rf_max_val<- train(FLOOR~max_wap+max_val, 
#                           data=wifilocation_grouped, method="rf",
#                           trControl=fitControl,
#                           ntree= 10)
#floor_rf_max_val #0.8982128  0.8669325
#floor_rf_max_TC<- train(FLOOR~max_wap, 
#                     data=wifilocation_grouped_TC, method="rf",
#                     trControl=fitControl,
#                     ntree= 10)
#floor_rf_max_TC #Accuracy:0.92, #kappa:0.90
## Predict the floor using the max_wap for each building

#Floor for Building TI ------------------------------------------
#remove max WAP not used and redefine levels
length(setdiff(wifilocation_grouped_TI$max_wap,wifilocation_validation_grouped_TI$max_wap))#4
length(setdiff(wifilocation_validation_grouped_TI$max_wap,wifilocation_grouped_TI$max_wap)) #0 OK!
WAPS_TI <- unique(wifilocation_grouped_TI$max_wap)
wifilocation_grouped_TI$max_wap <- factor(wifilocation_grouped_TI$max_wap,levels=WAPS_TI)
wifilocation_validation_grouped_TI$max_wap <- factor(wifilocation_validation_grouped_TI$max_wap,levels=WAPS_TI)
#remove 4th floor
three_floors <- c("Ground", "First" , "Second", "Third")
wifilocation_grouped_TI$FLOOR <- factor(wifilocation_grouped_TI$FLOOR,levels=three_floors)
wifilocation_validation_grouped_TI$FLOOR <- factor(wifilocation_validation_grouped_TI$FLOOR,levels=three_floors)
#PCA
WAPS <- grep("WAP",names(wifilocation_grouped_TI),value=TRUE)
wifilocation_grouped_TI_pca <- data.frame(select(wifilocation_grouped_TI,-WAPS), pca_ti$x[,1:18])
wifilocation_validation_grouped_TI_pca <- predict(pca_ti, newdata = wifilocation_validation_grouped_TI)
wifilocation_validation_grouped_TI_pca <- wifilocation_validation_grouped_TI_pca[,1:18]
wifilocation_validation_grouped_TI_pca <- data.frame(select(wifilocation_validation_grouped_TI,-WAPS), wifilocation_validation_grouped_TI_pca)
all.equal(names(wifilocation_grouped_TI_pca), names(wifilocation_validation_grouped_TI_pca))#TRUE

#Modelling
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                           classProbs=TRUE)
set.seed(123)
floor_rf_ti<- train(FLOOR~max_wap, data=wifilocation_grouped_TI, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_ti #Accuracy= 0.87, kappa=0.82
set.seed(123)
floor_knn_ti <- train(FLOOR ~ max_wap, 
  data = wifilocation_grouped_TI, method = "knn", 
  preProc = c("center","scale"), tuneLength = 10,
  trControl = fitControl)
floor_knn_ti #Accuracy=0.77, kappa=0.7
set.seed(123)
floor_knn_ti_all <- train(FLOOR ~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                      data = wifilocation_grouped_TI, method = "knn", 
                      preProc = c("center","scale"), tuneLength = 10,
                      trControl = fitControl)
floor_knn_ti_all #Accuracy=0.8868, kappa=0.84844
set.seed(123)
floor_rf_ti_all<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data=wifilocation_grouped_TI, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_ti_all #Accuracy=.9895, kappa=0.9857
set.seed(123)
floor_rf_ti_pca<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data=wifilocation_grouped_TI_pca, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_ti_pca #Accuracy=.97, kappa=0.96


#use floor_rf_ti_all
save(floor_rf_ti, file = "floor_rf_ti.rda")
save(floor_rf_ti_all, file = "floor_rf_ti_all.rda")
save(floor_rf_ti_pca, file = "floor_rf_ti_pca.rda")

#Predictions floor using max_wap
load("floor_rf_ti.rda")
predictions_floor_rf_ti<-predict(floor_rf_ti, wifilocation_validation_grouped_TI)
confusionMatrix(predictions_floor_rf_ti, wifilocation_validation_grouped_TI$FLOOR) 
#Accuracy:0.8645, kappa:0.8092

#Predictions floor using all waps
load("floor_rf_ti_all.rda")
predictions_floor_rf_ti_all<-predict(floor_rf_ti_all, wifilocation_validation_grouped_TI)
confusionMatrix(predictions_floor_rf_ti_all, wifilocation_validation_grouped_TI$FLOOR) 
#Accuracy:0.9542, kappa:0.9352. 3 in the ground predicted at 3rd, 1 second predicted at ground.

#Predictions floor using pca
load("floor_rf_ti_pca.rda")
predictions_floor_rf_ti_pca<-predict(floor_rf_ti_pca, wifilocation_validation_grouped_TI_pca)
confusionMatrix(predictions_floor_rf_ti_pca, wifilocation_validation_grouped_TI_pca$FLOOR) 
#Accuracy:0.94, kappa:0.91. 1 in the ground predicted at 2nd. #choose this, less "big mistakes"
wifilocation_validation_grouped_TI$FLOOR_pred <- predictions_floor_rf_ti_pca
#location
wifilocation_validation_grouped_TI$FLOOR_pred_bad <- ifelse(wifilocation_validation_grouped_TI$FLOOR_pred ==
                                                              wifilocation_validation_grouped_TI$FLOOR, "Right",
                                                            ifelse(wifilocation_validation_grouped_TI$FLOOR_pred == "Second" &
                                                                     wifilocation_validation_grouped_TI$FLOOR == "Ground","2 floor difference", "1 floor difference"))
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE,color=FLOOR_pred_bad)) +
  ggtitle("Errors in FLOOR")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  facet_wrap("FLOOR")

#Floor for Building TC ------------------------------------------
#remove max WAP not used and redefine levels
length(setdiff(wifilocation_grouped_TC$max_wap,wifilocation_validation_grouped_TC$max_wap))#5
length(setdiff(wifilocation_validation_grouped_TC$max_wap,wifilocation_grouped_TC$max_wap)) #0 OK!
WAPS_TC <- unique(wifilocation_grouped_TC$max_wap)
wifilocation_grouped_TC$max_wap <- factor(wifilocation_grouped_TC$max_wap,levels=WAPS_TC)
wifilocation_validation_grouped_TI$max_wap <- factor(wifilocation_validation_grouped_TI$max_wap,levels=WAPS_TI)
#PCA
WAPS <- grep("WAP",names(wifilocation_grouped_TI),value=TRUE)
wifilocation_grouped_TC_pca <- data.frame(select(wifilocation_grouped_TC,-WAPS), pca_tc$x[,1:19])
wifilocation_validation_grouped_TC_pca <- predict(pca_tc, newdata = wifilocation_validation_grouped_TC)
wifilocation_validation_grouped_TC_pca <- wifilocation_validation_grouped_TC_pca[,1:19]
wifilocation_validation_grouped_TC_pca <- data.frame(select(wifilocation_validation_grouped_TC,-WAPS), wifilocation_validation_grouped_TC_pca)
all.equal(names(wifilocation_grouped_TC_pca), names(wifilocation_validation_grouped_TC_pca))#TRUE


fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                           classProbs=TRUE)
set.seed(123)
floor_rf_tc_max<- train(FLOOR~max_wap, data=wifilocation_grouped_TC, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_tc_max #Accuracy= 0.9243, kappa=0.9025

#tune RF with different number trees
model_RF_floor_TC_max <- list() #create a list of models
for (ntree in c(10, 20, 50, 100, 500)) {  #for each number of trees
  set.seed(123)
  fit <- train(FLOOR~max_wap, data=wifilocation_grouped_TC, method="rf", trControl=fitControl,tuneLength = 10, ntree= ntree)
  key <- toString(ntree) #transform number of trees to string
  model_RF_floor_TC_max[[key]] <- fit #name each model and store it a variable of model_RF
}
#compare results
model_RF_floor_TC_max[["10"]] #Accuracy=0.92  kappa=0.90
model_RF_floor_TC_max[["20"]] #Accuracy= 0.92 kappa=0.90
model_RF_floor_TC_max[["50"]] #Accuracy= 0.92 kappa=0.90
model_RF_floor_TC_max[["100"]] #Accuracy= 0.93 kappa=0.90
model_RF_floor_TC_max[["500"]] #Accuracy= 0.93 kappa=0.90
##100 trees is the best
floor_rf2_tc_max <- model_RF_floor_TC_max[["100"]]
set.seed(123)
floor_gbm_tc_max <- train(FLOOR~max_wap, data=wifilocation_grouped_TC, method = "gbm", 
                        tuneLength = 10,
                        trControl = fitControl)
floor_gbm_tc_max #Accuracy=0.83   kappa=0.78
set.seed(123)
floor_knn_tc_max <- train(FLOOR ~ max_wap, 
                      data = wifilocation_grouped_TC, method = "knn", 
                      preProc = c("center","scale"), tuneLength = 10,
                      trControl = fitControl)
floor_knn_tc_max #Accuracy=0.8997, kappa=0.8708
set.seed(123)
floor_rf_tc_all<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data=wifilocation_grouped_TC, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_tc_all #Accuracy=.9859, kappa=0.9816
set.seed(123)
floor_rf_tc_pca<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data=wifilocation_grouped_TC_pca, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_tc_pca #Accuracy=.98, kappa=0.98

#use floor_rf_ti_all
save(floor_rf_tc_max, file = "floor_rf_tc_max.rda")
save(floor_rf2_tc_max, file = "floor_rf2_tc_max.rda")
save(floor_gbm_tc_max, file = "floor_gbm_tc_max.rda")
save(floor_rf_tc_all, file = "floor_rf_tc_all.rda")
save(floor_rf_tc_pca, file = "floor_rf_tc_pca.rda")

#Predictions floor using max_wap
load("floor_rf_tc_max.rda")
predictions_floor_rf_tc_max<-predict(floor_rf_tc_max, wifilocation_validation_grouped_TC)
confusionMatrix(predictions_floor_rf_tc_max, wifilocation_validation_grouped_TC$FLOOR) 
#Accuracy:0.9198, kappa:0.8929. 2 from the second floor predicted at the third
##USE These

load("floor_rf2_tc_max.rda")
predictions_floor_rf2_tc_max<-predict(floor_rf2_tc_max, wifilocation_validation_grouped_TC)
confusionMatrix(predictions_floor_rf2_tc_max, wifilocation_validation_grouped_TC$FLOOR) 
#Accuracy:0.9198, kappa:0.8929. 2 from the second floor predicted at the third, same

load("floor_gbm_tc_max.rda")
predictions_floor_gbm_tc_max<-predict(floor_gbm_tc_max, wifilocation_validation_grouped_TC)
confusionMatrix(predictions_floor_gbm_tc_max, wifilocation_validation_grouped_TC$FLOOR) 
#Accuracy:0.7672, kappa:0.6978. 2 from the second floor predicted at the third

#Predictions floor using all waps
load("floor_rf_tc_all.rda")
predictions_floor_rf_tc_all<-predict(floor_rf_tc_all, wifilocation_validation_grouped_TC)
confusionMatrix(predictions_floor_rf_tc_all, wifilocation_validation_grouped_TC$FLOOR) 
#Accuracy:0.8359, kappa:0.7778. 1 in the 4th predicted at 1st. 25+1 errors from the 4th, probably the spot with no training, and the phone 19.

#Predictions floor using pca
load("floor_rf_tc_pca.rda")
predictions_floor_rf_tc_pca<-predict(floor_rf_tc_pca, wifilocation_validation_grouped_TC_pca)
confusionMatrix(predictions_floor_rf_tc_pca, wifilocation_validation_grouped_TC_pca$FLOOR) 
#Accuracy:0.8779, kappa:0.8335. all wear errors from the 4th floor

##Add predicted to valdation set and visualize
wifilocation_validation_grouped_TC$FLOOR_pred <- predictions_floor_rf_tc_max
#location
wifilocation_validation_grouped_TC$FLOOR_pred_bad <- ifelse(wifilocation_validation_grouped_TC$FLOOR_pred ==
                                                              wifilocation_validation_grouped_TC$FLOOR, "Right",
                                                            ifelse(wifilocation_validation_grouped_TC$FLOOR_pred == "Third" &
                                                                     wifilocation_validation_grouped_TC$FLOOR == "First","2 floor difference", "1 floor difference"))
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE,color=FLOOR_pred_bad)) +
  ggtitle("Errors in FLOOR")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  facet_wrap("FLOOR")

#Floor for Building TD ------------------------------------------
#remove max WAP not used and redefine levels
length(setdiff(wifilocation_grouped_TD$max_wap,wifilocation_validation_grouped_TD$max_wap))#21
length(setdiff(wifilocation_validation_grouped_TD$max_wap,wifilocation_grouped_TD$max_wap)) #0 OK!
WAPS_TD <- unique(wifilocation_grouped_TD$max_wap)
wifilocation_grouped_TD$max_wap <- factor(wifilocation_grouped_TD$max_wap,levels=WAPS_TD)
wifilocation_validation_grouped_TD$max_wap <- factor(wifilocation_validation_grouped_TD$max_wap,levels=WAPS_TD)
#remove 4th floor
three_floors <- c("Ground", "First" , "Second", "Third")
wifilocation_grouped_TD$FLOOR <- factor(wifilocation_grouped_TD$FLOOR,levels=three_floors)
wifilocation_validation_grouped_TD$FLOOR <- factor(wifilocation_validation_grouped_TD$FLOOR,levels=three_floors)
#PCA
WAPS <- grep("WAP",names(wifilocation_grouped_TD),value=TRUE)
wifilocation_grouped_TD_pca <- data.frame(select(wifilocation_grouped_TD,-WAPS), pca_td$x[,1:19])
wifilocation_validation_grouped_TD_pca <- predict(pca_td, newdata = wifilocation_validation_grouped_TD)
wifilocation_validation_grouped_TD_pca <- wifilocation_validation_grouped_TD_pca[,1:19]
wifilocation_validation_grouped_TD_pca <- data.frame(select(wifilocation_validation_grouped_TD,-WAPS), wifilocation_validation_grouped_TD_pca)
all.equal(names(wifilocation_grouped_TD_pca), names(wifilocation_validation_grouped_TD_pca))#TRUE


fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                           classProbs=TRUE)
set.seed(123)
floor_rf_td_max<- train(FLOOR~max_wap, data=wifilocation_grouped_TD, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_td_max #Accuracy= 0.9077609, kappa=0.8760174
#tune RF with different number trees
model_RF_floor_TD_max <- list() #create a list of models
for (ntree in c(10, 20, 50, 100, 500)) {  #for each number of trees
  set.seed(123)
  fit <- train(FLOOR~max_wap, data=wifilocation_grouped_TD, method="rf", trControl=fitControl,tuneLength = 10, ntree= ntree)
  key <- toString(ntree) #transform number of trees to string
  model_RF_floor_TD_max[[key]] <- fit #name each model and store it a variable of model_RF
}
#compare results
model_RF_floor_TD_max[["10"]] #Accuracy=0.90  kappa=0.86
model_RF_floor_TD_max[["20"]] #Accuracy= 0.90 kappa=0.87
model_RF_floor_TD_max[["50"]] #Accuracy= 0.90 kappa=0.87
model_RF_floor_TD_max[["100"]] #Accuracy= 0.90 kappa=0.87
model_RF_floor_TD_max[["500"]] #Accuracy= 0.90 kappa=0.87
##all the same

set.seed(123)
floor_gbm_td_max <- train(FLOOR~max_wap, data=wifilocation_grouped_TD, method = "gbm", 
                          tuneLength = 10,
                          trControl = fitControl)
floor_gbm_td_max #Accuracy=0.51   kappa=0.34


set.seed(123)
floor_knn_td_max <- train(FLOOR ~ max_wap, 
                          data = wifilocation_grouped_TD, method = "knn", 
                          preProc = c("center","scale"), tuneLength = 10,
                          trControl = fitControl)
floor_knn_td_max #Accuracy=0.7876, kappa=0.71213
set.seed(123)
floor_rf_td_all<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data=wifilocation_grouped_TD, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_td_all #Accuracy=.9592630, kappa=0.9452
set.seed(123)
floor_rf_td_pca<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data=wifilocation_grouped_TD_pca, method="rf", trControl=fitControl,tuneLength = 10, ntree= 50)
floor_rf_td_pca #Accuracy=.9585, kappa=0.944

#use floor_rf_ti_all
save(floor_rf_td_max, file = "floor_rf_td_max.rda")
save(floor_rf_td_all, file = "floor_rf_td_all.rda")
save(floor_rf_td_pca, file = "floor_rf_td_pca.rda")

#Predictions floor using max_wap
load("floor_rf_td_max.rda")
predictions_floor_rf_td_max<-predict(floor_rf_td_max, wifilocation_validation_grouped_TD)
confusionMatrix(predictions_floor_rf_td_max, wifilocation_validation_grouped_TD$FLOOR) 
#Accuracy:0.9088, kappa:0.8619. 1 from the ground floor predicted at the second

#Predictions floor using all waps
load("floor_rf_td_all.rda")
predictions_floor_rf_td_all<-predict(floor_rf_td_all, wifilocation_validation_grouped_TD)
confusionMatrix(predictions_floor_rf_td_all, wifilocation_validation_grouped_TD$FLOOR) 
#Accuracy:0.772, kappa:0.6759.

#Predictions floor using pca
load("floor_rf_td_pca.rda")
predictions_floor_rf_td_pca<-predict(floor_rf_td_pca, wifilocation_validation_grouped_TD_pca)
confusionMatrix(predictions_floor_rf_td_pca, wifilocation_validation_grouped_TD_pca$FLOOR) 
#Accuracy:0.7655, kappa:0.6644.

##Add predicted to valdation set and visualize
wifilocation_validation_grouped_TD$FLOOR_pred <- predictions_floor_rf_td_max
#location
wifilocation_validation_grouped_TD$FLOOR_pred_bad <- ifelse(wifilocation_validation_grouped_TD$FLOOR_pred ==
                                                              wifilocation_validation_grouped_TD$FLOOR, "Right",
                                                            ifelse(wifilocation_validation_grouped_TD$FLOOR_pred == "Second" &
                                                                     wifilocation_validation_grouped_TD$FLOOR == "Ground","2 floor difference", "1 floor difference"))
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE,color=FLOOR_pred_bad)) +
  ggtitle("Errors in FLOOR")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  facet_wrap("FLOOR")

#-------------------Floor all building together----------------------------------------------
#PCA all together
WAPS <- grep("WAP",names(wifilocation_grouped),value=TRUE)
wifilocation_grouped_pca <- data.frame(select(wifilocation_grouped,-WAPS), pca$x[,1:50])
wifilocation_validation_grouped_pca <- predict(pca, newdata = wifilocation_validation_grouped)
wifilocation_validation_grouped_pca <- wifilocation_validation_grouped_pca[,1:50]
wifilocation_validation_grouped_pca <- data.frame(select(wifilocation_validation_grouped,-WAPS), wifilocation_validation_grouped_pca)
all.equal(names(wifilocation_grouped_pca), names(wifilocation_validation_grouped_pca))#TRUE

#Modelling
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                           classProbs=TRUE)
set.seed(123)
floor_rf_all<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data=wifilocation_grouped, method="rf", trControl=fitControl,tuneLength = 10, ntree= 10)
floor_rf_all #Accuracy=.96, kappa=0.95
set.seed(123)
floor_rf_pca<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data=wifilocation_grouped_pca, method="rf", trControl=fitControl,tuneLength = 10, ntree= 10)
floor_rf_pca #Accuracy=.93, kappa=0.90
set.seed(123)
floor_rf_pcab<- train(FLOOR~ .-USERID-LONGITUDE-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap, 
                     data=wifilocation_grouped_pca, method="rf", trControl=fitControl,tuneLength = 10, ntree= 10)
floor_rf_pcab #Accuracy=.93, kappa=0.90

#---------------------------------Longitude and Latitude TI ----------------------------------------------------------
#Subset by building, no need to take into account the floor because is not related with longitude/latitude.
#For each building try, all WAPS, PCA and WAPS detected on that building.

#-----Longitude TI-------------
#PCA
set.seed(123)
lon_knn_ti_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                          data = wifilocation_grouped_TI_pca, method = "knn", 
                          preProc = c("center","scale"), tuneLength = 10,
                          trControl = fitControl)
lon_knn_ti_pca #RMSE=5.26, R=0.95, MAE=3.95
set.seed(123)
lon_knn2_ti_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TI_pca, method = "knn", 
                        preProc = c("center","scale"), 
                        tuneGrid = expand.grid(k=c(1:5)),
                        trControl = fitControl)
lon_knn2_ti_pca #RMSE=5.10, R=0.96, MAE=3.78
set.seed(123)
lon_rf_ti_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TI_pca, method = "rf", 
                        ntree=50, tuneLength = 10,
                        trControl = fitControl)
lon_rf_ti_pca #RMSE=5.77, R=0.95, MAE=4.12
#all
set.seed(123)
lon_knn_ti_all <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TI, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lon_knn_ti_all #RMSE=6.33, R=0.94, MAE=4.65
set.seed(123)
lon_rf_ti_all <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TI, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lon_rf_ti_all #RMSE=5.94, R=0.95, MAE=4.4
#only WAPS located at TI
wifilocation_grouped_TI_waps <- data.frame(wifilocation_grouped_TI[,WAPS_loc_TI],wifilocation_grouped_TI[,"LONGITUDE"],
                                           wifilocation_grouped_TI[,"LATITUDE"])
set.seed(123)
lon_knn_ti_waps <- train(LONGITUDE ~ .-LATITUDE, 
                        data = wifilocation_grouped_TI_waps, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lon_knn_ti_waps #RMSE=6.80, R=0.93, MAE=4.95
set.seed(123)
lon_rf_ti_waps<- train(LONGITUDE ~ .-LATITUDE, 
                       data = wifilocation_grouped_TI_waps, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lon_rf_ti_waps #RMSE=5.99, R=0.94, MAE=4.46

#Tune using PCA
set.seed(123)
lon_gbm_ti_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TI_pca, method = "gbm", 
                       tuneLength = 10,
                       trControl = fitControl)
lon_gbm_ti_pca #RMSE=5.88, R=0.94, MAE=4.21
set.seed(123)
lon_svml_ti_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TI_pca, method = "svmLinear", 
                        tuneLength = 10,preProcess = c("center", "scale"),
                        trControl = fitControl)
lon_svml_ti_pca #RMSE=9.71, R=0.85, MAE=7.23
set.seed(123)
lon_svmr_ti_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TI_pca, method = "svmRadial", 
                         tuneLength = 10,preProcess = c("center", "scale"),
                         trControl = fitControl)
lon_svmr_ti_pca #RMSE=5.17, R=0.96, MAE=3.96
#tune svmr
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1),
                           C = c(2,4,8,16,32))
set.seed(123)
lon_svmr2_ti_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TI_pca, method = "svmRadial", 
                         tuneGrid = grid_radial,preProcess = c("center", "scale"),
                         trControl = fitControl)
lon_svmr2_ti_pca #RMSE=4.81, R=0.96, MAE=3.69

#save models to test
save(lon_knn_ti_pca, file = "lon_knn_ti_pca.rda")
save(lon_knn2_ti_pca, file = "lon_knn2_ti_pca.rda")
save(lon_svmr2_ti_pca, file = "lon_svmr2_ti_pca.rda")
save(lon_rf_ti_pca, file = "lon_rf_ti_pca.rda")
save(lon_gbm_ti_pca, file = "lon_gbm_ti_pca.rda")

#longitude of TI knn
load("lon_knn_ti_pca.rda")
predictions_lon_knn_ti_pca <-predict(lon_knn_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lon_knn_ti_pca, wifilocation_validation_grouped_TI_pca$LONGITUDE) 
#RMSE: 6.33, R:0.94, MAE=4.27
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_knn_ti_pca,y=LATITUDE,colour="red"))
#OK, maybe 1 or 2 points out of the building
plot(predictions_lon_knn_ti_pca, wifilocation_validation_grouped_TI_pca$LONGITUDE)
abline(a=0,b=1,col="red")
plot(density(wifilocation_validation_grouped_TI_pca$LONGITUDE-predictions_lon_knn_ti_pca))
 #ok, one small peak before -10
load("lon_knn2_ti_pca.rda")
predictions_lon_knn2_ti_pca <-predict(lon_knn2_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lon_knn2_ti_pca, wifilocation_validation_grouped_TI_pca$LONGITUDE) 
#RMSE: 6.55, R:0.94, MAE=4.30

#longitude of TI svmRadial
load("lon_svmr2_ti_pca.rda")
predictions_lon_svmr2_ti_pca <-predict(lon_svmr2_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lon_svmr2_ti_pca, wifilocation_validation_grouped_TI_pca$LONGITUDE) 
#RMSE: 8.63, R:0.90, MAE=6.10
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error svmr")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_svmr2_ti_pca,y=LATITUDE,colour="red"))
#some out, less overlay the knn
plot(density(wifilocation_validation_grouped_TI_pca$LONGITUDE-predictions_lon_svmr2_ti_pca))
#wider than knn

#longitude of TI rf
load("lon_rf_ti_pca.rda")
predictions_lon_rf_ti_pca <-predict(lon_rf_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lon_rf_ti_pca, wifilocation_validation_grouped_TI_pca$LONGITUDE) 
#RMSE: 7.51, R:0.92, MAE=5.10
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error rf")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_rf_ti_pca,y=LATITUDE,colour="red"))
#good overlay, some places out
plot(density(wifilocation_validation_grouped_TI_pca$LONGITUDE-predictions_lon_rf_ti_pca))

#longitude of TI gbm
load("lon_gbm_ti_pca.rda")
predictions_lon_gbm_ti_pca <-predict(lon_gbm_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lon_gbm_ti_pca, wifilocation_validation_grouped_TI_pca$LONGITUDE) 
#RMSE: 7.62, R:0.92, MAE=5.11
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error gbm")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_gbm_ti_pca,y=LATITUDE,colour="red"))
#good overlay, some places out
plot(density(wifilocation_validation_grouped_TI_pca$LONGITUDE-predictions_lon_gbm_ti_pca))
lines(density(wifilocation_validation_grouped_TI_pca$LONGITUDE-predictions_lon_knn_ti_pca),col="red")
lines(density(wifilocation_validation_grouped_TI_pca$LONGITUDE-predictions_lon_rf_ti_pca),col="blue")
lines(density(wifilocation_validation_grouped_TI_pca$LONGITUDE-predictions_lon_svmr2_ti_pca),col="green")

##Add predicted to valdation set and visualize
wifilocation_validation_grouped_TI$LONGITUDE_pred <- predictions_lon_knn_ti_pca
#location
wifilocation_validation_grouped_TI$LONGITUDE_error <- (wifilocation_validation_grouped_TI$LONGITUDE_pred - wifilocation_validation_grouped_TI$LONGITUDE)
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE,color=LONGITUDE_error)) +
  ggtitle("Errors in Longitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient2(high="red", low="blue",mid="white")+
  theme_dark()+
  facet_wrap("FLOOR")


#-----Latitude TI-------------
#PCA
set.seed(123)
lat_knn_ti_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TI_pca, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lat_knn_ti_pca #RMSE=4.81, R=0.98, MAE=3.56
set.seed(123)
lat_knn2_ti_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TI_pca, method = "knn", 
                        preProc = c("center","scale"), 
                        tuneGrid = expand.grid(k=c(1:5)),
                        trControl = fitControl)
lat_knn2_ti_pca #Chooses k=5, the same model as before.
set.seed(123)
lat_rf_ti_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TI_pca, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lat_rf_ti_pca #RMSE=4.78, R=0.98, MAE=3.33
#all
set.seed(123)
lat_knn_ti_all <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TI, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lat_knn_ti_all #RMSE=6.22, R=0.97, MAE=4.34
set.seed(123)
lat_rf_ti_all <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TI, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lat_rf_ti_all #RMSE=5.60, R=0.97, MAE=3.86
#only WAPS located at TI
set.seed(123)
lat_knn_ti_waps <- train(LATITUDE ~ .-LONGITUDE, 
                         data = wifilocation_grouped_TI_waps, method = "knn", 
                         preProc = c("center","scale"), tuneLength = 10,
                         trControl = fitControl)
lat_knn_ti_waps #RMSE=6.25, R=0.97, MAE=4.36
set.seed(123)
lat_rf_ti_waps<- train(LATITUDE ~ .-LONGITUDE, 
                       data = wifilocation_grouped_TI_waps, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lat_rf_ti_waps #RMSE=5.63, R=0.97, MAE=3.90

#Tune using PCA
set.seed(123)
lat_gbm_ti_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TI_pca, method = "gbm", 
                        tuneLength = 10,
                        trControl = fitControl)
lat_gbm_ti_pca #RMSE=4.79, R=0.98, MAE=3.63
set.seed(123)
lat_svmr_ti_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TI_pca, method = "svmRadial", 
                         tuneLength = 10,preProcess = c("center", "scale"),
                         trControl = fitControl)
lat_svmr_ti_pca #RMSE=4.96, R=0.98, MAE=3.76
#tune svmr
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1),
                           C = c(2,4,8,16,32))
set.seed(123)
lat_svmr2_ti_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                          data = wifilocation_grouped_TI_pca, method = "svmRadial", 
                          tuneGrid = grid_radial,preProcess = c("center", "scale"),
                          trControl = fitControl)
lat_svmr2_ti_pca #RMSE=4.92, R=0.98, MAE=3.72

#save models to test
save(lat_knn_ti_pca, file = "lat_knn_ti_pca.rda")
save(lat_svmr2_ti_pca, file = "lat_svmr2_ti_pca.rda")
save(lat_rf_ti_pca, file = "lat_rf_ti_pca.rda")
save(lat_gbm_ti_pca, file = "lat_gbm_ti_pca.rda")

#latitude of TI knn
load("lat_knn_ti_pca.rda")
predictions_lat_knn_ti_pca <-predict(lat_knn_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lat_knn_ti_pca, wifilocation_validation_grouped_TI_pca$LATITUDE) 
#RMSE: 5.50, R:0.97, MAE=3.90
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_knn_ti_pca,x=LONGITUDE,colour="red"))
#Some out, but others overlay well
plot(predictions_lat_knn_ti_pca, wifilocation_validation_grouped_TI_pca$LATITUDE)
abline(a=0,b=1,col="red")
plot(density(wifilocation_validation_grouped_TI_pca$LATITUDE-predictions_lat_knn_ti_pca))

#latitude of TI svmRadial
load("lat_svmr2_ti_pca.rda")
predictions_lat_svmr2_ti_pca <-predict(lat_svmr2_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lat_svmr2_ti_pca, wifilocation_validation_grouped_TI_pca$LATITUDE) 
#RMSE: 6.99, R:0.95, MAE=5.12
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error svmr")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_svmr2_ti_pca,x=LONGITUDE,colour="red"))
#Lower values out of range
plot(density(wifilocation_validation_grouped_TI_pca$LATITUDE-predictions_lat_svmr2_ti_pca))
#wider than knn

#latitude of TI rf
load("lat_rf_ti_pca.rda")
predictions_lat_rf_ti_pca <-predict(lat_rf_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lat_rf_ti_pca, wifilocation_validation_grouped_TI_pca$LATITUDE) 
#RMSE: 6.54, R:0.96, MAE=4.32
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error rf")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_rf_ti_pca,x=LONGITUDE,colour="red"))
#good overlay in general, but some places out of the building
plot(density(wifilocation_validation_grouped_TI_pca$LATITUDE-predictions_lat_rf_ti_pca))
lines(density(wifilocation_validation_grouped_TI_pca$LATITUDE-predictions_lat_knn_ti_pca),col="red")
#very similar error distribution

#longitude of TI gbm
load("lat_gbm_ti_pca.rda")
predictions_lat_gbm_ti_pca <-predict(lat_gbm_ti_pca, wifilocation_validation_grouped_TI_pca)
postResample(predictions_lat_gbm_ti_pca, wifilocation_validation_grouped_TI_pca$LATITUDE) 
#RMSE: 6.25, R:0.96, MAE=4.30
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error gbm")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_gbm_ti_pca,x=LONGITUDE,colour="red"))
#good overlay, several places out
plot(density(wifilocation_validation_grouped_TI_pca$LATITUDE-predictions_lat_gbm_ti_pca))
lines(density(wifilocation_validation_grouped_TI_pca$LATITUDE-predictions_lat_knn_ti_pca),col="red")
 #similar to knn

##Add predicted to valdation set and visualize
wifilocation_validation_grouped_TI$LATITUDE_pred <- predictions_lat_knn_ti_pca
#location
wifilocation_validation_grouped_TI$LATITUDE_error <- (wifilocation_validation_grouped_TI$LATITUDE_pred - wifilocation_validation_grouped_TI$LATITUDE)
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE,color=LATITUDE_error)) +
  ggtitle("Errors in Latitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient2(high="red",mid="white" ,low="blue")+
  theme_dark()+
  facet_wrap("FLOOR")

#See latitude and longitude errors together
wifilocation_validation_grouped_TI$LL_error <- sqrt(((abs(wifilocation_validation_grouped_TI$LONGITUDE_error))^ 2)+((abs(wifilocation_validation_grouped_TI$LATITUDE_error))^ 2))
summary(wifilocation_validation_grouped_TI$LL_error)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.1225  2.7817  5.1636  6.4973  8.6718 39.6908
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE,color=LL_error)) +
  ggtitle("Errors in Longitude and Latitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient(high="red",low="blue")+
  theme_dark()+
  facet_wrap("FLOOR")

#---------------------------------Longitude and Latitude TD ----------------------------------------------------------
#Subset by building, no need to take into account the floor because is not related with longitude/latitude.
#For each building try, all WAPS, PCA and WAPS detected on that building.

#-----Longitude TD-------------
#PCA
set.seed(123)
lon_knn_td_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD_pca, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lon_knn_td_pca #RMSE=9.47, R=0.96, MAE=7.02
set.seed(123)
lon_rf_td_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TD_pca, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lon_rf_td_pca #RMSE=8.85, R=0.97, MAE=6.50
#all
set.seed(123)
lon_knn_td_all <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lon_knn_td_all #RMSE=8.98, R=0.97, MAE=6.59
set.seed(123)
lon_rf_td_all <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TD, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lon_rf_td_all #RMSE=9.16, R=0.97, MAE=6.81
#only WAPS located at TD
wifilocation_grouped_TD_waps <- data.frame(wifilocation_grouped_TD[,WAPS_loc_TD],wifilocation_grouped_TD[,"LONGITUDE"],
                                           wifilocation_grouped_TD[,"LATITUDE"])
set.seed(123)
lon_knn_td_waps <- train(LONGITUDE ~ .-LATITUDE, 
                         data = wifilocation_grouped_TD_waps, method = "knn", 
                         preProc = c("center","scale"), tuneLength = 10,
                         trControl = fitControl)
lon_knn_td_waps #RMSE=9.48, R=0.96, MAE=6.80
set.seed(123)
lon_rf_td_waps<- train(LONGITUDE ~ .-LATITUDE, 
                       data = wifilocation_grouped_TD_waps, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lon_rf_td_waps #RMSE=9.18, R=0.97, MAE=6.92

#Tune using PCA
set.seed(123)
lon_gbm_td_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD_pca, method = "gbm", 
                        tuneLength = 10,
                        trControl = fitControl)
lon_gbm_td_pca #RMSE=9.13 R=0.97, MAE=6.78
set.seed(123)
lon_svmr_td_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TD_pca, method = "svmRadial", 
                         tuneLength = 10,preProcess = c("center", "scale"),
                         trControl = fitControl)
lon_svmr_td_pca #RMSE=9.70, R=0.96, MAE=7.12
#tune svmr
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1),
                           C = c(1,2,4,8,16,32))
set.seed(123)
lon_svmr2_td_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                          data = wifilocation_grouped_TD_pca, method = "svmRadial", 
                          tuneGrid = grid_radial,preProcess = c("center", "scale"),
                          trControl = fitControl)
lon_svmr2_td_pca #RMSE=9.68, R=0.96, MAE=7.09
#tune RF with different number trees
#model_RF_lon_TD <- list() #create a list of models
#for (ntree in c(10, 20, 50, 100, 500)) {  #for each number of trees
#  set.seed(123)
#  fit <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
#               data = wifilocation_grouped_TD_pca, method = "rf", 
#               ntree=ntree, tuneLength = 10,
#               trControl = fitControl)
#  key <- toString(ntree) #transform number of trees to string
#  model_RF_lon_TD[[key]] <- fit #name each model and store it a variable of model_RF
#}
#compare results
#model_RF_lon_TD[["10"]] #RMSE:9.18, R:0.96, MAE:6.71
#model_RF_lon_TD[["20"]] #RMSE:8.92, R:0.97, MAE:6.56
#model_RF_lon_TD[["50"]] #RMSE:8.85, R:0.97, MAE:6.50
#model_RF_lon_TD[["100"]] #RMSE:8.87, R:0.97, MAE:6.50
#model_RF_lon_TD[["500"]] #RMSE:8.89, R:0.97, MAE:6.52
 ##50 trees is the best,

#save models to test
save(lon_knn_td_pca, file = "lon_knn_td_pca.rda")
save(lon_svmr2_td_pca, file = "lon_svmr2_td_pca.rda")
save(lon_rf_td_pca, file = "lon_rf_td_pca.rda")
save(lon_gbm_td_pca, file = "lon_gbm_td_pca.rda")

#longitude of TI knn
load("lon_knn_td_pca.rda")
predictions_lon_knn_td_pca <-predict(lon_knn_td_pca, wifilocation_validation_grouped_TD_pca)
postResample(predictions_lon_knn_td_pca, wifilocation_validation_grouped_TD_pca$LONGITUDE) 
#RMSE:10.20, R:0.95, MAE=6.93
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_knn_td_pca,y=LATITUDE,colour="red"))
#OK, maybe 1 or 2 points out of the building
plot(predictions_lon_knn_td_pca, wifilocation_validation_grouped_TD_pca$LONGITUDE)
abline(a=0,b=1,col="red")
plot(density(wifilocation_validation_grouped_TD_pca$LONGITUDE-predictions_lon_knn_td_pca))

#longitude of TD svmRadial
load("lon_svmr2_td_pca.rda")
predictions_lon_svmr2_td_pca <-predict(lon_svmr2_td_pca, wifilocation_validation_grouped_TD_pca)
postResample(predictions_lon_svmr2_td_pca, wifilocation_validation_grouped_TD_pca$LONGITUDE) 
#RMSE: 12.03, R:0.93, MAE=9.18
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error svmr")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_svmr2_td_pca,y=LATITUDE,colour="red"))
#some out, less overlay the knn
plot(density(wifilocation_validation_grouped_TD_pca$LONGITUDE-predictions_lon_svmr2_td_pca))

#longitude of TD rf
load("lon_rf_td_pca.rda")
predictions_lon_rf_td_pca <-predict(lon_rf_td_pca, wifilocation_validation_grouped_TD_pca)
postResample(predictions_lon_rf_td_pca, wifilocation_validation_grouped_TD_pca$LONGITUDE) 
#RMSE: 8.98, R:0.96, MAE=6.44
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error rf")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_rf_td_pca,y=LATITUDE,colour="red"))
#good overlay, some places out
plot(density(wifilocation_validation_grouped_TD_pca$LONGITUDE-predictions_lon_rf_td_pca))
lines(density(wifilocation_validation_grouped_TD_pca$LONGITUDE-predictions_lon_knn_td_pca),col="red")

#longitude of TD gbm
load("lon_gbm_td_pca.rda")
predictions_lon_gbm_td_pca <-predict(lon_gbm_td_pca, wifilocation_validation_grouped_TD_pca)
postResample(predictions_lon_gbm_td_pca, wifilocation_validation_grouped_TD_pca$LONGITUDE) 
#RMSE: 9.20, R:0.96, MAE=6.57
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error gbm")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_gbm_td_pca,y=LATITUDE,colour="red"))
#good overlay, some places out
plot(density(wifilocation_validation_grouped_TD_pca$LONGITUDE-predictions_lon_gbm_td_pca))
lines(density(wifilocation_validation_grouped_TD_pca$LONGITUDE-predictions_lon_rf_td_pca),col="red")

##Add predicted to valdation set and visualize
wifilocation_validation_grouped_TD$LONGITUDE_pred <- predictions_lon_rf_td_pca
#location
wifilocation_validation_grouped_TD$LONGITUDE_error <- (wifilocation_validation_grouped_TD$LONGITUDE_pred - wifilocation_validation_grouped_TD$LONGITUDE)
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE,color=LONGITUDE_error)) +
  ggtitle("Errors in Longitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient2(high="red", low="blue",mid="white")+
  theme_dark()+
  facet_wrap("FLOOR")

#-----Latitude TD-------------
#PCA
set.seed(123)
lat_knn_td_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD_pca, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lat_knn_td_pca #RMSE=8.22, R=0.95, MAE=6.00 , k=5
set.seed(123)
lat_knn2_td_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD_pca, method = "knn", 
                        preProc = c("center","scale"), tuneGrid = expand.grid(k=c(1:5)),
                        trControl = fitControl)
lat_knn2_td_pca #RMSE=8.11, R=0.95, MAE=5.75 , k=5
set.seed(123)
lat_rf_td_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TD_pca, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lat_rf_td_pca #RMSE=8.14, R=0.95, MAE=6.02
#tune RF with different number trees
model_RF_lat_TD <- list() #create a list of models
for (ntree in c(10, 20, 50, 100, 500)) {  #for each number of trees
  set.seed(123)
  fit <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
               data = wifilocation_grouped_TD_pca, method = "rf", 
               ntree=ntree, tuneLength = 10,
               trControl = fitControl)
  key <- toString(ntree) #transform number of trees to string
  model_RF_lon_TD[[key]] <- fit #name each model and store it a variable of model_RF
}
#compare results
model_RF_lon_TD[["10"]] #RMSE:8.66, R:0.94, MAE:6.42
model_RF_lon_TD[["20"]] #RMSE:8.30, R:0.95, MAE:6.15
model_RF_lon_TD[["50"]] #RMSE:8.14, R:0.95, MAE:6.02
model_RF_lon_TD[["100"]] #RMSE:8.07, R:0.95, MAE:5.95
model_RF_lon_TD[["500"]] #RMSE:8.08, R:0.95, MAE:5.94
##100 trees is the best RMSE, 500 the best MAE, use 100 trees, no much difference
lat_rf2_td_pca <- model_RF_lon_TD[["100"]]

#all
set.seed(123)
lat_knn_td_all <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lat_knn_td_all #RMSE=8.48, R=0.95, MAE=6.33 k=5
set.seed(123)
lat_knn2_td_all <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD, method = "knn", 
                        preProc = c("center","scale"), tuneGrid=expand.grid(k=c(1:5)),
                        trControl = fitControl)
lat_knn2_td_all #RMSE=8.38, R=0.95, MAE=6.09 k=3
set.seed(123)
lat_rf_td_all <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TD, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lat_rf_td_all #RMSE=8.03, R=0.95, MAE=6.08
#only WAPS located at TI
set.seed(123)
lat_knn_td_waps <- train(LATITUDE ~ .-LONGITUDE, 
                         data = wifilocation_grouped_TD_waps, method = "knn", 
                         preProc = c("center","scale"), tuneLength = 10,
                         trControl = fitControl)
lat_knn_td_waps #RMSE=8.22, R=0.95, MAE=6.15 k=5
set.seed(123)
lat_knn2_td_waps <- train(LATITUDE ~ .-LONGITUDE, 
                         data = wifilocation_grouped_TD_waps, method = "knn", 
                         preProc = c("center","scale"), tuneGrid=expand.grid(k=c(1:5)),
                         trControl = fitControl)
lat_knn2_td_waps #RMSE=8.02, R=0.95, MAE=5.80 k=3
set.seed(123)
lat_rf_td_waps<- train(LATITUDE ~ .-LONGITUDE, 
                       data = wifilocation_grouped_TD_waps, method = "rf", 
                       ntree=50, tuneLength = 10,
                       trControl = fitControl)
lat_rf_td_waps #RMSE=8.07, R=0.95, MAE=6.19

#Tune using PCA
set.seed(123)
lat_gbm_td_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD_pca, method = "gbm", 
                        tuneLength = 10,
                        trControl = fitControl)
lat_gbm_td_pca #RMSE=7.82 R=0.95, MAE=5.90
set.seed(123)
lat_gbm2_td_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TD_pca, method = "gbm", 
                        tuneGrid = expand.grid(n.trees=seq(from=50,to=500,by=50),
                                               interaction.depth = c(1,4,6,7,8,9,10),
                                               shrinkage=c(0,0.05,0.1,0.2,0.3),
                                               n.minobsinnode=c(5,10,15,20)),
                        trControl = fitControl)
lat_gbm2_td_pca #RMSE=7.65 R=0.96, MAE=5.70

set.seed(123)
lat_svmr_td_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TD_pca, method = "svmRadial", 
                         tuneLength = 10,preProcess = c("center", "scale"),
                         trControl = fitControl)
lat_svmr_td_pca #RMSE=8.17, R=0.95, MAE=6.03
#tune svmr
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1),
                           C = c(1,2,3,4,8,16,32))
set.seed(123)
lat_svmr2_td_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                          data = wifilocation_grouped_TD_pca, method = "svmRadial", 
                          tuneGrid = grid_radial,preProcess = c("center", "scale"),
                          trControl = fitControl)
lat_svmr2_td_pca #RMSE=8.10, R=0.95, MAE=5.99

#save models to test
save(lat_knn2_td_pca, file = "lat_knn2_td_pca.rda")
save(lat_svmr2_td_pca, file = "lat_svmr2_td_pca.rda")
save(lat_rf2_td_pca, file = "lat_rf2_td_pca.rda")
save(lat_gbm2_td_pca, file = "lat_gbm2_td_pca.rda")

#latitude of TD knn
load("lat_knn2_td_pca.rda")
predictions_lat_knn2_td_pca <-predict(lat_knn2_td_pca, wifilocation_validation_grouped_TD_pca)
postResample(predictions_lat_knn2_td_pca, wifilocation_validation_grouped_TD_pca$LATITUDE) 
#RMSE:11.12, R:0.90, MAE=7.29
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_knn2_td_pca,x=LONGITUDE,colour="red"))
#A lot out of the building
plot(density(wifilocation_validation_grouped_TD_pca$LATITUDE-predictions_lat_knn2_td_pca))
#small peak at -20

#latitude of TD svmRadial
load("lat_svmr2_td_pca.rda")
predictions_lat_svmr2_td_pca <-predict(lat_svmr2_td_pca, wifilocation_validation_grouped_TD_pca)
postResample(predictions_lat_svmr2_td_pca, wifilocation_validation_grouped_TD_pca$LATITUDE) 
#RMSE: 10.95, R:0.91, MAE=8.18
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error svmr")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_svmr2_td_pca,x=LONGITUDE,colour="red"))
#some out,
plot(density(wifilocation_validation_grouped_TD_pca$LATITUDE-predictions_lat_svmr2_td_pca))
#wide peak

#latitude of TD rf
load("lat_rf2_td_pca.rda")
predictions_lat_rf2_td_pca <-predict(lat_rf2_td_pca, wifilocation_validation_grouped_TD_pca)
postResample(predictions_lat_rf2_td_pca, wifilocation_validation_grouped_TD_pca$LATITUDE) 
#RMSE: 10.97 R:0.91, MAE=8.01
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error rf")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_rf2_td_pca,x=LONGITUDE,colour="red"))
#good overlay, some places out
plot(density(wifilocation_validation_grouped_TD_pca$LATITUDE-predictions_lat_knn2_td_pca))
lines(density(wifilocation_validation_grouped_TD_pca$LATITUDE-predictions_lat_rf2_td_pca),col="red")

#latitude of TD gbm
load("lat_gbm2_td_pca.rda")
predictions_lat_gbm2_td_pca <-predict(lat_gbm2_td_pca, wifilocation_validation_grouped_TD_pca)
postResample(predictions_lat_gbm2_td_pca, wifilocation_validation_grouped_TD_pca$LATITUDE) 
#RMSE: 10.68, R:0.91, MAE=7.87
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error gbm")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_gbm2_td_pca,x=LONGITUDE,colour="red"))
#good overlay, some places out
plot(predictions_lat_gbm2_td_pca, wifilocation_validation_grouped_TD_pca$LATITUDE)
abline(a=0,b=1,col="red")
plot(density(wifilocation_validation_grouped_TD_pca$LATITUDE-predictions_lat_knn2_td_pca))
lines(density(wifilocation_validation_grouped_TD_pca$LATITUDE-predictions_lat_gbm2_td_pca),col="red")

##See predicted longitude and latitude for TD
#add to validation
wifilocation_validation_grouped_TD$LONGITUDE_pre <- predictions_lon_rf_td_pca
wifilocation_validation_grouped_TD$LATITUDE_pre_knn <- predictions_lat_knn2_td_pca
wifilocation_validation_grouped_TD$LATITUDE_pre_gbm <- predictions_lat_gbm2_td_pca
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Predicted")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=LATITUDE_pre_knn,x=LONGITUDE_pre,colour="red"))+
  geom_point(aes(y=LATITUDE_pre_gbm,x=LONGITUDE_pre,colour="blue"))+
  facet_wrap("FLOOR")+
  theme_dark()
#no much difference between latitude knn and gbm

##Add predicted to valdation set and visualize, use gbm2
wifilocation_validation_grouped_TD$LATITUDE_pred <- predictions_lat_gbm2_td_pca
#location
wifilocation_validation_grouped_TD$LATITUDE_error <- (wifilocation_validation_grouped_TD$LATITUDE_pred - wifilocation_validation_grouped_TD$LATITUDE)
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE,color=LATITUDE_error)) +
  ggtitle("Errors in Latitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient2(high="red", low="blue",mid="white")+
  theme_dark()+
  facet_wrap("FLOOR")

#See latitude and longitude errors together
wifilocation_validation_grouped_TD$LL_error <- sqrt(((abs(wifilocation_validation_grouped_TD$LONGITUDE_error))^ 2)+((abs(wifilocation_validation_grouped_TD$LATITUDE_error))^ 2))
summary(wifilocation_validation_grouped_TD$LL_error)
#Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#0.4246  4.8631  8.8601 11.1061 14.9653 47.1513 
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE,color=LL_error)) +
  ggtitle("Errors in Longitude and Latitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient(high="red",low="blue")+
  theme_dark()+
  facet_wrap("FLOOR")

#---------------------------------Longitude and Latitude TC ----------------------------------------------------------
#Subset by building, no need to take into account the floor because is not related with longitude/latitude.
#For each building try, all WAPS, PCA and WAPS detected on that building.

#-----Longitude TC-------------
#PCA
set.seed(123)
lon_knn_tc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TC_pca, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lon_knn_tc_pca #RMSE=8.16, R=0.93, MAE=5.53 k=5
set.seed(123)
lon_knn2_tc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TC_pca, method = "knn", 
                        preProc = c("center","scale"), tuneGrid = expand.grid(k=c(1:5)),
                        trControl = fitControl)
lon_knn2_tc_pca #RMSE=7.66, R=0.93, MAE=5.09 k=3
set.seed(123)
lon_knn3_tc_pca <- train(LONGITUDE ~ .-USERID-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TC_pca, method = "knn", 
                         preProc = c("center","scale"), tuneGrid = expand.grid(k=c(1:5)),
                         trControl = fitControl)
lon_knn3_tc_pca #RMSE=7.62, R=0.93, MAE=5.04 k=3

model_RF_lon_TC <- list() #create a list of models
for (ntree in c(10, 20, 50, 100, 500)) {  #for each number of trees
  set.seed(123)
  fit <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
               data = wifilocation_grouped_TC_pca, method = "rf", 
               ntree=ntree, tuneLength = 10,
               trControl = fitControl)
  key <- toString(ntree) #transform number of trees to string
  model_RF_lon_TC[[key]] <- fit #name each model and store it a variable of model_RF
}
#compare results
model_RF_lon_TC[["10"]] #RMSE:11.25, R:0.86, MAE:7.67
model_RF_lon_TC[["20"]] #RMSE:10.94, R:0.87, MAE:7.34
model_RF_lon_TC[["50"]] #RMSE:10.72, R:0.88, MAE:7.21
model_RF_lon_TC[["100"]] #RMSE:10.68, R:0.88, MAE:7.20
model_RF_lon_TC[["500"]] #RMSE:10.64, R:0.88, MAE:7.13
##500 trees is the best
lon_rf_tc_pca <- model_RF_lon_TC[["500"]]

#all
set.seed(123)
lon_knn_tc_all <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TC, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lon_knn_tc_all #RMSE=8.38, R=0.92, MAE=5.68 k=5
set.seed(123)
lon_knn2_tc_all <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TC, method = "knn", 
                        preProc = c("center","scale"), tuneGrid = expand.grid(k=c(1:5)),
                        trControl = fitControl)
lon_knn2_tc_all #RMSE=7.79, R=0.93, MAE=5.00 k=1
set.seed(123)
lon_rf_tc_all <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TC, method = "rf", 
                       ntree=500, tuneLength = 10,
                       trControl = fitControl)
lon_rf_tc_all #RMSE=8.86, R=0.92, MAE=5.96
#only WAPS located at TC
wifilocation_grouped_TC_waps <- data.frame(wifilocation_grouped_TC[,WAPS_loc_TC],wifilocation_grouped_TC[,"LONGITUDE"],
                                           wifilocation_grouped_TC[,"LATITUDE"])
set.seed(123)
lon_knn_tc_waps <- train(LONGITUDE ~ .-LATITUDE, 
                         data = wifilocation_grouped_TC_waps, method = "knn", 
                         preProc = c("center","scale"), tuneLength = 10,
                         trControl = fitControl)
lon_knn_tc_waps #RMSE=8.45, R=0.92, MAE=5.5 k=5
set.seed(123)
lon_knn2_tc_waps <- train(LONGITUDE ~ .-LATITUDE, 
                         data = wifilocation_grouped_TC_waps, method = "knn", 
                         preProc = c("center","scale"), tuneGrid=expand.grid(k=c(1:5)),
                         trControl = fitControl)
lon_knn2_tc_waps #RMSE=7.96, R=0.93, MAE=5.00 k=2
set.seed(123)
lon_rf_tc_waps<- train(LONGITUDE ~ .-LATITUDE, 
                       data = wifilocation_grouped_TC_waps, method = "rf", 
                       ntree=100, tuneLength = 10,
                       trControl = fitControl)
lon_rf_tc_waps #RMSE=8.93, R=0.91, MAE=5.96
set.seed(123)
lon_svmr_tc_waps <- train(LONGITUDE ~ .-LATITUDE, 
                         data = wifilocation_grouped_TC_waps, method = "svmRadial", 
                         tuneLength = 10,preProcess = c("center", "scale"),
                         trControl = fitControl)
lon_svmr_tc_waps #RMSE=10.01, R=0.89, MAE=7.11
#tune svmr
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1),
                           C = c(1,2,4,8,16,32))
set.seed(123)
lon_svmr2_tc_waps <- train(LONGITUDE ~ .-LATITUDE, 
                          data = wifilocation_grouped_TC_waps, method = "svmRadial", 
                          tuneGrid = grid_radial,preProcess = c("center", "scale"),
                          trControl = fitControl)
lon_svmr2_tc_waps #RMSE=10.01, R=0.89, MAE=7.11

#Tune using PCA
set.seed(123)
lon_gbm_tc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TC_pca, method = "gbm", 
                        tuneLength = 10,
                        trControl = fitControl)
lon_gbm_tc_pca #RMSE=10.46 R=0.88, MAE=7.23
set.seed(123)
lon_svmr_tc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TC_pca, method = "svmRadial", 
                         tuneLength = 10,preProcess = c("center", "scale"),
                         trControl = fitControl)
lon_svmr_tc_pca #RMSE=9.67, R=0.90, MAE=6.76
#tune svmr
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1),
                           C = c(1,2,4,8,16,32))
set.seed(123)
lon_svmr2_tc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                          data = wifilocation_grouped_TC_pca, method = "svmRadial", 
                          tuneGrid = grid_radial,preProcess = c("center", "scale"),
                          trControl = fitControl)
lon_svmr2_tc_pca #RMSE=9.53, R=0.90, MAE=6.76

#save models to test
save(lon_knn2_tc_pca, file = "lon_knn2_tc_pca.rda")
save(lon_knn3_tc_pca, file = "lon_knn3_tc_pca.rda")
save(lon_svmr2_tc_pca, file = "lon_svmr2_tc_pca.rda")
save(lon_rf_tc_pca, file = "lon_rf_tc_pca.rda")
save(lon_gbm_tc_pca, file = "lon_gbm_tc_pca.rda")

#longitude of TC knn
load("lon_knn2_tc_pca.rda")
predictions_lon_knn2_tc_pca <-predict(lon_knn2_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lon_knn2_tc_pca, wifilocation_validation_grouped_TC_pca$LONGITUDE) 
#RMSE:13.68, R:0.81, MAE=8.84
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_knn2_tc_pca,y=LATITUDE,colour="red"))+
  facet_wrap("FLOOR")
#OK, several in the middle, lots errors at the 4th floor
#longitude of TC knn using floor
load("lon_knn3_tc_pca.rda")
predictions_lon_knn3_tc_pca <-predict(lon_knn3_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lon_knn3_tc_pca, wifilocation_validation_grouped_TC_pca$LONGITUDE) 
#RMSE:12.93, R:0.84, MAE=8.52
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_knn3_tc_pca,y=LATITUDE,colour="red"))+
  facet_wrap("FLOOR")
#OK, better, still errors at the 4th floor.
#longitude of TC knn using floor, metric using the predicted floor
wifilocation_validation_grouped_TC_pca$FLOOR <- predictions_floor_rf_tc_max
predictions_lon_knn3_tc_pca_p <-predict(lon_knn3_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lon_knn3_tc_pca, wifilocation_validation_grouped_TC_pca$LONGITUDE) 
#RMSE:12.93, R:0.84, MAE=8.52
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_knn3_tc_pca_p,y=LATITUDE,colour="red"))+
  facet_wrap("FLOOR")

#longitude of TC svmRadial
load("lon_svmr2_tc_pca.rda")
predictions_lon_svmr2_tc_pca <-predict(lon_svmr2_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lon_svmr2_tc_pca, wifilocation_validation_grouped_TC_pca$LONGITUDE) 
#RMSE: 13.75, R:0.81, MAE=10.39
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error svmr")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_svmr2_tc_pca,y=LATITUDE,colour="red"))+
  facet_wrap("FLOOR")
#a lot in the middle, errors at 2nd and 4th floor

#longitude of TC rf
load("lon_rf_tc_pca.rda")
predictions_lon_rf_tc_pca <-predict(lon_rf_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lon_rf_tc_pca, wifilocation_validation_grouped_TC_pca$LONGITUDE) 
#RMSE: 13.19, R:0.83, MAE=9.33
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error rf")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_rf_tc_pca,y=LATITUDE,colour="red"))+
  facet_wrap("FLOOR")
#a lot in the middle

#longitude of TD gbm
load("lon_gbm_tc_pca.rda")
predictions_lon_gbm_tc_pca <-predict(lon_gbm_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lon_gbm_tc_pca, wifilocation_validation_grouped_TC_pca$LONGITUDE) 
#RMSE: 13.23, R:0.82, MAE=10.01
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error gbm")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_gbm_tc_pca,y=LATITUDE,colour="red"))+
  facet_wrap("FLOOR")
#good overlay, some places out

#-----Latitude TC-------------
#PCA
set.seed(123)
lat_knn_tc_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TC_pca, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lat_knn_tc_pca #RMSE=87.23, R=0.93, MAE=4.74 k=5
set.seed(123)
lat_knn2_tc_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TC_pca, method = "knn", 
                         preProc = c("center","scale"), tuneGrid = expand.grid(k=c(1:5)),
                         trControl = fitControl)
lat_knn2_tc_pca #RMSE=6.83, R=0.94, MAE=4.34 k=2
set.seed(123) #use floor
lat_knn3_tc_pca <- train(LATITUDE ~ .-USERID-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TC_pca, method = "knn", 
                         preProc = c("center","scale"), tuneGrid = expand.grid(k=c(1:5)),
                         trControl = fitControl)
lat_knn3_tc_pca #RMSE=6.05, R=0.95, MAE=4.17 k=2

model_RF_lat_TC <- list() #create a list of models
for (ntree in c(10, 20, 50, 100, 500)) {  #for each number of trees
  set.seed(123)
  fit <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
               data = wifilocation_grouped_TC_pca, method = "rf", 
               ntree=ntree, tuneLength = 10,
               trControl = fitControl)
  key <- toString(ntree) #transform number of trees to string
  model_RF_lat_TC[[key]] <- fit #name each model and store it a variable of model_RF
}
#compare results
model_RF_lat_TC[["10"]] #RMSE:8.48, R:0.90, MAE:5.69
model_RF_lat_TC[["20"]] #RMSE:8.17, R:0.91, MAE:5.57
model_RF_lat_TC[["50"]] #RMSE:7.95, R:0.92, MAE:5.35
model_RF_lat_TC[["100"]] #RMSE:7.89, R:0.92, MAE:5.31
model_RF_lat_TC[["500"]] #RMSE:7.84, R:0.92, MAE:5.27
##500 trees is the best
lat_rf_tc_pca <- model_RF_lat_TC[["500"]]

set.seed(123)
lat_rf2_tc_all <- train(LATITUDE ~ .-USERID-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TC_pca, method = "rf", 
                       ntree=500, tuneLength = 10,
                       trControl = fitControl)
lat_rf2_tc_pca <- lat_rf2_tc_all
lat_rf2_tc_pca #RMSE=7.86 , R=0.92 , MAE=5.32, better without floor 

set.seed(123)
lat_gbm_tc_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TC_pca, method = "gbm", 
                        tuneLength = 10,
                        trControl = fitControl)
lat_gbm_tc_pca #RMSE=8.26 R=0.91, MAE=5.69
set.seed(123)
lat_gbm2_tc_pca <- train(LATITUDE ~ .-USERID-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TC_pca, method = "gbm", 
                        tuneLength = 10,
                        trControl = fitControl)
lat_gbm2_tc_pca #RMSE=8.17 R=0.91, MAE=5.66
#set.seed(123)
#lat_svmr_tc_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
#                         data = wifilocation_grouped_TC_pca, method = "svmRadial", 
#                         tuneLength = 10,preProcess = c("center", "scale"),
#                         trControl = fitControl)
#lat_svmr_tc_pca #RMSE=7.95 , R=0.92 , MAE= 5.60
#tune svmr
#grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.03, 0.04,0.05, 0.06, 0.07,0.08, 0.09, 0.1),
#                           C = c(1,2,4,8,16,32))
#set.seed(123)
#lat_svmr2_tc_pca <- train(LATITUDE ~ .-USERID-FLOOR-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
#                          data = wifilocation_grouped_TC_pca, method = "svmRadial", 
#                          tuneGrid = grid_radial,preProcess = c("center", "scale"),
#                          trControl = fitControl)
#lat_svmr2_tc_pca #RMSE=7.77, R=0.92, MAE=5.64
#set.seed(123)
#lat_svmr3_tc_pca <- train(LATITUDE ~ .-USERID-LONGITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
#                          data = wifilocation_grouped_TC_pca, method = "svmRadial", 
#                          tuneGrid = grid_radial,preProcess = c("center", "scale"),
#                          trControl = fitControl)
#lat_svmr3_tc_pca #RMSE=6.85, R=0.94, MAE=5.04

#save models to test
save(lat_knn2_tc_pca, file = "lat_knn2_tc_pca.rda")
save(lat_knn3_tc_pca, file = "lat_knn3_tc_pca.rda")
save(lat_svmr2_tc_pca, file = "lat_svmr2_tc_pca.rda")
save(lat_svmr3_tc_pca, file = "lat_svmr3_tc_pca.rda")
save(lat_rf_tc_pca, file = "lat_rf_tc_pca.rda")
save(lat_gbm_tc_pca, file = "lat_gbm_tc_pca.rda")

#latitude of TC knn
load("lat_knn2_tc_pca.rda")
predictions_lat_knn2_tc_pca <-predict(lat_knn2_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lat_knn2_tc_pca, wifilocation_validation_grouped_TC_pca$LATITUDE) 
#RMSE:13.10, R:0.81, MAE=7.77
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_knn2_tc_pca,x=LONGITUDE,colour="red"))+
  facet_wrap("FLOOR")
#OK, several in the middle, lots errors at the 4th floor, and 2nd
#latitude of TC knn using floor
load("lat_knn3_tc_pca.rda")
predictions_lat_knn3_tc_pca <-predict(lat_knn3_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lat_knn3_tc_pca, wifilocation_validation_grouped_TC_pca$LATITUDE) 
#RMSE:12.71, R:0.82, MAE=7.75
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_knn3_tc_pca,x=LONGITUDE,colour="red"))+
  facet_wrap("FLOOR")
#4th floor is still bad and 2nd floor is worst.

#latitude of TC svmRadial
load("lat_svmr2_tc_pca.rda")
predictions_lat_svmr2_tc_pca <-predict(lat_svmr2_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lat_svmr2_tc_pca, wifilocation_validation_grouped_TC_pca$LATITUDE) 
#RMSE: 12.67, R:0.81, MAE=9.07
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error svmr")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_svmr2_tc_pca,x=LONGITUDE,colour="red"))+
  facet_wrap("FLOOR")
#a lot in the middle, errors at 2nd and 4th floor
load("lat_svmr3_tc_pca.rda")
predictions_lat_svmr3_tc_pca <-predict(lat_svmr3_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lat_svmr3_tc_pca, wifilocation_validation_grouped_TC_pca$LATITUDE) 
#RMSE: 12.59, R:0.81, MAE=9.17
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error svmr")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_svmr3_tc_pca,x=LONGITUDE,colour="red"))+
  facet_wrap("FLOOR")
#a lot in the middle, errors at 2nd and 4th floor

#latitude TC rf
load("lat_rf_tc_pca.rda")
predictions_lat_rf_tc_pca <-predict(lat_rf_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lat_rf_tc_pca, wifilocation_validation_grouped_TC_pca$LATITUDE) 
#RMSE: 10.49, R:0.88, MAE=7.53
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error rf")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_rf_tc_pca,x=LONGITUDE,colour="red"))+
  facet_wrap("FLOOR")
#a lot in the middle, still bad 4th but better than others

#latitude of TD gbm
load("lat_gbm_tc_pca.rda")
predictions_lat_gbm_tc_pca <-predict(lat_gbm_tc_pca, wifilocation_validation_grouped_TC_pca)
postResample(predictions_lat_gbm_tc_pca, wifilocation_validation_grouped_TC_pca$LATITUDE) 
#RMSE: 10.66, R:0.87, MAE=7.76
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error gbm")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=predictions_lat_gbm_tc_pca,x=LONGITUDE,colour="red"))+
  facet_wrap("FLOOR")
#gsimilar to RF

##See predicted longitude and latitude for TD
#add to validation
wifilocation_validation_grouped_TC$LONGITUDE_pre <- predictions_lon_knn3_tc_pca
wifilocation_validation_grouped_TC$LATITUDE_pre <- predictions_lat_rf_tc_pca
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Predicted")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(y=LATITUDE_pre,x=LONGITUDE_pre,colour="red"))+
  facet_wrap("FLOOR")+
  theme_dark()

#------------------------------------Longitude and Latitude TD+TC -----------------------------------------------------------
#-----Longitude TD+TC with PCA -------------
wifilocation_grouped_TDTC_pca <- wifilocation_grouped_pca[wifilocation_grouped_pca$BUILDINGID=="TC" | wifilocation_grouped_pca$BUILDINGID=="TD",]
wifilocation_validation_grouped_TDTC_pca <- wifilocation_validation_grouped_pca[wifilocation_validation_grouped_pca$BUILDINGID=="TC" | wifilocation_validation_grouped_pca$BUILDINGID=="TD",]
set.seed(123)
lon_knn_tdtc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                        data = wifilocation_grouped_TDTC_pca, method = "knn", 
                        preProc = c("center","scale"), tuneLength = 10,
                        trControl = fitControl)
lon_knn_tdtc_pca #RMSE=11.04, R=0.98, MAE=6.92 k=5
set.seed(123)
lon_knn2_tdtc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TDTC_pca, method = "knn", 
                         preProc = c("center","scale"), tuneGrid = expand.grid(k=c(1:5)),
                         trControl = fitControl)
lon_knn2_tdtc_pca #RMSE=10.85, R=0.98, MAE=6.47 k=3
set.seed(123)
lon_knn3_tdtc_pca <- train(LONGITUDE ~ .-USERID-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TDTC_pca, method = "knn", 
                         preProc = c("center","scale"), tuneGrid = expand.grid(k=c(1:5)),
                         trControl = fitControl)
lon_knn3_tdtc_pca #RMSE=11.52, R=0.98, MAE=6.97 k=3 , better without floor.

set.seed(123)
lon_rf_tdtc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                       data = wifilocation_grouped_TDTC_pca, method = "rf", 
                       ntree=10, tuneLength = 10,
                       trControl = fitControl)
lon_rf_tdtc_pca #RMSE=11.26, R=0.98, MAE=7.20
set.seed(123)
lon_rf2_tdtc_pca <- train(LONGITUDE ~ .-USERID-FLOOR-LATITUDE-SPACEID-RELATIVEPOSITION-PHONEID-max_val-max_wap-BUILDINGID, 
                         data = wifilocation_grouped_TDTC_pca, method = "rf", 
                         ntree=50, tuneLength = 10,
                         trControl = fitControl)
lon_rf2_tdtc_pca #RMSE=10.61, R=0.98, MAE=6.90

#save models to test
save(lon_knn2_tdtc_pca, file = "lon_knn2_tdtc_pca.rda")
save(lon_rf2_tdtc_pca, file = "lon_rf2_tdtc_pca.rda")

#longitude for TD and TC knn
load("lon_knn2_tdtc_pca.rda")
predictions_lon_knn2_tdtc_pca <-predict(lon_knn2_tdtc_pca, wifilocation_validation_grouped_TDTC_pca)
postResample(predictions_lon_knn2_tdtc_pca, wifilocation_validation_grouped_TDTC_pca$LONGITUDE) 
#RMSE:12.72, R:0.98, MAE=8.05
ggplot(wifilocation_validation_grouped_TDTC_pca, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_knn2_tdtc_pca,y=wifilocation_validation_grouped_TDTC_pca$LATITUDE),colour="red")+
  facet_wrap("FLOOR")
#4th floor still bad

#longitude for TD and TC rf
load("lon_rf2_tdtc_pca.rda")
predictions_lon_rf2_tdtc_pca <-predict(lon_rf2_tdtc_pca, wifilocation_validation_grouped_TDTC_pca)
postResample(predictions_lon_rf2_tdtc_pca, wifilocation_validation_grouped_TDTC_pca$LONGITUDE) 
#RMSE:12.19, R:0.98, MAE=8.27
ggplot(wifilocation_validation_grouped_TDTC_pca, aes(x=LONGITUDE,y=LATITUDE)) +
  ggtitle("Error knn")+
  labs(x='Longitude', y='Latitude') +
  geom_point(colour="grey")+
  geom_point(aes(x=predictions_lon_rf2_tdtc_pca,y=wifilocation_validation_grouped_TDTC_pca$LATITUDE),colour="red")+
  facet_wrap("FLOOR")
#4th floor better
##No better than separated.



#---------------------------------Error location ---------------------------------------------
#Floor TI
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE,color=FLOOR_pred_bad)) +
  ggtitle("Errors in FLOOR")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  facet_wrap("FLOOR")
#longitude TI
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE,color=LONGITUDE_error)) +
  ggtitle("Errors in Longitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient(high="red", low="blue")+
  theme_dark()+
  facet_wrap("FLOOR")
#latitude TI
ggplot(wifilocation_validation_grouped_TI, aes(x=LONGITUDE,y=LATITUDE,color=LATITUDE_error)) +
  ggtitle("Errors in Latitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient2(high="red",mid="white" ,low="blue")+
  theme_dark()+
  facet_wrap("FLOOR")
#longitude + latitude TI

#Floor TD
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE,color=FLOOR_pred_bad)) +
  ggtitle("Errors in FLOOR")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  facet_wrap("FLOOR")
#Longitude TD
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE,color=LONGITUDE_error)) +
  ggtitle("Errors in Longitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient2(high="red", low="blue",mid="white")+
  theme_dark()+
  facet_wrap("FLOOR")
#Latitude TD
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE,color=LATITUDE_error)) +
  ggtitle("Errors in Latitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient2(high="red", low="blue",mid="white")+
  theme_dark()+
  facet_wrap("FLOOR")
#Longitude and Latitude TD
ggplot(wifilocation_validation_grouped_TD, aes(x=LONGITUDE,y=LATITUDE,color=LL_error)) +
  ggtitle("Errors in Longitude and Latitude")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  scale_color_gradient(high="red",low="blue")+
  theme_dark()+
  facet_wrap("FLOOR")

#Floor TC
ggplot(wifilocation_validation_grouped_TC, aes(x=LONGITUDE,y=LATITUDE,color=FLOOR_pred_bad)) +
  ggtitle("Errors in FLOOR")+
  labs(x='Longitude', y='Latitude') +
  geom_point() +
  facet_wrap("FLOOR")



#Plot real and predicted


