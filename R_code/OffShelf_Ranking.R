############################################
# Create function for retreiving Off Shelf #
# feature set for a given month and UPCs   #
############################################

######## Get libraries
library(ggplot2)
library(caret)
library(DBI)
library(odbc)
library(dplyr)

######## Upc list -- user input

upcs<-c()




###### retail start and end dates of future promo -- user input

retail_start<-
retail_end<-
retail_promo_period<-
##### Last full promo period and last full promo period end date -- user input

promo_period<-
promo_period_end<-


##### Last year promo period end date for seasonality -- user input

retail_end_ly<-



###### get data

test.cases<- read.csv("dummy_data1.csv")

######## Attribute Data Clean up #############
test.cases<- test.cases[!is.na(test.cases$BRAND_NAME)|test.cases$PROMO_PERIOD>promo_period,]
test.cases$delta_arp<- ifelse(is.na(test.cases$ARP_going_in-test.cases$ARP), 0, test.cases$ARP_going_in - test.cases$ARP)
test.cases$comp_arp<- ifelse(is.na(test.cases$comp_arp), 0, test.cases$comp_arp)
test.cases$comp_index<- ifelse(is.na(test.cases$comp_index), 0, test.cases$comp_index)
test.cases$sindex<- ifelse(is.na(test.cases$sindex ), 0, test.cases$sindex)
test.cases$stores_rec<- ifelse(is.na(test.cases$stores_rec ), 0, test.cases$stores_rec)
test.cases$BRAND_NAME<- ifelse(is.na(test.cases$BRAND_NAME ), test.cases$BRAND, test.cases$BRAND_NAME)
test.cases$UPC<- test.cases$DIGIT_UPC
test.cases$stores_rec<- ifelse(test.cases$PROMO_PERIOD<promo_period,0,test.cases$stores_rec)
test.cases$stores_selling_mod<- ifelse(test.cases$stores_rec == 0, test.cases$stores, test.cases$stores_rec)
test.cases<- test.cases[!is.na(test.cases$stores_selling_mod),]

######## Aggregate by promo period and UPC #############
test.cases.a<- aggregate(list(test.cases$delta_arp, test.cases$stores_selling_mod,test.cases$comp_index,test.cases$comp_arp,test.cases$sindex,test.cases$nbr_days_offshelf),
                         by=list(test.cases$UPC, test.cases$BRAND, test.cases$CATEGORY_DESC,test.cases$PROMO_PERIOD,
                                 test.cases$PROMOTION_EVENT_ID), FUN = mean)
## rename columns in features data frame
colnames(test.cases.a)<-c("upc","brand","category","promo_period","promo_event_id","delta_arp",
                          "stores_selling", "comp_index", "comp_arp","seasonal_index","nbr_days_offshelf")  

## retain only needed rows and columns
test.cases.a<- test.cases.a[test.cases.a$promo_period==retail_promo_period,]
test.cases.b<-test.cases.a[,c(1,3,4,6,7,8,10,11)]

### Bind with training data for normalization

training.data<- read.csv("dummy_data_2.csv")


test.cases.c<- rbind(training.data,test.cases.b)

#### Normalize features ########
## min-max normalization

test.cases.c$stores_selling.n<-(test.cases.c$stores_selling-min(test.cases.c$stores_selling))/(max(test.cases.c$stores_selling)-min(test.cases.c$stores_selling))
test.cases.c$delta_arp.n<-(test.cases.c$delta_arp-min(test.cases.c$delta_arp))/(max(test.cases.c$delta_arp)-min(test.cases.c$delta_arp))
test.cases.c$comp_index.n<-(test.cases.c$comp_index-min(test.cases.c$comp_index))/(max(test.cases.c$comp_index)-min(test.cases.c$comp_index))
test.cases.c$seasonal_index.n<-(test.cases.c$seasonal_index-min(test.cases.c$seasonal_index))/(max(test.cases.c$seasonal_index)-min(test.cases.c$seasonal_index))
test.cases.c$nbr_days_offshelf.n<-(test.cases.c$nbr_days_offshelf-min(test.cases.c$nbr_days_offshelf))/(max(test.cases.c$nbr_days_offshelf)-min(test.cases.c$nbr_days_offshelf))


##### Delete uneeded rows and columns
test.cases.c<-test.cases.c[test.cases.c$promo_period==retail_promo_period,]
test.cases.c<-test.cases.c[,c(1,2,3,9,10,11,12,13)]



##### narrow down to just model features ##########

test.cases.c.model<-test.cases.c[,c(1,4)]

test.cases.c.model<-test.cases.c.model %>% inner_join(test.cases.a) %>% as.data.frame
test.cases.c.model<-test.cases.c.model[,c(1,3)]

test.cases.c.features.only<-test.cases.c[,c(2,4,5,6,7,8)]

###### run model #########
load("model.revised.RF.smote.RData")

final<- predict(object = model.revised.RF.smote, test.cases.c, type = "prob")

final.ouput<-cbind(final,test.cases.c.model)
