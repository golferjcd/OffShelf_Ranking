#####################################################################
# Successful Promotion Determination -- dichotomize                 #
# historical Off Shelf events into successful/not succesful         #
#####################################################################

## get all necessary libraries
library(ggplot2)
library(CORElearn)
library(pROC)
library(caret)
library(lubridate)
library(dplyr)
## Import data set - all promos from 01-01-2016 to present day for all dry grocery UPCs time = daily, location = national

All_Promos<- read.csv("dummy_data_model")

## test to see what data type each attribute is
sapply(All_Promos, class)

## convert UPC from numeric to factor
All_Promos$UPC<- as.factor(All_Promos$UPC)



## aggregate - average daily GM Dollars over promo period to normalize 2 and 3 weeks promos as well as partial periods 
Daily.GM.DLRS<- aggregate(All_Promos$GM_DLRS, by = list(All_Promos$UPC,All_Promos$BRAND_NAME,All_Promos$CATEGORY_DESC,
                                                        All_Promos$PROMO_LEVEL,All_Promos$PROMO_PERIOD, All_Promos$PROMOTION_EVENT_ID), FUN = mean)

## reorder columns
Daily.GM.DLRS<- Daily.GM.DLRS[,c(1,2,3,4,5,7,6)]

## rename columns
colnames(Daily.GM.DLRS)<- c("UPC", "Brand", "Category", "Promo_Level", "Promo_Period", "GM_DLRS", "Promo_Event_ID")

## clean up promo level

Daily.GM.DLRS$Promo_Level<- ifelse(Daily.GM.DLRS$Promo_Level=="Off Shelf", "Off Shelf", "TPR/Shelf")

## sort by promo level
sort.order<-order(Daily.GM.DLRS$Promo_Level,Daily.GM.DLRS$UPC,Daily.GM.DLRS$Promo_Event_ID)
Daily.GM.DLRS.sorted<-Daily.GM.DLRS[sort.order,]

## Create function to calculate off shelf lift



a<-0
b<-0
result<-NULL
c<-seq(1:length(Daily.GM.DLRS.sorted$UPC))
for (i in c) {
  a<- Daily.GM.DLRS.sorted[i,1]
  b<- Daily.GM.DLRS.sorted[i,5]
  result<-c(result,(Daily.GM.DLRS.sorted[i,6]-median(Daily.GM.DLRS.sorted$GM_DLRS[Daily.GM.DLRS.sorted$UPC==a& Daily.GM.DLRS.sorted$Promo_Level=='TPR/Shelf'
                                                                                  & Daily.GM.DLRS.sorted$Promo_Period<=b]))/median(Daily.GM.DLRS.sorted$GM_DLRS[Daily.GM.DLRS.sorted$UPC==a& Daily.GM.DLRS.sorted$Promo_Level=='TPR/Shelf'
                                                                                                                                                                & Daily.GM.DLRS.sorted$Promo_Period<=b]))}

revised.lift<-cbind(Daily.GM.DLRS.sorted,result)
revised.lift.clean<- revised.lift[revised.lift$Promo_Level=="Off Shelf",]
revised.lift.clean<- na.omit(revised.lift.clean)

## find outliers
summary(revised.lift.clean$result)

upper.outliers<- revised.lift.clean[revised.lift.clean$result>quantile(revised.lift.clean$result,.75)+1.5*IQR(revised.lift.clean$result),]
lower.outliers<- revised.lift.clean[revised.lift.clean$result<quantile(revised.lift.clean$result,.25)-(1.5*IQR(revised.lift.clean$result)),]
outliers<- rbind(upper.outliers,lower.outliers)

## remove outliers
revised.lift.clean.n.outliers<- revised.lift.clean[!(revised.lift.clean$result %in% outliers$result),]
plot(revised.lift.clean.n.outliers$result,revised.lift.clean.n.outliers$GM_DLRS)

h<-ggplot(revised.lift.clean.n.outliers, aes(revised.lift.clean.n.outliers$result, revised.lift.clean.n.outliers$GM_DLRS))
h+geom_point(aes(color=revised.lift.clean.n.outliers$Class))+
geom_vline(xintercept = mean(revised.lift.clean.n.outliers$result))+ 
geom_hline(yintercept = mean(revised.lift.clean.n.outliers$GM_DLRS))+labs(title = "Off Shelf Success Dichotomy",x="Lift",y="Performance", color = "Success/Failure")+
theme(axis.title=element_text(size=20, face = "bold"), legend.title=element_text(size =20, face = "bold"),legend.text=element_text(size =20, face = "bold"),title=element_text(size = 20, face = "bold"))  


## read in feature set
features<- read.csv("dummy_features.csv")

## aggregate feature set
features.a<- aggregate(list(features$delta_arp, features$stores,features$comp_index,features$comp_arp,features$sindex,features$nbr_days_offshelf),
                       by=list(features$UPC, features$BRAND_NAME, features$CATEGORY_DESC,features$PROMO_PERIOD,
                               features$PROMOTION_EVENT_ID,features$nbr_periods_offshelf), FUN = mean)
## rename columns in features data frame
colnames(features.a)<-c("upc","brand","category","promo_period","promo_event_id","nbr_periods_offshelf","delta_arp",
                        "stores_selling", "comp_index", "comp_arp","seasonal_index","nbr_days_offshelf")


## create class attribute
revised.lift.clean.n.outliers$Class<- as.factor(ifelse(revised.lift.clean.n.outliers$result>mean(revised.lift.clean.n.outliers$result)&revised.lift.clean.n.outliers$GM_DLRS>mean(revised.lift.clean.n.outliers$GM_DLRS),0,1))

## drop uneeded columns from class attribute

class.a<-revised.lift.clean.n.outliers[,c(1,7,9)]

## create merge attribute (upc+promo_event_id)
class.a$id<-paste(class.a$UPC, class.a$Promo_Event_ID)

features.a$id<- paste(features.a$upc, features.a$promo_event_id)
## merge class with features

features.w.class<- merge(features.a, class.a, by= "id")


## purge dups

features.w.class<- features.w.class[!duplicated(features.w.class$id),]


## trim uneeded features
features.w.class<-features.w.class[,c(4,8,9,10,12,13,16)]

## min-max normalization

features.w.class$stores_selling.n<-(features.w.class$stores_selling-min(features.w.class$stores_selling))/(max(features.w.class$stores_selling)-min(features.w.class$stores_selling))
#features.w.class$nbr_brands_in_cat.n<-(features.w.class$nbr_brands_in_cat-min(features.w.class$nbr_brands_in_cat))/(max(features.w.class$nbr_brands_in_cat)-min(features.w.class$nbr_brands_in_cat))
#features.w.class$nbr_periods_offshelf.n<-(features.w.class$nbr_periods_offshelf-min(features.w.class$nbr_periods_offshelf))/(max(features.w.class$nbr_periods_offshelf)-min(features.w.class$nbr_periods_offshelf))
features.w.class$delta_arp.n<-(features.w.class$delta_arp-min(features.w.class$delta_arp))/(max(features.w.class$delta_arp)-min(features.w.class$delta_arp))
features.w.class$comp_index.n<-(features.w.class$comp_index-min(features.w.class$comp_index))/(max(features.w.class$comp_index)-min(features.w.class$comp_index))
#features.w.class$comp_arp.n<-(features.w.class$comp_arp-min(features.w.class$comp_arp))/(max(features.w.class$comp_arp)-min(features.w.class$comp_arp))
features.w.class$seasonal_index.n<-(features.w.class$seasonal_index-min(features.w.class$seasonal_index))/(max(features.w.class$seasonal_index)-min(features.w.class$seasonal_index))
features.w.class$nbr_days_offshelf.n<-(features.w.class$nbr_days_offshelf-min(features.w.class$nbr_days_offshelf))/(max(features.w.class$nbr_days_offshelf)-min(features.w.class$nbr_days_offshelf))

### create dataframe with only normalized features

features.n<- features.w.class[,c(1,7,8,9,10,11,12)]
features.n$Class<-as.factor(features.n$Class)


## split into testing and training sets using random sampling

split.partition<- floor(0.7 * nrow(features.n))
set.seed(95)
tr.index<- sample(seq_len(nrow(features.n)), size=split.partition)
features.n.train<- features.n[tr.index,]
features.n.test<-features.n[-tr.index,]

load("model.revised.RF.smote.RData")

### run caret rf model with SMOTE
ctrl<- trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE, sampling = "smote")
set.seed(95)
model.revised.RF.smote<- train(Class~., data = features.n.train, method = "rf",trControl = ctrl)
final.revised.RF.smote<- predict.train(object = model.revised.RF.smote, features.n.test, type = "raw")
final.revised.RF.smote2<- predict.train(object = model.revised.RF.smote, features.n.test, type = "prob")
cm.revised.RF.smote<- confusionMatrix(final.revised.RF.smote, features.n.test$Class)

t<- roc(features.n.test$Class~final.revised.RF.smote2$`0`)


p<-qplot(1-t$specificities,t$sensitivities)
p+geom_line(aes(x=1-t$specificities,y=t$sensitivities), size=0.1,alpha=0.7)+geom_abline()+
  labs(title = "ROC Plot",x="1-specificity", y="sensitivity")+
theme(axis.title=element_text(size=20, face = "bold"),title=element_text(size = 20, face = "bold"))  
