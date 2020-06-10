
train=read.csv('store_train.csv',stringsAsFactors = F)

train

head(train)

test=read.csv('store_test.csv',stringsAsFactors = F)


head(test)


test$store=NA



train$data='train'
test$data='test'

dim(train)

dim(test)





all=rbind(train,test)


str(all)



CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


all=all %>% select(-countyname,-storecode,-Areaname,-countytownname,-Id,-state_alpha)


str(all)

for_dummy_vars=c('country','State','CouSub','store_Type')

for(var in for_dummy_vars){
  all=CreateDummies(all,var,50)
}


str(all)


lapply(all , function(x) sum(is.na(x)))



for(col in names(all)){
  
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","store"))){
    
    all[is.na(all[,col]),col]=mean(all[all$data=='train',col],na.rm=T)
  }
  
}


str(all)


lapply(all , function(x) sum(is.na(x)))

dim(all)



train=all %>% filter(data=='train') %>% select(-data)

test=all %>% filter(data=='test') %>% select(-data,-store)


dim(train)

dim(test)


head(train)

head(test)




set.seed(2)

s=sample(1:nrow(train),0.8*nrow(train))
s


train1 = train[s , ]
train1


train2 = train[-s , ]
train2


dim(train1)
dim(train2)

2670+668



table(train1$store)


library(car)


form = 
  as.formula(paste0("store  ~  "  ,  
                    
                    paste0(setdiff(names(train1) , 
                                   
                                   c("store")) , collapse = "+")))  




## For VIF calculation; we create a basic linear model
## lm = linear model
### we creating the linera model (lm)


for_vif=lm(form,data=train1)
for_vif


### now calculating the vif

vif(for_vif)



## k is the vif value initialized to a very high value

k = 100000000

## appended_dropped is a sentinal value which will change with k so that we don't run into infinite loop
appended_dropped = c('store')

## loop will run untill all the values have vif lower than 4
while(k > 4){
  for_vif=lm(form,data=train1) ## first a linear model for understanding the linear combination
  k <- sort(vif(for_vif),decreasing = T)[1] ## get the value of vif for highest value
  if (k <= 4){
    break
  }
  var_dropped <- names(k) ## get the name of the variable which has highest value
  print(k)
  appended_dropped <- c(var_dropped, appended_dropped) ## update the sentinal value with the new variable which needs to be dropped
  
  form <-
    as.formula(paste0('store ~ ', paste0(setdiff(
      names(train1), c('store', appended_dropped)
    ), collapse = ' + '))) ## update the formula everytime
}





train1$store = as.factor(train1$store)

is.factor(train1$store)



## creating the logistic model by using the glm function


fited=glm(form,data=train1,family = "binomial")
fited


## run the stepwise function for calculating the AIC

fited=step(fited)

# this might take 5-6 minutes to finish 


formula(fited)

summary(fited)

glm(formula = store ~ sales4 + population + country_29 + country_13 + 
      country_19 + country_15 + country_11 + country_9 + State_54 + 
      State_6 + State_8 + State_22 + State_72 + State_18 + State_47 + 
      State_13 + State_50 + State_33 + State_25, family = "binomial", 
    data = train1)



glm(formula = store ~ sales4 + population + country_29 + country_13 + 
      country_19 + country_15 + country_11 + country_9 + State_54 + 
      State_6 + State_8 + State_22 + State_72 + State_18 + State_47 + 
      State_13 + State_50 + State_33 + State_25, family = "binomial", 
    data = train1)




form <-
  as.formula("
   store ~ sales4 + population + country_29 + country_13 + 
      country_19 + country_15 + country_11 + country_9 + State_54 + 
      State_6 + State_8 + State_22 + State_72 + State_18 + State_47 + 
      State_13 + State_50 + State_33 + State_25 ")

form

fited=glm(form,data=train1,family='binomial')
fited


summary(fited)


library(caTools)

## caTools to get the ROC:

## Run it to determine the ROC Plot

## Install caTools library


caTools::colAUC(predict(fited, train1, type = 'response'), 
                train1$store, plotROC = TRUE)

caTools::colAUC(predict(fited, train2, type = 'response'), 
                train2$store, plotROC = TRUE)


round(0.7891455 - 0.7719538 , 2)



library(pROC)


val.score = predict(fited,newdata = train2,type='response')
val.score


## scoring the train (using rg_train1 data)

train.score = predict(fited, newdata=train1, type='response')
train.score



#comparing the auc for train and test

auc(roc(train2$store,val.score))

auc(roc(train1$store,train.score))


############################################################



val.score=predict(rf.fit,newdata = train,type='prob')[,2] ##check dimension error

val.score


pROC::roc(train$store,val.score)$auc #checking AUC


for_vif=lm(store~.,data=train)
library(car)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0,data=train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2,data=train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2-sales3,data=train)
sort(vif(for_vif),decreasing = T)[1:3]


train$store=as.factor(train$store)

library(randomForest)

rf.fit=randomForest(store~.,data=train,ntree=20)

rf.fit

### Make predictions on test and submit 

test.score=predict(rf.fit,newdata=test,type='prob')[,2]
test.score

######################################################################



for_vif=lm(store~.,data=train1)
for_vif

library(car)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0,data=train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2,data=train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2-sales3,data=train)
sort(vif(for_vif),decreasing = T)[1:3]



train$store=as.factor(train$store)

library(randomForest)

rf.fit=randomForest(store~.,data=train1,ntree=20)

rf.fit


val.score=predict(rf.fit,newdata = train1,type='prob')[,2] ##check dimension error

val.score


pROC::roc(train$store,val.score)$auc #checking AUC
