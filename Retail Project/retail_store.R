options(scipen=999, stringsAsFactors = FALSE)

train_data <- read.csv('D:/R_dir/project/store/store_train.csv', stringsAsFactors = FALSE)

test_data <- read.csv('D:/R_dir/project/store/store_test.csv', stringsAsFactors = FALSE)

dim(test_data)

library(dplyr)

setdiff(names(train_data), names(test_data))

test_data$store  <- NA

train_data$data <- 'train'
test_data$data <- 'test'

store_data <- rbind(train_data, test_data)

lapply(store_data,function(x) sum(is.na(x)))

store_data$population[is.na(store_data$population)] = median(store_data$population, na.rm =T)

# store_data  <-  store_data %>% 
#   mutate(pop_1 =  ifelse(population>0 & population<4247,1,0),
#          pop_2 =  ifelse(population>=4247 & population<15911,1,0),
#          pop_3 =  ifelse(population>=15911 & population<42745,1,0)) 

# library(corrplot)

# M <- cor(mtcars)
# corrplot(M, method='circle')

#correlation test when both are continuous variables
# cor.test(train_data$store, train_data$population)
# library(ggplot2)
# ggplot(train_data, aes(x=population, y=store)) + geom_point()

# library(ggplot2)
# ggplot(train_data, aes(x = population, y = store)) + geom_point()
# 
# t.test(train_data$population, train_data$store)
# 
# plot(store_data$population, store_data$store)
# lines(store_data$population ~ store_data$store)


store_data <-
  store_data %>%
  mutate(
    store_Type_1 = ifelse(store_Type %in% 'Supermarket Type1', 1, 0),
    store_Type_2 = ifelse(store_Type %in% c('Supermarket Type2','Supermarket Type3'), 1, 0)
  )
# for(states in unique(store_data$State))
# {
#   col=paste0('state','_',states)
#   train_data[,col] = as.numeric(train_data$State == states)
# }

store_data <-
  store_data %>% mutate(
    storecode_1 = grepl('METRO', store_data$storecode) + 0,
    CouSub_1 = (store_data$CouSub == 99999) + 0
  ) 
 
# select(-storecode, -population, -Id, -State, -country,-countyname,
#        -CouSub, -store_Type, -Areaname, -countytownname, -state_alpha)

# columns to be dropped
# countyname
# Id
# country
# State
#population

#columns o make dummy
#storecode
#CouSub
#store_Type

# store_data <- 
#   store_data %>% mutate(
#     change_in_sales_0to1 = ((sales1-sales0)/sales0)*100,
#     change_in_sales_1to2 = ((sales2-sales1)/sales1)*100,
#     change_in_sales_2to3 = ((sales3-sales2)/sales2)*100,
#     change_in_sales_3to4 = ((sales4-sales3)/sales3)*100
#   ) 

category <- names(table(store_data$state_alpha))
for (itr in category)
{
  name = paste0('state_alpha_',itr)
  store_data[,name] = as.numeric(store_data$state_alpha %in% itr)
}

store_data <- 
  store_data %>% select (-storecode, -CouSub, -Id, -store_Type, -state_alpha,
                         -countyname, -countytownname, -Areaname)

store_data <- 
  store_data %>% select(-state_alpha_DC, -state_alpha_VT, -state_alpha_WV ,
                        -state_alpha_WY, -state_alpha_GU, -state_alpha_VI)

glimpse(store_data)

store_data_train <- store_data %>% filter(data == 'train') %>% select(-data)
store_data_test <- store_data %>% filter(data == 'test') %>% select(-data,-store)

library(car)

s=sample(1:nrow(store_data_train), 0.7*nrow(store_data_train))
data_train_1 <- store_data_train[s,]
data_train_2 <- store_data_train[-s,]

glimpse(data_train_1)

form <-
  as.formula(paste0('store ~', paste0(setdiff(
    names(data_train_1), 'store'
  ), collapse = '+')))

variable_drop <- c()
repeat {
  vif_calc= lm(form, data=data_train_1)
  k <- sort(vif(vif_calc), decreasing = TRUE)[1]
  if(k < 4) {
    break
  }
  print(k)
  variable_drop <- c(variable_drop, names(k))
  
  form <-
    as.formula(paste0('store ~ ', paste0(setdiff(
      names(data_train_1), c('store', variable_drop)
    ), collapse = ' + ')))  
}

# To Remove the aliases which occur in fit model
# attributes(alias(fit)$Complete)$dimnames[[1]]

data_train_1$store <- as.factor(data_train_1$store)

log_fit=glm(form,data=data_train_1,family = "binomial")

log_fit_new <- step(log_fit)

formula(log_fit_new)
summary(log_fit_new)

form_new <- as.formula("store ~ population + store_Type_1 + storecode_1 + state_alpha_HI + 
    state_alpha_IL + state_alpha_IN + state_alpha_LA + state_alpha_NC + 
    state_alpha_OH + state_alpha_SC + state_alpha_TN + state_alpha_WA")

# form_new <-
#   as.formula(
#     "store ~ store_Type_1 + storecode_1 + state_alpha_IL + state_alpha_NC + 
#     state_alpha_ND + state_alpha_RI + state_alpha_SC + state_alpha_TN + 
#     state_alpha_WA"
#   )

log_fit_new=glm(form_new,data=data_train_1,family='binomial')

summary(log_fit_new)

caTools::colAUC(predict(log_fit_new, data_train_1, type = 'response'), 
                data_train_1$store, plotROC = TRUE)

caTools::colAUC(predict(log_fit_new, data_train_2, type = 'response'), 
                data_train_2$store, plotROC = TRUE)

library(car)
#predicting
data_train_1$score <- predict(log_fit_new, data=data_train_1, type='response')
#data_train_2$score <- predict(log_fit_new, data=data_train_2, type='response')

#comparing AUC
library(pROC)
auc(roc(data_train_1$store, data_train_1$score))
#auc(roc(data_train_2$store, data_train_2$score))


#submission of file
glimpse(store_data_test)
test.probs = predict(log_fit_new, data=store_data_test, type='response')


#calculating KS cutt-off
train_score = predict(log_fit_new, newdata = data_train_1, type = 'response')
real=data_train_1$store
cutoffs=seq(0.001,0.999,0.001)

## Create a data frame with initialised garbage values
cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)
for(cutoff in cutoffs){
  ## determine the prediction for each cut off here
  predicted=as.numeric(train_score>cutoff)
  
  ## fill the value of TP, FP, FN and TN
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  ## KS is the cutoff
  KS=(TP/P)-(FP/N)
  
  F5=(26*precision*recall)/((25*precision)+recall)
  ## F.1 score is maximum at 1 and min at 0
  ## A value of F.1 closer to 1 is good
  ## In case of low event rate model, F.1 closer to 1 is great
  ## F.1 score captures both precision and recall hence it is very useful in case of low event rate model
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  ## Binding the data
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}
View(cutoff_data)

#to remove garbage that we used to declare empty dataframe
cutoff_data=cutoff_data[-1,]

#row having max cuttoff
cutoff_data[cutoff_data$KS == max(cutoff_data$KS),]

# KS --> 0.684
#cutt of --> 0.3

##########
#submission for hard class
test.class=as.numeric(predict(log_fit_new,newdata=store_data_test,type='response')>0.3)
#test.class=ifelse(test.class==1,'Yes','No')

write.csv(test.class,'Nitesh_Bhosle_P2_part2.csv',row.names = F)


