setwd("D:/Users/venkat/Desktop/business/YEAR 2 SEM 2/BC2407/AY2020s2 BC2407 CBA")
library(data.table)
#Q1##############################################################################

# Q1a)----------------------------------------------------------------------------
data1 =  fread("resale-flat-prices-201701-202103.csv",stringsAsFactors = T)
summary(data1)
View(data1)
# Q1b)-------------------------------------------------------------------------
# the date of each of the month are differet
# total lease years is 99, the time that has passed is year recorded- lease start date
# the month is read as a string but the first 4 digits are the year-> convert to int
data1$remaining_lease_yrs = as.numeric(substring(data1$remaining_lease,0,2))
class(data1$remaining_lease_yrs)

#Q1c-------------------------------------------------------------------------------
data1$remaining_lease = NULL
data1$lease_commence_date = NULL
colnames(data1)
#1d)-----------------------------------------------
data1$block_street = paste(data1$block,data1$street_name)
summary(data1$block_street)
data1$block_street = factor(data1$block_street)
summary(data1$block_street)
data1$block = NULL
data1$street_name = NULL
colnames(data1)

#Q2################################################################################

#Q2a)---------------------------------------------------------------------
sort(summary(data1$month),decreasing = T)
# the year month with the highest transaction is 2018-07 with 2539 
# the year month with the lowest transaction is 2020-05 with 363

#Q2b)------------------------------------------------------------------------
sort(summary(data1$town),decreasing = T)
# the town with the highest transaction is sengkang with 7763
# the town with the lowest transaction is Bukit Timah with 264

#Q2c------------------------------------------------------------------------------
top_sale = data1[order(-resale_price),.(flat_type,block_street,town,floor_area_sqm,
                                    storey_range,resale_price)]
a = head(top_sale,5)
a = rbind(a,tail(top_sale,5))
# q2d)-----------------------------------------------------------------------------
# additional data exploration
# distribution of resale
boxplot(data1$resale_price)
summary(data1$resale_price)
# there is a lot of outliers in the data

# resale vs flat type------------------------------------------------------------
plot(data1$flat_type,data1$resale_price,main = "Resale price against Flat type",
     xlab = "Flat Type",ylab = "resale price")
# as expected the resale price differs across different flat type
# the higher the number of rooms-> the higher the resale price 
# does this mean that area will also play a part ?

# resale vs floor area-----------------------------------------------------------------
library(ggplot2)
ggplot(data1, aes(x = floor_area_sqm, y = resale_price)) +
  geom_point(aes(color = flat_type)) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)
# It seems that the floor area is indeed affect the resale price
# it has a positive correlation with resale price-> same with the flat type

# does the location affect the resale price
# location vs resale prices--------------------------------------------
ggplot(data1, aes(town, resale_price)) +
  geom_boxplot() + 
  labs(title = "Impact of location on resale price ") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# bukit timah had the highest median resale price
# is it becuase of the type of housing ?
# this could also explain why bukit timah had the lowest number of transaction
# ang mo kio had the lowest median resale price but why did sengkang have higher transaction

# town vs flat type----------------------------------------------------------
ggplot(data1, 
       aes(x = town, 
           fill = flat_type)) + 
  geom_bar(position = "dodge")+
  labs(title = "Location against flat type")+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# in bukit timah  most of the flat types are executive or 4 room which tends to be expensive

# but for sengkang most of the flat types sold are also 4 room then why did they manage to sell more 

# town vs floor area---------------------------------------------------------------
ggplot(data1, aes(town, floor_area_sqm)) +
  geom_boxplot() + 
  labs(title = "Impact of location on floor area ") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# looking at the different town, pasir ris had the highest floor area
# if the floor area is correlated with the resale price 
# the resale price of pasir ris should be the highest -> what other factor affect

#usually more lease years would mean a higher price
# remaining lease years against resale price-----------------------------------
plot(data1$remaining_lease_yrs,data1$resale_price)
ggplot(data1, aes(x = remaining_lease_yrs, y = resale_price)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)
# it has a positive correlation as well -> does it have a lower correlation when compared to square area 


# town vs remaining lease--------------------------------------------------------
ggplot(data1, aes(town, remaining_lease_yrs)) +
  geom_boxplot() + 
  labs(title = "Impact of location on floor area ") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# towns like punggol and senkgang tend to be the newer buildings
# and newer buildings tend to be much more expensive -> a higher resale value 
# and marine parade usually have older buildings-> should have a lower resale value

# could it be that the building type affect as well 

# flat model against resale price----------------------------------------------------
ggplot(data1, aes(flat_model, resale_price)) +
  geom_boxplot() + 
  labs(title = "Impact of flat model  on resale price ") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# type s2 had the highest median resale value followed by type s1

# story range against resale price-----------------------------------------------
ggplot(data1, aes(storey_range, resale_price)) +
  geom_boxplot() + 
  labs(title = "Impact of location on floor area ") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# The trend is a very obvious positive correlation between the story range and resale price
# as the story range increases the resale prices increasing

# correlation of all the variables that have a clear correlation
library(dplyr)
data1$storey_range_num = as.numeric(data1$storey_range)
a = select(data1,"floor_area_sqm","resale_price","remaining_lease_yrs","storey_range_num")
library(corrplot)
corrplot(cor(a))
# storey range is more correlated to resale price than the remaining lease years
data1$storey_range_num = NULL
# so why did storey range play a part ? is affected by another variable 
# is the storey range different for different towns?

# towns against storey_range--------------------------------------------------

ggplot(data1, 
       aes(x = storey_range)) + 
  geom_bar(position = "dodge")+
  labs(title = "Location against flat type")+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+ facet_wrap(~town)
# majority of the towns have the same distribution of storey range

# towns against flat model-------------------------------------------------------

ggplot(data1, 
       aes(x = flat_model)) + 
  geom_bar(position = "dodge")+
  labs(title = "Location against flat type")+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+ facet_wrap(~town)

# usaully older building tend to have lower floors when compared to the newer ones
# does this data follow the same distribution ?
# remaining lease against storey_range------------------------------------------
ggplot(data1, aes(storey_range, remaining_lease_yrs)) +
  geom_boxplot() + 
  labs(title = "Impact of storey_range on remaining_lease") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# similarly the distribution of lease years increases with the storey_range
# this could be one of the reason to why resale price of higher storey_range is higher

# across the years the floor area have been shrinking is it the same for this dataset ?

# remaining lease vs floor area ----------------------------------------------
plot(data1$remaining_lease_yrs,data1$floor_area_sqm)
ggplot(data1,aes(x = remaining_lease_yrs,y = floor_area_sqm,color = resale_price))+
  geom_point()
# the number of outliers have reduced across the timing, the newer flats have a smaller
# range when compared to the older flats

# floor area against flat_models-------------------------------------------------
ggplot(data1, aes(flat_model, floor_area_sqm)) +
  geom_boxplot() + 
  labs(title = "Impact of flat model on floor area ") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# types s2 and s1 didnt actaully have the highest floor area
# so this could not be the reason as to why type s1 and s2 have the highest resale price

# could it be due to the remaining lease 

# lease against flat model---------------------------------------------------------
ggplot(data1, aes(flat_model, remaining_lease_yrs)) +
  geom_boxplot() + 
  labs(title = "Impact of flat model on remaining lease ") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# type 1 and type 2 were actually newer buildings thats why it had a higher resale price
# however some flat models still have a higher resale value eventhough they have a lower remaining years
# these flats however actually had a higher floor square area such as terrace, ...
# therefore the price of different flat models is affected by the lease years and the area of the flat

# assuming everything is helod constant will the location play a part in resale price?
summary(data1[remaining_lease_yrs==75])
# for 75 median lease years the median area is 124
summary(data1[remaining_lease_yrs==75&floor_area_sqm==130,])


ggplot(data1[remaining_lease_yrs==75&floor_area_sqm==130,], 
       aes(town, resale_price)) +
  geom_boxplot() + 
  labs(title = "Impact of town on resale price (Ceteris paribus) ") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1)) 
# having the 2 major force constant we can see that the different town have different distribution of resale price 
# therefore the town also affects the resale price

# we can see that the major forces that affect the resale prices are 3 factors
# remaining lease, floor area and the town

#-----------------------------Q3------------------------------------------------

#q3 a)---------------------------------------------------------------------------
data2 = data.frame(data1)
tracemem(data1)==tracemem(data2)

# q3 b)---------------------------------------------------------------------------
data2 = data2[data2$flat_type!="1 ROOM" & data2$flat_type!="MULTI-GENERATION",]
data2$flat_type = droplevels.factor(data2$flat_type)
levels(data2$flat_type)
summary(data2$flat_type)

# q3c)----------------------------------------------------------------------------
data2$block_street=NULL
colnames(data2)

#q3d & e)------------------------------------------------------------------------
data2$storey = data2$storey_range
summary(data2$storey)
library(rockchalk)
data2$storey = combineLevels(data2$storey,c(14:17),"40 to 51")
summary(data2$storey)

#q3f)--------------------------------------------------------------------------
data2$storey_range =NULL
colnames(data2)

#q3g)-------------------------------------------------------------------------
summary(data2$flat_model)
data2 = data2[data2$flat_model!="2-room" &
                data2$flat_model!="Premium Maisonette" &
                data2$flat_model!="Improved-Maisonette",]

data2$flat_model = droplevels.factor(data2$flat_model)
levels(data2$flat_model)
summary(data2$flat_model)
#-----------Q3H)---------------------------------------------------------------
dim(data2)


#----------------------------------------Q4)-----------------------------------

#Linear regression--------------------------------------------------------------
library(caTools)
set.seed(2021)
train = sample.split(Y = data2$resale_price, SplitRatio = 0.7)
trainset <- subset(data2, train == T)
testset <- subset(data2, train == F)
# need to create an empty dataframe to store all the results 
# need to have 3 columns c(model,train,test)

RMSE_dataframe =  data.frame(matrix(ncol = 3,nrow = 0))
colnames(RMSE_dataframe) = c("Model Type","Train RMSE","Test RMSE")

#-----------------Linear Model----------------------------------------------

linear_model = lm(resale_price~.,data = trainset)
summary(linear_model)
library(Metrics)

# train RMSE 
lm_resalePrice_train = predict(linear_model,newdata = trainset)
lm_RMSE_train = rmse(trainset$resale_price,lm_resalePrice_train)
lm_RMSE_train
lm_resalePrice_test = predict(linear_model,newdata = testset)
lm_RMSE_test = rmse(testset$resale_price,lm_resalePrice_test)

RMSE_dataframe[1,] = c("Linear Regression",
                                        round(lm_RMSE_train),
                       round(lm_RMSE_test,0))

#--------------------------MARS MODELLING--------------------------------------
library(earth)

mars = earth(resale_price~.,data = trainset,degree = 2,trace = 3)

mars_resalePrice_train = predict(mars,newdata = trainset)
mars_resalePrice_test = predict(mars,newdata = testset)
mars_RMSE_train = rmse(trainset$resale_price,mars_resalePrice_train)
mars_RMSE_test = rmse(testset$resale_price,mars_resalePrice_test)

RMSE_dataframe[2,] = c("MARS with Degree 2",
                       round(mars_RMSE_train,0),round(mars_RMSE_test,0))

#----------------------------RANDOM FOREST-------------------------------------
library(randomForest)
set.seed(2021)
rand_forest = randomForest(resale_price~.,data = trainset,do.trace = T,
                           importance = T)

rf_ResalePrice_train = predict(rand_forest,newdata = trainset)
rf_ResalePrice_test = predict(rand_forest,newdata = testset)
rf_RMSE_train = rmse(trainset$resale_price,rf_ResalePrice_train)
rf_RMSE_test = rmse(testset$resale_price,rf_ResalePrice_test)

RMSE_dataframe[3,] = c("Random Forest ",
                       round(rf_RMSE_train,0),round(rf_RMSE_test,0))

#----------------------------------QUESTION 5 ------------------------------------
oob_mse = tail(rand_forest$mse,1)
oob_rmse = sqrt(oob_mse)
round(oob_rmse,0)

#-------------END OF SCRIPT--------------------------------------------------------

