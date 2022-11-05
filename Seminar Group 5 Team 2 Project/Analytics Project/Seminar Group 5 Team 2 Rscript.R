setwd("D:/Users/venkat/Desktop/business/YEAR 2 SEM 2/BC2407/BC2407_project")
library(data.table)
marketing_data = fread("bank.csv",stringsAsFactors = T)
marketing_data$month = factor(marketing_data$month,levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))


summary(marketing_data)
View(marketing_data)
library(ggplot2)
normalise = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

z_normalise = function(x)
{
  return((x-mean(x))/sd(x))
}

#---------------------DATA CLEANING----------------------------------------------
# need to clean the data to remove the unknown factors 
summary(marketing_data)
marketing_data[balance<0,.N]
# there is 688 rows that have negative balance in their account
# we are assuming that the customer has made a purchase greater than their account balance
# so the bank has to lend them money

marketing_data[poutcome=="unknown"&previous!=0,.N]
marketing_data[poutcome=="unknown"&previous!=0]
# not all the cases that were unknown were engaged for the first time
# need to make a assumption regarding this, so when they say its unknown
# they were not able to get the outcome of the campaign before the end of the call
# if that is the case what does it mean when we are talking about others? 
# is it something that falls outside of the normal categories. Does this mean a follow up is required ?

summary(marketing_data)


#find mode to replace categorical unknowns
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#education
modef=mode(marketing_data$education)
mode.df=data.frame(modef[1])
marketing_data$education_unknown = ifelse(marketing_data$education=="unknown",1,0)
marketing_data[education=="unknown",education:=mode.df]
marketing_data$education = factor(marketing_data$education)
marketing_data$education_unknown = as.logical(marketing_data$education_unknown)
summary(marketing_data$education_unknown)


#contact
modef=mode(marketing_data$contact)
mode.df=data.frame(modef[1])
marketing_data$contact_unknown = ifelse(marketing_data$contact=="unknown",1,0)
marketing_data[contact=="unknown",contact:=mode.df]
marketing_data$contact = factor(marketing_data$contact)
marketing_data$contact_unknown = as.logical(marketing_data$contact_unknown)
summary(marketing_data$education_unknown)
summary(marketing_data$contact)


#job
modef=mode(marketing_data$job)
mode.df=data.frame(modef[1])
marketing_data$job_unknown = ifelse(marketing_data$job=="unknown",1,0)
marketing_data[job=="unknown",job:=mode.df]
marketing_data$job = factor(marketing_data$job)
marketing_data$job_unknown = as.logical(marketing_data$job_unknown)
summary(marketing_data$job_unknown)
summary(marketing_data$job)



####BI Variate Data exploration########################################
summary(marketing_data)
# Majority of the people targeted is management, age between 35-40,married,secondary educated
#low balance , those that have cellular phone, contacted during may

# checking the proportion of deposit yes and no cases-----------------------------
ggplot(marketing_data, aes(deposit)) +
  geom_bar(fill="#FF7068") +
  labs(title = "Proportion of Deposit Cases: NO vs YES", y="Number of cases") +
  scale_x_discrete(labels=c("No","Yes"))
# the cases are about the same so there is no need to balance the data set 

#We we will look of the distribution of age with the deposit status---------------------
ggplot(marketing_data, aes(x=age, y=deposit)) + labs(title = "Age against Deposit")+
  geom_violin(trim=FALSE)
# the range of age is from 18 to 95 which seems to be reasonable, so no cleanup needed
# Observation : The median seems to be the same between yes and no which is about 30-40
plot(marketing_data$deposit,marketing_data$age)
# Those that said yes tend to have a larger range of age than those that said no
# this is because of the retired people saying yes ( job distribution)
# But the distribution seems to be around the same

# next variable will be regarding the job type --------------------------------
ggplot(marketing_data, aes(job)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "Job against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "Deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068")) 
# those who accepted the marketing campaign turns out to be students, and retired, unemployed 
# why did students and retired chose to deposit even though they do not have a stable income

#-> If that is the case why is the median of age 30 something for yes
# Most of the people who say no are in housemaid, entrepreneur and blue collar
#-> maybe income will play a part in why they accept, need to look into it 

# next variable will be marital status ----------------------------------------
ggplot(marketing_data, aes(marital)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "Marital against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "Deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# divorced is equal, married tend to say no and single tend to say yes
# Why did most of the single people agree to the deposit?

#-----------------MArital status against JOB-----------------------------------------
# is it because most of the single people are students? 
ggplot(marketing_data, 
       aes(x = marital, 
           fill = job)) + 
  geom_bar(position = "dodge")
# most of the single people are not students, most of the single people are
# management and technician
# management tend to be equal but technicians tend to say no
# so why did most of the single people said yes

# if we look at the married side, most of the people are blue-collar and management
# the count of those that were married are far greater than those of single
# those are the jobs that tend to say no as well


# most of the jobs that tend to say no are mostly married
# the outlier from that trend is retired and unemployed, even though most of them are married
# they said yes

ggplot(marketing_data, 
       aes(x = job, 
           fill = marital)) + 
  geom_bar(position = "dodge")

# so why did retired and unemployed tend to say yes even though most of them are married?


#next variable will be regarding education --------------------------------
ggplot(marketing_data, aes(education)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "education against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "Deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# those who have only completed primary and secondary tend to say
# was the result in the jobs because of their education , and their lack of knowledge deters them from accepting the marketing campaign 

#------------------------JOB vs education-----------------------------------------
# were retired and unemployed tend to be more educated so they say yes?
ggplot(marketing_data, 
       aes(x = job, 
           fill = education)) + 
  geom_bar(position = "dodge")

# management have most of them studying tertiary, so why did they say no 
# is it because of other commitments they have due to marriage

# students, retired and unemployed tend to be only have secondary education
# and secondary tend to say no, so why did they say yes? 
# The students might be because of marital status but the rest 2?
# could it be due to balance?


# the next variable is default --------------------------------

#proportion of default
ggplot(marketing_data, aes(default)) +
  geom_bar(fill="#FF7068") +
  labs(title = "Proportion of default Cases: NO vs YES", y="Number of cases") +
  scale_x_discrete(labels=c("No","Yes"))
# the number of defaulted case is severely low

# number of cases defaulted
summary(marketing_data$default)

#------------------------ JOB vs default---------------------------------------
# what is the jobs of these people who have defaulted?
ggplot(marketing_data[marketing_data$default=="yes"], aes(job)) +
  geom_bar(fill="#FF7068") +
  labs(title = "Proportion of default Cases: NO vs YES", y="Number of cases")

# those that defaulted tend to be in management, blue-collar and technician
# this could be one of the reasons to why those jobs tend to say no
# due to the high default rate
# those that tend to say no have a low education standard


#deposit v default----------------------------------------------------------
ggplot(marketing_data, aes(default)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "default against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "Deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# those who have defaulted their account tend to say no and those that have not defaulted have a equal probability of accepting
# this might be because of their balance in the bank which is affected by their job which is based on thier education level

# the next variable is balance  --------------------------------
ggplot(marketing_data, aes(x=balance, y=deposit)) + labs(title = "Balance against Deposit")+
  geom_violin(trim=FALSE)

library(e1071)
skewness(marketing_data$balance)
summary(marketing_data$balance)



library(LambertW)
marketing_data$balance_log = log(Gaussianize(marketing_data$balance+6848))
# very skewed data might have to do log transformation
# have a negative balance -> so might have to differently 

ggplot(marketing_data, aes(x=balance_log, y=deposit)) + labs(title = "Balance against Deposit")+
  geom_violin(trim=FALSE)

plot(marketing_data$deposit,marketing_data$balance_log)
# those that said no tend to have more outliers
# those that said yes tend to have a higher balance than those that said no
skewness(marketing_data$balance_log)
summary(marketing_data$balance_log)

qqnorm(marketing_data$balance, pch = 1, frame = FALSE)
qqline(marketing_data$balance, col = "steelblue", lwd = 2)

#--------------------- JOB vs BAlance----------------------------------------
plot(marketing_data$job,marketing_data$balance_log)
# retired people have the highest median balance
# followed by unemployed people
library(dplyr)
plotdata <- marketing_data %>%
  group_by(job) %>%
  summarize(median_balance = median(balance))

ggplot(plotdata, 
       aes(x = job, 
           y = median_balance)) +
  geom_bar(stat = "identity")

# we do not use mean in this case, as it has a lot of outliers and will ruin the data
# so we look at the median of the various jobs
# the median for retired and unemployed are the highest
# this might be the reason to why they accept even though they are married

# as for management, it could be due to the default status as well

# the next variable is housing --------------------------------
ggplot(marketing_data, aes(housing)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "housing loan against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "Deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# those who said yes tend to not have housing loan-> so maybe they have more funds to deposit in bank
# those who said no tend to have housing loan-> maybe they are afraid they cannot afford the deposit with the loan and all

# the next variable is loan --------------------------------

ggplot(marketing_data, aes(loan)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "personal loan against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "Deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# the distribution across loan is the same as the housing loan -> so does this mean the effect of both loans is negligible ?
# maybe can create new column to see if having both loan will make a difference 
marketing_data$general_loan = ifelse(marketing_data$loan=="yes" & marketing_data$housing=="yes"
                                     ,"Both loans",ifelse(marketing_data$loan=="no"&marketing_data$housing=="no",
                                                          "no loans",
                                                          ifelse(marketing_data$loan=="yes","only personal loan","only housing loan")))
marketing_data$general_loan = factor(marketing_data$general_loan)
summary(marketing_data$general_loan)

ggplot(marketing_data, aes(general_loan)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "loan against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "Deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# those that said yes tend to have no loans at all 
# people who have personal loans tend to decline more than those that have housing loan
# but both loans will make them decline the offer even more

#-------------------------- Job vs general loans------------------------------------
# why did management say no , even though they have a high median pay
ggplot(marketing_data, 
       aes(x = job, 
           fill = general_loan)) + 
  geom_bar(position = "dodge")

# the loans could also be the reason to why retired, unemployed and student said yes
# they all do not have any loans

# but at the same time management and technician also have no loans as the majority
# followed by housing loans
# could this be due to marital status ?

#--------------------MARITAL VS LOANS-------------------------------------------
ggplot(marketing_data, 
       aes(x = marital, 
           fill = general_loan)) + 
  geom_bar(position = "dodge")
# those that were married tend to have have no loans
# but from earlier we saw that married people said no
# so why do married people say no ?

# MARITAL, JOB against balance-----------------------------------------------
ggplot(marketing_data, aes(job, balance_log, fill=marital)) +
  geom_boxplot() + 
  labs(title = "Impact of job and balance against marital") 

# if we look at management, those that were single have higher balance than those married and divorced
# And if we look at retired and unemployed married couples they have higher balance than the other 2


#-----Balance , JOB against deposit----------------------------------------------
# does the balance and the type of job affect the outcome variable?
ggplot(marketing_data, aes(job, balance_log, fill=deposit)) +
  geom_boxplot() + 
  labs(title = "Impact of job and balance against deposit") 

# those that said no actually have lesser balance than those people who said yes
# balance might play a very important role 



# next variable is contact  --------------------------------
ggplot(marketing_data, aes(contact)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "contact against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# those with telephone tend to say yes when compared to cellphone

ggplot(marketing_data, 
       aes(x = contact, 
           fill = deposit)) + 
  geom_bar(position = "dodge")
# in detail, telephone doesn't seem to have a difference. Those with cellular tend to say no



# the next variable is day --------------------------------
ggplot(marketing_data, aes(x=day, y=deposit)) + labs(title = "day against Deposit")+
  geom_violin(trim=FALSE)

# which date had a highest conversion rate
ggplot(marketing_data, aes(x = day, fill = deposit)) +
  geom_bar(position = "dodge")

ggplot(marketing_data, aes(day)) +
  geom_bar(position = 'fill', aes(fill = deposit))

ggplot(marketing_data, aes(x=day,fill=deposit)) + 
  geom_density(position = "dodge")

plot(marketing_data$deposit,marketing_data$day)
# the number of people that said yes on different days is relatively constant
# since we do not have different dates at which the customer was contacted, we need to assume that the customer was contacted was on the same day and same month 
# the number of people that said no differ on different days 




# the next variable is month  --------------------------------
ggplot(marketing_data, aes(month)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "month against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# most of the customer said yes on mar,sep,oct,dec,feb,apr
# most of the customer that said no is on jan,may



# next variable is duration  --------------------------------
ggplot(marketing_data, aes(x=duration, y=deposit)) + labs(title = "duration against Deposit")+
  geom_violin(trim=FALSE)
# the duration goes beyond 2000 minutes 
# and there is numerous calls as well 
# those that said no lasted very little, but those that said yes needed longer period
# we have to assume that the duration is the sum of all the previous calls 
# both the data points are very skewed so we have to do a log transformation 
# maybe the duration of the calls was longer during the holiday -> need to look into it 
plot(marketing_data$deposit,marketing_data$duration_log)
skewness(marketing_data$duration)

marketing_data$duration_log = log(marketing_data$duration)
skewness(marketing_data$duration_log)
summary(marketing_data$duration_log)
ggplot(marketing_data, aes(x=duration_log, y=deposit)) + labs(title = "duration against Deposit")+
  geom_violin(trim=FALSE)

# those that wanted to sign the deposit, had to talk longer than those who did not

# next variable is campaign --------------------------------

qqnorm(marketing_data$campaign, pch = 1, frame = FALSE)
ggplot(marketing_data, aes(x=campaign, y=deposit)) + labs(title = "campaign calls against Deposit")+
  geom_violin(trim=FALSE)

# the campaign is the number of time they were contact during the current marketing campaign
skewness(log(marketing_data$campaign))
# Since the data is very skewed, we need to do a log transformation 
marketing_data$campaign_log = log(marketing_data$campaign)

ggplot(marketing_data, aes(x=campaign_log, y=deposit)) + labs(title = "campaign against Deposit")+
  geom_violin(trim=FALSE)

# The distribution across of campaigns for both outcomes are the same
# this would mean that the number of contacts for the current contact does not play such a important role
plot(marketing_data$deposit,marketing_data$campaign_log)
summary(marketing_data$campaign)
boxplot(marketing_data$campaign_log)

boxplot(marketing_data$campaign)

# next is pday and previous---------------------------------
ggplot(marketing_data, aes(x=pdays, y=deposit)) + labs(title = "pdays against Deposit")+
  geom_violin(trim=FALSE)
# the distribution is around the same for both outcomes
# but the distribution is very skewed
skewness(marketing_data$pdays)

skewness(log(marketing_data$pdays+2))

qqnorm(marketing_data$pdays, pch = 1, frame = FALSE)


# skewness still greater than 1, better to bin
library(OneR)
summary(marketing_data$pdays)
summary(marketing_data$previous)
boxplot(marketing_data$pdays)

# the customer who have been targeted before tend to be -1 for pdays and preivous is 0

# we are going to split the data into 2 portions
# customers who have been newly targeted and those who have been targeted before

marketing_data$customer_status = ifelse(marketing_data$pdays==-1,"Never contacted before",
                                        "contacted before")
marketing_data$customer_status = factor(marketing_data$customer_status)
summary(marketing_data$customer_status)


ggplot(marketing_data, aes(customer_status)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "pdays_bin against Deposit", y = "Proportion of cases") 

# those that were targeted for the first time tend to say
# while those that were targeted before tend to say yes
# why is there this difference??????
ggplot(marketing_data, aes( customer_status,balance_log)) +
  geom_boxplot()
# those who have been contacted before have a higher balance


# the next variable is poutcome  --------------------------------
ggplot(marketing_data, aes(poutcome)) +
  geom_bar(position = 'fill', aes(fill = deposit)) +
  labs(title = "poutcome against Deposit", y = "Proportion of cases") +
  scale_fill_manual(name = "deposit", labels = c("No","Yes"),
                    values=c("#02AEAE", "#FF7068"))
# those outcomes that were success,and other will accept the current campaign as well
# those that were not contacted will most likely decline the offer as well
# those that declined the previous offer will most likely decline this offer as well



# duration against different customer segments---------------------------------
ggplot(marketing_data, aes(customer_status,duration_log)) +
  geom_boxplot() 
# the duration across the different customer segments is around the same
# with more outliers in the contacted before segment


# from earlier analysis we saw that single people tend to say -> which is student
# and married retired people and unemployed said yes because they have no loans
# and their balance is higher than the rest of the marital status
# but if we looked at entrepreneur and technician they follow the same suit
# could it be due to the status 

#-----------------------------------Jobs against status-------------------------
ggplot(marketing_data, 
       aes(x = job, 
           fill = customer_status)) + 
  geom_bar(position = "dodge")

# most of them were not contacted before

# job  vs loans and balance with marital-------------------------------------------------------
ggplot(marketing_data[marital=="married",], aes(job, balance_log, fill=general_loan)) +
  geom_boxplot() + 
  labs(title = "Impact of job and balance against deposit")



# BALANCE VS DURATION on deposit-------------------------------------------------------
ggplot(marketing_data, aes(balance_log, duration_log, fill=deposit)) +
  geom_smooth(method=lm, alpha=0.2)
# one has a positive relationship and the other has a negative relationship
# so there might be other factors affecting it, so we generalized it
marketing_data$balance_per_duration = marketing_data$balance_log / marketing_data$duration_log
skewness(marketing_data$balance_per_duration)
# still very skewed so we will normalize this variable
marketing_data$balance_per_duration = Gaussianize(marketing_data$balance_per_duration)
skewness(marketing_data$balance_per_duration)

ggplot(marketing_data, aes(x=balance_per_duration, y=deposit)) + labs(title = "balance_per_duration against Deposit")+
  geom_violin(trim=FALSE)
# we can see a clear distinction, those that said yes mostly said yes when
# balance_per_duration is lesser than 1.5
# where as those who said no are more spread out

#job vs month-------------------------------------------------------------------

ggplot(marketing_data, aes(month)) +
  geom_bar(position = 'fill', aes(fill = job))
labs(title = "month against job", y = "Proportion of cases")

ggplot(marketing_data,
       aes(x = month,
           fill = job)) +
  geom_bar(position = "dodge")
# during the month of may they targeted blue collar job so maybe thats why they have low yes cases
# Management were targeted the most, but after that is retired which tends to say yes
# same for oct


# could it be that the months that were very popular had a higher duration

# Month vs duration on deposit---------------------------------------------------
ggplot(marketing_data, aes(month, duration_log, fill=deposit)) +
  geom_boxplot() +
  labs(title = "Impact of month and duration against deposit")
# THe people who said yes required more duration than those who said no in any of the month

# did the popular months have more campaign calls than the rest?

# Month vs campaign  on deposit -------------------------------------------------
ggplot(marketing_data, aes(month, campaign_log, fill=deposit)) +
  geom_boxplot() +
  labs(title = "Impact of month and campaign against deposit")

# it seems that the campaign calls during the different months did not affect the result
# the popular months such as march, seo, oct, dec have the same median


# could it be that the people who were targeted in the specific months have a higher balance than the rest 
# month vs balance on deposit-------------------------------------------------------
ggplot(marketing_data, aes(month, balance_log)) +
  geom_boxplot() +
  labs(title = "Impact of month and campaign against deposit")
# the customers who were targeted during sep and oct have the same distribution 
# but the median balance of nov is much higher than mar and dec
# so why did mar and dec are more successful than nov

# it was seen from previous analysis that during nov those jobs that send to say no were targeted
# and the month of mar and dec jobs who say yes were prevalent

# could this be due to the loans together with marital status affect?

# month against loans on deposit------------------------------------------------
ggplot(marketing_data, aes(month)) +
  geom_bar(position = 'fill', aes(fill = general_loan))
labs(title = "month against job", y = "Proportion of cases")

ggplot(marketing_data,
       aes(x = month,
           fill = general_loan)) +
  geom_bar(position = "dodge")

# most of the popular months targeted people with no loans
# but months like jan, jul, and jun have more no loans than housing 
# so why did most of them said no
# maybe because of this the proportion of yes and no during these months were smaller
# august had one of the highest proportion of cases whereby no loans were targeted so why did most of them say no

#august had one of the lowest balance 

# months against balance vs months-------------------------------------------------------
ggplot(marketing_data, aes(month, balance_log, fill=general_loan)) +
  geom_boxplot() + 
  labs(title = "Impact of job and balance against deposit") 
# during the month of august people who were targeted even though have no loans have the lowest balance


# months against customer status-----------------------------------------------------
ggplot(marketing_data,
       aes(x = month,
           fill = customer_status)) +
  geom_bar(position = "dodge")


# the three months that was earlier stated-> it target mostly that was not contacted before
# and this customer segment tend to say no
# but march also targeted people who have not been targeted before

# why do the customers who have not been targeted before tend  to say no 


# customer status on loans on deposit --------------------------------------------------------
ggplot(marketing_data,
       aes(x = customer_status,
           fill = general_loan)) +
  geom_bar(position = "dodge")+facet_wrap(~deposit)

# the type of loans affected the outcome of the different customer segments
# those who said yes regardless of the segment
# those who were not targeted before -> alot of them had no loans as well
# so why did they said no

#--------------------Balance with loans vs segment on deposit----------------------------------------

ggplot(marketing_data, aes(deposit, balance_log, fill=general_loan)) +
  geom_boxplot() +
  labs(title = "Impact of job and balance against contact")+facet_wrap(~customer_status)

# even though those customer who have not been contact and have no loans they have
# lower balance, the median is around the same as those with housing loan 


# why are some days more popular than the rest

# days against status -------------------------------------------------------------

ggplot(marketing_data,
       aes(x = day,
           fill = customer_status)) +
  geom_bar(position = "dodge")

# just from the customer status we are not able to see why were some dates more popular


# day with balance-----------------------------------------------------------------
ggplot(marketing_data, 
       aes(balance_log, color = deposit)) +
  geom_point(alpha = 0.8) +facet_wrap(~day)
# same result-> those with higher balance tend to say yes

ggplot(marketing_data, aes( balance_log)) +
  geom_boxplot() +
  labs(title = "Impact of job and balance against contact")+facet_wrap(~day)


ggplot(marketing_data, aes(balance_log, duration, fill=deposit)) +
  geom_smooth(method=lm, alpha=0.2)
# could it be due to the job?

# day against job---------------------------------------------------------------

ggplot(marketing_data,
       aes(x = deposit,
           fill = job)) +
  geom_bar(position = "dodge")+facet_wrap(~day)

# day vs age on deposit----------------------------------------------------------
ggplot(marketing_data, aes(age, day, fill=deposit)) +
  geom_smooth(method=lm, alpha=0.2)
# negative correlation between age on depsoit no
# but positive with deposit yes

marketing_data$age_per_day = marketing_data$age/marketing_data$day
skewness(marketing_data$age_per_day)
marketing_data$age_per_day = log(Gaussianize(marketing_data$age_per_day))
ggplot(marketing_data, aes(x=age_per_day, y=deposit)) + labs(title = "duration against Deposit")+
  geom_violin(trim=FALSE)
# the distribution is slightly different
# this could give us a better gauge when to target the customers as well

# DOES the previous outcome affect those who were targeted before?

#--------------------POUTCOME vs status----------------------------------------

ggplot(marketing_data[marketing_data$pdays!=-1,], aes(poutcome)) +
  geom_bar(position = 'fill', aes(fill = deposit))+
  labs(title = "poutcome with status against deposit", y = "Proportion of cases")

# for those customers who have been targeted before tend to say
# if the previous was success or other -> most likely they will say yes now

# so why did customers who were targeted before tend to say

# as seen previously majority of them do not have loans

# does campaign and previous play a part
# Previous vs campaign----------------------------------------------------------

ggplot(marketing_data[pdays!=-1,], 
       aes(x=previous, y=campaign, color = deposit)) +
  geom_point(alpha = 0.8) 
# there is a negative correlation
# those with more previous calls than the campaign tend to say

summary(marketing_data[previous<campaign & pdays!=-1,]) #(352/222)
summary(marketing_data[previous>campaign & pdays!=-1,]) # 1044/452
summary(marketing_data[previous==campaign& pdays!=-1,]) # 509/259

# it seems the relationship between campaign and previous do make a change 
marketing_data$calls_previous_customer = ifelse(marketing_data$pdays==-1,"Not targeted",
                                                ifelse(marketing_data$previous>=marketing_data$campaign,
                                                       "campaign<=previous",
                                                       "campaign>previous"))
                                                       

marketing_data$calls_previous_customer = factor(marketing_data$calls_previous_customer)
summary(marketing_data$calls_previous_customer)
 
# this column was introduced to see the relationship between the number of calls with
# campaign 

ggplot(marketing_data, aes(calls_previous_customer)) +
  geom_bar(position = 'fill', aes(fill = deposit))+
  labs(title = "previous comapred to campaing against deposit", y = "Proportion of cases")
# people tend to say yes when the current campaign calls are lesser or equal 
# to the previous calls

# if the campaign calls is greater than previous people tend to say no


# why do people tend to say yes when the previous is greater or equal than the campaign calls?

# previous_campaign against balance-----------------------------------------------

ggplot(marketing_data, aes(calls_previous_customer,balance_log)) +
  geom_boxplot() 
# the balance between the different factors are the same 
# so balance is not a important factor that affects those customers who have already been targeted

# calls_previous vs loans---------------------------------------------------------
ggplot(marketing_data,
       aes(x = calls_previous_customer,
           fill = general_loan)) +
  geom_bar(position = "dodge")
# the loans also have the same distribution

# maybe on of the reasons to why people who were called more during this campaign
# tend to say no is because, regular calling will make them annoying and
# therefore they will be irritated and say no
# this should be reflected in a shorter duration of calls

# duration vs calls_previous----------------------------------------------------
ggplot(marketing_data, aes(calls_previous_customer,duration_log)) +
  geom_boxplot() 
# the last duration of the call is around the same regardless of the category
# but there are more outliers in the lower tail than on the upper tail 


#age vs calls_previous---------------------------------------------------------
ggplot(marketing_data, aes(calls_previous_customer,age)) +
  geom_boxplot() 
# it is relatively the same, the IQR is smaller-> implying people who were 
#younger were targeted which are more likely to be irritated 


# marital status vs calls_previous with deposit-----------------------------------
ggplot(marketing_data,
       aes(x = calls_previous_customer,
           fill = marital)) +
  geom_bar(position = "dodge")+facet_wrap(~deposit)

# it has relatively the same distribution



#----------------------END OF EXPLORATORY ANALYSIS--------------------------------
# currently the model classify the customer based on whether have they been contacted before or not
# but if everyone was not contacted before or everyone has contacted before , there will be no customer segement
# So we plan to create a more general cluster based on the customer's information

# Before contacting the customer, we do not know the duration, campaign(if they are newly contacted)
# the previous(if they are newly contacted) and the day to contact

# the only numeric variable available to use even before calling the customer is 
# balance and age, so we will be clustering balance and age together
# to create a more general customer segmentation


#clustering----------------------------------------------------------------------
library(cluster)
library(factoextra)
# finding the optimal number of cluster
library(dplyr)
set.seed(1953)
marketing_data_NA_1 = select(marketing_data,1,21)
fviz_nbclust(marketing_data_NA_1, kmeans, method = "wss")
# optimal number of clusters seems to be 3

customer_segment = kmeans(marketing_data_NA_1  ,centers = 3,nstart = 25)

fviz_cluster(customer_segment, data = marketing_data_NA_1)

marketing_data_NA_1 = NULL
marketing_data$cluster_id = customer_segment$cluster
marketing_data$cluster_id = factor(marketing_data$cluster_id)

# impact of these customer segment on deposit-------------------------------------
ggplot(marketing_data, aes(cluster_id)) +
  geom_bar(position = 'fill', aes(fill = deposit))
labs(title = "cluster against deposit", y = "Proportion of cases")

# those customer who belong to cluster 2 regardless of previous contact tend to say


# MODELLING----------------------------------------------------------------

#logistic regression------------------------------------------------------
library(caTools)
set.seed(1953)
train = sample.split(Y = marketing_data$deposit, SplitRatio = 0.7)
trainset <- subset(marketing_data, train == T)
testset <- subset(marketing_data, train == F)

logistic = glm(deposit~.,family =binomial, data = trainset)
summary(logistic)

# optimizing logistic model-----------------------------------------------
logistic = step(logistic)
summary(logistic)
# predicting the outcome-----------------------------------------------
prob_norm <- predict(logistic, newdata = testset, type = 'response')
testset_logistic = testset
testset_logistic$prob = prob_norm
plot(testset_logistic$deposit,testset_logistic$prob)

# it seems that a threshold of 0.5 is suitable for this model
y.hat_norm <- ifelse(prob_norm > 0.5, "Yes", "No")

table(testset_logistic$deposit, y.hat_norm, deparse.level = 2)

# Evaluating the model------------------------------------------------------------

# accuracy---------------------------------------------------------------------
(1307+1487)/3349
# accuracy is 83.4 percent

# sensitivity----------------------------------------------------------------
# proportion of positive cases predicted accurately
1307/(1307+280)
# 82.4 %

# specificity-----------------------------------------------------------------
1487/(1487+275)
# 84.4%

# could finding another threshold make the accuracy of the model better ?---------
library(rpart)
library(rpart.plot)
set.seed(1953)
thres = rpart(deposit~prob,data = testset_logistic ,
                 method = 'class',control = rpart.control(minsplit = 2,cp = 0))

plotcp(thres)

cv_errorcap = thres$cptable[which.min(thres$cptable[,"xerror"]),"xerror"]+
  thres$cptable[which.min(thres$cptable[,"xerror"]),"xstd"]
i = 1
j = 4
while(thres$cptable[i,j]>cv_errorcap)
{
  i = i+1
}
optimal_cp = ifelse(i>1,sqrt(thres$cptable[i,1] * thres$cptable[i-1,1]),1)
optimal_cp 

thres <- prune(thres, cp = optimal_cp)
rpart.plot(thres)
# threshold of 0.4 would be better than 0.5
y.hat_norm_1 <- ifelse(prob_norm > 0.47, "yes", "no")

table(testset_logistic$deposit, y.hat_norm_1, deparse.level = 2)
#accuracy 
(1465+1338)/3349 #83.7
# specificity
1465/(1465+297) #83.1
# sensitivity
1338/(1338+249) # 84.3





#----------------MARS MODELLING--------------------------------------------------
# mars model without any tuning
set.seed(1953)
library(earth)
mars <- earth(deposit~., data=trainset,
                    glm=list(family=binomial), trace=3)

summary(mars)
mars_test = testset
mars_test$predicted = predict(mars,newdata = testset,type = "response")
summary(mars_test$predicted)
plot(mars_test$deposit,mars_test$predicted)
# threshold of 0.5 is suitable for this data set
mars_test$predicted = ifelse(mars_test$predicted<0.5,"no","yes")

table(mars_test$deposit, mars_test$predicted, deparse.level = 2)
# what are the important variables selected--------------------------------
summary(logistic)

# evaluating the effectiveness of the model------------------------------------
#accuracy
(1494+1313)/3349 # 83.8 
# sensitivity
1313/(1313+274) # 82.7
# sepecificity
1494/(1494+268) #84.8

# trying to tune the mars model using 10 fold cross validation 
set.seed(1953)
library(caret)
hyper_grid <- expand.grid(
       degree = 1:3, 
       nprune = seq(2, 100, length.out = 10) %>% floor()
   )
tuned_mars <- train(
  x = subset(trainset, select = -deposit),
  y = trainset$deposit,
  method = "earth",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

mars_test$predicted_tune = predict(tuned_mars$finalModel,newdata = testset,type = "response")

plot(mars_test$deposit,mars_test$predicted_tune)

# using cart to predict the optimal threshold
library(rpart)
library(rpart.plot)
set.seed(1953)
thres_1 = rpart(deposit~predicted_tune,data = mars_test ,
              method = 'class',control = rpart.control(minsplit = 2,cp = 0))

plotcp(thres_1)

cv_errorcap = thres_1$cptable[which.min(thres_1$cptable[,"xerror"]),"xerror"]+
  thres_1$cptable[which.min(thres_1$cptable[,"xerror"]),"xstd"]
i = 1
j = 4
while(thres_1$cptable[i,j]>cv_errorcap)
{
  i = i+1
}
optimal_cp = ifelse(i>1,sqrt(thres_1$cptable[i,1] * thres_1$cptable[i-1,1]),1)
optimal_cp 

thres_1 <- prune(thres_1, cp = optimal_cp)
rpart.plot(thres_1)
#optimal threshold is 0.39

predicted_tune = ifelse(mars_test$predicted_tune<0.39,"no","yes")

table(testset$deposit, predicted_tune, deparse.level = 2)

# what are the important factors selected----------------------------------------
summary(tuned_mars)
# evaluating the model------------------------------------------------
#accuracy
(1427+1444)/3349 # 85.7
# sensitivity
1444/(1444+143) # 90.1
# specificity
1427/(1427+335) # 80.1

exp(tuned_mars$finalModel$coefficients)




# CART model---------------------------------------------------------------------
# for cart model we will be training the model with the data with missing values
# due to its ability to use surrogate to predict the outcome

# Growing the tree----------------------------------------------------------------------------------------
library(rpart.plot)
library(rpart)
set.seed(1953)

cart_train = trainset
cart_train$job[cart_train$job_unknown==T] = NA
cart_train$job_unknown= NULL

cart_train$contact[cart_train$contact_unknown==T]=NA
cart_train$contact_unknown = NULL

cart_train$education[cart_train$education_unknown==T]=NA
cart_train$education_unknown=NULL

max_tree = rpart(deposit~.,data = cart_train ,
                 method = 'class',control = rpart.control(minsplit = 2,cp = 0))

# Check the pruning sequence and 10-fold CV errors--------------------------------------------------------


#-------------------------------------------------------------------
cv_errorcap = max_tree$cptable[which.min(max_tree$cptable[,"xerror"]),"xerror"]+
  max_tree$cptable[which.min(max_tree$cptable[,"xerror"]),"xstd"]
i = 1
j = 4
while(max_tree$cptable[i,j]>cv_errorcap)
{
  i = i+1
}
optimal_cp = ifelse(i>1,sqrt(max_tree$cptable[i,1] * max_tree$cptable[i-1,1]),1)
optimal_cp 

# Pruning the tree to optimal size (based on 1SE rule)----------------------------------------------------
prune_train <- prune(max_tree, cp = optimal_cp)


# Plotting the tree and checking the variable importance--------------------------------------------------
rpart.plot(prune_train, nn= T, main = "Pruned Tree 4")

prune_train$variable.importance

# Predicting the test set with the trained model----------------------------------------------------------
predict_test <- predict(prune_train, newdata = testset, type = "class")

table(testset$deposit, predict_test, deparse.level = 2)
# evaluating the CART model-------------------------------------------------------
#accuracy
(1431+1373)/3349 # 83.7
# sensitivity
1373/(1373+214) # 86.5

# specificity
1431/(1431+331)# 81.2


# RANDOM FOREST---------------------------------------------------------------
# train random forest based on the normal dataset with the unknown values
library(randomForest)
set.seed(1953)
rand_forest = randomForest(deposit~.,data = trainset)
# checking if we need to increase the number of trees
plot(rand_forest)
rand_forest
# the default 500 seems sufficient to stabilize the error
varImpPlot(rand_forest)

# predicting the outcome variable
predicted_deposit = predict(rand_forest,testset)
table(testset$deposit, predicted_deposit, deparse.level = 2)
# evaluating the model-----------------------------------------------------------
#accuracy
(1453+1418)/3349 # 85.7
# sensitivity
1418/(1418+169) # 89.4
# specificity
1453/(1453+309) # 82.5


# seeing if filling the data using the rfimpute will improve the accuracy-----------------
set.seed(1953)
rand_train = rfImpute(deposit~.,data = cart_train)
rand_forest_1 = randomForest(deposit~.,data = rand_train)
rand_forest_1
# the out of bag error is higher in this case

#evaluating the model
predicted_deposit_1 = predict(rand_forest_1,testset)
table(testset$deposit, predicted_deposit_1, deparse.level = 2)

#acccuracy
(1442+1403)/3349 # 84.9

#sensitivity
1403/(1403+184) #88.4

#specificity 
1442/(1442+320) # 81.8

# by making use of the rfimpute it has worsen the accuracy of the model and the sensitivity

# therefore it seems making use of the actual dataset is better for random forest


# from the plot earlier we saw that 500 trees are okay since the errors stablise after
# after 200

# now to identify the optimal number of variables to be selected at each level
tuneRF(x = subset(trainset,select = -deposit),y = trainset$deposit,ntreeTry = 500
       ,plot = T)
# the lowest oob error occurs when mtry = 10

# tuning the random forest using caret and the new value of mtry
library(caret)
control <- trainControl(method="cv",number = 10)
set.seed(1953)
mtry <- 10
rf_random <- train(deposit~., data=trainset, method="rf", metric="Accuracy" ,trControl=control)
print(rf_random)
plot(rf_random)

predicted_deposit_2 = predict(rf_random,testset)
table(testset$deposit, predicted_deposit_2, deparse.level = 2)

# evaluating the tuned random forest----------------------------------------------
#accuracy
(1465  +1409)/3349 # 85.8 %

#sensitivity
1409/(1409+178) # 88.7 %

# specificity 
1465  /(1465  +297) # 83.1 %


# Neural Network----------------------------------------------------------------
# training nnet
library(caret)
library(nnet)
set.seed(1953)
nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
nnmodel <- train(
  x = subset(trainset, select = -deposit),
  y = trainset$deposit,
  method = "nnet",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10)
)
nnmodel$finalModel
plot(nnmodel)

nnmodel_predict = predict(nnmodel,testset)
table(testset$deposit, nnmodel_predict, deparse.level = 2)

# evaluating the model--------------------------------------------------------
# accuracy
(1402+1338)/3349 # 81.8

#sensitivity
1338/(1338+249) # 84.3

# specificity
1402/(1402 +360) # 79.5


# will normalizing all the numeric data points make the model better

x_train = select(trainset,age,balance,duration,day,campaign, pdays,previous,
                 balance_log,duration_log, balance_per_duration,age_per_day,campaign_log)
x_train = as.data.frame(lapply(x_train, normalise))
x_train$job = trainset$job
x_train$marital = trainset$marital
x_train$education = trainset$education
x_train$default = trainset$default
x_train$housing  = trainset$housing
x_train$loan = trainset$loan
x_train$contact = trainset$contact
x_train$month = trainset$month
x_train$deposit = trainset$deposit
x_train$poutcome= trainset$poutcome
x_train$education_unknown = trainset$education_unknown
x_train$contact_unknown = trainset$contact_unknown
x_train$general_loan = trainset$general_loan
x_train$job_unknown = trainset$job_unknown
x_train$customer_status = trainset$customer_status
x_train$calls_previous_customer = trainset$calls_previous_Customer
x_train$cluster_id = trainset$cluster_id


nnmodel_3 <- train(
  x = subset(x_train, select = -deposit),
  y = x_train$deposit,
  method = "nnet",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid=nnetGrid
)

nnmodel_predict_3 = predict(nnmodel_3,testset)
table(testset$deposit, nnmodel_predict_3, deparse.level = 2)
# accuracy 
(503 +1290)/3349 #53.5
# sensitivity 
1290/(1290+297) # 81.2


# normalizing the entire dataset made the accuracy of the model even worse




# maybe using only the relevant attributes improve the accuracy of the nnet
# using the 4 variables selected by random forest
library(dplyr)
x_train = select(trainset,duration_log,duration,balance_per_duration,month,deposit)

set.seed(1953)

nnmodel_1 <- train(
  x = subset(x_train, select = -deposit),
  y = x_train$deposit,
  method = "nnet",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = nnetGrid
)

nnmodel_predict_1 = predict(nnmodel_1,testset)
table(testset$deposit, nnmodel_predict_1, deparse.level = 2)

# accuracy 
(1302+1245)/3349 #76.0
1245/(1245+342) # 78.4

# will normalizing these variables change the accuracy
x_train = as.data.frame(lapply(select(x_train,-deposit,-month,), normalise))
x_train$deposit = trainset$deposit
x_train$month = trainset$month

set.seed(1953)
nnmodel_2 <- train(
  x = subset(x_train, select = -deposit),
  y = x_train$deposit,
  method = "nnet",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = nnetGrid
)


nnmodel_predict_2 = predict(nnmodel_2,testset)
table(testset$deposit, nnmodel_predict_2, deparse.level = 2)
#accuracy
1587/3349 # 47.4
# sensitivity is 100 percent
# by using the important 11 variables selected from random forest we are able to achieve 
# highest sensitivity from normalization

# normalizing the variables greatly improved the sensitivity
# but it caused alot more false positive cases as well-> causing the accuracy to drop







#----------------------------END OF RSCRIPT-------------------------------------
