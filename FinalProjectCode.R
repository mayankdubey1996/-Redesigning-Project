library(naniar)
library(onehot)
library(randomForest)
library(caret)
library(e1071)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrgram)
library(glmnet)
library(naniar)


########################### READING CSV FILE ##############################

getwd()
setwd('D:/STAT 515/Final project')

########################### READING CSV FILE ##############################

df =  read.csv('suicidedataextrafestures.csv')

######################### SUMMARY #########################################

str(df)
summary(df)

###################### RENAMING DATA FRAME#################################

names(df) = c('country', 'year', 'sex', 'age', 'suicides_no', 'population','suicidesper100k', 'country-year', 'yearlyHDI',
              'GDPpyear','GDPpcapita', 'generation','suicide%','Internetusers','Expenses','employeecompensation','Unemployment','Physiciansp1000',
              'Legalrights','Laborforcetotal','Lifeexpectancy','Mobilesubscriptionsp100','Refugees', 'Selfemployed','electricityacess','secondarycompletion')

#################### FILTERING DATA SET BETWEEN 1995 AND 2013 ###############

df <- df %>%
  filter(year >= 1995 & year <= 2013)


str(df)
+
  summary(df)

############################# DATA VISUALIZATION ########################

#dev.off()

p1 = ggplot() + geom_bar(aes(x = reorder(age,suicidesper100k), y = suicidesper100k, fill = sex), data = df,
                         stat="identity")+
  scale_fill_manual(values=c('#f03b20','#2c7fb8'))+
  labs(x = "Different Age groups", y = "Suicides Rate(Per 100k)",
       title="Suicide rate per 100k in different age groups")
p1

d=df %>%
  group_by(country) %>% 
  summarize(suicidesper100k = mean(suicidesper100k)) %>%
  arrange(suicidesper100k) %>%
  top_n(9)

d = data.frame(d)


p2 =  ggplot(d,aes(x = reorder(country,suicidesper100k), y = suicidesper100k, fill = country )) +
  geom_bar(stat="identity")+ 
  coord_flip()+ 
  labs(x = "Country", y = "Average Suicides/100k", title="Top 9 countries")+
  scale_fill_manual(values = c('#fc9272' , '#fff5f0' , '#ef3b2c' , '#fee0d2' , '#fcbba1' , '#a50f15' , 
                               '#fb6a4a' , '#67000d' , '#cb181d'))
p2


d1 = df %>%
  group_by(country) %>%
  summarize(suicidesper100k = mean(suicidesper100k)) %>%
  arrange(suicidesper100k)%>%
  head(9)

p3 =  ggplot(d1,aes(x = reorder(country,suicidesper100k), y = suicidesper100k, fill = country )) +
  geom_bar(stat="identity")+ 
  coord_flip()+ 
  labs(x = "Country", y = "Average Suicides/100k", title="Least 9 countries")+
  scale_fill_manual(values = c('#78c679' , '#004529' , '#238443' , '#41ab5d' , '#ffffe5' , '#006837' , 
                               '#f7fcb9' , '#addd8e' , '#d9f0a3'))
p3

d2=df %>%
  group_by(year) %>%
  summarize(suicidesper100k = sum(suicidesper100k))

p4 = ggplot(d2,aes(x=year, y=suicidesper100k, fill = suicidesper100k ))+geom_point()+
  geom_smooth()+
  labs(x="Year",y= "Suicides/100k", title="Year wise trend of suicide rate")
p4

d3=df %>%
  group_by(year) %>%
  summarize(population = sum(population))


p5 = ggplot(d3,aes(x=year, y=population, fill = population ))+
  geom_point()+
  geom_smooth()+
  labs(x="Year",y= "population", title="Year wise trend of Population")
p5


d5=df %>%
  group_by(year) %>%
  summarize(Unemployment = sum(Unemployment))

p7 = ggplot(d5,aes(x=year, y=Unemployment, fill = Unemployment ))+
  geom_point()+
  geom_smooth()+
  labs(x="Year",y= "Unemployment", title="Year wise trend of Unemployment")
p7


########################## ANALYSIS OF NA ##################################### 

#----- REFUGEES ------

########## plotting missing values in Refugees country wise ##########
refugees = ggplot(df,aes(x = country,y = Refugees)) +
  geom_miss_point()+ labs(x= "Country", title="Missing values of Refugees (Country wise)")+
  coord_flip()
refugees


################ ANALYSISNG NA COUNTRY WISE ################

iceland = subset(df, country == 'Iceland')

hist(iceland$Refugees,xlab="Iceland's Refugees",main="Histogram of Iceland's Refugees")
plot(iceland$year, iceland$Refugees,xlab="Year",ylab="Iceland's Refugees"
     ,main="Year wise scatter plot of Iceland's Refugees")
summary(iceland$Refugees)

ireland = subset(df, country == 'Ireland')
hist(ireland$Refugees,xlab="Ireland's Refugees",main="Histogram of Ireland's Refugees")
plot(ireland$year, ireland$Refugees,xlab="Year",ylab="Ireland's Refugees"
     ,main="Year wise scatter plot of Ireland's Refugees")
summary(ireland$Refugees)
        
        
nz = subset(df, country == 'New Zealand')
nrow(nz)
hist(nz$Refugees,xlab="New Zealand's Refugees",main="Histogram of New Zealand's Refugees")
plot(nz$year, nz$Refugees,xlab="Year",ylab="New Zealand's Refugees"
     ,main="Year wise scatter plot of New Zealand's Refugees")


############################ IMPUTING NA's of Refugees ###################################################

df$Refugees[ is.na(df$Refugees) & df$country == 'Iceland' ] = median(iceland$Refugees, na.rm = TRUE)
df$Refugees[ is.na(df$Refugees) & df$country == 'Ireland' ] = 2
df$Refugees[ is.na(df$Refugees) & df$country == 'New Zealand' ] = 1

summary(df$Refugees)

################# ANALYSIS OF NA ->  INTERNET USER ######################

####### 1) AUSTRALIA ###############
p1 = ggplot(df,aes(x = country,y = Internetusers)) +
  geom_miss_point()+ labs(x= "Country",y="Internet Users", title="Missing values of Internet Users (Country wise)")+
  coord_flip()
p1


aus = subset(df, country == 'Australia')
mean(aus$Internetusers, na.rm = TRUE)
median(aus$Internetusers, na.rm= TRUE)
hist(aus$Internetusers,xlab="Australia's Internet users",main="Histogram of Australia's Internet users")
plot(aus$year, aus$Internetusers,xlab="Year",ylab="Australia's Internet users"
     ,main="Year wise scatter plot of Australia's Internet users")
summary(aus$Internetusers)

#df$Internetusers[ is.na(df$Internetusers) & df$country == 'Australia' ] = median(aus$Internetusers, na.rm= TRUE)


####### 2) Turkmenistan #############

tk = subset(df, country == 'Turkmenistan')
nrow(tk)
mean(tk$Internetusers, na.rm = TRUE)
median(tk$Internetusers, na.rm= TRUE)
hist(tk$Internetusers,xlab="Turkmenistan's Internet users",main="Histogram of Turkmenistan's Internet users")
plot(tk$year, tk$Internetusers,xlab="Year",ylab="Turkmenistan's Internet users"
     ,main="Year wise scatter plot of Turkmenistan's Internet users")
#df$Internetusers[ is.na(df$Internetusers) & df$country == 'Turkmenistan' ] = 0


############## EMPLOYEE COMPENSATION ##########################
emp = ggplot(df,aes(x = country,y = employeecompensation)) +
  geom_miss_point()+labs(x= "Country", y="Employee Compensation", 
                         title="Missing values of Employee Compensation
                                (Country wise)")+coord_flip()
emp

hist(df$employeecompensation,
     xlab="Employee Compensation",main="Histogram of Employee Compensation")
mean(df$employeecompensation,na.rm=T)
median(df$employeecompensation, na.rm=T)

plot(df$year,df$employeecompensation,xlab="Year",ylab="Employee Compensation"
     ,main="Scatter plot of Employee Compensation")

#df$employeecompensation[ is.na(df$employeecompensation) ] = median(df$employeecompensation, na.rm = TRUE)


####-------------------------------------------------------------------------------------------------------

####################### ELECTRICITY ACCESS ####################

electricity_access = ggplot(df,aes(x = country,y = electricityacess)) +
  geom_miss_point()+ labs(x= "Country", y="Electricity Access", 
                         title="Missing values of Electricity Access (Country wise)")+coord_flip()
electricity_access

hist(df$electricityacess,xlab="Electricity Access",main="Histogram of Electricity Access")
summary(df$electricityacess,)

plot(df$year,df$electricityacess,xlab="Year",ylab="Electricity Access"
     ,main="Scatter plot of Electricity Access")

#df$electricityacess[ is.na(df$electricityacess) ] = median(df$electricityacess, na.rm = TRUE)


######################################### FEATURE SELECTION ##################################

features= c('country', 'year','sex','age','population','GDPpyear','GDPpcapita','generation','employeecompensation','Unemployment',
            'Lifeexpectancy','Refugees','Selfemployed','Internetusers','suicidesper100k','electricityacess')
str(df)

#################################### Final Data frame ###########################

final = subset(df,select=features)
str(final)
final$year = as.factor(final$year)

#pairs(suicidesper100k ~ ., data = final, main="Simple Scatterplot Matrix")


cor_features= c('population','GDPpyear','GDPpcapita','employeecompensation','Unemployment',
            'Lifeexpectancy','Refugees','Selfemployed','Internetusers','suicidesper100k','electricityacess')

cor_df =  subset(final,select=cor_features)


#corrplot(corrgram(cor_df), method="number")

######################## ONE HOT ENCODING ############################

one_hot = dummyVars("~ .", data = final)
final_oh = data.frame(predict(one_hot, newdata = final))

View(head(final_oh))

############################################ RISK VARIBALE #############################################

hist(final$suicidesper100k)

median_risk = median(final$suicidesper100k)

final_oh$risk = ifelse(final_oh$suicidesper100k>= median_risk, 1, 0 )

final_oh = subset(final_oh, select = -c(suicidesper100k))

# ############### TRAIN TEST SPLIT ###############################

train = sample(1:nrow(final),0.7*nrow(final))
test = final$suicidesper100k[-train]
training <- final[train, ]
testing <- final[-train, ]

train = sample(1:nrow(final_oh),0.7*nrow(final_oh))
test = final_oh[-train]
training <- final_oh[train, ]
testing <- final_oh[-train, ]

########################## RANDOM FOREST #################################

training$risk = as.factor(training$risk)
testing$risk = as.factor(testing$risk)

rf = randomForest(risk ~. , data=training, importance=TRUE,
                  proximity=TRUE)

round(importance(rf), 2)

prediction = predict(rf, testing)

confusionMatrix(prediction, testing$risk)

##############################  LOGISTIC REGRESSION #######################

x = subset(training,select = -c(risk))
y = training$risk

x = as.matrix(x)

lm = glmnet(x,as.factor(y),family = "binomial")#round(importance(rf), 2)

xtr = subset(testing,select = -c(risk))
xtr = as.matrix(xtr)



prediction_lm = predict(lm, xtr ,type="class")

l = prediction_lm[,86]
confusionMatrix(as.factor(l), testing$risk)





