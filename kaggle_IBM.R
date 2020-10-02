library(plotrix)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(pyramid)

general1 <- datasets_1067_1925_WA_Fn_UseC_HR_Employee_Attrition
sum(is.na(general1))
str(general1)
sapply(general1, function(x) sum(is.na(x)))
general1$Attrition <- as.factor(general1$Attrition)
general1$BusinessTravel <- as.factor(general1$BusinessTravel)
general1$Department <- as.factor(general1$Department)
general1$Gender <- as.factor(general1$Gender)
general1$JobRole <- as.factor(general1$JobRole)
general1$MaritalStatus <- as.factor(general1$MaritalStatus)
general1$EducationField <- as.factor(general1$EducationField)
general1$Education <- as.factor(general1$Education)
general1$JobLevel <- as.factor(general1$JobLevel)
general1$StockOptionLevel <- as.factor(general1$StockOptionLevel)
general1$EnvironmentSatisfaction <- as.factor(general1$EnvironmentSatisfaction)
general1$JobSatisfaction <- as.factor(general1$JobSatisfaction)
general1$WorkLifeBalance <- as.factor(general1$WorkLifeBalance)
general1$JobInvolvement <- as.factor(general1$JobInvolvement)
general1$PerformanceRating <- as.factor(general1$PerformanceRating)
general1$OverTime <- as.factor(general1$OverTime)
str(general1) #after this point, only 
general1$EmployeeCount <- NULL #Every value is 1 so, we are droping this variable
general1$StandardHours <- NULL #Every value is 8 so, we are droping this variable
general1$Over18 <- NULL
str(general1)


####Exploring each variable in dataset####
#Attrition - Whether the employee left in the previous year or not
table(general1$Attrition) #237 employee quit job
ggplot(general1, aes(Attrition)) +
  geom_bar(position = "dodge", aes(y=(..count..)/sum(..count..), fill=Attrition)) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  xlab("Attriton") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y=(..count..)/sum(..count..)), stat= "count",vjust =-.5)+
  scale_fill_brewer(palette="Set1")

###Age - Age of the employee
general1 %>% 
  summarise(Median = median(`Age`), #36
            Mean = mean(`Age`), #36.92
            Max = max(`Age`), #60
            Min = min(`Age`)) #18
#Plots
ggplot(general1, aes(Age, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "Age vs. Attrition")+
  scale_fill_brewer(palette="Set1")#The attriton of younger employees are more than the old employees.
ggplot(general1, aes(x=Attrition, Age, color=Attrition)) +
  geom_boxplot() +
  scale_color_manual(values=c("#CB181D", "#2171B5")) #There is a two outlier for Attrition is yes. #The people age betwwon 40 and 33 quiting their jobs
#Test stat
shapiro.test(general1$Age) #the age distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(Age ~ Attrition, data=general1) #As p value is lower than 0,05 than the difference of ages between attritions is statistically significant



###Department - Department in company
summary(general1$Department)
table(general1$Department, general1$Attrition)
sum(general$Department == "Sales") #446 employee work in sales department
sum(general$Department == "Research & Development") #961 employee work in R&D
sum(general$Department == "Human Resources") #63 employee works in HT
#The percentages 
ggplot(general1,aes(x=Attrition,group=Department))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Department)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Attrition vs. Department")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_brewer(palette="Set1")
#Test stat
chisq.test(general1$Department, general1$Attrition) #there is a significant relationship between department and Attrition
#HR has the highest attrition between departments 
#However in R&D the highest number of employees quit their job
#Even the employees in R&D has the highest number of employee quit ther job, in the scales the percentage of emplyees in HR higher than R&D




###BusinessTravel - How frequently the employees travelled for business purposes in the last year
summary(general1$BusinessTravel)
table(general1$BusinessTravel, general1$Attrition)
#The percenatges 
ggplot(general1,aes(x=Attrition,group=BusinessTravel))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~BusinessTravel)+
  labs(x="Attrition",y="Percentage",title="Attrition vs. BusinessTravel")+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_brewer(palette="Set1")
#Test stat
chisq.test(general1$BusinessTravel, general1$Attrition) #there is a significant relationship between two variables
#There is a relationship between business travel and attrition and we can see that in the scales plot the highest attriton rate in employees travel frequently
# In the numerical the highest employee quiting their job is travel_rarely




###DistanceFromHome - Distance from home in kms
general1 %>% 
  summarise(Median = median(DistanceFromHome),  #7
            Mean = mean(DistanceFromHome), #9.19
            Max = max(DistanceFromHome), #29
            Min = min(DistanceFromHome)) #1
#Plots
ggplot(general1, aes(DistanceFromHome, fill=Attrition)) +
  geom_density()
ggplot(general1, aes(x=Attrition, DistanceFromHome)) +
  geom_boxplot() #There is no outlier
#Test stat
shapiro.test(general1$DistanceFromHome) #the DistanceFromHome  distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(DistanceFromHome ~ Attrition, data=general1) # we can conclude that there is not significant difference between attritions in terms of DistanceFromHome





###Education - Education Level	1 'Below College' 2 'College' 3 'Bachelor' 4 'Master' 5 'Doctor'
summary(general1$Education)
table(general1$Education, general1$Attrition)
#Plot
ggplot(general1,aes(x=Attrition,group=Education))+
  geom_bar(aes(y=..prop..,fill=factor(..group..)),stat="count")+
  facet_grid(~Education)+
  labs(x="Attrition",y="Percentage",title="Attrition vs. EducationLevel")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_fill_brewer(palette="Set1") +
  scale_fill_discrete(name="Education Level", label=c("Below College", "College", "Bachelor", "Master", "Doctor") )
#Test stat
chisq.test(general1$Education, general1$Attrition) #Since p>0.05 there is no significant relationshio between Attrition and education
#In the plot we can conclude that the attrition rate is simillar for all education groups.



####EducationField - Field of education
summary(general1$EducationField)
#Percentage plot
options(repr.plot.width=10, repr.plot.height=6) 
attr.edu <- general1 %>% select(EducationField, Attrition) %>% group_by(EducationField, Attrition) %>% summarize(amount=n(), .groups='drop') %>%
  mutate(pct=round(prop.table(amount),2) * 100) %>% arrange(pct)

nofunc <- colorRampPalette(c("#2171B5"))
yesfunc <- colorRampPalette(c("#CB181D"))

yes.attr <- attr.edu %>% filter(Attrition == "Yes") %>% arrange(EducationField) 
no.attr <- attr.edu %>% filter(Attrition == "No") %>% arrange(EducationField)

par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, labels = unique(attr.edu$EducationField),
                       top.labels=c("No","","Yes"), main = "Attrition by Field of Education", 
                       gap=30, show.values = T, rxcol = yesfunc(6), lxcol = nofunc(6)))

#Test statistics
chisq.test(general1$EducationField, general1$Attrition)





###Gender
table(general1$Gender, general1$Attrition)
#Plot
ggplot(general,aes(x=Attrition,group=Gender))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Gender)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Gender Vs Attrition %")+
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
ggplot(general,aes(x=Gender,group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Attrition)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Gender Vs Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test stat
chisq.test(general1$Gender, general1$Attrition) #Insignifcant, There is no relationship between gender and attrition





#JobLevel	Job level at company on a scale of 1 to 5
table(general1$JobLevel)
summary(general1$JobLevel)
#Plot
ggplot(general1,aes(x=Attrition,group=JobLevel))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~JobLevel)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Job Level Vs Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test stat
chisq.test(general1$JobLevel, general1$Attrition) #significant




#####JobRole - Name of job role in company
table(general1$JobRole, general1$Attrition)
summary(general1$JobRole)
#Plot
library(dplyr)
options(repr.plot.width=10, repr.plot.height=6)
attr.job <- general1 %>% select(JobRole,Attrition) %>% group_by(JobRole, Attrition) %>% summarize(amount=n(), .groups='drop') %>%
  mutate(pct=round(prop.table(amount),2) * 100) %>% arrange(pct)
nofunc <- colorRampPalette(c("#2171B5"))
yesfunc <- colorRampPalette(c("#CB181D"))
yes.attr <- attr.job %>% filter(Attrition == "Yes") %>% arrange(JobRole) 
no.attr <- attr.job %>% filter(Attrition == "No") %>% arrange(JobRole)
par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, labels = unique(attr.job$JobRole),
                       top.labels=c("No","","Yes"), main = "Attrition by Job Role", 
                       gap=9, show.values = T, rxcol = yesfunc(9), lxcol = nofunc(9)))
#Test stat
chisq.test(general1$JobRole, general1$Attrition) #significant




###MaritalStatus - Marital status of the employee
table(general1$MaritalStatus)
summary(general1$MaritalStatus)
#Plot
ggplot(general1,aes(x=Attrition,group=MaritalStatus))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~MaritalStatus)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Marital Status Vs Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test stat
chisq.test(general1$MaritalStatus, general1$Attrition) #significant



####MonthlyIncome - Monthly income in rupees per month
general1 %>% 
  summarise(Median = median(MonthlyIncome), 
            Mean = mean(MonthlyIncome),
            Max = max(MonthlyIncome), 
            Min = min(MonthlyIncome))
#plots
ggplot(general1, aes(MonthlyIncome, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "Monthly Income vs. Attrition")+
  scale_fill_brewer(palette="Set1")
ggplot(general1, aes(x=Attrition, MonthlyIncome, color=Attrition)) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") #There are several outlier however, the outliers not excluded in the dataset because the employees with manager level also quit their job and stay in the company.
#Therefore, we can conclude that montly income not change with attrition but in the analysis, we can see that several relationship bwteen MonthlyIncome and Attrition
#Test stat
shapiro.test(general1$MonthlyIncome) #the MonthlyIncome  distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(MonthlyIncome ~ Attrition, data=general1) #As p value is lower than 0,05 than the difference of Mounthly Income between attritions is statistically significant



###NumCompaniesWorked - Total number of companies the employee has worked for
table(general1$NumCompaniesWorked, general1$Attrition)
#Plot
ggplot(general1,aes(x=Attrition,group=NumCompaniesWorked))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~NumCompaniesWorked)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="NumCompaniesWorked Vs Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1") #If an emplyee worked 5 or more companies they are tendencing to subject to attrition
#Test statistic
shapiro.test(general1$NumCompaniesWorked) #NumCompaniesWorked is not normal. Therefore we conduct the wilcox test
wilcox.test(NumCompaniesWorked ~ Attrition, data=general1) #Not significant


###PercentSalaryHike - Percent salary hike for last year
general1 %>% 
  summarise(Median = median(PercentSalaryHike), 
            Mean = mean(PercentSalaryHike),
            Max = max(PercentSalaryHike), 
            Min = min(PercentSalaryHike))
#Plots
ggplot(general1, aes(PercentSalaryHike, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "PercentSalaryHike vs. Attrition")+
  scale_fill_brewer(palette="Set1")#The attriton of emplyees are high when the percentsalaryhike increase
ggplot(general1, aes(x=Attrition, PercentSalaryHike, color=Attrition)) +
  geom_boxplot() +
  scale_color_manual(values=c("#CB181D", "#2171B5")) #There is a no outlier.
#Test stat
shapiro.test(general1$Age) #the age distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(Age ~ Attrition, data=general1) #As p value is lower than 0,05 than the difference of ages between attritions is statistically significant


#StandardHours - Standard hours of work for the employee
##the standard hours of work 8 for every employee

#Over18 - Whether the employee is above 18 years of age or not
##All emplyees are age over 18

#StockOptionLevel - Stock option level of the employee
table(general1$StockOptionLevel)
#Plot
ggplot(general1,aes(x=Attrition,group=StockOptionLevel))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~StockOptionLevel)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="StockOptionLevel Vs Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test statistics
chisq.test(general1$StockOptionLevel, general1$Attrition) # It is not significant #The attrition ratio is not differ between stockoption level

#TotalWorkingYears - Total number of years the employee has worked so far
general1 %>% 
  summarise(Median = median(TotalWorkingYears), 
            Mean = mean(TotalWorkingYears),
            Max = max(TotalWorkingYears), 
            Min = min(TotalWorkingYears))
#Plots
ggplot(general1, aes(TotalWorkingYears, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "TotalWorkingYears vs. Attrition")+
  scale_fill_brewer(palette="Set1")#The attriton of employees with lower Total working years tend to more to quit their job.
ggplot(general1, aes(x=Attrition, TotalWorkingYears, color=Attrition)) +
  geom_boxplot() +
  scale_color_manual(values=c("#CB181D", "#2171B5")) #There is a several outlier in the attrition ratio "Yes". However, we can think that this outliers can be retirement age and they are exit the labor force.
#Test stat
shapiro.test(general1$TotalWorkingYears) #the TotalWorkingYears distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(TotalWorkingYears ~ Attrition, data=general1) #As p value is lower than 0,05 than the difference of TotalWorkingYears between attritions is statistically significant


#TrainingTimesLastYear -	Number of times training was conducted for this employee last year
table(general1$TrainingTimesLastYear)
#Plots
ggplot(general1, aes(TrainingTimesLastYear, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "TrainingTimesLastYear vs. Attrition")+
  scale_fill_brewer(palette="Set1")#
ggplot(general1, aes(x=Attrition, TrainingTimesLastYear, color=Attrition)) +
  geom_boxplot() +
  scale_color_manual(values=c("#CB181D", "#2171B5")) #There is a several outlier in the attrition ratio "Yes". However, we can think that this outliers can be retirement age and they are exit the labor force.
#Test stat
shapiro.test(general1$TrainingTimesLastYear) #the TrainingTimesLastYear distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(TrainingTimesLastYear ~ Attrition, data=general1) #As p value is lower than 0,05 than the difference of TrainingTimesLastYear between attritions is statistically significant



#YearsAtCompany -	Total number of years spent at the company by the employee
general1 %>% 
  summarise(Median = median(YearsAtCompany), 
            Mean = mean(YearsAtCompany),
            Max = max(YearsAtCompany), 
            Min = min(YearsAtCompany))
#Plots
ggplot(general1, aes(YearsAtCompany, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "YearsAtCompany vs. Attrition")+
  scale_fill_brewer(palette="Set1")#
ggplot(general1, aes(x=Attrition, YearsAtCompany, color=Attrition)) +
  geom_boxplot() +
  scale_color_manual(values=c("#CB181D", "#2171B5")) #There is a several outlier in the attrition ratio "Yes". However, we can think that this outliers can be retirement age and they are exit the labor force.
#Test stat
shapiro.test(general1$YearsAtCompany) #the YearsAtCompany distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(YearsAtCompany ~ Attrition, data=general1) #As p value is lower than 0,05 than the difference of YearsAtCompany between attritions is statistically significant


#YearsSinceLastPromotion - Number of years since last promotion
general1 %>% 
  summarise(Median = median(YearsSinceLastPromotion), 
            Mean = mean(YearsSinceLastPromotion),
            Max = max(YearsSinceLastPromotion), 
            Min = min(YearsSinceLastPromotion))
#Plots
ggplot(general1, aes(YearsSinceLastPromotion, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "YearsSinceLastPromotion vs. Attrition")+
  scale_fill_brewer(palette="Set1")#The last promotion between years 5 to 10, the employees have mor tendecing to subject to attrition
ggplot(general1, aes(x=Attrition, YearsSinceLastPromotion, color=Attrition)) +
  geom_boxplot() +
  scale_color_manual(values=c("#CB181D", "#2171B5")) #There is a several outlier in the attrition ratio "Yes". However, we can think that this outliers can be retirement age and they are exit the labor force.
#Test stat
shapiro.test(general1$YearsSinceLastPromotion) #the YearsSinceLastPromotion distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(YearsSinceLastPromotion ~ Attrition, data=general1) #As p value is lower than 0,05 than the difference of YearsSinceLastPromotion between attritions is statistically significant



#YearsWithCurrManager -	Number of years under current manager
general1 %>% 
  summarise(Median = median(YearsWithCurrManager), 
            Mean = mean(YearsWithCurrManager),
            Max = max(YearsWithCurrManager), 
            Min = min(YearsWithCurrManager))
#Plots
ggplot(general1, aes(YearsWithCurrManager, color=Attrition, fill=Attrition)) +
  geom_density() +
  labs(title = "YearsWithCurrManager vs. Attrition")+
  scale_fill_brewer(palette="Set1")#The attriton of employees with same manager is lower
ggplot(general1, aes(x=Attrition, YearsWithCurrManager, color=Attrition)) +
  geom_boxplot() +
  scale_color_manual(values=c("#CB181D", "#2171B5")) 
#Test statistics
shapiro.test(general1$YearsWithCurrManager) #the YearsWithCurrManager distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(YearsWithCurrManager ~ Attrition, data=general1) #As p value is lower than 0,05 than the difference of YearsWithCurrManager between attritions is statistically significant

#ggplot of employee survey with their percentages
survey_eplot <- gather(general1, Satisfaction, value, EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance, na.rm = TRUE)
ggplot(survey_eplot, aes(factor(Satisfaction), fill=factor(value))) +
  geom_bar(position = "dodge", aes(y=(..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")+
  xlab(" ") +
  scale_fill_brewer(palette = "Set1")
#EnvironmentSatisfaction
ggplot(general1,aes(x=Attrition,group=EnvironmentSatisfaction), ordered=T)+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~EnvironmentSatisfaction)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Environment Satisfaction Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
ggplot(general1,aes(x=EnvironmentSatisfaction,group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Attrition)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Environment Satisfaction Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test.stat
chisq.test(general1$EnvironmentSatisfaction, general1$Attrition)#Significant

#JobSatisfaction
ggplot(general1,aes(x=Attrition,group=JobSatisfaction))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~JobSatisfaction)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Job Satisfaction Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
ggplot(general1,aes(x=JobSatisfaction,group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Attrition)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Job Satisfaction Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test.stat
chisq.test(general1$JobSatisfaction, general1$Attrition)#Significant


#WorkLifeBalance
ggplot(general1,aes(x=Attrition,group=WorkLifeBalance))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~WorkLifeBalance)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Work Life Balance Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
ggplot(general1,aes(x=WorkLifeBalance,group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Attrition)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Work Life Balance Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test.stat
chisq.test(general1$WorkLifeBalance, general1$Attrition)#Significant

#ggplot of manager survey with their percentages
#WorkLifeBalance
ggplot(general1,aes(x=Attrition,group=PerformanceRating))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~PerformanceRating)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Performance Rating Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
ggplot(general1,aes(x=PerformanceRating,group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Attrition)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Performance Rating Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test.stat
chisq.test(general1$PerformanceRating, general1$Attrition)#Insignificant

#JobInvolvement
ggplot(general1,aes(x=Attrition,group=JobInvolvement))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~JobInvolvement)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="JobInvolvement Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
ggplot(general1,aes(x=JobInvolvement,group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Attrition)+
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Attrition",y="Percentage",title="Job Involvement Vs. Attrition %")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_fill_brewer(palette="Set1")
#Test.stat
chisq.test(general1$JobInvolvement, general1$Attrition)#Insignificant




###Creating data for logistic regression####
logisticdata <- general1
logisticdata$EmployeeNumber <- NULL
str(logisticdata)
#Logistic model with all variables
log_model_all<-glm(Attrition~.,data=logisticdata,family = binomial(link = "logit"))
summary(log_model_all)
car::vif(log_model_all)
library(MASS)
log_model_all2<- stepAIC(log_model_all, direction="both")

mypreds <- predict(log_model_sig, newdata = logisticdata)
mypreds1 <- ifelse(exp(mypreds) < 0.5, "No", "Yes")
#Confusion matrix 
library(caret)
confusionMatrix(factor(mypreds1), factor(logisticdata$Attrition))
Accuracy(mypreds1, logisticdata$Attrition)



#Therefore the first significant model is the fit model the accurcy level is %88. It is predict the attrition .
#In overall;
#Technical Degree and Human Resources are outstanding with high attrition.
#Laboratory technician, human resources and sales executive roles have more attrition.
#Employees who travel frequently have more probability to attrition.
#Single employees more tendency to subject to attrition.
#The employees stay in the company when the monthly income increase.
#The younger employees tent to attrition more than the older employees.
#Therfore, the company should consider this variables. For example single employees tent to attriton more thena married employees so, in the hiring process, they should be careful about the single peoples.
#On the other hand, for their employees with travel frequently, they can give more day-off those employees. 

