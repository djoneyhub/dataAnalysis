#Adjani Putri Cemylla Udayana
#TP061651
#read employee_attrition.csv file
emp_data = read.csv("/Users/djoneeeeyyy/Documents/PFDA /PFDA ASS/employee_attrition.csv")
emp_data
View(emp_data)

#assigning headers
names(emp_data) = c("EMP_ID", "RECDATE_KEY", "DOBIRTH", "HIREDATE", "TERMDATE", "EMP_AGE", "SERV_LENGTH", "OFFICE_CITY", "DEP_NAME","JOB_TITLE", "STORE_NAME", "GENDER_SYM", "GENDER","TERM_REASON", "TERM_TYPE","STATUS_YEAR","EMP_STATUS","BUSINESS_UNIT")

#GETTING SAMPLE DATA
head(emp_data)
tail(emp_data)
length(emp_data)
nrow(emp_data)
class(emp_data)

#-----------------------------------data cleansing---------------------------------------------------------
#installing packages
install.packages("tidyverse")
install.packages("plotly") #interactive chart
install.packages("zoo") #dealing with dates
install.packages("stringr") #dealing with strings)
install.packages("vcd")
#deploying packages
library(tidyverse)
library(plotly)
library(zoo)
library(stringr)
library(vcd)

#checking null data
any(is.na(emp_data))
#exploring data and finding out types
str(emp_data)
summary(emp_data)
boxplot(emp_data$SERV_LENGTH)
hist(emp_data$EMP_AGE)
plot(emp_data$STORE_NAME)

#changing employee gender to upper case
emp_data$GENDER <- toupper(emp_data$GENDER)
sample(emp_data$GENDER)

#converting the dates into date or posix type
emp_data$HIREDATE <- as.Date(emp_data$HIREDATE, origin = "12/31/2006", format= "%m/%d/%Y", optional = FALSE)
class(emp_data$HIREDATE)

emp_data$RECDATE_KEY <- as.POSIXct(emp_data$RECDATE_KEY, tz = "GMT", origin = "12/31/2006 0:00", format= "%m/%d/%Y %H:%M")
class(emp_data$RECDATE_KEY)

emp_data$TERMDATE <- as.Date(emp_data$TERMDATE, origin = "12/31/2006", format= "%m/%d/%Y", optional = FALSE)
class(emp_data$TERMDATE)

emp_data$DOBIRTH <- as.Date(emp_data$DOBIRTH, origin = "12/31/2006", format= "%m/%d/%Y", optional = FALSE)
class(emp_data$DOBIRTH)

#convert categorical values into factor
emp_data$OFFICE_CITY <- as.factor(emp_data$OFFICE_CITY)
emp_data$GENDER <-as.factor(emp_data$GENDER)
emp_data$GENDER_SYM <- as.factor(emp_data$GENDER_SYM)
emp_data$JOB_TITLE <- as.factor (emp_data$JOB_TITLE)
emp_data$STORE_NAME <- as.factor(emp_data$STORE_NAME)
emp_data$TERM_TYPE <- as.factor(emp_data$TERM_TYPE)
emp_data$OFFICE_CITY <- as.factor(emp_data$OFFICE_CITY)
emp_data$EMP_STATUS <- as.factor(emp_data$EMP_STATUS)
emp_data$BUSINESS_UNIT <- as.factor(emp_data$BUSINESS_UNIT)
emp_data$TERM_REASON <- as.factor(emp_data$TERM_REASON)
emp_data$DEP_NAME <- as.factor(emp_data$DEP_NAME)

summary(emp_data)
str(emp_data)
summary(emp_data$JOB_TITLE)
summary(emp_data$DEP_NAME)
summary(emp_data$BUSINESS_UNIT)

summary(emp_data) #checking null/garbage data
#putting and dropping null service length into different variable
null_serv = subset(emp_data, (SERV_LENGTH == 0 & TERMDATE < HIREDATE))
null_serv

tail(emp_data)
tail(null_serv)


#---------------------------------------DATA EXPLORATION PORTION----------------------------------------------------------
install.packages("DataExplorer")
library(DataExplorer)

#calculating basic descriptive statistics
summary(emp_data)

#getting basic information about the data 
#number of column
length(emp_data) #18 column

#number of row
nrow(emp_data) #49653 rows
#conclusion : there are 49653 employee datas with 18 different information each

#finding out different categories/ information available
names(emp_data) 

#list structure of dataset
str(emp_data)

#getting sample data
head(emp_data) #first 6 rows of dataset
tail(emp_data) #last 6 rows of dataset
emp_data[40:60,] #40th to 60th rows
tail(emp_data,n = -1) #all rows but the first row
library(dplyr)
sample_n(emp_data,6) #getting 6 random rows from dataset
sample_frac(emp_data, 0.1) #getting 10% random rows

#checking missing values
colSums(is.na(emp_data))
#no missing variables in every dataset

#to see different category
#different category for office_city
summary(emp_data$OFFICE_CITY)
off_city = factor(emp_data$OFFICE_CITY)
nlevels(off_city) 
#from this we know that the company has branches in 40 different cities

#different category for dep_name
summary(emp_data$DEP_NAME)
dep_name = factor(emp_data$DEP_NAME)
nlevels(dep_name)
#from here we know that the company has 21 departments

#different category for job_title
summary(emp_data$JOB_TITLE)
job_title = factor(emp_data$JOB_TITLE)
nlevels(job_title)
#from here we know that the company has 47 different positions for its employee

#different category for store_name
summary(emp_data$STORE_NAME)
store_name = factor(emp_data$STORE_NAME)
nlevels(store_name)
#from here we know that the company has 46 stores

#different term_reason
summary(emp_data$TERM_REASON)
term_reason = factor(emp_data$TERM_REASON)
nlevels(term_reason)
#there are 4 different reason for their termination (layoff, not applicable, resignation, retirement)

#different term_type
summary(emp_data$TERM_TYPE)
term_type = factor(emp_data$TERM_TYPE)
nlevels(term_type)
#there are 3 different termination type (Involuntary,Not Applicable, Voluntary)

#different bus_unit
summary(emp_data$BUSINESS_UNIT)
bus_unit = factor(emp_data$BUSINESS_UNIT)
nlevels(bus_unit)
#there are 2 different business unit in this company (head office and stores)

#summary hire_date
hireDate = emp_data$HIREDATE
hire_date = as.POSIXct(hireDate)
summary(hire_date)
min(emp_data$HIREDATE) #min = 28/08/1989
max(emp_data$HIREDATE) #max = 11-12-2013
#conclusion :company has been running from 1989 at the very least and data last updated on 20113


#summary length of service
los = emp_data$SERV_LENGTH
summary(los)
min(emp_data$SERV_LENGTH)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    5.00   10.00   10.43   15.00   26.00 

nrow(emp_data[emp_data$SERV_LENGTH == 0,])
nrow (emp_data[emp_data$EMP_STATUS == "ACTIVE",])

#number of employee hired each year
test = format(emp_data$HIREDATE,format = "%Y") #to take just the year from hiredate
test = as.integer(test)
testdob = format(emp_data$DOBIRTH, format = "%Y")
testdob = as.integer(testdob)
table(test) #to find out amount hired each year
nrow(emp_data[(emp_data$SERV_LENGTH <= 10) & (emp_data$GENDER == "Male"),])
#numerical value : ID, SERV_LENGTH,EMP_AGE
#categorical value: office city, dep_name, job_title<gender sym, gender, term reason, term type, business unit, status

#making new dataset with fixed logic
empd2 <- emp_data[order(emp_data$EMP_ID, -abs(emp_data$STATUS_YEAR) ), ] ### sort first
empd2 <- empd2[ !duplicated(empd2$EMP_ID), ]  ### Keep highest
test = format(empd2$HIREDATE,format = "%Y")
test <- as.integer(test)
testdob = format(empd2$DOBIRTH, format = "%Y")
testdob <- as.integer(testdob)

empd2 = empd2%>%mutate(empd2, HIRE_AGE = test - testdob) %>%
  mutate(empd2, FIX_TERMDATE = test + SERV_LENGTH)
summary(empd2)
summary(emp_data)

#data explorer
create_report(empd2)
introduce(empd2)
plot

#------------------------------------ANALYSIS PORTION---------------------------------------------------------------------
#---------------------------library installation & Deployment--------------------------------------------------
install.packages("tidyverse") #include ggplot2,dplyr,tidyr,readr, purrr, tibble,stringr & forcats
install.packages("funModeling") #exploratory da
intall.packages("Hmisc") #describing and chart making using html
install.packages("ggplot2")
install.packages("gcookbook")
install.packages("plotly")
install.packages("gapminder")


library(tidyverse)
library(funModeling)
library(Hmisc)
library(ggplot2)
library(dplyr) #data manipulation
library(gcookbook)
library(plotly)
library(gapminder)
library(hrbrthemes)
library(plotrix)

summary(emp_data$JOB_TITLE)
summary(fix_empdata$JOB_TITLE)




#---------------------------Q1 : which department has the highest employee satisfaction-------------------------------------------


#to view how much employee still active on each department
ggplot(data = empd2, aes(y = DEP_NAME, fill = EMP_STATUS)) + 
  geom_bar()+
  xlab("Number of Employee")+
  ylab("Department Name")+
  labs(fill = "Employee Status", title = "Employee Status Ratio in Each Department")

#make a subset with just active employee
actemp = subset(empd2,EMP_STATUS == "ACTIVE")
actemp
max(actemp$SERV_LENGTH)

freq =actemp %>% select(DEP_NAME,SERV_LENGTH) %>% count(DEP_NAME,SERV_LENGTH) %>% 
  filter(SERV_LENGTH>0 & n<100) %>% filter(n > 10)

max(freq$n)

#bar chart of department, service length and its quantity
ggplot(data = freq, aes(x = n,y = SERV_LENGTH, fill = DEP_NAME)) + 
  geom_col()
#conclusion : produce and meats leads with most employee serves for more than 20 years

#count the number of employee and which department has the most employee working over 20 years
mostEmPdEp = freq %>% select(DEP_NAME,SERV_LENGTH,n) %>% filter(SERV_LENGTH > 20) %>% 
  group_by(DEP_NAME) %>% summarise(n = sum(n))
mostEmPdEp 
ggplot(data = mostEmPdEp, aes( x = n, y = DEP_NAME)) + 
  geom_col()  
#found: Produce, meats, dairy

#find store name for each department with most loyal emp
#Produce
storez = actemp %>% select(DEP_NAME,SERV_LENGTH,STORE_NAME,BUSINESS_UNIT) %>% 
  filter(SERV_LENGTH >20 & DEP_NAME == "Produce") %>% count(SERV_LENGTH,STORE_NAME) %>% arrange(n) %>% filter(n > 5) %>% group_by(STORE_NAME) %>% summarise(n = sum(n))
storez
storez %>% ggplot(aes(x=reorder(STORE_NAME,n),y=n)) +
  geom_point(size = 3, colour = "blue", aes(color=)) + 
  geom_segment(aes(xend = STORE_NAME, yend = 0), size = 1.2)+  geom_label(aes(STORE_NAME, n+1.5, label = signif(n,2)), colour = "darkred", nudge_x = 0.35, size = 4)
  labs(y= "n", x="Store Name")
#store 41 found to be those who got employee that stays the longest
actemp %>% select(DEP_NAME,STORE_NAME,BUSINESS_UNIT,OFFICE_CITY)%>% filter(DEP_NAME == "Produce" & STORE_NAME == 41) %>% select(STORE_NAME, BUSINESS_UNIT,OFFICE_CITY) %>% count(STORE_NAME, BUSINESS_UNIT, OFFICE_CITY)
#41 = vancouver, stores

#finding hire age with longest service length for this department in particular
#finding age pool with most employee serving over 20 years
actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% filter(DEP_NAME == "Produce") %>% filter(SERV_LENGTH >20) %>% count(HIRE_AGE,STORE_NAME) %>% filter(n >10)
#finding overall correlation between hire age and quantity of high service length
hi0vr = actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% filter(DEP_NAME == "Produce") %>% filter(SERV_LENGTH >20) %>% count(HIRE_AGE, DEP_NAME, SERV_LENGTH) 
actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% filter(DEP_NAME == "Produce") %>% filter(SERV_LENGTH >20) %>% count(HIRE_AGE, DEP_NAME, SERV_LENGTH)%>% group_by(HIRE_AGE) %>% summarise(n = sum(n)) 
summary(hi0vr)
#age 36 is found to be the age pool with the most employee serving over 20 years


#Processed Food
storez2 = actemp %>% select(DEP_NAME,SERV_LENGTH,STORE_NAME,BUSINESS_UNIT) %>% filter(SERV_LENGTH >20 & DEP_NAME == "Processed Foods") %>% count(SERV_LENGTH,STORE_NAME) %>% arrange(n) %>% group_by(STORE_NAME) %>% summarise(n = sum(n)) 
storez2

storez2 %>% ggplot(aes(x=reorder(STORE_NAME,n),y=n)) +
  geom_point(size = 3, colour = "blue", aes(color=)) + 
  geom_segment(aes(xend = STORE_NAME, yend = 0), size = 1.2)+  geom_label(aes(STORE_NAME, n+1.5, label = signif(n,2)), colour = "darkred", nudge_x = 0.35, size = 4)
labs(y= "n", x="Store Name")


actemp %>% select(DEP_NAME,SERV_LENGTH,STORE_NAME,BUSINESS_UNIT,OFFICE_CITY) %>% filter(DEP_NAME == "Processed Foods" & STORE_NAME == 46) %>% select(STORE_NAME, BUSINESS_UNIT, OFFICE_CITY) %>% count(STORE_NAME, BUSINESS_UNIT, OFFICE_CITY) %>% arrange(n) 
# store name = 41, UNIT = STORES, CITY = Vancouver


#Dairy
storez3 = actemp %>% select(DEP_NAME,SERV_LENGTH,STORE_NAME,BUSINESS_UNIT) %>% filter(SERV_LENGTH >20 & DEP_NAME == "Dairy") %>% count(SERV_LENGTH,STORE_NAME) %>% arrange(n) %>% group_by(STORE_NAME) %>% summarise(n = sum(n)) 
storez3

storez3 %>% ggplot(aes(x=reorder(STORE_NAME,n),y=n)) +
  geom_point(size = 3, colour = "blue", aes(color=)) + 
  geom_segment(aes(xend = STORE_NAME, yend = 0), size = 1.2)+  geom_label(aes(STORE_NAME, n+1.5, label = signif(n,2)), colour = "darkred", nudge_x = 0.35, size = 4)
labs(y= "n", x="Store Name")
# store name = 41, Unit = Stores, CITY = Vancouver

#comparison
#count the number of employee and which department has the most employee working over 20 years
leastEmPdEp = freq %>% select(DEP_NAME,SERV_LENGTH,n) %>% filter(SERV_LENGTH < 10) %>% 
  group_by(DEP_NAME) %>% summarise(n = sum(n))
leastEmPdEp 
ggplot(data = leastEmPdEp, aes( x = n, y = DEP_NAME)) + 
  geom_col()  
#found: processed foods, Dairy, Customer Service
actemp %>% select(DEP_NAME) %>% count(DEP_NAME) %>% arrange(n)

#for cust serv
storez4 = actemp %>% select(DEP_NAME,SERV_LENGTH,STORE_NAME,BUSINESS_UNIT) %>% filter(SERV_LENGTH <10 & DEP_NAME == "Customer Service") %>% count(SERV_LENGTH,STORE_NAME) %>% arrange(n) %>% group_by(STORE_NAME) %>% summarise(n = sum(n)) 
storez4

storez4 %>% ggplot(aes(x=reorder(STORE_NAME,n),y=n)) +
  geom_point(size = 3, colour = "blue", aes(color=)) + 
  geom_segment(aes(xend = STORE_NAME, yend = 0), size = 1.2)+  geom_label(aes(STORE_NAME, n+1.5, label = signif(n,2)), colour = "darkred", nudge_x = 0.35, size = 4)
labs(y= "n", x="Store Name")


actemp %>% select(DEP_NAME,SERV_LENGTH,STORE_NAME,BUSINESS_UNIT,OFFICE_CITY) %>% filter(DEP_NAME == "Customer Service" & STORE_NAME == 18) %>% select(STORE_NAME, BUSINESS_UNIT, OFFICE_CITY) %>% count(STORE_NAME, BUSINESS_UNIT, OFFICE_CITY) %>% arrange(n) 






#--------------------------------------------------Q2: Connection with Gender---------------------------------------------------------------------


#length of service and gender - find reason
#overall spread of female and male employee
genXlength = empd2 %>% select(SERV_LENGTH, GENDER) %>% filter(SERV_LENGTH >0) %>% 
  group_by(SERV_LENGTH, GENDER)
ggplot(data = genXlength,aes(y = SERV_LENGTH, fill = GENDER)) + 
  geom_bar(aes(fill = GENDER), position = "dodge")
#found that this company has more female and consequently, has female employee with highest and lowest
#overall service length

#why male stop
empd2 %>% select(GENDER, SERV_LENGTH, TERM_REASON) %>% filter(GENDER == "MALE") %>%
  filter(SERV_LENGTH < 10) %>% count(TERM_REASON)

empd2 %>% select(GENDER, SERV_LENGTH, TERM_REASON) %>% filter(GENDER == "FEMALE") %>%
  filter(SERV_LENGTH < 20) %>% filter(SERV_LENGTH > 10) %>% count(TERM_REASON)

empd2 %>% select(GENDER, SERV_LENGTH, TERM_REASON, EMP_AGE) %>% filter(GENDER == "MALE") %>%
  filter(SERV_LENGTH < 10) %>% filter(TERM_REASON == "Retirement") %>% count(EMP_AGE)

empd2 %>% select(GENDER, SERV_LENGTH, TERM_REASON, EMP_AGE) %>% filter(GENDER == "FEMALE") %>%
  filter(SERV_LENGTH < 20) %>% filter(SERV_LENGTH > 10) %>% 
  filter(TERM_REASON == "Retirement") %>% 
  count(EMP_AGE)

empd2 %>% select(GENDER, SERV_LENGTH, TERM_REASON, HIRE_AGE) %>% filter(GENDER == "MALE") %>%
  filter(SERV_LENGTH < 10) %>% filter(TERM_REASON == "Retirement") %>% count(HIRE_AGE)

empd2 %>% select(GENDER, SERV_LENGTH, TERM_REASON, HIRE_AGE) %>% filter(GENDER == "FEMALE") %>%
  filter(SERV_LENGTH < 20) %>% filter(SERV_LENGTH > 10) %>% 
  filter(TERM_REASON == "Retirement") %>% 
  count(HIRE_AGE)

  
#finding department with lowest service length for female and male employee
empd2 %>% select(DEP_NAME, SERV_LENGTH, GENDER) %>% filter(SERV_LENGTH < 10)%>%
  ggplot(aes(y = DEP_NAME, fill = GENDER)) + 
  geom_bar(aes(fill = GENDER), position = "dodge") 

empd2 %>% select(DEP_NAME, SERV_LENGTH, GENDER) %>% filter(SERV_LENGTH < 10)%>% 
  group_by(DEP_NAME) %>%count(DEP_NAME, GENDER)

#dairy - customer service - processed foods
summary(empd2)
dairy= empd2 %>% select(DEP_NAME, HIRE_AGE, GENDER, TERM_REASON, TERM_TYPE, EMP_STATUS, STORE_NAME) %>% filter(DEP_NAME == "Dairy") %>%filter(EMP_STATUS == "TERMINATED") %>% filter(TERM_REASON != "Not Applicable" & TERM_REASON != "Retirement")
custSer = empd2 %>% select(DEP_NAME, HIRE_AGE, GENDER, TERM_REASON, TERM_TYPE, EMP_STATUS,STORE_NAME) %>% filter(DEP_NAME == "Customer Service") %>%filter(EMP_STATUS == "TERMINATED") %>% filter(TERM_REASON != "Not Applicable" & TERM_REASON != "Retirement") 
procFoods = empd2 %>% select(DEP_NAME, HIRE_AGE, GENDER, TERM_REASON, TERM_TYPE, EMP_STATUS,STORE_NAME) %>% filter(DEP_NAME == "Processed Foods") %>%filter(EMP_STATUS == "TERMINATED") %>% filter(TERM_REASON != "Not Applicable" & TERM_REASON != "Retirement") 
ovr4ll = rbind(dairy,custSer,procFoods)

ovr4ll%>%filter(GENDER == "FEMALE")%>% 
  ggplot(aes(y = DEP_NAME, fill = TERM_REASON)) + 
  geom_bar(aes(fill = TERM_REASON), position = "dodge") 
ovr4ll %>% select(GENDER,TERM_REASON,STORE_NAME) %>% filter(GENDER == "FEMALE") %>% filter(TERM_REASON == "Resignaton") %>% count(STORE_NAME) %>% arrange(n)
  
  
ovr4ll%>%filter(GENDER == "MALE")%>% 
  ggplot(aes(y = DEP_NAME, fill = TERM_REASON)) + 
  geom_bar(aes(fill = TERM_REASON), position = "dodge") 
ovr4ll %>% select(GENDER,TERM_REASON,STORE_NAME) %>% filter(GENDER == "MALE") %>% filter(TERM_REASON == "Resignaton") %>% count(STORE_NAME)%>% arrange(n)


#finding gender and department spread for highest service length 
empd2 %>% select(DEP_NAME, SERV_LENGTH, GENDER) %>% filter(SERV_LENGTH >= 20) %>% 
  ggplot(aes(y = DEP_NAME, fill = GENDER))+  
  geom_bar(aes(fill = GENDER), position = "dodge") 

empd2 %>% select(DEP_NAME, SERV_LENGTH, GENDER) %>% filter(SERV_LENGTH >= 20)%>% 
  group_by(DEP_NAME) %>%count(DEP_NAME, GENDER) %>% filter(n > 100)

#---------------------Q3 : criteria (GENDER,AGE) in different level of the company---------------------------------------------
summary(fix_empdata$DEP_NAME)
library(dplyr)
#-------level 1--------------------------------------------
a_ = empd2 %>% filter(DEP_NAME == "Executive")
b_ = empd2 %>% filter(DEP_NAME == "Investment") 
c_ = empd2 %>% filter(DEP_NAME == "Legal")
d_ = empd2 %>% filter(DEP_NAME == "IT")
e_ = empd2 %>% filter(DEP_NAME == "Store Management")
f_ = empd2 %>% filter(DEP_NAME == "Employee Record")
g_ = empd2 %>% filter(DEP_NAME == "HR Technology")
level1 = rbind(a_,b_,c_,d_,e_,f_,g_) #creating level 1
summary(level1$EMP_STATUS)

#gender based analysis
level1 %>% select(GENDER) %>% count(GENDER) %>%
  ggplot(aes(x=n,y = GENDER, fill = GENDER)) + 
  geom_col(aes(fill = GENDER)) + coord_flip()
level1 %>% select(GENDER, DEP_NAME) %>% 
  ggplot(aes(y = DEP_NAME, fill = GENDER)) + 
  geom_bar(aes(fill = GENDER), position = "dodge") #for the plot

level1 %>% select(GENDER, DEP_NAME) %>% count(DEP_NAME, GENDER) #for the data
level5%>% select(GENDER) %>% count(GENDER)

#age based analysis
yes = level1 %>% select(HIRE_AGE) %>% count(HIRE_AGE)%>% 
  ggplot( aes(HIRE_AGE,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw() 
ggplotly(yes)

#hire level each year
level1 %>% select(HIREDATE) %>% 
  mutate(level1, HIREYEAR = format(level1$HIREDATE,format = "%Y")) %>% count(HIREYEAR) %>%
  plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')
summary(level1$HIREDATE)

#---------------level 2------------------------------------------
a. = empd2 %>% filter(DEP_NAME == "Audit")
b. = empd2 %>% filter(DEP_NAME == "Accounting") 
c. = empd2 %>% filter(DEP_NAME == "Accounts Payable")
d. = empd2 %>% filter(DEP_NAME == "Accounts Receivable")
e. = empd2 %>% filter(DEP_NAME == "Compensation")
level2 = rbind(a.,b.,c.,d.,e.)
summary(level2$EMP_STATUS)

#gender based analysis
level2 %>% select(GENDER) %>% count(GENDER) %>%
  ggplot(aes(x=n,y = GENDER, fill = GENDER)) + 
  geom_col(aes(fill = GENDER)) + coord_flip()
level2 %>% select(GENDER, DEP_NAME) %>% 
  ggplot(aes(y = DEP_NAME, fill = GENDER)) + 
  geom_bar(aes(fill = GENDER), position = "dodge") #for the plot

level2 %>% select(GENDER, DEP_NAME) %>% count(DEP_NAME, GENDER) #for the data

#age based analysis
no = level2 %>% select(HIRE_AGE) %>% count(HIRE_AGE)%>% 
  ggplot( aes(HIRE_AGE,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw() 
ggplotly(no)

#hire level progression
level2 %>% select(HIREDATE) %>% 
  mutate(level2, HIREYEAR = format(level2$HIREDATE,format = "%Y")) %>% count(HIREYEAR) %>%
  plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')
summary(level2$HIREDATE)

#-----------------------level 3--------------------------------------
a_. = empd2 %>% filter(DEP_NAME == "Training")
b_. = empd2 %>% filter(DEP_NAME == "Recruitment") 
c_. = empd2 %>% filter(DEP_NAME == "Labor Relation")
d_. = empd2 %>% filter(DEP_NAME == "Customer Service")
level3 = rbind(a_.,b_.,c_.,d_.)
summary(level6$EMP_STATUS)

#gender based analysis
level3 %>% select(GENDER) %>% count(GENDER) %>%
  ggplot(aes(x=n,y = GENDER, fill = GENDER)) + 
  geom_col(aes(fill = GENDER)) + coord_flip()
level3 %>% select(GENDER, DEP_NAME) %>% 
  ggplot(aes(y = DEP_NAME, fill = GENDER)) + 
  geom_bar(aes(fill = GENDER), position = "dodge") #for the plot

level3 %>% select(GENDER, DEP_NAME) %>% count(DEP_NAME, GENDER) #for the data

#age based analysis
maybe = level3 %>% select(HIRE_AGE) %>% count(HIRE_AGE)%>% 
  ggplot( aes(HIRE_AGE,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw() 
ggplotly(maybe)

#hire level progression
level3 %>% select(HIREDATE) %>% 
  mutate(level3, HIREYEAR = format(level3$HIREDATE,format = "%Y")) %>% count(HIREYEAR) %>%
  plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')
summary(level3$HIREDATE)
#--------------------------level 4---------------------------------------
a._ = empd2 %>% filter(DEP_NAME == "Processed Foods")
b._ = empd2 %>% filter(DEP_NAME == "Bakery") 
c._ = empd2 %>% filter(DEP_NAME == "Produce")
level4 = rbind(a._,b._,c._)
summary(level14$EMP_STATUS)

#gender based analysis
level4 %>% select(GENDER) %>% count(GENDER) %>%
  ggplot(aes(x=n,y = GENDER, fill = GENDER)) + 
  geom_col(aes(fill = GENDER)) + coord_flip()
level4 %>% select(GENDER, DEP_NAME) %>% 
  ggplot(aes(y = DEP_NAME, fill = GENDER)) + 
  geom_bar(aes(fill = GENDER), position = "dodge") #for the plot

level4 %>% select(GENDER, DEP_NAME) %>% count(DEP_NAME, GENDER) #for the data

#age based analysis
perhaps = level4 %>% select(HIRE_AGE) %>% count(HIRE_AGE)%>% 
  ggplot( aes(HIRE_AGE,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw() 
ggplotly(perhaps)

#hire level progression
level4 %>% select(HIREDATE) %>% 
  mutate(level4, HIREYEAR = format(level4$HIREDATE,format = "%Y")) %>% count(HIREYEAR) %>%
  plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')
summary(level4$HIREDATE)
#-------------------------------level 5--------------------------------------
a_._ = empd2 %>% filter(DEP_NAME == "Dairy")
b_._ = empd2 %>% filter(DEP_NAME == "Meats") 
level5 = rbind(a_._,b_._)
summary(level5$EMP_STATUS)

summary(emp_data$EMP_AGE)
#gender based analysis
level5 %>% select(GENDER) %>% count(GENDER) %>%
  ggplot(aes(x=n,y = GENDER, fill = GENDER)) + 
  geom_col(aes(fill = GENDER)) + coord_flip()
level5 %>% select(GENDER, DEP_NAME) %>% 
  ggplot(aes(y = DEP_NAME, fill = GENDER)) + 
  geom_bar(aes(fill = GENDER), position = "dodge") #for the plot

level5 %>% select(GENDER, DEP_NAME) %>% count(DEP_NAME, GENDER) #for the data

#age based analysis
deffo = level5 %>% select(HIRE_AGE) %>% count(HIRE_AGE)%>% 
  ggplot( aes(HIRE_AGE,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw() 
ggplotly(deffo)

#hire level progression
level5 %>% select(HIREDATE) %>% 
  mutate(level5, HIREYEAR = format(level5$HIREDATE,format = "%Y")) %>% count(HIREYEAR) %>%
  plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')
summary(level5$HIREDATE)


#--------------------------------Q4: age hired and length of service----------------------------------------------

#PRODUCE
#finding hire age with longest service length for this department in particular
#finding age pool with most employee serving over 20 years
actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Produce") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE,STORE_NAME) %>% filter(n >10) %>% arrange(n)
#finding overall correlation between hire age and quantity of high service length
hi0vr = actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Produce") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE, DEP_NAME, SERV_LENGTH) %>% arrange(n)

actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Produce") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE, DEP_NAME, SERV_LENGTH)%>% group_by(HIRE_AGE) %>% summarise(n = sum(n)) 
summary(hi0vr)
#age 36 is found to be the age pool with the most employee serving over 20 years

#PROCESSED FOODS
#finding hire age with longest service length for this department in particular
#finding age pool with most employee serving over 20 years
actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Processed Foods") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE,STORE_NAME) %>% filter(n >10) %>% arrange(n)
#finding overall correlation between hire age and quantity of high service length
hi0vr2 = actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Processed Foods") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE, DEP_NAME, SERV_LENGTH)

actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Processed Foods") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE, DEP_NAME, SERV_LENGTH) %>% group_by(HIRE_AGE) %>% summarise(n = sum(n))
summary(hi0vr2)
#age 37 is found to be the age pool with the most employee serving over 20 years

#DAIRY
#finding hire age with longest service length for this department in particular
#finding age pool with most employee serving over 20 years
actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Dairy") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE,STORE_NAME) %>% filter(n >10) %>% arrange(n)
#finding overall correlation between hire age and quantity of high service length
hi0vr3 = actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Dairy") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE, DEP_NAME, SERV_LENGTH) 
hi0vr3
actemp %>% select(DEP_NAME, HIRE_AGE, SERV_LENGTH,STORE_NAME) %>% 
  filter(DEP_NAME == "Dairy") %>% filter(SERV_LENGTH >20) %>% 
  count(HIRE_AGE, DEP_NAME, SERV_LENGTH)%>% group_by(HIRE_AGE) %>% summarise(n = sum(n))
summary(hi0vr3)
#age 38 is found to be the age pool with the most employee serving over 20 years


hi0vrAll = rbind(hi0vr,hi0vr2,hi0vr3)
hi0vrAll%>%count(DEP_NAME,HIRE_AGE)
hi0vrAll %>% count(DEP_NAME)
hi0vrAll %>% count(SERV_LENGTH,HIRE_AGE)

#interactive scat plot
p <- hi0vrAll %>% ggplot( aes(SERV_LENGTH,n, size = HIRE_AGE, color=DEP_NAME)) +
  geom_point() +
  theme_bw()

ggplotly(p)

inactemp = subset(empd2,EMP_STATUS == "TERMINATED")
inactemp

inactemp%>% select(SERV_LENGTH, HIRE_AGE) %>% filter(SERV_LENGTH <10) %>% 
  count(SERV_LENGTH, HIRE_AGE)

y = inactemp%>% select(SERV_LENGTH, HIRE_AGE) %>% filter(SERV_LENGTH <10) %>% 
  count(SERV_LENGTH, HIRE_AGE) %>% ggplot( aes(SERV_LENGTH,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw()

ggplotly(y)


#-------------------------Q4: different hire age preference throughout year---------------------------------------
summary(empd2$HIREDATE)

win1 = empd2 %>% select(HIRE_AGE, HIREDATE) %>% 
  mutate(empd2, HIREYEAR = format(empd2$HIREDATE,format = "%Y")) %>%
  filter(HIREYEAR>=1989) %>% filter(HIREYEAR <= 1994) %>% count(HIRE_AGE,HIREYEAR)%>%
  arrange(n)%>%ggplot( aes(HIREYEAR,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw()

empd2 %>% select(HIRE_AGE, HIREDATE) %>% 
  mutate(empd2, HIREYEAR = format(empd2$HIREDATE,format = "%Y")) %>%
  filter(HIREYEAR>=1989) %>% filter(HIREYEAR <= 1994) %>% count(HIREYEAR)%>%
  arrange(n)%>%plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')

ggplotly(win1)

win2 = empd2 %>% select(HIRE_AGE, HIREDATE) %>% 
  mutate(empd2, HIREYEAR = format(empd2$HIREDATE,format = "%Y")) %>%
  filter(HIREYEAR>=1995) %>% filter(HIREYEAR <= 2000) %>% count(HIRE_AGE,HIREYEAR)%>%
  arrange(n)%>%ggplot( aes(HIREYEAR,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw()

empd2 %>% select(HIRE_AGE, HIREDATE) %>% 
  mutate(empd2, HIREYEAR = format(empd2$HIREDATE,format = "%Y")) %>%
  filter(HIREYEAR>=1995) %>% filter(HIREYEAR <= 2000) %>% count(HIREYEAR)%>%
  arrange(n)%>%plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')



ggplotly(win2)

win3 = empd2 %>% select(HIRE_AGE, HIREDATE) %>% 
  mutate(empd2, HIREYEAR = format(empd2$HIREDATE,format = "%Y")) %>%
  filter(HIREYEAR>=2001) %>% filter(HIREYEAR <= 2006) %>% count(HIRE_AGE,HIREYEAR)%>%
  arrange(n)%>%ggplot( aes(HIREYEAR,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw()

empd2 %>% select(HIRE_AGE, HIREDATE) %>% 
  mutate(empd2, HIREYEAR = format(empd2$HIREDATE,format = "%Y")) %>%
  filter(HIREYEAR>=2001) %>% filter(HIREYEAR <= 2006) %>% count(HIREYEAR)%>%
  arrange(n)%>%plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')


ggplotly(win3)

win3 = empd2 %>% select(HIRE_AGE, HIREDATE) %>% 
  mutate(empd2, HIREYEAR = format(empd2$HIREDATE,format = "%Y")) %>%
  filter(HIREYEAR>=2007) %>% filter(HIREYEAR <= 2013) %>% count(HIRE_AGE,HIREYEAR)%>%
  arrange(n)%>%ggplot( aes(HIREYEAR,n, size = n, color=HIRE_AGE)) +
  geom_point() +
  theme_bw()

ggplotly(win3)

empd2 %>% select(HIRE_AGE, HIREDATE) %>% 
  mutate(empd2, HIREYEAR = format(empd2$HIREDATE,format = "%Y")) %>%
  filter(HIREYEAR>=2007) %>% filter(HIREYEAR <= 2013) %>% count(HIREYEAR)%>%
  arrange(n)%>%plot_ly(x = ~HIREYEAR, y = ~n, type = 'scatter', mode = 'lines')

















#-------------------experiment--------------------------------------------------------
install.packages("DataExplorer")
library(DataExplorer)
#funModeling
plot_num(emp_data)
profiling_num(emp_data)  
install.packages("DataExplorer")

#Hmisc
d <- describe(emp_data)
html(d, size = 80, scroll = TRUE) #NOT USEABLE, MESSY
p <- plot(d)
htmltools::tagList(p) 
lapply(p, plotly::as.widget)

#data explorer
create_report(empd2)
introduce(empd2)
plot

plot_scatterplot(split_columns(empd2)$continuous, by = "SERV_LENGTH", sampled_rows = 1000L)


#-----OEOE
ggplot(data = freq, aes(x = n, y = SERV_LENGTH, fill = DEP_NAME))+ geom_col()+ facet_grid(~DEP_NAME)

install.packages("vtree")
library(vtree)
vtree(freq, c("DEP_NAME", "SERV_LENGTH", "n"), 
      horiz = FALSE, fillcolor = c(DEP_NAME = "#e7d4e8", 
                                   SERV_LENGTH = "#99d8c9", n = "#9ecae1"))




less = nrow(emp_data[emp_data$JOB_TITLE,])
filter(emp_data, less>100)

actemp %>% select(JOB_TITLE, SERV_LENGTH, DEP_NAME) %>% filter("Manager" %in% JOB_TITLE) %>% count(DEP_NAME)


