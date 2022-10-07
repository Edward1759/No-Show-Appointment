#importing data from my working directory
Df<-read.csv("noshowappointment.csv")

#viewing features name
names(Df)

#to show the first five rows
head(Df)
#showing the last five rows
tail(Df)

#the data shape " rows and column"
dim(Df)
#number of duplicate values in rows
sum(duplicated(Df))

#checking missing values
is.na(Df)
sum(is.na(Df))

#checking the structure of the data 
str(Df)

#cleaning the dataset by changing data types of some columns to be more readable
names(Df)[names(Df)=='No.show']='No_show'

#removing row with age <0
Df=Df[Df$Age>=0,]

new_DF=Df%>% mutate(Gender=as.factor(Gender), ScheduledDay=as.Date(ScheduledDay),
                     AppointmentDay=as.Date(AppointmentDay),Neighbourhood=as.factor(Neighbourhood),
                     Scholarship=as.logical(Scholarship), Hipertension=as.logical(Hipertension),
                     Diabetes=as.logical(Diabetes), Alcoholism=as.logical(Alcoholism),
                     Handcap=as.logical(Handcap), SMS_received=as.logical(SMS_received),
                     No_show=as.factor(No_show), Age=as.numeric(Age))

#checking the summary
summary(new_DF)

#the relationship between no show and what may affect it
new_DF%>% group_by(SMS_received)%>%count(No_show)

pie

#the difference btween gender and attendance
new_DF %>% group_by(Gender) %>% count(No_show)

#using visual representation to know the difference between  Gender
ggplot(data = new_DF)+
  geom_bar(aes(x=Gender,fill=Gender))+
  ggtitle("comparing Male and Female")
  

#relationship betwen who has scholarship and attendance
new_DF %>% group_by(Scholarship) %>% count(No_show)

ggplot(data = new_DF) + geom_bar(aes(AppointmentDay, fill=No_show))+
  ggtitle("^AppointmentDay vs No_show")+
  ylab("count")+
  xlab("Day")

#difference between males and females
new_DF %>% group_by(Gender) %>% count(Scholarship)

ggplot(data = new_DF)+
  geom_bar(aes(x=Gender, fill=Scholarship))+
  ggtitle("Gender vs Scholarship")
  

new_DF %>% group_by(Gender) %>% count(SMS_received)

#the rate of difference between males and females for each disease
new_DF %>% group_by(Gender) %>% count(Hipertension)

new_DF%>% group_by(Gender)%>% count(Diabetes)

new_DF%>% group_by(Gender) %>% count(Alcoholism)

#the number of attendance for each group
new_DF%>% group_by(No_show) %>% count(Handcap)
new_DF %>% group_by(No_show)%>% count(Hipertension)
new_DF%>% group_by(No_show)%>% count(Alcoholism)
new_DF %>% group_by(No_show)%>% count(Diabetes)
# the rate of difference between male and females that recieved sms and in attendance
new_DF %>% group_by(Gender) %>% count(SMS_received,No_show)

#the difference between males and females for all diseases and attendance rate 
new_DF%>% group_by(Gender) %>% count(Alcoholism,Hipertension,Diabetes,Handcap,No_show)

#to check the rate of difference in each neigbourhood
new_DF %>% group_by(Neighbourhood) %>%
  count(Diabetes,Alcoholism,Handcap,Hipertension)



Gender_count=new_DF%>%count(Gender)
barplot(Gender_count$n,names.arg = c("F","M"),main ="Gender_count",
        xlab = 'Gender', ylab = 'Count',col = c('Green', 'Red')
        
        