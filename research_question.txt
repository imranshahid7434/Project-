library(readr)
sp= read_csv("StudentsPerformance.csv")
View(sp)

# creating data frame
data1=data.frame(sp)
View(data1)

#Finding unique vales in each column
for (i in seq(1,ncol(data1)-3,1)){
  print(unique(data1[i]))
}

# cleaning data/missing values
clean_data=complete.cases(data1)
a=data1[clean_data,]
View(a)
any(is.na(data1))

# Summarizing all the columns
summary(data1)

#Data Visualization


library(ggplot2)
#Frequency of Reading score in terms of Gender
ggplot(data=data1,aes(x=reading.score,fill=gender))+
  geom_histogram(col="red",binwidth =1)+ylab("frequency")+
  ggtitle(" Histogram of Reading score")+theme(text=element_text(size=11))+
  xlab("Reading_score")

# Frequency of Writing score in terms of Gender
ggplot(data=data1,aes(x=writing.score,fill=gender))+
  geom_histogram(col="red",binwidth =1)+
  ggtitle("   Histogram of Writing score")+
  theme(text=element_text(size=11))+xlab("Wrting score")+
  ylab("freqeuncy")

#Frequency of Maths Score in terms of Genders
ggplot(data=data1,aes(x=math.score,fill=gender))+
  geom_histogram(col="red",binwidth =1)+ ylab("Freqeuncy")+
  ggtitle("   Histogram of Maths score")+
  theme(text=element_text(size=11))+xlab("Maths_score")

# Frequency of Genders Quantity in terms of Test Prep Course
y1=ggplot(data=data1,aes(x=gender,fill=test.preparation.course))+
  geom_bar()+ylab("Frequency")+ggtitle("test.preparation.course")+
  theme(axis.text = element_text(size=8),axis.title = element_text(size=7))
print(y1)

# Frequency Race/ethics in terms of Test Prep Course
y2=ggplot(data=data1,aes(x=race.ethnicity,fill=test.preparation.course))+
  geom_bar()+ylab("Frequency")+ggtitle("test.preparation.course")+
  theme(axis.title = element_text(size=10),axis.text = element_text(size=7))
print(y2)

# Counting Prep level of education in terms of Test Prep Course
y3=ggplot(data=data1,aes(x=parental.level.of.education,fill=test.preparation.course))+
  geom_bar()+ggtitle("test.preparation.course")+
  theme(axis.title = element_text(size = 8),axis.text = element_text(size = 7))+
  ylab("Frequency")
print(y3)

library(ggpubr)
ggarrange(y1,y2,y3,ncol=1,nrow=3)

# Relationship between Reading Score and Maths Score in terms of Writing Score
ggplot(data=data1,aes(x=reading.score,y=math.score,col=writing.score))+
  geom_point()+ggtitle("Reading score vs math score")+
  geom_smooth(method = 'lm',se=FALSE)

# Relationship between Reading Score and Maths Score in terms of Gender
ggplot(data=data1,aes(x=reading.score,y=math.score,col=gender))+geom_point()+
  geom_smooth(method='lm',se=FALSE)+ggtitle("Reading score vs math score")

# Relationship between Reading Score and Maths Score in terms of Test prep
ggplot(data=data1,aes(x=reading.score,y=math.score,col=test.preparation.course))+
  geom_point()+ ggtitle("Reading score vs math score")+
  geom_smooth(method='lm',se=FALSE)

# Relationship between Reading Score and Maths Score in terms of Race/Ethics
ggplot(data=data1,aes(x=reading.score,y=math.score,col=race.ethnicity))+
  geom_point()+ ggtitle("Reading score vs math score")

# Correlation between Maths Score and Reading Score
cor.test(data1$math.score,data1$reading.score)

#boxplot
#Summary of Maths score for Race/ethics in terms of Gender
ggplot(data=data1,aes(x=race.ethnicity,y=math.score,fill=gender))+
  geom_boxplot()+facet_grid(~gender)+
  theme(text = element_text(size = 10),axis.text = element_text(size = 7) )

#Summary of Reading score for Race/ethics in terms of Gender
#reading score;
ggplot(data=data1,aes(x=race.ethnicity,y=reading.score,fill=gender))+
  geom_boxplot()

# Summary of writing score for Race/ethics in terms of Gender
#writing score;
ggplot(data=data1,aes(x=race.ethnicity,y=writing.score,fill=gender))+
  geom_boxplot()+facet_grid(~gender)

# Now applying hypothesis Test:

#i) 

# Is there a difference in Maths mean score among students who's test 
#preparation is "None" or "complete"

#Null hypothesis: There is no difference in Maths mean score among students 
#who's test preparation is "None" or "complete"

#Alternate hypothesis: There is difference in Maths mean score among students 
#who's test preparation is "None" or "complete"


library(dplyr)
install.packages("moments")
library(moments)
# making Two separate data frames for maths score in terms of test preparation
a=data1%>%select("test.preparation.course","math.score")%>%
  filter(data1$test.preparation.course=="none")
View(a)
b=data1%>%select("test.preparation.course","math.score")%>%
  filter(data1$test.preparation.course=="completed")
View(b)


#normality test to check Normal distribution;
# i)visualization test for normality for maths score with complete Tests prep
qqnorm(a$math.score)
qqline(a$math.score, col = "steelblue",lwd = 2)

# taken alpha =5% 

# ii)Statistic test for normality
shapiro.test(a$math.score)
# since P-value < alpha value so it is not normal distribution

agostino.test(a$math.score)
# since P-value < alpha value so it is not normal distribution

#visualization test for normality for maths score with None Tests prep
qqnorm(b$math.score) # qq-plot
qqline(b$math.score, col = "steelblue",lwd = 2)

#Statistic test for normality
shapiro.test(b$math.score)
# since P-value > alpha value so it is normal distribution

agostino.test(b$math.score)
# since P-value > alpha value so it is normal distribution

# checking the difference between the two data frames of maths score
wilcox.test(a$math.score,b$math.score)

# hence it is proves that mean of both data frames are different as 
#alternate hypothesis is seen as a result






#ii)


# Is there a difference in Reading mean score among students who's test 
#preparation is "None" or "complete"

#Null hypothesis: There is no difference in Reading mean score among students 
#who's test preparation is "None" or "complete"

#Alternate hypothesis: There is difference in Reading mean score among students 
#who's test preparation is "None" or "complete"



# making Two separate data frames for maths score in terms of test preparation
a=data1%>%select("test.preparation.course","reading.score")%>%
  filter(data1$test.preparation.course=="none")
View(a)
b=data1%>%select("test.preparation.course","reading.score")%>%
  filter(data1$test.preparation.course=="completed")
View(b)


#normality test to check Normal distribution;
# i)visualization test for normality for maths score with complete Tests prep
qqnorm(a$reading.score)
qqline(a$reading.score, col = "red",lwd = 2)

# taken alpha =5% 

# ii)Statistic test for normality
shapiro.test(a$reading.score)
# since P-value < alpha value so it is not normal distribution

agostino.test(a$reading.score)
# since P-value < alpha value so it is not normal distribution

#visualization test for normality for maths score with None Tests prep
qqnorm(b$reading.score) # qq-plot
qqline(b$reading.score, col = "steelblue",lwd = 2)

#Statistic test for normality
shapiro.test(b$reading.score)
# since P-value < alpha value so it is not normal distribution

agostino.test(b$reading.score)
# since P-value < alpha value so it is not normal distribution

# checking the difference between the two data frames of maths score
wilcox.test(a$reading.score,b$reading.score)

# hence it is proves that mean of both data frames are different as 
#alternate hypothesis is seen as a result



#iii) 

# Is there a difference in Writing mean score among students who's test 
#preparation is "None" or "complete"

#Null hypothesis: There is no difference in writing mean score among students 
#who's test preparation is "None" or "complete"

#Alternate hypothesis: There is difference in writing mean score among students 
#who's test preparation is "None" or "complete"



# making Two separate data frames for maths score in terms of test preparation
a=data1%>%select("test.preparation.course","writing.score")%>%
  filter(data1$test.preparation.course=="none")
View(a)
b=data1%>%select("test.preparation.course","writing.score")%>%
  filter(data1$test.preparation.course=="completed")
View(b)


#normality test to check Normal distribution;
# i)visualization test for normality for maths score with complete Tests prep
qqnorm(a$writing.score)
qqline(a$writing.score, col = "red",lwd = 2)

# taken alpha =5% 

# ii)Statistic test for normality
shapiro.test(a$writing.score)
# since P-value < alpha value so it is not normal distribution

agostino.test(a$writing.score)
# since P-value < alpha value so it is not normal distribution

#visualization test for normality for maths score with None Tests prep
qqnorm(b$writing.score) # qq-plot
qqline(b$writing.score, col = "steelblue",lwd = 2)

#Statistic test for normality
shapiro.test(b$writing.score)
# since P-value < alpha value so it is not normal distribution

agostino.test(b$writing.score)
# since P-value < alpha value so it is not normal distribution

# checking the difference between the two data frames of maths score
wilcox.test(a$writing.score,b$writing.score)

# hence it is proves that mean of  both data frames are different as 
#alternate hypothesis is seen as a result
















