
#About the data set:
#The data set is based on the rates of suicides for different countries,spanning from 
#1985 to 2015.

#It includes: country, year, sex, age group, count of suicides, population, 
#suicide rate, country-year composite key, HDI for year, gdp_for_year,
#gdp_per_capita, and generation.

#Key terms to note: HDI is the Human Developement Index. GDP is Gross domestic Prod.
#The generations included are: Boomers, G.I., Generation X, Generation z,
#Millenials, Silent.

#Boomers:Baby boomer is a term used to describe a person who was born between 1946
#and 1964(During Vietnam War). The baby boomer generation makes up a substantial 
#portion of the world's population, especially in developed nations.

#Generation x:1950s (originally referring to a generation of young people about
#whose future there was uncertainty).

#GI Generation: The G.I. Generation (also known as the World War II Generation or 
#The Greatest Generation in the United States or the Federation Generation in 
#Australia).

#Generation Z: the generation reaching adulthood in the second decade of the 
#21st century, perceived as being familiar with the Internet from a very young age.

#Millenials:The most talked about generation. Born after 1996.

#Silent: Lesser known. No precise timeline is known.

#Reference: The data set has been taken from kaggle.com.

#No missing values or outliers were encountered.


library(dplyr)
library(ggplot2)
load(file = "DataCIA3.RData")
Suicide_data<-filter(DataCIA3, DataCIA3$country=="United States")
View(Suicide_data)
head(Suicide_data)

#Data Visualisation
#In order to understand the data better, we use data visualisation.

p<-ggplot(data = Suicide_data, aes(xaxis=Suicide_data$population,
                                   yaxis= Suicide_data$`gdp_per_capita ($)`,
                                   size=Suicide_data$suicides_no,
                                   color=Suicide_data$generation))
p+geom_point(aes(x=Suicide_data$population,y=Suicide_data$`gdp_per_capita ($)`))
#This plot takes into account 4 variables: GDP per capita, Population, Generation
#and No. of Suicides.
#On the x axis it is is obvious that the no. of suicides go up with the increase
#in population because of the crowding, race for jobs, scarcity of resources etc.
#On the y axis the points usually go on decreasing in size as the GDP per capita
#increases becuase it will result in a better standard of living.
#The dots for boomers(pink) appear the largest everywhere along with Generation x.
#For these particular generations, the population factor overpowers the GPD factor.
#This tells us that these are not mainly concerned with their standard of living,
#and depend on other factors.
#For all other other generations, the size goes on decreasing drastically up the 
#y axis which tells that they are satisfied with life as their standard of living
#is improved.

p<-ggplot(data = Suicide_data, aes(xaxis=Suicide_data$age,
                                   yaxis= Suicide_data$`gdp_per_capita ($)`,
                                   size=Suicide_data$suicides_no,
                                   color=Suicide_data$generation))
p+geom_point(aes(x=Suicide_data$age,y=Suicide_data$`gdp_per_capita ($)`))
#This plot takes into account the Age instead of the population, and shows how the
#suicide rate depends on the factors of Generation and GDP per capita.
#The first thing to note here is that in age groups 35-54 and 55-74 the suicide rate
#is the highest.
#Almost all the age groups are divided between 2 or 3 generation groups.
#In the age group of 15-24, the generatoins Millenials and Generation X commit the
#most suicides.
#Boomers, Generation z and Xfor 25-34 and so on and so forth for all the groups.


ggplot(data=Suicide_data, aes(x=Suicide_data$population, y=Suicide_data$suicides_no,
                              group=Suicide_data$sex))+
  geom_line(aes(color=Suicide_data$sex))+
  geom_point(aes(color=Suicide_data$sex))
#This plot gives a comparision of the No. of suicides commited by males and females.
#Except for a small range of population, right thorughout the years, males have 
#commited more suicides by a landslide.

ggplot(data=Suicide_data, aes(x=Suicide_data$year, y=Suicide_data$suicides_no))+geom_point()
#This plot shows how the number of suicides increased every year from 1985 to 2015.

ggplot(data=Suicide_data, aes(x=Suicide_data$population, y=Suicide_data$suicides_no,
                              group=Suicide_data$age))+
  
  geom_point(aes(color=Suicide_data$age))  
#This plot shows how the number of suicides Varies with the age and population.
#There is a constant increase in the rate with the increase in population.
#The highest number of suicides is for the age group 35-54, followed by 55-74
#and the least is for the group 5-14.

#Analysis
summary(Suicide_data)
#The summary tells us about all the basic Statitics for the given data for all variables
#some important conclusions to draw from that are:
#1.The mean number of suicides every year in the US is 2779. Which means that, on an
#average, 2779 people in the US commit suicide.
#2.The mean of suicide/100k population tells us that on an average, 14 people commit
#suicide out of every 100k.
#3.The average gdp per capita is $39270. This is higher than 80% of the countries in
#the world, yet the suicide rate per 100k in the US is pretty high as compared
#to other countries. It is ranked 38th in the world for suicides per 100k.

cor(Suicide_data$`gdp_per_capita ($)`,Suicide_data$`suicides/100k pop`)
plot(Suicide_data$`gdp_per_capita ($)`,Suicide_data$`suicides/100k pop`)
#The correlation between Gdp per capita and Suicide per 100k is -0.06,
#which is negative and weak.
#This is unexpected because one would think that as gdp per capita tells us about
#the standard of living, it must be strongly correlated to the suicide/100k.
#However, we can drawn an important conclusion from this: that, people
#commiting suicides have reasons which have to do with their mental health,
#or something else which has nothing to do with their standard of living.

sexr<-as.factor(Suicide_data$sex)
table(Suicide_data$sex)

cor(Suicide_data$population,Suicide_data$suicides_no)
plot(Suicide_data$population,Suicide_data$suicides_no)
#The correlation between Population and No. of suicides is moderate positive.
#This is as one would have expected because as population increases, the number 
#of suicides will tend to be higher because the competition for jobs, marks 
#getting a place to live, food, all increases, hence detoriating the mental health of
#the people.

tapply(Suicide_data$suicides_no, Suicide_data$age, mean)
#This gives us the mean of the number of suicides by age category.
#The greatest mean no. of suicides is for the group 35-54 years followed by 55-74.
#This can be uderstood because 35-54 years of age accounts for the most hard working
#years of a person's life and majorly because of mid life crisis.
#Second is 55-74 which is because of the serious mental health detoriation, due to
#the pressure of work before retirement.
#Means of 15-24 and 25-34 follows these with 2285 and 2936 which is obviously because
#of the pressure of school finals, college and early days of working.

a<-tapply(Suicide_data$suicides_no, Suicide_data$year, mean)
max(a)
min(a)
#This gives us the mean of the number of suicides by year category.
#It is very clear that the mean goes on increasing from 1985 to 2015 right thorugh
#all 30 years without fail.
#This is because of the constant rise in population and replacement of jobs by 
#machines and scarcity of food and water and the spread of diseases as already
#mentioned above.

tapply(Suicide_data$suicides_no,Suicide_data$generation, mean)
#The highest mean of No. of Suicides is for the generation of Boomers.
#As already mentioned, Boomers were born during the era of the Vietnam war, and are
#expected to have seen some difficult times because of that in their childhood
#which can be assumed for the high rate of suicides.
#Generation x has the second highest mean. This can be interpreted by their
#description that they were born in era with an uncertainity of their future.
#Generation z has the least mean which can be interpreted by the fact that they
#were born where they knew about the internet and they were more aware about the 
#importance of mental health.