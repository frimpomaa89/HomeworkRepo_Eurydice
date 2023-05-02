library(tidyverse)
library(ggplot2)
library(ggthemes)

z <- read.table("BiofilmCounts.csv",header=TRUE,sep=",")
z <- z[complete.cases(z), ]
str(z$QAC_conc)
summary(z)


z1<-select(z, Treatment, SanitizerType, SanitizerConc, logcfu)


z1 %>%
  group_by(z1[,3])%>%
  summarise(AverageCfu=mean(logcfu), variance=var(logcfu), n=n()) ->z2
print(z2)

isolates<-rep(c("A","B","C","D","E"),each=10)
control<-rnorm(50, mean = 7.47, sd=0.716)
low<-rnorm(50, mean=6.86, sd=1.50)
medium<-rnorm(50, mean=6.91, sd=1.31)
high<- rnorm(50, mean=6.59, sd=1.92)

dframe<- data.frame(isolates, control, low, medium, high)

head(dframe)
dframe %>%
  pivot_longer(cols= control:high, names_to = "SanitizerConc", values_to = "logcfu", values_drop_na=T) -> dframe2

head(dframe2)

one.way <- aov(logcfu ~ SanitizerConc, data=dframe2)
summary(one.way)

plot1<-ggplot(dframe2) +
  geom_bar(aes(x = isolates, y = logcfu, fill = SanitizerConc ), stat = "identity", position = "dodge") +
  theme_minimal() 
print(plot1)

# running the analyses again with a different set of random numbers
isolates<-rep(c("A","B","C","D","E"),each=10)
control<-rnorm(50, mean = 7.47, sd=0.716)
low<-rnorm(50, mean=6.86, sd=1.50)
medium<-rnorm(50, mean=6.91, sd=1.31)
high<- rnorm(50, mean=6.59, sd=1.92)

dframe<- data.frame(isolates, control, low, medium, high)

head(dframe)
dframe %>%
  pivot_longer(cols= control:high, names_to = "SanitizerConc", values_to = "logcfu", values_drop_na=T) -> dframe2

head(dframe2)

one.way <- aov(logcfu ~ SanitizerConc, data=dframe2)
summary(one.way)

plot1<-ggplot(dframe2) +
  geom_bar(aes(x = isolates, y = logcfu, fill = SanitizerConc ), stat = "identity", position = "dodge") +
  theme_minimal() 
print(plot1)

#now varying the means to determine effect sizes

isolates<-rep(c("A","B","C","D","E"),each=10)
control<-rnorm(50, mean = 6.47, sd=0.716) #adjusted the means to be closer to treatment values
low<-rnorm(50, mean=6.86, sd=1.50)
medium<-rnorm(50, mean=6.91, sd=1.31)
high<- rnorm(50, mean=6.99, sd=1.92)# Adjusted the mean to be closer to the medium values

dframe<- data.frame(isolates, control, low, medium, high)

head(dframe)
dframe %>%
  pivot_longer(cols= control:high, names_to = "SanitizerConc", values_to = "logcfu", values_drop_na=T) -> dframe2

head(dframe2)

one.way <- aov(logcfu ~ SanitizerConc, data=dframe2)
summary(one.way)

plot1<-ggplot(dframe2) +
  geom_bar(aes(x = isolates, y = logcfu, fill = SanitizerConc ), stat = "identity", position = "dodge") +
  theme_minimal() 
print(plot1)

#I found significance, I then run post hoc analyses to determine significane within groups

TukeyHSD(one.way)

#running the model a few times with the same parameters set to get a feeling for the effect of random variation in the data.

isolates<-rep(c("A","B","C","D","E"),each=10)
control<-rnorm(50, mean = 9.47, sd=0.716) #2log increase in control samples
low<-rnorm(50, mean=6.86, sd=1.50)
medium<-rnorm(50, mean=6.91, sd=1.31)
high<- rnorm(50, mean=3.59, sd=1.92)# 3 log decrease in high sanitizer concentration samples

dframe<- data.frame(isolates, control, low, medium, high)

head(dframe)
dframe %>%
  pivot_longer(cols= control:high, names_to = "SanitizerConc", values_to = "logcfu", values_drop_na=T) -> dframe2

head(dframe2)

one.way <- aov(logcfu ~ SanitizerConc, data=dframe2)
summary(one.way)

plot1<-ggplot(dframe2) +
  geom_bar(aes(x = isolates, y = logcfu, fill = SanitizerConc ), stat = "identity", position = "dodge") +
  theme_minimal() 
print(plot1)

TukeyHSD(one.way)# this time there was no difference found between medium sanitizer conc and low sanitizer conc
