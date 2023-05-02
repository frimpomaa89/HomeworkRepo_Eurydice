
library(ggplot2)
library(ggthemes)
library(tidyverse)
data()
d<- as.data.frame(UCBAdmissions)# chose to use built-in R dataframes
  

# i think this data set is best represented by a group bar charts

# I first looked at Admissions by gender
groupbar <- ggplot(d) +
  geom_bar(aes(x = Gender, y = Freq, fill = Admit), stat = "identity", position = "dodge") +
  theme_minimal() 
print(groupbar)

# this time i grouped the bars accoding to admissions
groupbar2 <- ggplot(d) +
  geom_bar(aes(x = Admit, y = Freq, fill = Gender), stat = "identity", position = "dodge") +
  theme_minimal() 
print(groupbar2)

# I then decided to just look at rejection rates by gender

d1<-filter(d, Admit=="Rejected")
groupbar3 <- ggplot(d1) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge", scale_fill_hue(c=40)) +
  theme_minimal() # controlling the hue
print(groupbar3)

# I also looked at admission rates within departments by gender

d2<-filter(d, Admit=="Admitted")
groupbar4 <- ggplot(d2) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge") +
  theme_minimal() 
print(groupbar4)

#orienting the chart horizontally
groupbar5 <- ggplot(d2) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge") +
  theme_minimal() + coord_flip()
print(groupbar5)

#Using Color brewer
groupbar6 <- ggplot(d2) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge") + scale_fill_brewer(palette = "Set1") + theme_minimal()
print(groupbar6)


#Using Grey scale
groupbar7 <- ggplot(d2) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge") + scale_fill_grey(start = 0.25, end = 0.75) + theme_minimal()
print(groupbar7)


#Selecting my own colors
groupbar8 <- ggplot(d2) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge") + scale_fill_manual(values = c("green", "blue") ) +
 theme_minimal()
print(groupbar8)

# Add a chart Title
groupbar6 <- ggplot(d2) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge") + scale_fill_brewer(palette = "Set1") + ggtitle("Gender Disparities in Departmental Admissions at UCB") + theme_minimal()
print(groupbar6)

# Presenting two small charts side by side
groupbar9 <- ggplot(d) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge") + ggtitle("Gender Disparities in Departmental Admissions at UCB") + facet_wrap(~Admit) + theme_minimal() 
print(groupbar9)

#Removing Grid lines
groupbar10 <- ggplot(d) +
  geom_bar(aes(x = Dept, y = Freq, fill = Gender), stat = "identity", position = "dodge") + ggtitle("Gender Disparities in Departmental Admissions at UCB") + facet_wrap(~Admit) + theme_minimal() + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
print(groupbar10)
