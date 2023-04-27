#Tidy Verse Lecture
# 03/01/2023
# Eurydice Aboagye


library(tidyverse)
library(dplyr)
library(sqldf)

# start with template data sets
data("starwars")
class(starwars)
glimpse(starwars)



# Clean Dataset
starwarsClean<-starwars[complete.cases(starwars[,1:10]),]

is.na(starwarsClean[1,1]) #useful for individual/few observations

anyNA(starwarsClean)

# the core verbs:

# filter() subsets obervations byu their values.Uses conditional statements and logical operators (>,<, !=, ==), $ | .

filter(starwarsClean, gender=="feminine" & height<180)

filt<-filter(starwarsClean, gender=="feminine" & height< 180, height>100)

# %in% is used for a few conditions, similar to ==

filt<-filter(starwars, eye_color %in% c("blue","brown"))


# arrange() reorders rows by either ascending, descending, or whatever you want

order<-arrange(starwarsClean, by=height) # ascending order

order<-arrange(starwarsClean, by=desc(height))

order<-arrange(starwarsClean, height, desce(mass))
#Missing values are places at the end of dataset


# select() Chossing variables/columns by thier names

starwarsClean[1:10, ] #Base R
select(starwarsClean, 1:10)
x<- select(starwarsClean, name:species) #using column names
x<-select(starwarsClean, -(films:starships)) #will subset everything except these paticular variables


#rearrange columns
x<-select(starwarsClean, gender, name, species, everything()) # everything pulls in whatever is left

X<-select(starwarsClean, contains("colour"))
#contains() looks for a pattern to match

#Rename columns
X<-select(starwarsClean, haircolor=hair_color) #New name= old name

x<-rename(starwarsClean, haircolor=hair_color)



# Summarize() and group_by() collapses values down to single summary points

X<-summarize(starwarsClean, meanHeight=mean(height)) #wont work with NA's still inside the dataset

summarize(starwars, meanHeight, na.rm=TRUE), TotalNumber=n())

#Use group_by to specify grouping variables
starwarsGender<-group_by(starwars, gender)


# Mutate() adding/creating new variables
X<- mutate(starwarsClean, ratio=height/mass)




X<-select(starwars_lbs, 1:3, mass_lbs, everythihng())

#If we only want the new variable
transmute(starwarsClean, mass, mass_lbs=mass*2.2)



#Pipe Statements, or Piping %>%
#Pipe statements roughly translate to "and then"

starwarsClean2<-starwarsClean %>%
  group_by(gender)%>%
  summarize(meanHeight=mean(height, na.rm=TRUE), number=n())


starwarsClean2<-starwarsClean %>%
  mutate(sp=case_when(species="Human")~"Human", TRUE ~ "Non-Human"))%>%
  select(name, sp, everything())


#Pivoting Datasets

#Long Format to wide Format
starwarsClean

wideSW<-starwarsClean%>%
  select(name, sex, height)%>%
  pivot_wider(names_from=sex, values_from = height, values_fill = NA)%>%
  pivot_longer(cols=male:female, names_to="sex", values_to="height", values_drop_na=T)


# Lecture by George on sql

#Read in our datafiles

setwd("`/Documents/")

Homework 6

# Question One
data("iris")

iris1<- filter(iris, Species == "virginica" |Species == "versicolor", "sepal.Length" > 6, "sepal.Width)"> 2.5)

iris2<- select(iris1, Species, Sepal.Length, Sepal.Width)

iris3<- arrange(iris2, by=desc(Sepal.Length), Sepal.Width)
head(iris3)

iris4<- mutate(iris3, Sepal.Area= Sepal.Length* Sepal.Width)

iris5<-summarise()

group_by(iris4, Species)
iris5<-summarise(iris4, avgSepalLength=mean(Sepal.Length), avgSepalWidth=mean(Sepal.Width), n=n())

print(iris5)

iris4 %>%
  group_by(Species)%>%
  summarise(avgSepalLength=mean(Sepal.Length), avgSepalWidth=mean(Sepal.Width), n=n()) ->iris6

print(iris6)

iris %>%
  filter(Species == "virginica" |Species == "versicolor", "sepal.Length" > 6, "sepal.Width)"> 2.5)%>%
  select(Species, Sepal.Length, Sepal.Width)%>%
  arrange(by=desc(Sepal.Length), Sepal.Width)%>%
  mutate(Sepal.Area= Sepal.Length* Sepal.Width)%>%
  group_by(Species)%>%
  summarise(avgSepalLength=mean(Sepal.Length), avgSepalWidth=mean(Sepal.Width), n=n())->irisFinal

print(irisFinal)

iris %>%
  pivot_longer(cols= Sepal.Length:Petal.Width, names_to = "Measure", values_to = "Value", values_drop_na=T) -> iris7
  
  
