# Call Libraries -------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)






# Global Variables -------------------------
z<- "BiofilmCounts.csv"
d <- read.table(z, header=TRUE,sep=",")
d <- d[complete.cases(d), ]
d <-select(d, Treatment, SanitizerType, SanitizerConc, logcfu)
isolates<-rep(c("A","B","C","D","E"),each=10)






#######################################
# FUNCTION: tidy_data
# packages: tidyverse
# purpose:generates mean and variances of outcome variable
# input: data frame with named variables of interest
# output: a Tibble grouped according to predictor variable
# -------------------------------------
tidy_data <- function(d) {
  d_org<-d%>%
    group_by(SanitizerConc)%>%
    summarise(Average=mean(logcfu), variance=var(logcfu),n=n())
  return(as.data.frame(d_org))
    }

d_org<- tidy_data(d=d) 


#######################################
# FUNCTION: sim_data
# packages: tidyverse
# purpose:Simulates random uniform dataset
# input: output of tidy_data
# output: dataframe that simulates original data set
# -------------------------------------
sim_data <- function(d){
  dframe<-data.frame(iso=isolates, 
            control=rnorm(50, d_org$Average[1], d_org$variance[1]),
              low=rnorm(50, d_org$Average[2], d_org$variance[2]),
            medium=rnorm(50, d_org$Average[3], d_org$variance[3]),
            high=rnorm(50, d_org$Average[4], d_org$variance[4]))
  #created df called dframe
  
   x<-dframe%>%
    pivot_longer(cols= control:high, names_to = "SanitizerConc", values_to = "logcfu", values_drop_na=T)
  return(x)
  }
  
sim_data(d=d_org)
x<- sim_data(d=d_org)
#######################################
# FUNCTION: run_stats
# packages: tidyverse
# purpose: runs anova
# input: Output of sim_data function
# output:
# -------------------------------------
run_stats <- function(d) {
  one.way<- aov(logcfu ~ SanitizerConc, data=d)
  return(summary(one.way))
  
  }
 
run_stats(d=x) 

#######################################
# FUNCTION: Plot_graphs
# packages: none
# purpose: plots bar graphs
# input: output of sim_data
# output: Grouped bar graphs
# -------------------------------------
Plot_graphs <- function(d) {
  graph<- ggplot(d) +
    geom_bar(aes(x = iso, y = logcfu, fill = SanitizerConc ), stat = "identity", position = "dodge") +
    theme_minimal() 
  return(print(graph))
  
  }
Plot_graphs(d=x)  


ggplot(data = x, aes(x=iso, y=logcfu)) + geom_line(aes(colour=SanitizerConc))  
  