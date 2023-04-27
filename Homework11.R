library (tidyverse)

x<-read.csv("~/Github repositories/RepoTrial/HomeworkRepo_Eurydice/DeerBodyMass.csv")

for(i in 2003:2010) {
  y<-x%>%
    filter(Year==i)
  output<-paste0(i)
  setwd("~/Github repositories/RepoTrial/HomeworkRepo_Eurydice/NewDeerData")
  write.csv(y, file=output, sep=",", col.names=TRUE, row.names=FALSE)

}

# I then deleted the file for 2008 because the dataset did not have any data for 2008

file_name <- "2008"

if (file.exists(file_name)) {
  unlink(file_name)
  print("File is deleted..")
} else{
  print("File not exists..")
}
# Global Variables -------------------------
file_folder <- "~/Github repositories/RepoTrial/HomeworkRepo_Eurydice/NewDeerData"
n_files <- 7
file_out <- "StatsSummary.csv"
file_names<- list.files(path=file_folder)



# Create data frame to hold file summary statistics
ID <- seq_along(file_names)
slope <- rep(NA, length(file_names))
p_val <- rep(NA, length(file_names))
r2 <- rep(NA, length(file_names))

stats_out <- data.frame(ID,file_names,slope,p_val,r2)



# batch process by looping through individual files
for (i in seq_along(file_names)) {
  data <- read.table(file=file_names[i], sep=",", header=TRUE)# read in next data file 
  . <- lm(data=data, data[ ,8]~data[ ,6])  #function in y~x
  . <- summary(.)
  .$coefficients[1,1]
  stats_list <- list(slope=.$coefficients[2,1],
                     p_val=.$coefficients[2,4],
                     r2=.$r.squared)
  stats_out[i,3:5] <- unlist(stats_list) # unlist, copy into last 3 columns

}    


# set up output file and incorporate time stamp and minimal metadata
write.table(cat("# Summary stats for ",
                "batch processing of regression models","\n",
                "# timestamp: ",as.character(Sys.time()),"\n",
                "# NJG","\n",
                "# ------------------------", "\n",
                "\n",
                file=file_out,
                row.names="",
                col.names="",
                sep=""))

# now add the data frame
write.table(x=stats_out,
            file=file_out,
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)
x<-read.table("StatsSummary.csv", sep=",", header=TRUE)


# now for the graphs

for (i in 1: length(file_names)){
  #read in data from current file
  data <-read.csv(file_names[i])
  #generate a scatter plot of the regression variables
  p<- ggplot(data, aes(y=data[, 8], x=data[, 6], geom_point() + labs(title = file_list[i])))+ stat_smooth(method=lm,se=0.95)
  print(p)
  #save the plots
  ggsave("p.pdf")
}
