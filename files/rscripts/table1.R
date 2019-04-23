#########################################################
#                 Replication Exercise
########################################################

rm(list=ls()) #clear all

library(foreign)
library(stargazer)
library(data.table)
library(xlsx)
library(plyr)
library(dplyr)
library(lubridate)
library(plm) 
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")


#Directory
setwd("/Users/Carlos Cardona/Dropbox/PhD Warwick/Second Year/Labour Econ/Replication/Paper Files/")

fd <- read.dta("1. fd_data.dta")
pool <- read.dta("2. pooled_data.dta")

###############
#  TABLE 1
###############

# Panel A

fd$unit <- 1
panelA <-data.frame(ddply(fd, .(cobx_p), summarize,Americanized=mean(dextm), N=sum(unit)))
panelA <- data.frame(lapply(panelA, as.character), stringsAsFactors=FALSE)
panelA$N <- as.numeric(panelA$N )

panelA <- rbind(panelA, c("Total", mean(fd$dextm),sum(panelA$N)))
panelA$Americanized <- as.numeric(panelA$Americanized )

panelA$Americanized <- panelA$Americanized*100

panelA %>% mutate_at(vars(Americanized), funs(round(., 2)))

names(panelA)[1]<-"Country of Origin"
panelA$id = 1:nrow(panelA)





#Panel B

panelB <- subset(fd, dextm == 1)
panelB <-data.frame(ddply(panelB, .(name_p), summarize, N=sum(unit)))
panelB$total <- sum(panelB$N)
panelB$Americanized <- panelB$N/panelB$total



panelB$sample <- ifelse((panelB$name_p == "john") | (panelB$name_p == "william") 
          | (panelB$name_p == "joseph" ) | (panelB$name_p == "charles" ) |
            (panelB$name_p == "george" ) | (panelB$name_p == "patrick" )
          | (panelB$name_p == "moishe" ) | (panelB$name_p == "giulio" ), 1,0)


panelB <- subset(panelB, sample == 1)
panelB <- panelB[order(-panelB$Americanized) , ]
panelB$Americanized <- panelB$Americanized*100
panelB <- select(panelB,name_p, Americanized)
panelB <- rbind(panelB, c("Moishe", 0.00 ))
panelB <- rbind(panelB, c("Giulio", 0.00 ))
panelB$Americanized <- as.numeric(panelB$Americanized )

panelB %>% mutate_at(vars(Americanized), funs(round(., 2)))

panelB$id = 1:nrow(panelB)



# Joining both tables
table1 <- join(panelA, panelB, by = "id")
table1$id <- NULL
names(table1)[4]<-"Name"
names(table1)[5]<-"% Americanized Migrants"


# The table
table1 <-xtable(table1)
print(table1, include.rownames = FALSE)