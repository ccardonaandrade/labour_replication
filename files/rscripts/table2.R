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
library(statar)
library(xtable)
library(oce)

options(xtable.floating = FALSE)
options(xtable.timestamp = "")


#Directory
setwd("/Users/Carlos Cardona/Dropbox/PhD Warwick/Second Year/Labour Econ/Replication/Paper Files/")

fd <- read.dta("1. fd_data.dta")
pool <- read.dta("2. pooled_data.dta")

###############
#  TABLE 2
###############

subfd <- subset(fd,dni_ny_n>0)


fd$dnix <- ifelse(fd$dni_ny_n>0, ntile(subfd$dni_ny_n, 3) , NA)



fd <- transform(fd, dnix = ifelse(dnix==3, 4,
                                  ifelse(dnix==2,3,ifelse(dnix==1,2, dnix))))

fd <- transform(fd, dnix = ifelse(dni_ny_n==0 & type4e==0, 0, dnix))
fd <- transform(fd, dnix = ifelse(type4e==1, 1, dnix))


panelA <-data.frame(ddply(fd, .(dnix), summarize,mean=mean(logoccscore_d), sd=sd(logoccscore_d)))



panelA <- t(panelA)
panelA <- panelA[c(2:3),c(1:5)]


panelA <- cbind(panelA, c(mean(fd$logoccscore_d), sd(fd$logoccscore_d) ))
panelA <- cbind(panelA, c(mean(subfd$logoccscore_d), sd(subfd$logoccscore_d) ))


panelA <-as.data.frame(panelA)
panelA <- select(panelA,V6,V1,V2,V7,V3,V4,V5)

colnames(panelA) <- c("All", "Keepers", "Others", "Americanize", "First", "Second", "Third")

######### I need a loop here

panelB <-data.frame(ddply(fd, .(dnix), summarize,"Americanization index"=mean(rf_ny_n_d), sd=sd(rf_ny_n_d)))
panelB <- t(panelB)
panelB <- panelB[c(2:3),c(1:5)]
panelB <- cbind(panelB, c(mean(fd$rf_ny_n_d), sd(fd$rf_ny_n_d) ))
panelB <- cbind(panelB, c(mean(subfd$rf_ny_n_d), sd(subfd$rf_ny_n_d) ))

panelB <-as.data.frame(panelB)
panelB <- select(panelB,V6,V1,V2,V7,V3,V4,V5)
colnames(panelB) <- c("All", "Keepers", "Others", "Americanize", "First", "Second", "Third")

#
panelC <-data.frame(ddply(fd, .(dnix), summarize,"Age"=mean(age_d), sd=sd(age_d)))
panelC <- t(panelC)
panelC <- panelC[c(2:3),c(1:5)]
panelC <- cbind(panelC, c(mean(fd$age_d), sd(fd$age_d) ))
panelC <- cbind(panelC, c(mean(subfd$age_d), sd(subfd$age_d) ))

panelC <-as.data.frame(panelC)
panelC <- select(panelC,V6,V1,V2,V7,V3,V4,V5)
colnames(panelC) <- c("All", "Keepers", "Others", "Americanize", "First", "Second", "Third")

#

panelD <-data.frame(ddply(fd, .(dnix), summarize,"Years since migration"=mean(ysm_d,na.rm=TRUE), sd=sd(ysm_d,na.rm=TRUE)))
panelD <- t(panelD)
panelD <- panelD[c(2:3),c(1:5)]
panelD <- cbind(panelD, c(mean(fd$ysm_d,na.rm=TRUE), sd(fd$ysm_d,na.rm=TRUE) ))
panelD <- cbind(panelD, c(mean(subfd$ysm_d,na.rm=TRUE), sd(subfd$ysm_d,na.rm=TRUE) ))

panelD <-as.data.frame(panelD)
panelD <- select(panelD,V6,V1,V2,V7,V3,V4,V5)
colnames(panelD) <- c("All", "Keepers", "Others", "Americanize", "First", "Second", "Third")


panelA <- rbind(panelA, panelB, panelC, panelD)

panelA %>% mutate_at(vars("All", "Keepers", "Others", "Americanize", "First", "Second", "Third"), funs(round(., 2)))


almost <- panelA



rm(panelA, panelB,panelC,panelD)
# Panel B


panelA <-data.frame(ddply(fd, .(dnix), summarize,mean=mean(dlogoccscore), sd=sd(dlogoccscore)))



panelA <- t(panelA)
panelA <- panelA[c(2:3),c(1:5)]


panelA <- cbind(panelA, c(mean(fd$dlogoccscore), sd(fd$dlogoccscore) ))
panelA <- cbind(panelA, c(mean(subfd$dlogoccscore), sd(subfd$dlogoccscore) ))


panelA <-as.data.frame(panelA)
panelA <- select(panelA,V6,V1,V2,V7,V3,V4,V5)

colnames(panelA) <- c("All", "Keepers", "Others", "Americanize", "First", "Second", "Third")

######### I need a loop here

panelB <-data.frame(ddply(fd, .(dnix), summarize,"Americanization index"=mean(dni_ny_n), sd=sd(dni_ny_n)))
panelB <- t(panelB)
panelB <- panelB[c(2:3),c(1:5)]
panelB <- cbind(panelB, c(mean(fd$dni_ny_n), sd(fd$dni_ny_n) ))
panelB <- cbind(panelB, c(mean(subfd$dni_ny_n), sd(subfd$dni_ny_n) ))

panelB <-as.data.frame(panelB)
panelB <- select(panelB,V6,V1,V2,V7,V3,V4,V5)
colnames(panelB) <- c("All", "Keepers", "Others", "Americanize", "First", "Second", "Third")

#
panelC <-data.frame(ddply(fd, .(dnix), summarize,"Age"=mean(dage), sd=sd(dage)))
panelC <- t(panelC)
panelC <- panelC[c(2:3),c(1:5)]
panelC <- cbind(panelC, c(mean(fd$dage), sd(fd$dage) ))
panelC <- cbind(panelC, c(mean(subfd$dage), sd(subfd$dage) ))

panelC <-as.data.frame(panelC)
panelC <- select(panelC,V6,V1,V2,V7,V3,V4,V5)
colnames(panelC) <- c("All", "Keepers", "Others", "Americanize", "First", "Second", "Third")

#

panelD <-data.frame(ddply(fd, .(dnix), summarize,"Years since migration"=mean(dysm,na.rm=TRUE), sd=sd(dysm,na.rm=TRUE)))
panelD <- t(panelD)
panelD <- panelD[c(2:3),c(1:5)]
panelD <- cbind(panelD, c(mean(fd$dysm,na.rm=TRUE), sd(fd$dysm,na.rm=TRUE) ))
panelD <- cbind(panelD, c(mean(subfd$dysm,na.rm=TRUE), sd(subfd$dysm,na.rm=TRUE) ))

panelD <-as.data.frame(panelD)
panelD <- select(panelD,V6,V1,V2,V7,V3,V4,V5)
colnames(panelD) <- c("All", "Keepers", "Others", "Americanize", "First", "Second", "Third")


panelA <- rbind(panelA, panelB, panelC, panelD)

panelA %>% mutate_at(vars("All", "Keepers", "Others", "Americanize", "First", "Second", "Third"), funs(round(., 2)))

rm( panelB,panelC,panelD)


almost <- rbind(almost,panelA)

table2 <-xtable(almost)
print(table2, include.rownames = TRUE)


