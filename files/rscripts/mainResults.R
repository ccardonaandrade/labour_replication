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
library(RCurl)





#Directory
setwd("/Users/Carlos Cardona/Dropbox/PhD Warwick/Second Year/Labour Econ/Replication/Paper Files/")

#Loading both data sets. The panel and the first difference. 

fd <- read.dta("1. fd_data.dta")
pool <- read.dta("2. pooled_data.dta")


################################################
##            Table 3
################################################

 
# The regressions
  


# I realize that the variable cohort was not a dummy so I transformed into it.
#However ther results just change in the second decimal in few occasions.
#pool$newcohort <- as.numeric(pool$cohort>0)
#fd$newcohort <- as.numeric(fd$cohort>0)

#pool$cohort <- NULL
#fd$cohort <- NULL

#setnames(pool, "newcohort", "cohort")
#setnames(fd, "newcohort", "cohort")

### Column 1
### For Colum 1 and 2, in the original code they do not estimate the equation
### using the interaction of cohort and ysm. Despite this, the results do not
### change much. 

  column.1 <- lm(logoccscore ~ rf_ny_n  + married + usspouse + nchild + uskid +  migd + ysm + cohort + cohort:ysm , data=pool)

# Here they calculate the predicted score for the entire sample and for Americanizers  
  yhato1 <- predict(column.1, pool)
  yhatos1 <- predict(column.1, subset(pool,extmg==1))
  
# This is for getting the mean of the predicted values
  y1<-mean(yhato1)
  y2<-mean(yhatos1)
  

### Column 2

  column.2 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
             migd + cohort  + ysm + cohort:ysm + cobx : ysm  + factor(cd) :ysm, 
            data = pool)
  
  proof.2 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                   migd + cohort + cohort : ysm  + ysm + cobx : ysm  + factor(cd) :ysm, 
                 data = pool)
  yhato2 <- predict(column.2, pool)
  yhatos2 <- predict(column.2, subset(pool,extmg==1))
  y3<-mean(yhato2)
  y4<-mean(yhatos2)
  

  ## Column 3
  # These changes of name is for facilitating typing the regressions
  # Also I think the variable of interest should have the same name in order
  # to have in the table at the end all the coefficients in the  same row
  
  setnames(fd, "dni_ny_n", "rf_ny_n")
  setnames(fd, "dmarried", "married")
  setnames(fd, "dusspouse", "usspouse")
  setnames(fd, "dnchild", "nchild")
  setnames(fd, "duskid", "uskid")
  setnames(fd, "dmig1", "migd")
  
  column.3 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                   nchild + uskid + migd + dysm + cohort, data = fd)
  
  # Predicted values and their means
  yhato3 <- predict(column.3, fd)
  yhatos3 <- predict(column.3, subset(fd,rf_ny_n>0))
  y5<-mean(yhato3)
  y6<-mean(yhatos3)
 
  ## Column 4
  
  column.4 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                   nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + factor(cd_d) :dysm, data = fd)
  
  yhato4 <- predict(column.4, fd)
  yhatos4 <- predict(column.4, subset(fd,rf_ny_n>0))
  y7<-mean(yhato4)
  y8<-mean(yhatos4)
  

  # column 5

  setnames(fd,"rf_ny_n", "dni_ny_n")
  setnames(fd, "dni_ny_n_pd", "rf_ny_n")
  
  column.5 <- lm(dlogoccscore ~ -1 + rf_ny_n  + married + usspouse + nchild + uskid +  migd + dysm + cohort , data=subset(fd, type1e==0))
  yhato5 <- predict(column.5, subset(fd, type1e==0))
  yhatos5 <- predict(column.5, subset(fd,rf_ny_n>0 & type1e==0))
  y9<-mean(yhato5)
  y10<-mean(yhatos5)
  

  #Column 6
  column.6 <- lm(dlogoccscore ~ -1 + rf_ny_n  + married + usspouse + nchild + uskid +  migd + dysm + cohort +cobx_p : dysm  + factor(cd_d) :dysm, data=subset(fd, type1e==0))

  yhato6 <- predict(column.6,subset(fd, type1e==0))
  yhatos6 <- predict(column.6, subset(fd,type1e==0 & rf_ny_n>0))
  y11<-mean(yhato6)
  y12<-mean(yhatos6)
  

  # This is a loop for rounding the mean of predicting values for fitting them
  # in the table
  for(i in 1:12) {
    
  eval(parse(text=paste(paste0("y", i), paste0("format(round(y", i,", 3), nsmall = 3)"), sep="<-")))
  }
  
  # This is a loop for calculating robust standard errors equally as in Stata
  for(i in 1:6) {
    
    eval(parse(text=paste(paste0("cov", i), paste0("vcovHC(column.", i,", type = 'HC1')"), sep="<-")))
    eval(parse(text=paste(paste0("robust_se.", i), paste0("sqrt(diag(cov", i,"))"), sep="<-")))
    
  }


 # This is for the Table 3 of the paper.  
  
  stargazer(column.1, column.2, column.3, column.4, column.5, column.6,
            column.labels=c("OLS", "OLS", "First Differences",  "First Differences","Name Changers Only","Name Changers Only"),
            keep=c("rf_ny_n","married","usspouse","nchild","uskid","migd"),
            covariate.labels=c("A","Married","Has US-born spouse",
           "Number of children","Has US-born child(ren)","Resides outside NYC"), digits = 3,
           omit.stat=c("ser","adj.rsq","f", "ser"),
           title="Effect of Name Americanization on Log Occupational Score",
           table.placement = "H", no.space=TRUE, model.names = FALSE,
           se = list(robust_se.1,robust_se.2,robust_se.3,robust_se.4, robust_se.5, robust_se.6),
           add.lines = list(c("YSM", "Yes", "Yes","Yes","Yes","Yes","Yes"),
                            c("Arrival prior to 1921", "Yes", "Yes","","","",""),
                            c("Arrival prior to 1921 X YSM", "Yes", "Yes","Yes","Yes","Yes","Yes"),
                            c("Country of birth", "No", "Yes","","","",""),
                            c("Country of birth X YSM", "No", "Yes","No","Yes","No","Yes"),
                            c("Labor market", "No", "Yes","","","",""),
                            c("Labor market X YSM", "No", "Yes","No","Yes","No","Yes"),
                            c("Predicted occupational score (whole sample)", y1, y3,y5,y7,y9,y11),
                            c("Predicted occupational score (Americanizers)", y2, y4,y6,y8,y10,y12)))
  
  
  