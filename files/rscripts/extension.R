#########################################################
#                 Replication Exercise
#                 Extension
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
pool$newcohort <- as.numeric(pool$cohort>0)
fd$newcohort <- as.numeric(fd$cohort>0)

pool$cohort <- NULL
fd$cohort <- NULL

setnames(pool, "newcohort", "cohort")
setnames(fd, "newcohort", "cohort")

### Column 1


column.1 <- lm(logoccscore ~ rf_ny_n +rf_ny_n : cohort + married + usspouse + nchild + uskid +  migd + ysm + cohort , data=pool)


### Column 2

column.2 <- lm(logoccscore ~  rf_ny_n+rf_ny_n : cohort  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
               data = pool)




## Column 3

setnames(fd, "dni_ny_n", "rf_ny_n")
setnames(fd, "dmarried", "married")
setnames(fd, "dusspouse", "usspouse")
setnames(fd, "dnchild", "nchild")
setnames(fd, "duskid", "uskid")
setnames(fd, "dmig1", "migd")

column.3 <- lm(dlogoccscore ~ -1 +   rf_ny_n+  rf_ny_n : cohort + married + usspouse + 
                 nchild + uskid + migd + dysm + cohort, data = fd)



## Column 4

column.4 <- lm(dlogoccscore ~ -1 +   rf_ny_n+ rf_ny_n : cohort + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + factor(cd_d) :dysm, data = fd)



# column 5

setnames(fd,"rf_ny_n", "dni_ny_n")
setnames(fd, "dni_ny_n_pd", "rf_ny_n")

column.5 <- lm(dlogoccscore ~ -1 +   rf_ny_n+ rf_ny_n : cohort  + married + usspouse + nchild + uskid +  migd + dysm + cohort , data=subset(fd, type1e==0))



#Column 6
column.6 <- lm(dlogoccscore ~ -1 +   rf_ny_n+ rf_ny_n : cohort  + married + usspouse + nchild + uskid +  migd + dysm + cohort +cobx_p : dysm  + factor(cd_d) :dysm, data=subset(fd, type1e==0))



# This is a loop for calculating robust standard errors equally as in Stata
for(i in 1:6) {
  
  eval(parse(text=paste(paste0("cov", i), paste0("vcovHC(column.", i,", type = 'HC1')"), sep="<-")))
  eval(parse(text=paste(paste0("robust_se.", i), paste0("sqrt(diag(cov", i,"))"), sep="<-")))
  
}

stargazer(column.1, column.2, column.3, column.4, column.5, column.6,
          column.labels=c("OLS", "OLS", "First Differences",  "First Differences","Name Changers Only","Name Changers Only"),
          keep=c("rf_ny_n","cohort","rf_ny_n:cohort"),
          covariate.labels=c("A","Arrival prior 1921","A*Arrival prior 1921"), digits = 3,
          omit.stat=c("ser","adj.rsq","f", "ser"),
          title="Extension Exercise",
          table.placement = "H", no.space=TRUE, model.names = FALSE,
          add.lines = list(c("Country of birth", "No", "Yes","","","",""),
          c("Country of birth X YSM", "No", "Yes","No","Yes","No","Yes"),
          c("Labor market", "No", "Yes","","","",""),
          c("Labor market X YSM", "No", "Yes","No","Yes","No","Yes")),
          se = list(robust_se.1,robust_se.2,robust_se.3,robust_se.4, robust_se.5, robust_se.6))

