#########################################################
#                 Replication Exercise
#                 Heterogeneity Analysis
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
##            Table 4
################################################

# The regressions
# Old Migrants
column.1 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
               data = subset(pool,newmig == 0))

# These changes of name is for facilitating typing the regressions
# Also I think the variable of interest should have the same name in order
# to have in the table at the end all the coefficients in the  same row

setnames(fd, "dni_ny_n", "rf_ny_n")
setnames(fd, "dmarried", "married")
setnames(fd, "dusspouse", "usspouse")
setnames(fd, "dnchild", "nchild")
setnames(fd, "duskid", "uskid")
setnames(fd, "dmig1", "migd")

column.2 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + 
                 factor(cd_d) :dysm, data = subset(fd,newmig == 0))

# New Migrants

column.4 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
               data = subset(pool,newmig == 1))




column.5 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + 
                 factor(cd_d) :dysm, data = subset(fd,newmig == 1))


# Tall sample
column.7 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
               data = subset(pool,tall == 1))




column.8 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + 
                 factor(cd_d) :dysm, data = subset(fd,tall == 1))

# Short sample

column.10 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
               data = subset(pool,tall == 0))




column.11 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + 
                 factor(cd_d) :dysm, data = subset(fd,tall == 0))

# I did in this order since the following regressions need the same change
# of name for the explanatory variable
setnames(fd,"rf_ny_n", "dni_ny_n")
setnames(fd, "dni_ny_n_pd", "rf_ny_n")

column.3 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + 
                 factor(cd_d) :dysm, data = subset(fd,newmig == 0  & type1e == 0))

column.6 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + 
                 factor(cd_d) :dysm, data = subset(fd,newmig == 1  & type1e == 0))


column.9 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + 
                 factor(cd_d) :dysm, data = subset(fd,tall == 1  & type1e == 0))


column.12 <- lm(dlogoccscore ~ -1 + rf_ny_n + married + usspouse + 
                 nchild + uskid + migd + dysm  + cohort + cobx_p : dysm  + 
                 factor(cd_d) :dysm, data = subset(fd,tall == 0  & type1e == 0))


#Calculating robust standard errors like in Stata
for(i in 1:12) {

  eval(parse(text=paste(paste0("cov", i), paste0("vcovHC(column.", i,", type = 'HC1')"), sep="<-")))
  eval(parse(text=paste(paste0("robust_se.", i), paste0("sqrt(diag(cov", i,"))"), sep="<-")))
  
  }


# Table 4 of the paper for Old and New Migrants
stargazer(column.1, column.2, column.3, column.4, column.5, column.6,
          column.labels=c("OLS", "FD", "NC Only",  "OLS","FD","NC Only"),
          keep=c("rf_ny_n"),
          covariate.labels=c("A"), digits = 3,
          omit.stat=c("ser","adj.rsq","f", "ser", "rsq"),
          se = list(robust_se.1,robust_se.2,robust_se.3,robust_se.4, robust_se.5, robust_se.6))

# Table 4 of the paper for Tall and Short Migrants

stargazer(column.7, column.8, column.9, column.10, column.11, column.12,
          column.labels=c("OLS", "FD", "NC Only",  "OLS","FD","NC Only"),
          keep=c("rf_ny_n"),
          covariate.labels=c("A"), digits = 3,
          omit.stat=c("ser","adj.rsq","f", "ser", "rsq"),
          se = list(robust_se.7,robust_se.8,robust_se.9,robust_se.10, robust_se.11, robust_se.12))

