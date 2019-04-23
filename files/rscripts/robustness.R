#########################################################
#                 Replication Exercise
#                 Robustness Checks
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


# import the function from repository to do standard errors
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)



#Directory
setwd("/Users/Carlos Cardona/Dropbox/PhD Warwick/Second Year/Labour Econ/Replication/Paper Files/")

fd <- read.dta("1. fd_data.dta")
pool <- read.dta("2. pooled_data.dta")


################################################
##            Table 5
################################################



# Using name at declaration

column.1 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
               data = pool)

setnames(fd, "dni_ny_n", "rf_ny_n")
setnames(fd, "dmarried", "married")
setnames(fd, "dusspouse", "usspouse")
setnames(fd, "dnchild", "nchild")
setnames(fd, "duskid", "uskid")
setnames(fd, "dmig1", "migd")
setnames(fd, "cobx_p", "cobx")
setnames(fd, "cd_d", "cd")


column.2 <- lm(dlogoccscore ~ -1 +  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + dysm + cobx : dysm  + factor(cd) :dysm, 
               data = fd)

setnames(fd, "rf_ny_n", "dni_ny_n")
setnames(fd, "dni_ny_n_pd", "rf_ny_n")

column.3 <- lm(dlogoccscore ~ -1 +  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + dysm + cobx : dysm  + factor(cd) :dysm, 
               data = subset(fd,type1e == 0))


# Early declarants

column.4 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
               data = subset(pool,distdecl == 1))

setnames(fd, "rf_ny_n", "dni_ny_n_pd")
setnames(fd, "dni_ny_n", "rf_ny_n")


column.5 <- lm(dlogoccscore ~ -1 +  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + dysm + cobx : dysm  + factor(cd) :dysm, 
               data = subset(fd,distdecl == 1))

setnames(fd, "rf_ny_n", "dni_ny_n")

setnames(fd, "dni_ny_n_pd", "rf_ny_n")

column.6 <- lm(dlogoccscore ~ -1 +  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + dysm + cobx : dysm  + factor(cd) :dysm, 
               data = subset(fd,type1e == 0 & distdecl == 1))

# Upgrading


column.7 <- lm(upgrading ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
             data = pool)

setnames(fd, "rf_ny_n", "dni_ny_n_pd")
setnames(fd, "dni_ny_n", "rf_ny_n")

column.8 <- lm(upgrading ~ -1 +  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + dysm + cobx : dysm  + factor(cd) :dysm, 
               data = fd)

setnames(fd, "rf_ny_n", "dni_ny_n")

setnames(fd, "dni_ny_n_pd", "rf_ny_n")

column.9 <- lm(upgrading ~ -1 +  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + dysm + cobx : dysm  + factor(cd) :dysm, 
               data = subset(fd,type1e == 0 ))


# NYSIIS

fd <- read.dta("1. fd_data.dta")
pool <- read.dta("2. pooled_data.dta")
setnames(pool, "rf_ny_n", "outcome")
setnames(pool, "rf_ny_nq", "rf_ny_n")
setnames(fd, "dmarried", "married")
setnames(fd, "dusspouse", "usspouse")
setnames(fd, "dnchild", "nchild")
setnames(fd, "duskid", "uskid")
setnames(fd, "dmig1", "migd")
setnames(fd, "cobx_p", "cobx")
setnames(fd, "cd_d", "cd")


column.10 <- lm(logoccscore ~  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + ysm + cobx : ysm  + factor(cd) :ysm, 
               data = pool)

setnames(fd, "dni_ny_nq", "rf_ny_n")


column.11 <- lm(dlogoccscore ~ -1 +  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + dysm + cobx : dysm  + factor(cd) :dysm, 
               data = fd)


fd <- read.dta("1. fd_data.dta")
setnames(fd, "dni_ny_nqc", "rf_ny_n")
setnames(fd, "dmarried", "married")
setnames(fd, "dusspouse", "usspouse")
setnames(fd, "dnchild", "nchild")
setnames(fd, "duskid", "uskid")
setnames(fd, "dmig1", "migd")
setnames(fd, "cobx_p", "cobx")
setnames(fd, "cd_d", "cd")



column.12 <- lm(dlogoccscore ~ -1 +  rf_ny_n  + married + usspouse + nchild + uskid  + 
                 migd + cohort  + dysm + cobx : dysm  + factor(cd) :dysm, 
               data = subset(fd,type1e == 0 ))


for(i in 1:12) {
  
  eval(parse(text=paste(paste0("cov", i), paste0("vcovHC(column.", i,", type = 'HC1')"), sep="<-")))
  eval(parse(text=paste(paste0("robust_se.", i), paste0("sqrt(diag(cov", i,"))"), sep="<-")))
  
}


stargazer(column.1, column.2, column.3, column.4, column.5, column.6,
          column.labels=c("OLS", "FD", "NC Only",  "OLS","FD","NC Only"),
          keep=c("rf_ny_n"),
          covariate.labels=c("A"), digits = 3,
          omit.stat=c("ser","adj.rsq","f", "ser", "rsq"),out="models.txt",
          se = list(robust_se.1,robust_se.2,robust_se.3,robust_se.4, robust_se.5, robust_se.6))

stargazer(column.7, column.8, column.9, column.10, column.11, column.12,
          column.labels=c("OLS", "FD", "NC Only",  "OLS","FD","NC Only"),
          keep=c("rf_ny_n"),
          covariate.labels=c("A"), digits = 3,
          omit.stat=c("ser","adj.rsq","f", "ser", "rsq"),out="models2.txt",
          se = list(robust_se.7,robust_se.8,robust_se.9,robust_se.10, robust_se.11, robust_se.12))




