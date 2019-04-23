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
library(ggplot2)
library(Rmisc)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

#Directory
setwd("/Users/Carlos Cardona/Dropbox/PhD Warwick/Second Year/Labour Econ/Replication/Paper Files/")

fd <- read.dta("1. fd_data.dta")
pool <- read.dta("2. pooled_data.dta")

###############
#  Figure 1
###############


#Generating a new data that is a subsample of the original who follows the condition
subfd <- subset(fd,dni_ny_n>0)
# Calculatin the terciles for the outcome if it is positive. Missing otherwise
fd$dnix <- ifelse(fd$dni_ny_n>0, ntile(subfd$dni_ny_n, 3) , NA)

# Recoding the variable in order to include more categories
fd <- transform(fd, dnix = ifelse(dnix==3, 4,
                                  ifelse(dnix==2,3,ifelse(dnix==1,2, dnix))))

fd <- transform(fd, dnix = ifelse(dni_ny_n==0 & type4e==0, 0, dnix))
fd <- transform(fd, dnix = ifelse(type4e==1, 1, dnix))


#Creating a variable type according to the previous indicator variable
fd$type <- ifelse(fd$dnix==0, "Keepers",
                ifelse(fd$dnix==1,"Others" , ifelse(fd$dnix==2,"1st" , 
                ifelse(fd$dnix==3,"2nd"  ,"3rd"))))

#It should be a factor variable for ggplot
fd$type  <- as.factor(fd$type )

#This is Figure 1 of the paper
p <- ggplot(aes(x = factor(type, level = c('Keepers', 'Others', '1st', "2nd", "3rd")), y = dlogoccscore), data = fd) + stat_summary(fun.y = "mean", geom = "bar")
p +labs(x ="Terciles of Americanization index", y = "Change in occupational score (log)")


################################
#         Figure 4
#########################

#Creating the variable for all the types
pool$typeall <- ifelse(pool$type1e==1, 1,
                  ifelse(pool$type4e==1,2 , ifelse(pool$type2e==1,3 , 
                                                      ifelse(pool$type3e==1,4 ,NA))))

# Conditional mean across documents and types

stats <- summarySE(pool, measurevar="logoccscore", groupvars=c("document","typeall"))

# Creating the variable according to the document
stats$new_document <- ifelse(stats$document==1, "Declaration", ifelse(stats$document==2,"Petition"  ,"Nothing"))

#It should be factor for ggplot
stats$new_document  <- as.factor(stats$new_document )

#Creating a type variable
stats$type <- ifelse(stats$typeall==1, "Keepers",
                     ifelse(stats$typeall==2,"Others" , ifelse(stats$typeall==3,"Early American" , "Late American")))

stats$type  <- as.factor(stats$type )


# This is the code for Figure 2 of the paper
  ggplot(stats, aes(y=logoccscore, x=factor(type, level = c('Keepers', 'Others', 'Early American', "Late American")), fill=type)) + 
    geom_bar( position=position_dodge(),stat="identity") + facet_wrap(~new_document) + coord_cartesian( ylim = c(3, 3.4))+ 
    geom_errorbar(aes(ymin=logoccscore-ci, ymax=logoccscore+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +labs(x ="", y = "Occupational Score (log)") + scale_fill_grey(start = .5, end = .5) +
    theme(  panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.5, linetype = "solid"),legend.position="none",axis.text=element_text(size=7))