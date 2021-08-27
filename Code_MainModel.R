# R code for the main model in the paper #
# Childhood maltreatment mediates the relation between schizophrenia vulnerability genes and psychotic like experience #
# R code by Mattia Marchi (mattiamarchimd@gmail.com) 
# May 20, 2021


#-------------------------------------------------------------
#-------------1. Load the required packages-------------------
#-------------------------------------------------------------

# install.packages("lavaan") # run if not already installed
library(lavaan)
# install.packages("tidySEM") # run if not already installed
library(tidySEM)


#--------------------------------------------------------------
#-------------2. Reproduce the var/cov matrix------------------
#--------------------------------------------------------------

lower <- '70.1399943
          46.355122	  212.538809
          4.8813426	  10.0447004	26.1217353
          0.5704782	  1.0006415	  0.4124109	  0.2217328'
  
m <- getCov(lower, names = c("CM", "PLE", "SZ_PRS", "Cannabis"))


#---------------------------------------------------------------
#-----------3. Estimate the multiple mediation model------------
#---------------------------------------------------------------

#Fit the model
model_mma <- '
#outcome model
PLE ~ c*SZ_PRS + b1*CM + b2*Cannabis

#mediator models
CM ~ a1*SZ_PRS
Cannabis ~ a2*SZ_PRS

#indirect effects (IDEs)
CM_IDE := a1*b1
Cannabis_IDE := a2*b2
sum_IDE := (a1*b1) + (a2*b2)

#total effect
total := c + (a1*b1) + (a2*b2)
CM ~~ Cannabis #model correlation between mediators
'
#a1, a2, b1, b2, c: are labels. I suggest to not change them.
fit <- sem(model_mma, sample.cov = m,
           sample.nobs = 918)

#Estimate the model
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Generate bootstrap CIs
boot.fit <- parameterEstimates(fit, boot.ci.type = "bca.simple")

#Define a convenient layout for the path diagram
lay <- get_layout("", "PLE", "",
                  "Cannabis", "", "CM",
                  "", "SZ_PRS", "", rows = 3)
#Plot the path diagram and visualize it
graph_sem(model = fit, layout = lay)
