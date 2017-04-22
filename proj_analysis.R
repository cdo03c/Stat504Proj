### Data Ingest ###

#Load in subsetted NHES 2012 survey data for analysis
NHES = read.csv('./processed_NHES_data.csv')

#Removes the row ID column read in from the .csv
NHES = NHES[-1]


### Dataset Transformation ###

#Replaces all the -1 values from the NHES survey with NA bceause -1 represents a
#valid skip
NHES[NHES == -1] <- NA

#Creates factors for columns that are categorical variables so that they can be
#used directly in models
factorCols = c(4:20,26:56,58:71,73:84,86:88, 90:140,141:194, 196:215, 217:226,228,
               232:242,244:253,255,259, 271:284, 286:299, 301:302, 308:324,
               330:ncol(NHES))
NHES[factorCols] = lapply(NHES[factorCols],factor)

#Remove columns that are completely full of NAs to reduce the number of
#possible explanatory variables
NHES = NHES[,colSums(is.na(NHES)) != nrow(NHES)]

#Create a version of the data set where all the variables have no missing
#values so we can run StepAIC without getting an error
NHES.comp = NHES[!is.na(NHES$FHWKHRS),]
NHES.comp = NHES.comp[,colSums(is.na(NHES.comp)) == 0]
NHES.comp = NHES.comp[-c(1:4)]


### Variable Transformation ###

#Create an ordered factor for the dependent variable SEGRADES
NHES$SEGRADES = factor(NHES$SEGRADES, order = T)

#Recode the interval variables for easier incorporation into the model.
NHES.comp$SESCHWRK = factor(ifelse(NHES.comp$SESCHWRK == 0, 0, 1))
NHES.comp$SEGWORK = factor(ifelse(NHES.comp$SEGWORK == 0, 0, 1))

#Creates the logarithm of FSFREQ except where FSFREQ was 0, it sets the lower 
#limit to a really small variable.
NHES.comp$FSFREQ = ifelse(NHES.comp$FSFREQ == 0, .00001, log(NHES.comp$FSFREQ))
#Creates the logarithm of YRSADDR rounded up to 1 whole year, instead of rounded
#down
NHES.comp$YRSADDR = log(NHES.comp$YRSADDR + 1)
#Creates the logarithm of FHWKHRS rounded up to 1 whole hour, instead of rounded
#down
NHES.comp$FHWKHRS = log(NHES.comp$FHWKHRS + 1)


### Model Evaluation ###

library(MASS)

#Create a model with the top ten variables after a decision tree
mod2 <- polr(SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY + YRSADDR + SEFUTUREX +
               RACEETHN+ CENREG+ P1ENRL+ OWNRNTHB+ FHWKHRS, data=NHES.comp)

#Run stepAIC using AIC penalty to find best model.  It return a large interaction
#model that would be hard to interpret
mod2.AIC = stepAIC(mod2, scope = list(upper = ~SEGRADEQ * SESCHWRK * SEENJOY * YRSADDR * SEFUTUREX *
                                        RACEETHN* CENREG* P1ENRL* OWNRNTHB* FHWKHRS, lower = ~1))

#Run stepAIC using BIC penalty.  Returned a six variable model with no interaction
#terms
mod2.BIC = stepAIC(mod2, scope = list(upper = ~SEGRADEQ * SESCHWRK * SEENJOY * YRSADDR * SEFUTUREX *
                                        RACEETHN* CENREG* P1ENRL* OWNRNTHB* FHWKHRS, lower = ~1),k = log(nrow(NHES.comp)))

#Run stepAIC on the six-variable model and found a single interaction term was
#the best fitting model.
mod2.BIC.AIC = stepAIC(mod2.BIC, scope = list(upper = ~SEGRADEQ * SESCHWRK * SEENJOY * SEFUTUREX *
                                                RACEETHN* CENREG, lower = ~1))

#Creates all the models from intercept only to fully saturated with the six variable
#model suggested by running StepAIC with BIC penalty.

#Intercept-only model
mod6.0 = polr(NHES$SEGRADES ~ 1,Hess=TRUE)

#Complete independence model
mod6.ind <- polr(SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY + SEFUTUREX +
               RACEETHN + CENREG, data=NHES.comp)

#Joint independence models

#Conditional independence models

#Homogenous association models

#Fully saturated model
mod6.sat <- polr(SEGRADES ~ SEGRADEQ * SESCHWRK * SEENJOY * SEFUTUREX *
               RACEETHN * CENREG, data=NHES.comp)




# mod3.12 = polr(NHES$SEGRADES ~ SEGRADEQ * SESCHWRK, data = NHES.comp,Hess=TRUE)
# mod3.13 = polr(NHES$SEGRADES ~ SEGRADEQ * SEENJOY, data = NHES.comp,Hess=TRUE)
# mod3.23 = polr(NHES$SEGRADES ~ SESCHWRK * SEENJOY, data = NHES.comp,Hess=TRUE)
# mod3.1.23 = polr(NHES$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY, data = NHES.comp,Hess=TRUE)
# mod3.12.3 = polr(NHES$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY, data = NHES.comp,Hess=TRUE)
# mod3.2.13 = polr(NHES$SEGRADES ~ SEGRADEQ * SEENJOY + SESCHWRK, data = NHES.comp,Hess=TRUE)
# mod3.12.23 = polr(NHES$SEGRADES ~ SEGRADEQ*SESCHWRK + SESCHWRK * SEENJOY, data = NHES.comp,Hess=TRUE)
# mod3.12.13 = polr(NHES$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ*SEENJOY, data = NHES.comp,Hess=TRUE)
# mod3.23.13 = polr(NHES$SEGRADES ~ SEGRADEQ * SEENJOY + SESCHWRK*SEENJOY, data = NHES.comp,Hess=TRUE)
# mod3.1.2.3 = polr(NHES$SEGRADES ~ SEGRADEQ + SESCHWRK +SEENJOY, data = NHES.comp,Hess=TRUE)
# mod3.123 = polr(NHES$SEGRADES ~ SESCHWRK * SEENJOY * SEGRADEQ, data = NHES.comp,Hess=TRUE)
# anova(mod3.0,mod3.1,mod3.1.2, mod3.12, mod3.2, mod3.3,mod3.1.3, mod3.2.3,
#       mod3.13,mod3.23,mod3.1.23,mod3.12.3,mod3.2.13,mod3.12.23,mod3.12.13,mod3.23.13,
#       mod3.1.2.3,mod3.123)
# 
# mod3.1.2.3.4 = polr(NHES$SEGRADES ~ SEGRADEQ + SESCHWRK +SEENJOY + YRSADDR, data = NHES.comp,Hess=TRUE)
# 
# vars = c('SESCHWRK','SEENJOY', 'SEFUTUREX', 'RACEETHN', 'CENREG', 'OWNRNTHB', 'FHWKHRS','YRSADDR','S1STCHOI', 'FSFREQ', 'SEGBEHAV', 'SEGWORK', 'SEABSNT', 'SEREPEAT', 'SESUSOUT')
# df = NHES.comp[,c('SEGRADEQ','SEGRADES')]
# for(v in vars){
#   temp.mod1 = polr(SEGRADES~., data = df)
#   names = c(colnames(df),v)
#   df = cbind(df,NHES.comp[,v])
#   colnames(df) = names
#   #addvars = c(addvars,v)
#   temp.mod2 = polr(SEGRADES~., data = df)
#   print(anova(temp.mod1,temp.mod2))
# }

# #When adding in variables, the only variable that did not have a significant decrease was
# #YRSADDR.  Run it again without YRSADDR and see if we still get significant delta
# #deviances when adding new variables.
# vars = c('SEGRADEQ','SESCHWRK', 'RACEETHN', 'CENREG', 'OWNRNTHB', 'FHWKHRS','SEENJOY', 'SEFUTUREX','S1STCHOI', 'FSFREQ', 'SEGBEHAV', 'SEGWORK', 'SEABSNT', 'SEREPEAT', 'SESUSOUT')
# LRstats = data.frame(vars, stringsAsFactors = F)
# for(i in 1:100){
#   df = data.frame(SEGRADES = NHES.comp$SEGRADES)
#   stats = data.frame(var = "", LRStat = 0, stringsAsFactors = F)
#   for(v in sample(vars,length(vars))){
#     if(ncol(df) == 1){
#       temp.mod1 = polr(SEGRADES~1,data = df)
#     } else{
#       temp.mod1 = polr(SEGRADES~., data = df)
#     }
#     names = c(colnames(df),v)
#     df = cbind(df,NHES.comp[,v])
#     colnames(df) = names
#     temp.mod2 = polr(SEGRADES~., data = df)
#     aov = anova(temp.mod1,temp.mod2)
#     stats = rbind(stats,c(names[length(names)],as.character(aov$`LR stat.`[2])))
#   }
#   LRstats = cbind(LRstats,stats[-1,2])
#   if(i%%10 == 0){
#     print(paste("Processed: ", i))
#   }
# }
# LRstats[2:ncol(LRstats)] = lapply(LRstats[2:ncol(LRstats)],as.numeric)
# LRmeans = data.frame(vars = LRstats$vars,means = rowMeans(LRstats[,2:ncol(LRstats)]))
# LRmeans = LRmeans[order(LRmeans$means, decreasing = T),]
