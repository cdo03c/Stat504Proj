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

diagnoseModel = function(mod){
  ## store coefficient table
  ctable <- coef(summary(mod))
  
  ## calculate and store p values
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  
  ## combined table
  print(ctable <- cbind(ctable, "p value" = p))
  
  #Calculate the confidence intervals for explantory variables
  (ci <- confint(mod))
}

#Proposed model 1
mod.fin1 <- polr(SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY + SEFUTUREX + RACEETHN +
                  CENREG + SEGRADEQ:RACEETHN, data=NHES.comp, Hess = T)

## store coefficient table
ctable <- coef(summary(mod.fin2))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
print(ctable <- cbind(ctable, "p value" = p))

ci <- confint(mod.fin2)

exp(cbind(OR = coef(mod.fin2), ci))

#Further exploration of the model shows that coefficients for SEFUTUREX are not significant
#and that the 95% confidnece interval for category 5 for SEGRADEQ goes to Inf.  
#Therefore, we are going to remove SEFUTUREX from the model and combine 
#categories 4 and 5 for SEGRADEQ.

NHES.comp$SEGRADEQ = factor(ifelse(NHES.comp$SEGRADEQ == 5, 4, NHES.comp$SEGRADEQ))

mod.fin2 = polr(SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY + RACEETHN +
                  CENREG + SEGRADEQ:RACEETHN, data=NHES.comp, Hess = T)

diagnoseModel(mod.fin2)

#Creates all the models from intercept only to fully saturated with the five variable
#model suggested by running StepAIC with BIC penalty and evaluating the significance
#of the coefficients.

#Intercept-only model
fit0 = polr(NHES.comp$SEGRADES ~ 1,Hess=TRUE)

### Single Variable Models ###

fit1.1 = polr(NHES.comp$SEGRADES ~ SEGRADEQ,Hess=TRUE, data = NHES.comp)
fit1.2 = polr(NHES.comp$SEGRADES ~ SESCHWRK,Hess=TRUE, data = NHES.comp)
fit1.3 = polr(NHES.comp$SEGRADES ~ SEENJOY,Hess=TRUE, data = NHES.comp)
fit1.4 = polr(NHES.comp$SEGRADES ~ RACEETHN,Hess=TRUE, data = NHES.comp)
fit1.5 = polr(NHES.comp$SEGRADES ~ CENREG,Hess=TRUE, data = NHES.comp)
anova(fit0, fit1.1, fit1.2, fit1.3, fit1.4, fit1.5)

cum.anova = anova(fit0, fit1.1)

### Two variable models ###

#The models that include SEGRADEQ are always the lowest so after the two
#variable models we will be testing only models with SEGRADEQ in them.

#Independence models
fit2.1.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK,Hess=TRUE, data = NHES.comp)
fit2.1.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SEENJOY,Hess=TRUE, data = NHES.comp)
fit2.1.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + RACEETHN,Hess=TRUE, data = NHES.comp)
fit2.1.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + CENREG,Hess=TRUE, data = NHES.comp)
fit2.2.3 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEENJOY,Hess=TRUE, data = NHES.comp)
fit2.2.4 = polr(NHES.comp$SEGRADES ~ SESCHWRK + RACEETHN,Hess=TRUE, data = NHES.comp)
fit2.2.5 = polr(NHES.comp$SEGRADES ~ SESCHWRK + CENREG,Hess=TRUE, data = NHES.comp)
fit2.3.4 = polr(NHES.comp$SEGRADES ~ SEENJOY+ RACEETHN,Hess=TRUE, data = NHES.comp)
fit2.3.5 = polr(NHES.comp$SEGRADES ~ SEENJOY+ CENREG,Hess=TRUE, data = NHES.comp)
fit2.4.5 = polr(NHES.comp$SEGRADES ~ RACEETHN + CENREG,Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2,fit2.1.3,
      fit2.1.4, fit2.1.5, fit2.2.3, fit2.2.4, fit2.2.5, fit2.3.4, fit2.3.5,
      fit2.4.5)
cum.anova = anova(fit0, fit1.1, fit2.1.2)

#Association models
fit2.12 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK,Hess=TRUE, data = NHES.comp)
fit2.13 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY,Hess=TRUE, data = NHES.comp)
fit2.14 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN,Hess=TRUE, data = NHES.comp)
fit2.15 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * CENREG,Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit2.13, fit2.14, fit2.15)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12)


### Three variable models ###

#Independence models
fit3.1.2.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY,Hess=TRUE, data = NHES.comp)
fit3.1.2.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + RACEETHN,Hess=TRUE, data = NHES.comp)
fit3.1.2.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + CENREG,Hess=TRUE, data = NHES.comp)
fit3.1.3.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SEENJOY + RACEETHN,Hess=TRUE, data = NHES.comp)
fit3.1.3.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SEENJOY + CENREG,Hess=TRUE, data = NHES.comp)
fit3.1.4.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + RACEETHN + CENREG,Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.1.2.4, fit3.1.2.5,
      fit3.1.3.4, fit3.1.3.5, fit3.1.4.5)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3)

#The independence model with the most significant delta deviance is the model,
#SEGRADEQ + SESCHWRK + SEENJOY and all the subsequent three variable model will
#be based only on this model.

#Joint independence models
fit3.12.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY,Hess=TRUE, data = NHES.comp)
fit3.1.23 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY,Hess=TRUE, data = NHES.comp)
fit3.13.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ  * SEENJOY + SESCHWRK,Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2,fit3.1.23)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2)

#Conditional independence models
fit3.12.13 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ * SEENJOY,Hess=TRUE, data = NHES.comp)
fit3.12.23 = polr(NHES.comp$SEGRADES ~ SESCHWRK * SEGRADEQ + SESCHWRK * SEENJOY,Hess=TRUE, data = NHES.comp)
fit3.13.23 = polr(NHES.comp$SEGRADES ~ SEGRADEQ  * SEENJOY + SESCHWRK * SEENJOY,Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2,fit3.12.13,fit3.12.23, fit3.13.23)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2)

#Homogenous association models
fit3.13.23.12 = polr(NHES.comp$SEGRADES ~ SEGRADEQ  * SEENJOY + SESCHWRK * SEENJOY +
                       SEGRADEQ * SESCHWRK,Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2, fit3.13.23.12)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2)

#Fully saturated model
fit3.123 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK * SEENJOY,Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2, fit3.123)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2)

### Four variable models ###

#The four variable models will start with the most significant three variable
#model: SEGRADEQ + SESCHWRK + SEENJOY.

#Independence models
fit4.1.2.3.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY + RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.1.2.3.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY + CENREG,
                    Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2, fit4.1.2.3.5, fit4.1.2.3.4)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,fit3.13.2, fit4.1.2.3.4)

#The independence model with the most significant delta deviance is the model,
#SEGRADEQ + SESCHWRK + SEENJOY + RACEETHN and all the subsequent three variable 
#model will be based only on this model.

#Joint independence models
fit4.12.3.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY + RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.13.2.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SESCHWRK + RACEETHN,
                   Hess=TRUE, data = NHES.comp)
fit4.14.2.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SESCHWRK + SEENJOY ,
                   Hess=TRUE, data = NHES.comp)
fit4.1.23.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY + RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.1.24.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * RACEETHN + SEENJOY ,
                   Hess=TRUE, data = NHES.comp)
fit4.1.2.34 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY * RACEETHN,
                   Hess=TRUE, data = NHES.comp)

fit4.12.34 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY * RACEETHN,
                   Hess=TRUE, data = NHES.comp)
fit4.13.24 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SESCHWRK * RACEETHN,
                  Hess=TRUE, data = NHES.comp)
fit4.14.23 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SESCHWRK * SEENJOY,
                  Hess=TRUE, data = NHES.comp)

fit4.1.234 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY * RACEETHN,
                   Hess=TRUE, data = NHES.comp)
fit4.2.134 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEGRADEQ * SEENJOY * RACEETHN,
                  Hess=TRUE, data = NHES.comp) #Does not converge
fit4.3.124 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEGRADEQ * SESCHWRK * RACEETHN,
                  Hess=TRUE, data = NHES.comp)
fit4.4.123 = polr(NHES.comp$SEGRADES ~ RACEETHN + SEGRADEQ * SESCHWRK * SEENJOY,
                  Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
      fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4,fit4.14.2.3, fit4.1.23.4,
      fit4.1.24.3, fit4.1.2.34, fit4.12.34, fit4.13.24, fit4.14.23, fit4.1.234,
      fit4.3.124, fit4.4.123)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, fit4.14.23)

#Conditional independence models
fit4.12.13.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ *SEENJOY + RACEETHN,
                   Hess=TRUE, data = NHES.comp)
fit4.12.3.14 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY + SEGRADEQ *RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.2.13.14 = polr(NHES.comp$SEGRADES ~  SESCHWRK + SEGRADEQ * SEENJOY + SEGRADEQ *RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.12.23.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY* SESCHWRK + RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.12.3.24 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY + RACEETHN* SESCHWRK,
                    Hess=TRUE, data = NHES.comp)
fit4.1.23.24 = polr(NHES.comp$SEGRADES ~ SEGRADEQ  + SEENJOY * SESCHWRK+ RACEETHN* SESCHWRK,
                    Hess=TRUE, data = NHES.comp)
fit4.13.23.4 = polr(NHES.comp$SEGRADES ~ SEENJOY*SEGRADEQ + SEENJOY* SESCHWRK + RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.13.2.34 = polr(NHES.comp$SEGRADES ~ SEENJOY*SEGRADEQ + SESCHWRK + SEENJOY* RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.1.32.34 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SEENJOY*SESCHWRK + SEENJOY* RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.14.2.34 = polr(NHES.comp$SEGRADES ~ SEGRADEQ* RACEETHN + SESCHWRK + SEENJOY* RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.1.24.34 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK* RACEETHN + SEENJOY* RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit4.14.24.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ* RACEETHN + SESCHWRK* RACEETHN + SEENJOY,
                    Hess=TRUE, data = NHES.comp)


fit4.12.134 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ *SEENJOY * RACEETHN,
                   Hess=TRUE, data = NHES.comp) #Does not converge
fit4.12.234 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY * RACEETHN* SESCHWRK,
                  Hess=TRUE, data = NHES.comp)
fit4.13.124 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SEGRADEQ *SESCHWRK * RACEETHN,
                  Hess=TRUE, data = NHES.comp)
fit4.13.324 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SESCHWRK * RACEETHN* SEENJOY,
                   Hess=TRUE, data = NHES.comp)
fit4.14.123 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SEGRADEQ *SESCHWRK * SEENJOY,
                  Hess=TRUE, data = NHES.comp)
fit4.14.423 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SESCHWRK * SEENJOY* RACEETHN,
                   Hess=TRUE, data = NHES.comp)

fit4.1.1234 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SEGRADEQ* SESCHWRK * SEENJOY * RACEETHN,
                  Hess=TRUE, data = NHES.comp) #Does not converge
fit4.2.2134 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SESCHWRK * SEGRADEQ * SEENJOY * RACEETHN,
                  Hess=TRUE, data = NHES.comp) #Does not converge
fit4.3.3124 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEENJOY * SEGRADEQ * SESCHWRK * RACEETHN,
                  Hess=TRUE, data = NHES.comp) #Does not converge
fit4.4.4123 = polr(NHES.comp$SEGRADES ~ RACEETHN + RACEETHN * SEGRADEQ * SESCHWRK * SEENJOY,
                  Hess=TRUE, data = NHES.comp) #Does not converge

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
      fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, fit4.14.23,
      fit4.12.13.4, fit4.12.3.14, fit4.2.13.14, fit4.12.23.4, fit4.12.3.24,
      fit4.1.23.24, fit4.13.23.4, fit4.13.2.34, fit4.1.32.34,fit4.14.2.34,
      fit4.1.24.34, fit4.14.24.3, fit4.12.234, fit4.13.124, fit4.13.324,
      fit4.14.123, fit4.14.423)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
      fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, fit4.14.23,
       fit4.2.13.14, fit4.13.124)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, fit4.14.23,
                  fit4.2.13.14, fit4.13.124)

#Homogenous association model
fit3.123.234.124.134 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK  * SEENJOY +
                              SESCHWRK * SEENJOY * RACEETHN +
                              SEGRADEQ * SESCHWRK  * RACEETHN +
                              SEGRADEQ * SEENJOY  * RACEETHN,
                            Hess=TRUE, data = NHES.comp) #Does not converge

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, fit4.14.23,
                  fit4.2.13.14, fit4.13.124)

#Fully saturated model
fit4.1234 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK * SEENJOY * RACEETHN,Hess=TRUE, data = NHES.comp)
#Does not converge

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, fit4.14.23,
                  fit4.2.13.14, fit4.13.124)

### Five variable models ###

#Independence models
fit5.1.2.3.4.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY + RACEETHN + CENREG,
                    Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
      fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
      fit5.1.2.3.4.5)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
                  fit5.1.2.3.4.5)

#Joint independence models
fit5.12.3.4.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY + RACEETHN + CENREG,
                      Hess=TRUE, data = NHES.comp)
fit5.13.2.4.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SESCHWRK + RACEETHN + CENREG,
                     Hess=TRUE, data = NHES.comp)
fit5.14.2.3.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SEENJOY + SESCHWRK + CENREG,
                     Hess=TRUE, data = NHES.comp)
fit5.15.2.3.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * CENREG + SEENJOY + SESCHWRK + RACEETHN,
                     Hess=TRUE, data = NHES.comp)
fit5.1.23.4.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY + RACEETHN + CENREG,
                     Hess=TRUE, data = NHES.comp)
fit5.1.24.3.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * RACEETHN + SEENJOY + CENREG,
                     Hess=TRUE, data = NHES.comp)
fit5.1.25.3.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * CENREG + SEENJOY + RACEETHN,
                     Hess=TRUE, data = NHES.comp)
fit5.1.2.34.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY * RACEETHN + CENREG,
                     Hess=TRUE, data = NHES.comp)
fit5.1.2.35.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY * CENREG + RACEETHN,
                     Hess=TRUE, data = NHES.comp)
fit5.1.2.3.45 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY + CENREG * RACEETHN,
                     Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
      fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
      fit5.1.2.3.4.5,
      fit5.12.3.4.5, fit5.13.2.4.5, fit5.14.2.3.5, fit5.15.2.3.4, fit5.1.23.4.5,
      fit5.1.24.3.5,fit5.1.25.3.4, fit5.1.2.34.5, fit5.1.2.35.4, fit5.1.2.3.45)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
                  fit5.1.2.3.4.5, fit5.14.2.3.5)

fit5.1.23.45 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY + RACEETHN * CENREG,
                  Hess=TRUE, data = NHES.comp)
fit5.1.24.35 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * RACEETHN + SEENJOY * CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.1.25.34 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * CENREG + RACEETHN * SEENJOY,
                    Hess=TRUE, data = NHES.comp)
fit5.2.13.45 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEGRADEQ * SEENJOY + RACEETHN * CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.2.14.35 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEGRADEQ * RACEETHN + SEENJOY * CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.2.15.34 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEGRADEQ * CENREG + RACEETHN * SEENJOY,
                    Hess=TRUE, data = NHES.comp)
fit5.3.12.45 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEGRADEQ * SESCHWRK + RACEETHN * CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.3.14.25 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEGRADEQ * RACEETHN + SESCHWRK * CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.3.15.24 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEGRADEQ * CENREG + RACEETHN * SESCHWRK,
                    Hess=TRUE, data = NHES.comp)
fit5.4.12.35 = polr(NHES.comp$SEGRADES ~ RACEETHN + SEGRADEQ * SESCHWRK + SEENJOY * CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.4.13.25 = polr(NHES.comp$SEGRADES ~ RACEETHN + SEGRADEQ * SEENJOY + SESCHWRK * CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.4.15.23 = polr(NHES.comp$SEGRADES ~ RACEETHN + SEGRADEQ * CENREG + SEENJOY * SESCHWRK,
                    Hess=TRUE, data = NHES.comp)
fit5.5.12.34 = polr(NHES.comp$SEGRADES ~ CENREG + SEGRADEQ * SESCHWRK + SEENJOY * RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit5.5.13.24 = polr(NHES.comp$SEGRADES ~ CENREG + SEGRADEQ * SEENJOY + SESCHWRK * RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit5.5.14.23 = polr(NHES.comp$SEGRADES ~ CENREG + SEGRADEQ * RACEETHN + SEENJOY * SESCHWRK,
                    Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
      fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
      fit5.1.2.3.4.5, fit5.14.2.3.5, fit5.1.23.45, fit5.1.24.35, fit5.1.25.34,
      fit5.2.13.45,fit5.2.14.35, fit5.2.15.34,
      fit5.3.12.45, fit5.3.14.25,fit5.3.15.24,
      fit5.4.12.35, fit5.4.13.25, fit5.4.15.23,
      fit5.5.12.34, fit5.5.13.24,fit5.5.14.23 )

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
                  fit5.1.2.3.4.5, fit5.14.2.3.5)

fit5.1.234.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY * RACEETHN + CENREG,
                  Hess=TRUE, data = NHES.comp)
fit5.1.235.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY *CENREG  + RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit5.1.245.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * RACEETHN *CENREG  + SEENJOY,
                    Hess=TRUE, data = NHES.comp)
fit5.1.345.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SEENJOY * RACEETHN *CENREG  + SESCHWRK,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.2.134.5 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEGRADEQ * SEENJOY * RACEETHN + CENREG,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.2.135.4 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEGRADEQ * SEENJOY *CENREG  + RACEETHN,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.2.145.3 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEGRADEQ * RACEETHN *CENREG  + SEENJOY,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.2.345.1 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEENJOY * RACEETHN *CENREG  + SEGRADEQ,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.3.124.5 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEGRADEQ * SESCHWRK * RACEETHN + CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.3.125.4 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEGRADEQ * SESCHWRK *CENREG  + RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit5.3.145.2 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEGRADEQ * RACEETHN *CENREG  + SESCHWRK,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.3.245.1 = polr(NHES.comp$SEGRADES ~ SEENJOY + SESCHWRK * RACEETHN *CENREG  + SEGRADEQ,
                    Hess=TRUE, data = NHES.comp)
fit5.4.123.5 = polr(NHES.comp$SEGRADES ~ RACEETHN + SEGRADEQ * SESCHWRK * SEENJOY + CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.4.125.3 = polr(NHES.comp$SEGRADES ~RACEETHN  + SEGRADEQ * SESCHWRK *CENREG  + SEENJOY,
                    Hess=TRUE, data = NHES.comp)
fit5.4.135.2 = polr(NHES.comp$SEGRADES ~ RACEETHN + SEGRADEQ * SEENJOY *CENREG  + SESCHWRK,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.4.235.1 = polr(NHES.comp$SEGRADES ~ RACEETHN + SESCHWRK * SEENJOY *CENREG  + SEGRADEQ,
                    Hess=TRUE, data = NHES.comp)
fit5.5.123.4 = polr(NHES.comp$SEGRADES ~ CENREG + SEGRADEQ * SESCHWRK * SEENJOY + RACEETHN,
                    Hess=TRUE, data = NHES.comp)
fit5.5.124.3 = polr(NHES.comp$SEGRADES ~CENREG  + SEGRADEQ * SESCHWRK *RACEETHN  + SEENJOY,
                    Hess=TRUE, data = NHES.comp)
fit5.5.134.2 = polr(NHES.comp$SEGRADES ~ CENREG + SEGRADEQ * SEENJOY *RACEETHN  + SESCHWRK,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.5.234.1 = polr(NHES.comp$SEGRADES ~ CENREG + SESCHWRK * SEENJOY *RACEETHN  + SEGRADEQ,
                    Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
      fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
      fit5.1.2.3.4.5, fit5.14.2.3.5,
      fit5.1.234.5, fit5.1.235.4, fit5.1.245.3, fit5.3.124.5, fit5.3.125.4,
      fit5.3.245.1, fit5.4.123.5, fit5.4.125.3, fit5.4.235.1, fit5.5.123.4,
      fit5.5.124.3, fit5.5.234.1)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
                  fit5.1.2.3.4.5, fit5.14.2.3.5,
                  fit5.3.124.5)

fit5.1.2345 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK * SEENJOY * RACEETHN * CENREG,
                    Hess=TRUE, data = NHES.comp) #Does not converge
fit5.2.1345 = polr(NHES.comp$SEGRADES ~ SESCHWRK + SEGRADEQ * SEENJOY * RACEETHN * CENREG,
                   Hess=TRUE, data = NHES.comp) #Does not converge
fit5.3.1245 = polr(NHES.comp$SEGRADES ~ SEENJOY + SEGRADEQ * SESCHWRK * RACEETHN * CENREG,
                   Hess=TRUE, data = NHES.comp) #Does not converge
fit5.4.1235 = polr(NHES.comp$SEGRADES ~ RACEETHN + SEGRADEQ * SEENJOY * SESCHWRK * CENREG,
                   Hess=TRUE, data = NHES.comp) #Does not converge
fit5.5.1234 = polr(NHES.comp$SEGRADES ~ CENREG + SEGRADEQ * SEENJOY * RACEETHN * SESCHWRK,
                   Hess=TRUE, data = NHES.comp) #Does not converge

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
                  fit5.1.2.3.4.5, fit5.14.2.3.5,
                  fit5.3.124.5)

#Conditional independence models
fit5.12.13.4.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ *SEENJOY + RACEETHN + CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.12.3.14.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY + SEGRADEQ *RACEETHN + CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.12.3.4.15 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY + RACEETHN + SEGRADEQ *CENREG,
                      Hess=TRUE, data = NHES.comp)
fit5.2.13.14.5 = polr(NHES.comp$SEGRADES ~  SESCHWRK + SEGRADEQ * SEENJOY + SEGRADEQ *RACEETHN+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.2.13.4.15 = polr(NHES.comp$SEGRADES ~  SESCHWRK + SEGRADEQ * SEENJOY + RACEETHN+ SEGRADEQ *CENREG,
                      Hess=TRUE, data = NHES.comp)
fit5.2.3.14.15 = polr(NHES.comp$SEGRADES ~  SESCHWRK +  SEENJOY + SEGRADEQ *RACEETHN+ SEGRADEQ *CENREG,
                      Hess=TRUE, data = NHES.comp)
fit5.12.23.4.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY* SESCHWRK + RACEETHN+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.12.3.24.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY + RACEETHN* SESCHWRK+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.12.3.4.25 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY + RACEETHN+ CENREG* SESCHWRK,
                      Hess=TRUE, data = NHES.comp)
fit5.1.23.24.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ  + SEENJOY * SESCHWRK+ RACEETHN* SESCHWRK+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.1.23.4.25 = polr(NHES.comp$SEGRADES ~ SEGRADEQ  + SEENJOY * SESCHWRK+ RACEETHN+ CENREG* SESCHWRK,
                      Hess=TRUE, data = NHES.comp)
fit5.1.3.24.25 = polr(NHES.comp$SEGRADES ~ SEGRADEQ  + SEENJOY + RACEETHN* SESCHWRK+ CENREG* SESCHWRK,
                      Hess=TRUE, data = NHES.comp)
fit5.13.23.4.5 = polr(NHES.comp$SEGRADES ~ SEENJOY*SEGRADEQ + SEENJOY* SESCHWRK + RACEETHN+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.13.2.34.5 = polr(NHES.comp$SEGRADES ~ SEENJOY*SEGRADEQ + SESCHWRK + SEENJOY* RACEETHN+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.13.2.4.35 = polr(NHES.comp$SEGRADES ~ SEENJOY*SEGRADEQ + SESCHWRK +  RACEETHN+ SEENJOY*CENREG,
                      Hess=TRUE, data = NHES.comp)
fit5.1.32.34.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SEENJOY*SESCHWRK + SEENJOY* RACEETHN+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.1.32.4.35 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SEENJOY*SESCHWRK +  RACEETHN+ SEENJOY*CENREG,
                      Hess=TRUE, data = NHES.comp)
fit5.1.2.34.35 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK +  SEENJOY*RACEETHN+ SEENJOY*CENREG,
                      Hess=TRUE, data = NHES.comp)
fit5.14.24.3.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ* RACEETHN + SESCHWRK* RACEETHN + SEENJOY+ CENREG,
                      Hess=TRUE, data = NHES.comp)
fit5.14.2.34.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ* RACEETHN + SESCHWRK + SEENJOY* RACEETHN+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.14.2.3.45 = polr(NHES.comp$SEGRADES ~ SEGRADEQ* RACEETHN + SESCHWRK + SEENJOY+ CENREG* RACEETHN,
                      Hess=TRUE, data = NHES.comp)
fit5.1.24.34.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK* RACEETHN + SEENJOY* RACEETHN+ CENREG,
                    Hess=TRUE, data = NHES.comp)
fit5.1.24.3.45 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK* RACEETHN + SEENJOY+ CENREG* RACEETHN,
                      Hess=TRUE, data = NHES.comp)
fit5.1.2.43.45 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY* RACEETHN+ CENREG* RACEETHN,
                      Hess=TRUE, data = NHES.comp)
fit5.15.25.3.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ* CENREG + SESCHWRK* CENREG + SEENJOY+ RACEETHN,
                      Hess=TRUE, data = NHES.comp)
fit5.15.2.35.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ* CENREG + SESCHWRK + SEENJOY* CENREG+ RACEETHN,
                      Hess=TRUE, data = NHES.comp)
fit5.15.2.3.45 = polr(NHES.comp$SEGRADES ~ SEGRADEQ* CENREG + SESCHWRK + SEENJOY+ CENREG* RACEETHN,
                      Hess=TRUE, data = NHES.comp)
fit5.1.25.35.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK* CENREG + SEENJOY* CENREG+ RACEETHN,
                      Hess=TRUE, data = NHES.comp)
fit5.1.25.3.45 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK* CENREG + SEENJOY+ CENREG* RACEETHN,
                      Hess=TRUE, data = NHES.comp)
fit5.1.2.35.45 = polr(NHES.comp$SEGRADES ~ SEGRADEQ + SESCHWRK + SEENJOY* CENREG+ CENREG* RACEETHN,
                      Hess=TRUE, data = NHES.comp)

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
      fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
      fit5.1.2.3.4.5, fit5.14.2.3.5,fit5.3.124.5, 
      fit5.1.2.35.45, fit5.1.25.3.45,fit5.1.25.35.4, fit5.15.2.3.45,
      fit5.15.2.35.4,fit5.15.25.3.4,fit5.1.2.43.45, fit5.1.24.3.45,
      fit5.1.24.34.5, fit5.14.2.3.45, fit5.14.2.34.5, fit5.14.24.3.5,
      fit5.1.2.34.35, fit5.1.32.4.35,fit5.1.32.34.5,fit5.1.32.34.5,
      fit5.13.2.4.35,fit5.13.2.34.5,fit5.13.23.4.5,fit5.1.3.24.25,fit5.1.23.4.25,
      fit5.1.23.24.5,fit5.12.3.4.25,fit5.12.3.24.5,fit5.12.23.4.5,fit5.2.3.14.15,
      fit5.12.3.24.5, fit5.2.13.14.5, fit5.12.3.4.15, fit5.12.3.14.5, fit5.12.13.4.5)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
                  fit5.1.2.3.4.5, fit5.14.2.3.5,fit5.2.3.14.15,fit5.12.3.14.5)

fit5.12.134.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ *SEENJOY * RACEETHN + CENREG,
                   Hess=TRUE, data = NHES.comp) #Does not converge
fit5.12.234.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY * RACEETHN* SESCHWRK+ CENREG,
                   Hess=TRUE, data = NHES.comp)
fit5.13.124.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SEGRADEQ *SESCHWRK * RACEETHN+ CENREG,
                   Hess=TRUE, data = NHES.comp)
fit5.13.324.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SESCHWRK * RACEETHN* SEENJOY+ CENREG,
                   Hess=TRUE, data = NHES.comp)
fit5.14.123.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SEGRADEQ *SESCHWRK * SEENJOY+ CENREG,
                   Hess=TRUE, data = NHES.comp)
fit5.14.423.5 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SESCHWRK * SEENJOY* RACEETHN+ CENREG,
                   Hess=TRUE, data = NHES.comp)
fit5.12.135.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ *SEENJOY * CENREG +RACEETHN,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.12.235.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEENJOY * CENREG* SESCHWRK+ RACEETHN,
                     Hess=TRUE, data = NHES.comp)
fit5.13.125.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SEGRADEQ *SESCHWRK * CENREG+ RACEETHN,
                     Hess=TRUE, data = NHES.comp)
fit5.13.325.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SESCHWRK * CENREG* SEENJOY+ RACEETHN,
                     Hess=TRUE, data = NHES.comp)
fit5.15.123.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * CENREG + SEGRADEQ *SESCHWRK * SEENJOY+ RACEETHN,
                     Hess=TRUE, data = NHES.comp)
fit5.15.523.4 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * CENREG + SESCHWRK * SEENJOY* CENREG + RACEETHN,
                     Hess=TRUE, data = NHES.comp)
fit5.12.145.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ *RACEETHN * CENREG +SEENJOY,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.12.245.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + RACEETHN * CENREG* SESCHWRK+ SEENJOY,
                     Hess=TRUE, data = NHES.comp)
fit5.14.125.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SEGRADEQ *SESCHWRK * CENREG+ SEENJOY,
                     Hess=TRUE, data = NHES.comp)
fit5.14.425.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SESCHWRK * CENREG* RACEETHN+SEENJOY,
                     Hess=TRUE, data = NHES.comp)
fit5.15.124.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * CENREG + SEGRADEQ *SESCHWRK * RACEETHN+ SEENJOY,
                     Hess=TRUE, data = NHES.comp)
fit5.15.524.3 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * CENREG + SESCHWRK * RACEETHN* CENREG + SEENJOY,
                     Hess=TRUE, data = NHES.comp)
fit5.15.134.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK + SEGRADEQ *SEENJOY * RACEETHN + SESCHWRK,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.15.534.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * CENREG + SEENJOY * RACEETHN* CENREG+ SESCHWRK,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.13.154.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + SEGRADEQ *CENREG * RACEETHN+ SESCHWRK,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.13.354.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SEENJOY + CENREG * RACEETHN* SEENJOY+ SESCHWRK,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.14.153.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SEGRADEQ *CENREG * SEENJOY+ SESCHWRK,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.14.453.2 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + CENREG * SEENJOY* RACEETHN+ SESCHWRK,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.25.234.1 = polr(NHES.comp$SEGRADES ~ SESCHWRK * CENREG + SESCHWRK *SEENJOY * RACEETHN + SEGRADEQ,
                     Hess=TRUE, data = NHES.comp) 
fit5.25.534.1 = polr(NHES.comp$SEGRADES ~ SESCHWRK * CENREG + SEENJOY * RACEETHN* CENREG+ SEGRADEQ,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.23.254.1 = polr(NHES.comp$SEGRADES ~ SESCHWRK * SEENJOY + SESCHWRK *CENREG * RACEETHN+ SEGRADEQ,
                     Hess=TRUE, data = NHES.comp)
fit5.23.354.1 = polr(NHES.comp$SEGRADES ~ SESCHWRK * SEENJOY + CENREG * RACEETHN* SEENJOY+SEGRADEQ,
                     Hess=TRUE, data = NHES.comp) #Does not converge
fit5.24.253.1 = polr(NHES.comp$SEGRADES ~ SESCHWRK * RACEETHN + SESCHWRK *CENREG * SEENJOY+ SEGRADEQ,
                     Hess=TRUE, data = NHES.comp)
fit5.24.453.1 = polr(NHES.comp$SEGRADES ~ SESCHWRK * RACEETHN + CENREG * SEENJOY* RACEETHN+ SEGRADEQ,
                     Hess=TRUE, data = NHES.comp) #Does not converge

anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
                  fit5.1.2.3.4.5, fit5.14.2.3.5,fit5.2.3.14.15,fit5.12.3.14.5,
      fit5.24.253.1,fit5.23.254.1,fit5.25.234.1,fit5.15.524.3,fit5.15.124.3,
      fit5.14.425.3,fit5.14.125.3,fit5.14.125.3,fit5.12.245.3,fit5.15.523.4,
      fit5.15.123.4,fit5.13.325.4,fit5.13.125.4,fit5.12.235.4,fit5.14.423.5,
      fit5.14.123.5, fit5.13.324.5,fit5.13.124.5, fit5.12.234.5)

cum.anova = anova(fit0, fit1.1, fit2.1.2, fit2.12, fit3.1.2.3, fit3.12.3,
                  fit3.13.2, fit4.1.2.3.4,fit4.12.3.4, fit4.13.2.4, 
                  fit5.1.2.3.4.5, fit5.14.2.3.5,fit5.2.3.14.15,fit5.12.3.14.5,
                  fit5.15.124.3)

#Since all the four variable models failed to converge when assessing four variable
#association, no more higher association models will be assessed because they will
#also fail to converge.


#Fully saturated model
fit4.1234 = polr(NHES.comp$SEGRADES ~ SEGRADEQ * SESCHWRK * SEENJOY * RACEETHN,Hess=TRUE, data = NHES.comp)
#Does not converge

#Final Analysis-of-Deviance Table
cum.anova

#Final Model
fit.final = polr(NHES.comp$SEGRADES ~ SEGRADEQ * RACEETHN + SESCHWRK + SEENJOY + CENREG,Hess=TRUE, data = NHES.comp)
summary(fit.final)

## store coefficient table
ctable <- coef(summary(fit.final))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
print(ctable <- cbind(ctable, "p value" = p))

ci <- confint(fit.final)

exp(cbind(OR = coef(fit.final), ci))

exp(fit.final$zeta)

#Chi-Square Goodness-of-Fit Test
1-pchisq(deviance(fit.final),df.residual(fit.final))
