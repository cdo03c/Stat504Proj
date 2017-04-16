#Load in subsetted NHES 2012 survey data for analysis
NHES = read.csv('./processed_NHES_data.csv')

#Removes the row ID column read in from the .csv
NHES = NHES[-1]

#Replaces all the -1 values from the NHES survey with NA bceause -1 represents a
#valid skip
NHES[NHES == -1] <- NA

#Creates factors for columns that are categorical variables so that they can be
#used directly in models
factorCols = c(4:20,26:71,73:84,86:88, 90:140,141:194, 196:215, 217:226,228,
               232:242,244:253,255,259, 271:284, 286:299, 301:302, 308:324,
               330:ncol(NHES))
NHES[factorCols] = lapply(NHES[factorCols],factor)

#hist(NHES$SEGRADES)

library(VGAM)

Mostly.A <- 1*(df2$SEGRADES==1)
Mostly.B <- 1*(df2$SEGRADES==2)
Mostly.C <- 1*(df2$SEGRADES==3)
Mostly.D <- 1*(df2$SEGRADES==4)
model.dat <- data.frame(Mostly.A,Mostly.B,Mostly.C,Mostly.D,FSFREQ=df2$FSFREQ,FHWKHRS=df2$FHWKHRS,SEGBEHAV=df2$SEGBEHAV,SEGWORK=df2$SEGWORK)
head(model.dat)# Observed Counts / Proportions
colSums(model.dat[,c("Mostly.A","Mostly.B","Mostly.C","Mostly.D")])
colSums(model.dat[,c("Mostly.A","Mostly.B","Mostly.C","Mostly.D")]) / nrow(model.dat)# Model
fit = vglm(cbind(Mostly.A,Mostly.B,Mostly.C,Mostly.D)~FSFREQ+FHWKHRS+SEGBEHAV+SEGWORK, data=model.dat, family=cumulative(parallel=T))
summary(fit)