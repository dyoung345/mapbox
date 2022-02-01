# SHORT RUN MODEL

# SOME ISSUES.
# SLOPES ARE IN NOMINAL TERMS NOT PERCENTAGES.
# SE IS IN TERMS OF UNITS OF SLOPE SO IF THE SLOPE IS TO BE DIVIDED BY A FIGURE THEREBY CHANGING
#  IT'S MAGINITUDE THEN TEH SE SHOULD BE TOO.
#  IF ON THE OTHER HAND SOMETHING IS JUST ADDED TO THE X VALUES THEN THE SLOPE MAGNITUDE REMAINS UNCHANGED
#  AND THEREFORE SO DOES THE ERROR OF THE SLOPE.

setwd("C://projects//Loan Investment data//Prosper//data")


library(ggplot2)
# GRAPHING FUNCTION DEFINITIONS
graph8by1 <- function( var1,  dep, data) {

 temp1 <- summary(data[,var1])
 fac1 <- factor(cut(data[,var1] ,
    breaks=c(temp1["Min."], temp1["1st Qu."]-((temp1["Median"]-temp1["1st Qu."])/2),
             temp1["1st Qu."], temp1["Median"]-((temp1["Median"]-temp1["1st Qu."])/2), 
             temp1["Median"], temp1["Median"]+((temp1["3rd Qu."]-temp1["Median"])/2), 
             temp1["3rd Qu."], temp1["3rd Qu."]+((temp1["3rd Qu."]-temp1["Median"])/2),
             temp1["Max."]) ))
 temp3 <- aggregate(data[, dep] , 
    list( var1 =fac1),
    mean)
 temp4 <- aggregate(data[, dep] , 
    list( var1 =fac1),
    length)
 temp4 <- as.vector(temp4$x)

 temp3$sample.size <- factor(cut(temp4 ,  breaks=c(0, 100, 1000, 5000, 1000000 ) ))

 qplot(var1, x, fill= sample.size, position="identity", geom="bar",   data= temp3,
    xlab = var1 , ylab = paste(dep) , 
    main = paste(dep, "\n by", var1) )
}


graph8by2 <- function( var1, var2, dep, data) {

 temp1 <- summary(data[,var1])
 temp2 <- summary(data[,var2])
 fac1 <- factor(cut(data[,var1] ,
    breaks=c(temp1["Min."], temp1["1st Qu."]-((temp1["Median"]-temp1["1st Qu."])/2),
             temp1["1st Qu."], temp1["Median"]-((temp1["Median"]-temp1["1st Qu."])/2), 
             temp1["Median"], temp1["Median"]+((temp1["3rd Qu."]-temp1["Median"])/2), 
             temp1["3rd Qu."], temp1["3rd Qu."]+((temp1["3rd Qu."]-temp1["Median"])/2),
             temp1["Max."]) ))
 fac2 <- factor(cut(data[,var2] ,
    breaks=c(temp2["Min."], temp2["1st Qu."]-((temp2["Median"]-temp2["1st Qu."])/2),
             temp2["1st Qu."], temp2["Median"]-((temp2["Median"]-temp2["1st Qu."])/2), 
             temp2["Median"], temp2["Median"]+((temp2["3rd Qu."]-temp2["Median"])/2), 
             temp2["3rd Qu."], temp2["3rd Qu."]+((temp2["3rd Qu."]-temp2["Median"])/2),
             temp2["Max."]) ))

 temp3 <- aggregate(data[, dep] , 
    list( var1 =fac1, var2 =fac2),
    mean)
 temp4 <- aggregate(data[, dep] , 
    list( var1 =fac1, var2 =fac2 ),
    length)
 temp4 <- as.vector(temp4$x)

 temp3$sample.size <- factor(cut(temp4 ,  breaks=c(0, 100, 1000, 5000, 1000000 ) ))

 qplot(var1, x, fill= sample.size, position="identity", geom="bar", facets= var2 ~ . ,  data= temp3,
    xlab = var1 , ylab = paste(dep," faceted by ", var2) , 
    main = paste(dep, "\n by", var1, ",", var2) )
}

# DEFINING FUNCTION TO CUT 8 BLOCKS OF ONE VAR AND PLOT AGAIST A CATEGORICAL VAR.
graph8by.cat <- function( var1, cat1, dep, data) {
 temp1 <- summary(data[, var1 ])
 fac1 <- factor(cut(data[, var1 ] ,
    breaks=c(temp1["Min."], temp1["1st Qu."]-((temp1["Median"]-temp1["1st Qu."])/2),
             temp1["1st Qu."], temp1["Median"]-((temp1["Median"]-temp1["1st Qu."])/2), 
             temp1["Median"], temp1["Median"]+((temp1["3rd Qu."]-temp1["Median"])/2), 
             temp1["3rd Qu."], temp1["3rd Qu."]+((temp1["3rd Qu."]-temp1["Median"])/2),
             temp1["Max."]) ))
  fac2 <- as.factor(data[, cat1])

 temp3 <- aggregate(data[, dep] , 
    list( var1=fac1, cat=fac2),
    mean)
 temp4 <- aggregate(data[, dep] , 
    list( var1=fac1, cat=fac2 ),
    length)
 temp4 <- as.vector(temp4$x)

 temp3$sample.size <- factor(cut(temp4 ,  breaks=c(0, 100, 1000, 5000, 1000000 ) ))

 qplot(var1, x, fill= sample.size, position="identity", geom="bar", facets= cat ~ . ,  data= temp3,
    xlab = var1 , ylab = paste(dep," faceted by ", cat1) , 
    main = paste(dep, "\n by", var1, ",", cat1) )
}


# DEFINING FUNCTION TO TAKE QUARTILES AND PLOT MEANS OF 3 VARS.
graph4by3 <- function( var1, var2, var3, dep, data) {

 temp1 <- summary(data[,var1])
 temp2 <- summary(data[,var2])
 temp3 <- summary(data[,var3])

 fac1 <- factor(cut(data[,var1] , breaks=c(temp1["Min."], temp1["1st Qu."], temp1["Median"], 
             temp1["3rd Qu."], temp1["Max."]) ))
 fac2 <- factor(cut(data[,var2] , breaks=c(temp2["Min."], temp2["1st Qu."], temp2["Median"], 
             temp2["3rd Qu."], temp2["Max."]) ))
 fac3 <- factor(cut(data[,var3] , breaks=c(temp3["Min."], temp3["1st Qu."], temp3["Median"], 
             temp3["3rd Qu."], temp3["Max."]) ))


 temp4 <- aggregate(data[, dep] , 
    list( var1 =fac1, var2 =fac2, var3 =fac3),
    mean)
 temp5 <- aggregate(data[, dep] , 
    list( var1 =fac1, var2 =fac2 , var3 =fac3),
    length)
 temp5 <- as.vector(temp5$x)

 temp4$sample.size <- factor(cut(temp5 ,  breaks=c(0, 100, 1000, 5000, 1000000 ) ))

 qplot(var1, x, fill= sample.size, position="identity", geom="bar", facets= var2 ~ var3 ,  data= temp4,
    xlab = paste(var1, " faceted by ", var3) , ylab = paste(dep," faceted by ", var2 ) , 
    main = paste(dep, "\n by", var1, ",", var2 , "," , var3 ) )
}

# THIS SETS THE MEMORY LIMIT TO 2000 MB THE LARGEST FOR A SINGLE PROCESS.
memory.limit(size = 2000)

cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')

#load("model.data.Rdata")

what.is <- function ( symbol  ) 
  {
    print("class is>");   print(class(symbol))
    print("mode is>");    print(mode(symbol))
    print("length is>");  print(length(symbol))
    print("dimensions are>"); print(dim(symbol))
  }

model.data$yearfac <- cut(model.data$all.dates.sorted, "year")
summary(model.data$yearfac)






# MODEL FOR SP500.  THE INITIAL INDIVIDIUAL STOCK MODEL WORKED OK FOR SP500, BUT NOT AT ALL FOR THE OTHER 3 INDICIES.
#   HERE I AM TRYING A SEPARATE MODEL
load("unislopes_index4.Rdata")
#load("unislopes_index3.Rdata")
#unislopes_index4 <- unislopes_index3
#load("unislopes_index2.Rdata")
#unislopes_index4 <- unislopes_index2
#load("unislopes_index1.Rdata")
#unislopes_index4 <- unislopes_index1

unislopes_index4$yearfac <- cut(unislopes_index4$all.dates.sorted, "year")
hold500.select <- unislopes_index4$all.dates.sorted >= "2005-01-01"
model500 <- unislopes_index4[ !hold500.select, ]
hold500 <- unislopes_index4[ hold500.select, ]
# IN THE SP500 YEARS 1999, 2000, 2001, 2002, AND 2008 ARE DOWN YEARS.
aggregate(unislopes_index4$FUT.slope.short ,  list( var1 =unislopes_index4$yearfac), mean)



# TAKING OUT MISSING
model500 <- model500[ !is.na(model500$CUR.1.2.slope.price), ]
model500 <- model500[ !is.na(model500$FUT.slope.short), ]
hold500 <- hold500[ !is.na(hold500$CUR.1.2.slope.price), ]
hold500 <- hold500[ !is.na(hold500$FUT.slope.short), ]



cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')
lmtest <- lm(formula=((FUT.slope.short/close5day) ~ (CUR.1.2.slope.price/close5day) *
                                        I((CUR.1.2.slope.price/close5day)^2)     *
                                        CUR.1.2.last.resids.price  *
                                        I(CUR.1.2.last.resids.price^2)
                                        
                                        ), 
                                        data=model500,  na.action=na.exclude)
summary(lmtest)

#  CUR.1.2.se.slope.price +  CUR.1.2.last.resids.price + 
#                    CUR.11.20.slope +   CUR.11.20.last.resids +  CUR.11.20.r2 + 
#                    CUR.21.50.slope +   CUR.21.50.last.resids +  CUR.21.50.r2 + 
#                    CUR.vol.ratio1

# WHAT SEEMS CLEAR IS THAT YOU CAN MODEL SP500 PRETTY WELL IN TERMS OF CONGRUENT SLOPE, BUT
#  IF THE OVERALL MARKET IS DOWN IT DRAGS IT ALL WITH IT JUST LIKE ON INDIVIDUAL STOCKS.
# I AM NOT SURE THEN THAT IT WILL BE POSSIBLE TO USE THE SP500 DIRECTION TO PREDICT OVERALL
#  MARKET DIRECTION.  PERHAPS A MIX OF SHORT/LONG PREDICTIONS WILL WORK.  IT IS AFTERALL
# THE LONG PREDICTION THAT REPRESENTS THE OVERRIDING DIRECTION.

lmtest <- lm(formula=(FUT.slope.short ~ 1 +
((CUR.1.2.slope.price + I(CUR.1.2.slope.price^2) + I(CUR.1.2.slope.price^3)) *
 CUR.1.2.last.resids.price) * CUR.1.2.se.slope.price 
   		
     ), 
                                        data=model500,  na.action=na.exclude)
summary(lmtest)

#sub<- model500[ , c("CUR.1.2.slope.price", "CUR.1.2.se.slope.price", "CUR.1.2.last.resids.price",
#                    "CUR.11.20.slope",  "CUR.11.20.last.resids", "CUR.11.20.r2",
#                    "CUR.21.50.slope",  "CUR.21.50.last.resids", "CUR.21.50.r2",
#                    "CUR.vol.ratio1")]
#cor(sub, method="kendall", use="pairwise.complete.obs")


slm1 <- step(lmtest, direction = c("backward") )
summary(slm1)

model500$predicted1 <- predict(slm1, model500) 
hold500$predicted1 <- predict(slm1, hold500) 

model500$set <- "model" 
hold500$set <- "hold"
all500 <- rbind(model500, hold500)
all500 <- all500[ !is.na(all500$predicted1), ]
all500 <- all500[ !is.na(all500$FUT.slope.short), ]
all500 <- all500[ !is.na(all500$set), ]

graph8by.cat(var1="predicted1", dep="FUT.slope.short", 
          cat1="set" , data=all500)

graph8by.cat(var1="predicted1", dep="FUT.slope.long", 
          cat1="set" , data=all500)

# LONG TERM PREDICTION
hold500<- hold500[ !is.na(hold500$FUT.slope.long), ]
model500<- model500[ !is.na(model500$FUT.slope.long), ]



# STANDARD LINEAR REGRESSION
lmtest.long <- lm(formula=(FUT.slope.long ~ 1 +
(CUR.1.2.slope.price +
CUR.11.20.slope +
CUR.201.500.last.resids	+
CUR.201.500.se	+
CUR.201.500.slope	+
CUR.201.500.last.resids*CUR.201.500.slope +
CUR.11.20.slope*CUR.201.500.slope
  )) ,   data=model500,  na.action=na.exclude)
summary(lmtest.long)

# STEPWISE AIC SELECTION BASED ON LINEAR MODEL
slmlong <- step(lmtest.long, direction = c("backward") )
summary(slmlong)

# BEST ANSWER SO FAR FOR LONG TERM.  MAYBE I SHOULD MIX UP THE HOLD/MODEL SPLIT.
library(MASS)
# ROBUST REGRESSION, MM-ESTIMATION
slmlong <- rlm(formula=(FUT.slope.long ~ 1 +
(CUR.1.2.slope.price +
CUR.11.20.slope +
CUR.201.500.se	+
CUR.201.500.slope	+
  )) , maxit=1500 ,  data=model500, method="MM",  na.action=na.exclude)
summary(slmlong)

# RESISTANT REGRESSION
slmlong <- lqs(formula=(FUT.slope.long ~ 1 +
(CUR.1.2.slope.price +
CUR.11.20.slope +
CUR.201.500.last.resids	+
CUR.201.500.se	+
CUR.201.500.slope	+
CUR.201.500.last.resids*CUR.201.500.slope 
  )) ,   data=model500, method="lts",  na.action=na.exclude)
summary(slmlong)
# BOOTSTRAP COEFFICIENT SIGNIFICANCE FOR RESISTANT REGRESSION
#slmlong$coef
#what.is(slmlong$resid)
#what.is(model500)



model500$predicted.long <- predict(slmlong, model500) 
hold500$predicted.long <- predict(slmlong, hold500) 

# R2 CHECK
summary(lm(formula=(FUT.slope.long ~ -1 + predicted.long ), data=model500, na.action=na.exclude))
summary(lm(formula=(FUT.slope.long ~ -1 + predicted.long ), data=hold500, na.action=na.exclude))



model500$set <- "model" 
hold500$set <- "hold"
all500 <- rbind(model500, hold500)
all500 <- all500[ !is.na(all500$predicted.long), ]
all500 <- all500[ !is.na(all500$FUT.slope.long), ]
all500 <- all500[ !is.na(all500$set), ]


#qplot(all.dates.sorted, FUT.slope.long, data=all500)
#qplot(all.dates.sorted, predicted.long, data=all500)
#summary(all500$FUT.slope.long)
#summary(all500$predicted.long)

graph8by.cat(var1="predicted.long", dep="FUT.slope.long", 
          cat1="set" , data=all500)
qplot(all.dates.sorted, FUT.slope.long, facets=set ~ . , data=all500)

graph8by2(var1="predicted.long", dep="FUT.slope.long", 
          var2="all.dates.sorted" , data=all500)

# STOPPED HERE. LOOKS LIKE THE SAME OVERWHELM PROBLEM ON LONG TERM AS SHORT TERM PREDICTION.
#  MAYBE I NEED TO LOOK AT MACRO STUFF TO GET THE OVERALL DIRECTION PICKED.

#####



graph4by3(data=model500, dep="FUT.slope.short", 
          var1="CUR.1.2.slope.price", var2="CUR.1.2.last.resids.price",
          var3="CUR.1.2.se.slope.price") 
graph4by3(data=model500, dep="FUT.slope.short", 
          var1="CUR.1.2.slope.price", var2="CUR.1.2.last.resids.price",
          var3="CUR.1.2.r2.price") 

graph4by3(data=hold500, dep="FUT.slope.short", 
          var1="CUR.1.2.slope.price", var2="CUR.1.2.last.resids.price",
          var3="CUR.1.2.se.slope.price") 

graph4by3(data=model500, dep="FUT.slope.short", 
          var1="CUR.3.10.slope", var2="CUR.3.10.last.resids",
          var3="CUR.3.10.r2") 
graph4by3(data=model500, dep="FUT.slope.short", 
          var1="CUR.11.20.slope", var2="CUR.1.2.last.resids.price",
          var3="CUR.11.20.se") 


graph8by2(data=model500, dep="FUT.slope.short", 
          var1="CUR.1.2.slope.price", var2="CUR.1.2.last.resids.price")


graph8by1(data=model500, dep="FUT.slope.short", 
          var1="CUR.3.10.slope")
graph8by1(data=hold500, dep="FUT.slope.short", 
          var1="CUR.1.2.slope.price")
graph8by1(data=model500, dep="FUT.slope.short", 
          var1="CUR.1.2.last.resids.price")

qplot(CUR.1.2.slope.price, FUT.slope.short, data=model500)

# STOPPED HERE, IT LOOKS LIKE ALL THE SOME OF THE RELATIONSHIPS STILL HOLD BUT NOT ALL.
#  ALSO LOOKS LIKE THE APPROACHING POSITIVE SLOPE IDEA SHOULD BE CHECKED OUT.  

# YEARS 2001 AND 2002 ARE DOWN YEARS.  THE REST ARE UP. NOT SURE ABOUT IT.
# aggregate((model.data$CUR.51.100.slope / model.data$close5day),  list( var1 =model.data$yearfac), mean, na.rm=TRUE)


# EVALUATION OF IND STOCK MODELS RESTARTED MAY 26, 2010.
load("model.data.Rdata")
hold.select <- model.data$all.dates.sorted >= "2001-01-01"
model.small <- model.data[ !hold.select, ]

set.seed(1410)
model.small <- model.small[sample(nrow(model.small), 150000) , ]
what.is(model.small)
hold.small <- model.data[ hold.select, ]
what.is(hold.small)
cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')
rm(model.data)
rm(hold.select)

load("hold.data.Rdata")
set.seed(1410)
hold.small2 <- hold.data[sample(nrow(hold.data), 150000) , ]
rm(hold.data)
hold.small2$yearfac <- cut(hold.small2$all.dates.sorted, "year")
hold.small$yearfac <- cut(hold.small$all.dates.sorted, "year")
model.small$yearfac <- cut(model.small$all.dates.sorted, "year")

summary(hold.small2$yearfac)
summary(hold.small$yearfac)
summary(model.small$yearfac)
cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')
hold.small$weight <- 0
hold.small2$weight <- 0

# WEIGHT FOR MODELING 
model.small$weight <- 1


# PUTTING MODEL AND HOLD TOGETHER, BUT WITH HOLD OF WEIGHT 0.
#all.small <- rbind(model.small, hold.small, hold.small2)


model.small$FUT.slope.short.perc <- model.small$FUT.slope.short / model.small$close5day
model.small$CUR.1.2.slope.price.perc <- model.small$CUR.1.2.slope.price / model.small$close5day
model.small$CUR.11.20.slope.perc <- model.small$CUR.11.20.slope / model.small$close5day
model.small$CUR.21.50.slope.perc <- model.small$CUR.21.50.slope / model.small$close5day
model.small$CUR.3.10.slope.perc <- model.small$CUR.3.10.slope / model.small$close5day

hold.small$FUT.slope.short.perc <- hold.small$FUT.slope.short / hold.small$close5day
hold.small$CUR.1.2.slope.price.perc <- hold.small$CUR.1.2.slope.price / hold.small$close5day
hold.small$CUR.11.20.slope.perc <- hold.small$CUR.11.20.slope / hold.small$close5day
hold.small$CUR.21.50.slope.perc <- hold.small$CUR.21.50.slope / hold.small$close5day
hold.small$CUR.3.10.slope.perc <- hold.small$CUR.3.10.slope / hold.small$close5day

hold.small2$FUT.slope.short.perc <- hold.small2$FUT.slope.short / hold.small2$close5day
hold.small2$CUR.1.2.slope.price.perc <- hold.small2$CUR.1.2.slope.price / hold.small2$close5day
hold.small2$CUR.11.20.slope.perc <- hold.small2$CUR.11.20.slope / hold.small2$close5day
hold.small2$CUR.21.50.slope.perc <- hold.small2$CUR.21.50.slope / hold.small2$close5day
hold.small2$CUR.3.10.slope.perc <- hold.small2$CUR.3.10.slope / hold.small2$close5day



graph8by1(var1="CUR.1.2.slope.price.perc", dep="FUT.slope.short.perc", data=model.small)
graph4by3(var1="CUR.1.2.slope.price.perc", var2="CUR.vol.ratio1", 
          var3="CUR.1.2.last.resids.price", dep="FUT.slope.short.perc", data=model.small)
graph8by2(var1="CUR.1.2.slope.price.perc", var2="CUR.vol.ratio1", 
           dep="FUT.slope.short.perc", data=model.small)



graph8by1(var1="CUR.11.20.slope.perc", dep="FUT.slope.short.perc", data=model.small)
graph4by3(var1="CUR.11.20.slope.perc", var2="CUR.11.20.se", 
          var3="CUR.11.20.last.resids", dep="FUT.slope.short.perc", data=model.small)

graph4by3(var2="CUR.21.50.slope.perc", var1="CUR.1.2.slope.price.perc", 
          var3="CUR.21.50.last.resids", dep="FUT.slope.short.perc", data=model.small)



cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')


# HERE, NEED TO SET UP A DUMMY VARIABLE TO SELECT LOW 11.20.SE VALUES WHERE 11.20.SLOPE IS MEANINGFUL.
#  THEN NEED TO CHECK FOR INTERACTONS WITH THE MEANINGFUL PART OF THE SLOPE. AND SINCE I WILL BE ADDING
#  SUBTRACTING FROM THE MODEL IN ONLY ONE RANGE OF 11.20.SE THEN THAT COULD ALSO IMPACT THE PARAMETER OF
#  OTHER VARIALBE IN THAT SAME RANGE SO I SHOULD ALSO INTERACT THE DUMMY WITH THE OTHER MODEL VARS.
#  ESSENTIALLY IT AMOUNTS TO A SEPARATE MODEL WITHIN THE RANGE WHERE THE 11.20.SLOPE IS USEFUL.
# NEXT STEPS
#  CHECK ON HOLDOUT
#  CHECK OVERALL PREDICTIVE LEVEL
#  CHECK IF GENERAL MARKET TENDANCY STILL OVERPOWERS PREDICTION
#  CHECK CLOSE5DAY PRICE AS A PREDICTOR
#  CHECK RESIDUALS VERSUS PREDICTOR VARIALBES TO SEE IF THEY NEED MORE ADJUSTMENT
#  CHECK POLYNOMIAL TERMS
# MARKET INDICE MODEL
# INDICE VARS AS PREDICTORS FOR IND STOCKS.
# LONG TERM MODEL.


lmtest <- lm(formula=((FUT.slope.short.perc) ~ 1+ 
         
(CUR.1.2.slope.price.perc +  
CUR.1.2.se.slope.price  +
CUR.1.2.last.resids.price +
CUR.1.2.r2.price +
I(CUR.1.2.slope.price.perc^2) +                               
I(CUR.1.2.se.slope.price^2)  +
I(CUR.1.2.last.resids.price^2) +
I(CUR.1.2.slope.price.perc^3)                                
 )^2 +

(CUR.11.20.slope.perc +
CUR.11.20.se +
CUR.11.20.last.resids +
CUR.11.20.r2 +
I(CUR.11.20.slope.perc^2) +
I(CUR.11.20.se^2) +
I(CUR.11.20.last.resids^2) +
I(CUR.11.20.slope.perc^3) 
 )^2 +

(CUR.1.2.slope.price.perc +  
CUR.1.2.se.slope.price  +
CUR.1.2.last.resids.price +
CUR.11.20.slope.perc +
CUR.11.20.se +
CUR.11.20.last.resids )^2 +


(CUR.vol.ratio1 +
CUR.vol.ratio2 +
CUR.vol.ratio3 +
I(CUR.vol.ratio1^2) +
I(CUR.vol.ratio2^2) 
 )^2
        ), 
        data=model.small, weight=weight, na.action=na.exclude)
summary(lmtest)

# MEMORY PROBLEM	
save(hold.small, file="hold.small.Rdata")
save(model.small, file="model.small.Rdata")
rm(hold.small)
rm(model.small)

load("model.small.Rdata")
cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')

# BASED ON THE LM ABOVE BUT WITH SOME INSIGNIFICANT HIGH LEVEL INTERACTIONS TAKEN OUT.
lmtest <- lm(formula=((FUT.slope.short.perc) ~ 1+ 
CUR.1.2.slope.price.perc	+
CUR.1.2.se.slope.price	+
CUR.1.2.last.resids.price	+
CUR.1.2.r2.price	+
I(CUR.1.2.slope.price.perc^2)	+
I(CUR.1.2.se.slope.price^2)	+
I(CUR.1.2.last.resids.price^2)	+
I(CUR.1.2.slope.price.perc^3)	+
CUR.11.20.slope.perc	+
CUR.11.20.se	+
CUR.11.20.last.resids	+
CUR.11.20.r2	+
I(CUR.11.20.slope.perc^2)	+
I(CUR.11.20.se^2)	+
I(CUR.11.20.last.resids^2)	+
I(CUR.11.20.slope.perc^3)	+
CUR.vol.ratio1	+
CUR.vol.ratio2	+
CUR.vol.ratio3	+
I(CUR.vol.ratio1^2)	+
I(CUR.vol.ratio2^2)	+
CUR.1.2.slope.price.perc:CUR.1.2.se.slope.price	+
CUR.1.2.slope.price.perc:CUR.1.2.last.resids.price	+
CUR.1.2.slope.price.perc:CUR.1.2.r2.price	+
CUR.1.2.slope.price.perc:I(CUR.1.2.se.slope.price^2)	+
CUR.1.2.slope.price.perc:I(CUR.1.2.last.resids.price^2)	+
CUR.1.2.slope.price.perc:I(CUR.1.2.slope.price.perc^3)	+
CUR.1.2.se.slope.price:CUR.1.2.last.resids.price	+
CUR.1.2.se.slope.price:CUR.1.2.r2.price	+
CUR.1.2.se.slope.price:I(CUR.1.2.slope.price.perc^2)	+
CUR.1.2.se.slope.price:I(CUR.1.2.se.slope.price^2)	+
CUR.1.2.se.slope.price:I(CUR.1.2.last.resids.price^2)	+
CUR.1.2.se.slope.price:I(CUR.1.2.slope.price.perc^3)	+
CUR.1.2.last.resids.price:CUR.1.2.r2.price	+
CUR.1.2.last.resids.price:I(CUR.1.2.slope.price.perc^2)	+
CUR.1.2.last.resids.price:I(CUR.1.2.se.slope.price^2)	+
CUR.1.2.last.resids.price:I(CUR.1.2.last.resids.price^2)	+
CUR.1.2.last.resids.price:I(CUR.1.2.slope.price.perc^3)	+
CUR.1.2.r2.price:I(CUR.1.2.slope.price.perc^2)	+
CUR.1.2.r2.price:I(CUR.1.2.se.slope.price^2)	+
CUR.1.2.r2.price:I(CUR.1.2.last.resids.price^2)	+
CUR.1.2.r2.price:I(CUR.1.2.slope.price.perc^3)	+
I(CUR.1.2.slope.price.perc^2):I(CUR.1.2.se.slope.price^2)	+
I(CUR.1.2.slope.price.perc^2):I(CUR.1.2.last.resids.price^2)	+
I(CUR.1.2.slope.price.perc^2):I(CUR.1.2.slope.price.perc^3)	+
I(CUR.1.2.se.slope.price^2):I(CUR.1.2.last.resids.price^2)	+
I(CUR.1.2.se.slope.price^2):I(CUR.1.2.slope.price.perc^3)	+
I(CUR.1.2.last.resids.price^2):I(CUR.1.2.slope.price.perc^3)	+
CUR.11.20.slope.perc:CUR.11.20.se	+
CUR.11.20.slope.perc:CUR.11.20.last.resids	+
CUR.11.20.slope.perc:CUR.11.20.r2	+
CUR.11.20.slope.perc:I(CUR.11.20.se^2)	+
CUR.11.20.slope.perc:I(CUR.11.20.last.resids^2)	+
CUR.11.20.slope.perc:I(CUR.11.20.slope.perc^3)	+
CUR.11.20.se:CUR.11.20.last.resids	+
CUR.11.20.se:CUR.11.20.r2	+
CUR.11.20.se:I(CUR.11.20.slope.perc^2)	+
CUR.11.20.se:I(CUR.11.20.se^2)	+
CUR.11.20.se:I(CUR.11.20.last.resids^2)	+
CUR.11.20.se:I(CUR.11.20.slope.perc^3)	+
CUR.11.20.last.resids:CUR.11.20.r2	+
CUR.11.20.last.resids:I(CUR.11.20.slope.perc^2)	+
CUR.11.20.last.resids:I(CUR.11.20.se^2)	+
CUR.11.20.last.resids:I(CUR.11.20.last.resids^2)	+
CUR.11.20.last.resids:I(CUR.11.20.slope.perc^3)	+
CUR.11.20.r2:I(CUR.11.20.slope.perc^2)	+
CUR.11.20.r2:I(CUR.11.20.se^2)	+
CUR.11.20.r2:I(CUR.11.20.last.resids^2)	+
I(CUR.11.20.slope.perc^2):I(CUR.11.20.last.resids^2)	+
I(CUR.11.20.se^2):I(CUR.11.20.last.resids^2)	+
I(CUR.11.20.se^2):I(CUR.11.20.slope.perc^3)	+
I(CUR.11.20.last.resids^2):I(CUR.11.20.slope.perc^3)	+
CUR.1.2.slope.price.perc:CUR.11.20.slope.perc	+
CUR.1.2.slope.price.perc:CUR.11.20.se	+
CUR.1.2.se.slope.price:CUR.11.20.slope.perc	+
CUR.1.2.se.slope.price:CUR.11.20.se	+
CUR.1.2.se.slope.price:CUR.11.20.last.resids	+
CUR.1.2.last.resids.price:CUR.11.20.slope.perc	+
CUR.1.2.last.resids.price:CUR.11.20.se	+
CUR.vol.ratio1:CUR.vol.ratio2	+
CUR.vol.ratio1:CUR.vol.ratio3	+
CUR.vol.ratio1:I(CUR.vol.ratio1^2)	+
CUR.vol.ratio2:CUR.vol.ratio3	+
CUR.vol.ratio3:I(CUR.vol.ratio1^2)	+
CUR.vol.ratio3:I(CUR.vol.ratio2^2)	

 ), 
        data=model.small, weight=weight, na.action=na.exclude)
summary(lmtest)


cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')
load("hold.small.Rdata")


model.small$predicted1 <- predict(lmtest, model.small)
hold.small$predicted1 <- predict(lmtest, hold.small)

#("index_valueline")
#("index_DJI")
#("index_NYSEcomposite")
#("index_SP500")

# STEPWISE AIC SELECTION BASED ON LINEAR MODEL
slm <- step(lmtest, direction = c("forward") )
summary(slm)

library(MASS)
# ROBUST REGRESSION, MM-ESTIMATION was too memory intensive so i had to take it out.
slm <- rlm(formula=(FUT.slope.short.perc ~ 1 +
CUR.1.2.slope.price.perc	+
CUR.1.2.se.slope.price	+
CUR.1.2.last.resids.price	+
CUR.1.2.r2.price	+
I(CUR.1.2.slope.price.perc^2)	+
I(CUR.1.2.se.slope.price^2)	+
I(CUR.1.2.last.resids.price^2)	+
I(CUR.1.2.slope.price.perc^3)	+
CUR.11.20.slope.perc	+
CUR.11.20.se	+
CUR.11.20.last.resids	+
CUR.11.20.r2	+
I(CUR.11.20.slope.perc^2)	+
I(CUR.11.20.se^2)	+
I(CUR.11.20.last.resids^2)	+
I(CUR.11.20.slope.perc^3)	+
CUR.vol.ratio1	+
CUR.vol.ratio2	+
CUR.vol.ratio3	+
I(CUR.vol.ratio1^2)	+
I(CUR.vol.ratio2^2)	+
CUR.1.2.slope.price.perc:CUR.1.2.se.slope.price	+
CUR.1.2.slope.price.perc:CUR.1.2.last.resids.price	+
CUR.1.2.slope.price.perc:CUR.1.2.r2.price	+
CUR.1.2.slope.price.perc:I(CUR.1.2.se.slope.price^2)	+
CUR.1.2.slope.price.perc:I(CUR.1.2.last.resids.price^2)	+
CUR.1.2.slope.price.perc:I(CUR.1.2.slope.price.perc^3)	+
CUR.1.2.se.slope.price:CUR.1.2.last.resids.price	+
CUR.1.2.se.slope.price:CUR.1.2.r2.price	+
CUR.1.2.se.slope.price:I(CUR.1.2.slope.price.perc^2)	+
CUR.1.2.se.slope.price:I(CUR.1.2.se.slope.price^2)	+
CUR.1.2.se.slope.price:I(CUR.1.2.last.resids.price^2)	+
CUR.1.2.se.slope.price:I(CUR.1.2.slope.price.perc^3)	+
CUR.1.2.last.resids.price:CUR.1.2.r2.price	+
CUR.1.2.last.resids.price:I(CUR.1.2.slope.price.perc^2)	+
CUR.1.2.last.resids.price:I(CUR.1.2.se.slope.price^2)	+
CUR.1.2.last.resids.price:I(CUR.1.2.last.resids.price^2)	+
CUR.1.2.last.resids.price:I(CUR.1.2.slope.price.perc^3)	+
CUR.1.2.r2.price:I(CUR.1.2.slope.price.perc^2)	+
CUR.1.2.r2.price:I(CUR.1.2.se.slope.price^2)	+
CUR.1.2.r2.price:I(CUR.1.2.last.resids.price^2)	+
CUR.1.2.r2.price:I(CUR.1.2.slope.price.perc^3)	+
I(CUR.1.2.slope.price.perc^2):I(CUR.1.2.se.slope.price^2)	+
I(CUR.1.2.slope.price.perc^2):I(CUR.1.2.last.resids.price^2)	+
I(CUR.1.2.slope.price.perc^2):I(CUR.1.2.slope.price.perc^3)	+
I(CUR.1.2.se.slope.price^2):I(CUR.1.2.last.resids.price^2)	+
I(CUR.1.2.se.slope.price^2):I(CUR.1.2.slope.price.perc^3)	+
I(CUR.1.2.last.resids.price^2):I(CUR.1.2.slope.price.perc^3)	+
CUR.11.20.slope.perc:CUR.11.20.se	+
CUR.11.20.slope.perc:CUR.11.20.last.resids	+
CUR.11.20.slope.perc:CUR.11.20.r2	+
CUR.11.20.slope.perc:I(CUR.11.20.se^2)	+
CUR.11.20.slope.perc:I(CUR.11.20.last.resids^2)	+
CUR.11.20.slope.perc:I(CUR.11.20.slope.perc^3)	+
CUR.11.20.se:CUR.11.20.last.resids	+
CUR.11.20.se:CUR.11.20.r2	+
CUR.11.20.se:I(CUR.11.20.slope.perc^2)	+
CUR.11.20.se:I(CUR.11.20.se^2)	+
CUR.11.20.se:I(CUR.11.20.last.resids^2)	+
CUR.11.20.se:I(CUR.11.20.slope.perc^3)	+
CUR.11.20.last.resids:CUR.11.20.r2	+
CUR.11.20.last.resids:I(CUR.11.20.slope.perc^2)	+
CUR.11.20.last.resids:I(CUR.11.20.se^2)	+
CUR.11.20.last.resids:I(CUR.11.20.last.resids^2)	+
CUR.11.20.last.resids:I(CUR.11.20.slope.perc^3)	+
CUR.11.20.r2:I(CUR.11.20.slope.perc^2)	+
CUR.11.20.r2:I(CUR.11.20.se^2)	+
CUR.11.20.r2:I(CUR.11.20.last.resids^2)	+
I(CUR.11.20.slope.perc^2):I(CUR.11.20.last.resids^2)	+
I(CUR.11.20.se^2):I(CUR.11.20.last.resids^2)	+
I(CUR.11.20.se^2):I(CUR.11.20.slope.perc^3)	+
I(CUR.11.20.last.resids^2):I(CUR.11.20.slope.perc^3)	+
CUR.1.2.slope.price.perc:CUR.11.20.slope.perc	+
CUR.1.2.slope.price.perc:CUR.11.20.se	+
CUR.1.2.se.slope.price:CUR.11.20.slope.perc	+
CUR.1.2.se.slope.price:CUR.11.20.se	+
CUR.1.2.se.slope.price:CUR.11.20.last.resids	+
CUR.1.2.last.resids.price:CUR.11.20.slope.perc	+
CUR.1.2.last.resids.price:CUR.11.20.se	+
CUR.vol.ratio1:CUR.vol.ratio2	+
CUR.vol.ratio1:CUR.vol.ratio3	+
CUR.vol.ratio1:I(CUR.vol.ratio1^2)	+
CUR.vol.ratio2:CUR.vol.ratio3	+
CUR.vol.ratio3:I(CUR.vol.ratio1^2)	+
CUR.vol.ratio3:I(CUR.vol.ratio2^2)	


) ,   data=model.small,   na.action=na.exclude)
summary(slm)


#  RESISTANT VERSION TAKES OUT SO MUCH THAT R2 IS WORTHLESS SO I'VE DELETED ITS CODE.


# ADDITIIVE REGRESSION.
memory.limit(size = 2000)
library(ggplot2)
load("model.small.Rdata")
cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')

model.small.add <- model.small[ , c("FUT.slope.short.perc", "FUT.N.short",
"CUR.1.2.slope.price.perc",
"CUR.1.2.se.slope.price", 
"CUR.1.2.last.resids.price", 
"CUR.1.2.r2.price", 
"CUR.11.20.slope.perc", 
"CUR.11.20.se",
"CUR.11.20.last.resids", 
"CUR.11.20.r2",
"CUR.vol.ratio1", 
"CUR.vol.ratio2",
"CUR.vol.ratio3",
"yearfac")]


model.small.add$slope.1.2.r2.2of4 <- ifelse(model.small.add$CUR.1.2.r2.price > 0.06904, 
                                            model.small.add$CUR.1.2.slope.price.perc, 0)
model.small.add$slope.1.2.r2.3of4 <- ifelse(model.small.add$CUR.1.2.r2.price > 0.24160, 
                                            model.small.add$CUR.1.2.slope.price.perc, 0)
model.small.add$slope.1.2.r2.4of4 <- ifelse(model.small.add$CUR.1.2.r2.price > 0.47850, 
                                            model.small.add$CUR.1.2.slope.price.perc, 0)

model.small.add$last.resids.1.2.r2.2of4 <- ifelse(model.small.add$CUR.1.2.r2.price > 0.06904, 
                                            model.small.add$CUR.1.2.last.resids.price, 0)
model.small.add$last.resids.1.2.r2.3of4 <- ifelse(model.small.add$CUR.1.2.r2.price > 0.24160, 
                                            model.small.add$CUR.1.2.last.resids.price, 0)
model.small.add$last.resids.1.2.r2.4of4 <- ifelse(model.small.add$CUR.1.2.r2.price > 0.47850, 
                                            model.small.add$CUR.1.2.last.resids.price, 0)

model.small.add$slope.1.2.se.2of4 <- ifelse(model.small.add$CUR.1.2.se.slope.price > 1.652e-02, 
                                            model.small.add$CUR.1.2.slope.price.perc, 0)
model.small.add$slope.1.2.se.3of4 <- ifelse(model.small.add$CUR.1.2.se.slope.price > 3.335e-02, 
                                            model.small.add$CUR.1.2.slope.price.perc, 0)
model.small.add$slope.1.2.se.4of4 <- ifelse(model.small.add$CUR.1.2.se.slope.price > 6.549e-02, 
                                            model.small.add$CUR.1.2.slope.price.perc, 0)

model.small.add$last.resids.1.2.se.2of4 <- ifelse(model.small.add$CUR.1.2.se.slope.price > 1.652e-02, 
                                            model.small.add$CUR.1.2.last.resids.price, 0)
model.small.add$last.resids.1.2.se.3of4 <- ifelse(model.small.add$CUR.1.2.se.slope.price > 3.335e-02, 
                                            model.small.add$CUR.1.2.last.resids.price, 0)
model.small.add$last.resids.1.2.se.4of4 <- ifelse(model.small.add$CUR.1.2.se.slope.price > 6.549e-02, 
                                            model.small.add$CUR.1.2.last.resids.price, 0)



names(model.small.add)

library(mda)
cat('point 1 mem', memory.size(), memory.size(max=TRUE), '\n')
model.small.add <- model.small.add[ complete.cases(model.small.add), ]
model.small.x <- model.small.add[ , c(-1, -2,-14) ]
model.small.y <- model.small.add[ , 1 ]
model.small.yN <- model.small.add[ , 2 ]
slm <- mars(model.small.x, model.small.y, degree=2)
slmN <- mars(model.small.x, model.small.yN, degree=1)
summary(slm)
summary(slmN)
 
slmN$selected.terms
showcuts <- function(obj)
{
  tmp <- obj$cuts[obj$sel, ]
  dimnames(tmp) <- list(NULL, names(model.small.x))
  tmp
}
showcuts(slm)
showcuts(slmN)


load("hold.small.Rdata")
load("hold.small2.Rdata")

# ADDITIONAL CODE NECESSARY FOR MARS MODELS THAT SEEM TO NOT BE ABLE TO HANDLE MISSING OBS.
# ASLO IT APPEARS THAT THE VARIABLES HAVE TO BE IN EXACTLY THE SAME POSITIONS!! THUS
# NO DEPENDENT ALLOWED!
hold.small.add <- hold.small[ , c("FUT.slope.short.perc", "FUT.N.short",
"CUR.1.2.slope.price.perc",
"CUR.1.2.se.slope.price", 
"CUR.1.2.last.resids.price", 
"CUR.1.2.r2.price", 
"CUR.11.20.slope.perc", 
"CUR.11.20.se",
"CUR.11.20.last.resids", 
"CUR.11.20.r2", 
"CUR.vol.ratio1", 
"CUR.vol.ratio2",
"CUR.vol.ratio3",
"yearfac")]
hold.small.add$slope.1.2.r2.2of4 <- ifelse(hold.small.add$CUR.1.2.r2.price > 0.06904, 
                                            hold.small.add$CUR.1.2.slope.price.perc, 0)
hold.small.add$slope.1.2.r2.3of4 <- ifelse(hold.small.add$CUR.1.2.r2.price > 0.24160, 
                                            hold.small.add$CUR.1.2.slope.price.perc, 0)
hold.small.add$slope.1.2.r2.4of4 <- ifelse(hold.small.add$CUR.1.2.r2.price > 0.47850, 
                                            hold.small.add$CUR.1.2.slope.price.perc, 0)

hold.small.add$last.resids.1.2.r2.2of4 <- ifelse(hold.small.add$CUR.1.2.r2.price > 0.06904, 
                                            hold.small.add$CUR.1.2.last.resids.price, 0)
hold.small.add$last.resids.1.2.r2.3of4 <- ifelse(hold.small.add$CUR.1.2.r2.price > 0.24160, 
                                            hold.small.add$CUR.1.2.last.resids.price, 0)
hold.small.add$last.resids.1.2.r2.4of4 <- ifelse(hold.small.add$CUR.1.2.r2.price > 0.47850, 
                                            hold.small.add$CUR.1.2.last.resids.price, 0)

hold.small.add$slope.1.2.se.2of4 <- ifelse(hold.small.add$CUR.1.2.se.slope.price > 1.652e-02, 
                                            hold.small.add$CUR.1.2.slope.price.perc, 0)
hold.small.add$slope.1.2.se.3of4 <- ifelse(hold.small.add$CUR.1.2.se.slope.price > 3.335e-02, 
                                            hold.small.add$CUR.1.2.slope.price.perc, 0)
hold.small.add$slope.1.2.se.4of4 <- ifelse(hold.small.add$CUR.1.2.se.slope.price > 6.549e-02, 
                                            hold.small.add$CUR.1.2.slope.price.perc, 0)

hold.small.add$last.resids.1.2.se.2of4 <- ifelse(hold.small.add$CUR.1.2.se.slope.price > 1.652e-02, 
                                            hold.small.add$CUR.1.2.last.resids.price, 0)
hold.small.add$last.resids.1.2.se.3of4 <- ifelse(hold.small.add$CUR.1.2.se.slope.price > 3.335e-02, 
                                            hold.small.add$CUR.1.2.last.resids.price, 0)
hold.small.add$last.resids.1.2.se.4of4 <- ifelse(hold.small.add$CUR.1.2.se.slope.price > 6.549e-02, 
                                            hold.small.add$CUR.1.2.last.resids.price, 0)
hold.small.add <- hold.small.add[ complete.cases(hold.small.add), ]
hold.small.x <- hold.small.add[ , c(-1, -2, -14) ]
hold.small.y <- hold.small.add[ , 1 ]
hold.small.yN <- hold.small.add[ , 2 ]

hold.small2.add <- hold.small2[ , c("FUT.slope.short.perc", "FUT.N.short",
"CUR.1.2.slope.price.perc",
"CUR.1.2.se.slope.price", 
"CUR.1.2.last.resids.price", 
"CUR.1.2.r2.price", 
"CUR.11.20.slope.perc", 
"CUR.11.20.se",
"CUR.11.20.last.resids", 
"CUR.11.20.r2", 
"CUR.vol.ratio1", 
"CUR.vol.ratio2",
"CUR.vol.ratio3",
"yearfac")]
hold.small2.add$slope.1.2.r2.2of4 <- ifelse(hold.small2.add$CUR.1.2.r2.price > 0.06904, 
                                            hold.small2.add$CUR.1.2.slope.price.perc, 0)
hold.small2.add$slope.1.2.r2.3of4 <- ifelse(hold.small2.add$CUR.1.2.r2.price > 0.24160, 
                                            hold.small2.add$CUR.1.2.slope.price.perc, 0)
hold.small2.add$slope.1.2.r2.4of4 <- ifelse(hold.small2.add$CUR.1.2.r2.price > 0.47850, 
                                            hold.small2.add$CUR.1.2.slope.price.perc, 0)

hold.small2.add$last.resids.1.2.r2.2of4 <- ifelse(hold.small2.add$CUR.1.2.r2.price > 0.06904, 
                                            hold.small2.add$CUR.1.2.last.resids.price, 0)
hold.small2.add$last.resids.1.2.r2.3of4 <- ifelse(hold.small2.add$CUR.1.2.r2.price > 0.24160, 
                                            hold.small2.add$CUR.1.2.last.resids.price, 0)
hold.small2.add$last.resids.1.2.r2.4of4 <- ifelse(hold.small2.add$CUR.1.2.r2.price > 0.47850, 
                                            hold.small2.add$CUR.1.2.last.resids.price, 0)

hold.small2.add$slope.1.2.se.2of4 <- ifelse(hold.small2.add$CUR.1.2.se.slope.price > 1.652e-02, 
                                            hold.small2.add$CUR.1.2.slope.price.perc, 0)
hold.small2.add$slope.1.2.se.3of4 <- ifelse(hold.small2.add$CUR.1.2.se.slope.price > 3.335e-02, 
                                            hold.small2.add$CUR.1.2.slope.price.perc, 0)
hold.small2.add$slope.1.2.se.4of4 <- ifelse(hold.small2.add$CUR.1.2.se.slope.price > 6.549e-02, 
                                            hold.small2.add$CUR.1.2.slope.price.perc, 0)

hold.small2.add$last.resids.1.2.se.2of4 <- ifelse(hold.small2.add$CUR.1.2.se.slope.price > 1.652e-02, 
                                            hold.small2.add$CUR.1.2.last.resids.price, 0)
hold.small2.add$last.resids.1.2.se.3of4 <- ifelse(hold.small2.add$CUR.1.2.se.slope.price > 3.335e-02, 
                                            hold.small2.add$CUR.1.2.last.resids.price, 0)
hold.small2.add$last.resids.1.2.se.4of4 <- ifelse(hold.small2.add$CUR.1.2.se.slope.price > 6.549e-02, 
                                            hold.small2.add$CUR.1.2.last.resids.price, 0)
hold.small2.add <- hold.small2.add[ complete.cases(hold.small2.add), ]
hold.small2.x <- hold.small2.add[ , c(-1, -2, -14) ]
hold.small2.y <- hold.small2.add[ , 1 ]
hold.small2.yN <- hold.small2.add[ , 2 ]

predicted1 <- predict(slm, model.small.x, na.action=na.exclude) 
predictedN <- predict(slmN, model.small.x, na.action=na.exclude) 
model.small.x$predicted1 <- as.vector(predicted1)
model.small.x$predictedN <- as.vector(predictedN)
rm(predicted1, predictedN)

predicted1 <- predict(slm, hold.small.x, na.action=na.exclude)  
predictedN <- predict(slmN, hold.small.x, na.action=na.exclude)  
hold.small.x$predicted1 <- as.vector(predicted1)
hold.small.x$predictedN <- as.vector(predictedN)
rm(predicted1, predictedN)

predicted1 <- predict(slm, hold.small2.x, na.action=na.exclude)  
predictedN <- predict(slmN, hold.small2.x, na.action=na.exclude)  
hold.small2.x$predicted1 <- as.vector(predicted1)
hold.small2.x$predictedN <- as.vector(predictedN)
rm(predicted1, predictedN)

model.small.x$yearfac <- model.small.add$yearfac
hold.small.x$yearfac <- hold.small.add$yearfac
hold.small2.x$yearfac <- hold.small2.add$yearfac

model.small.add <- cbind(model.small.y, model.small.x)
hold.small.add <- cbind(hold.small.y, hold.small.x)
hold.small2.add <- cbind(hold.small2.y, hold.small2.x)
names(model.small.add)[1] <- "FUT.slope.short.perc"
names(hold.small.add)[1] <- "FUT.slope.short.perc"
names(hold.small2.add)[1] <- "FUT.slope.short.perc"

model.small.add <- cbind(model.small.yN, model.small.add)
hold.small.add <- cbind(hold.small.yN, hold.small.add)
hold.small2.add <- cbind(hold.small2.yN, hold.small2.add)
names(model.small.add)[1] <- "FUT.N.short"
names(hold.small.add)[1] <- "FUT.N.short"
names(hold.small2.add)[1] <- "FUT.N.short"


# ELIMINATING THE ORIGINAL DATA WHICH CAN BE RELOADED IN ORDER TO NOT HAVE TWO VERSIONS OF 
#  ALL THE CODE BELOW.
#save(hold.small, file="hold.small.Rdata")
#save(hold.small2, file="hold.small2.Rdata")
#save(model.small, file="model.small.Rdata")

model.small <- model.small.add
hold.small <- hold.small.add
hold.small2 <- hold.small2.add

# R2 CHECK
summary(lm(formula=(FUT.slope.short.perc ~ -1 + predicted1 ), data=model.small, na.action=na.exclude))
summary(lm(formula=(FUT.slope.short.perc ~ -1 + predicted1 ), data=hold.small, na.action=na.exclude))
summary(lm(formula=(FUT.slope.short.perc ~ -1 + predicted1 ), data=hold.small2, na.action=na.exclude))

summary(lm(formula=(FUT.N.short ~ -1 + predictedN ), data=model.small, na.action=na.exclude))
summary(lm(formula=(FUT.N.short ~ -1 + predictedN ), data=hold.small, na.action=na.exclude))
summary(lm(formula=(FUT.N.short ~ -1 + predictedN ), data=hold.small2, na.action=na.exclude))



model.small$set <- "model" 
hold.small$set <- "hold"
hold.small2$set <- "hold2"
all.small <- rbind(model.small, hold.small, hold.small2)
all.small <- all.small[ !is.na(all.small$predicted1), ]
all.small <- all.small[ !is.na(all.small$FUT.slope.short.perc), ]
all.small <- all.small[ !is.na(all.small$set), ]
all.small <- all.small[ !is.na(all.small$FUT.N.short), ]

graph8by.cat(var1="predicted1", dep="FUT.slope.short.perc", 
          cat1="set" , data=all.small)
graph8by.cat(var1="predicted1", dep="FUT.slope.short.perc", 
          cat1="yearfac" , data=model.small)
graph8by.cat(var1="predicted1", dep="FUT.slope.short.perc", 
          cat1="yearfac" , data=hold.small)
graph8by.cat(var1="predicted1", dep="FUT.slope.short.perc", 
          cat1="yearfac" , data=hold.small2)

graph8by.cat(var1="predictedN", dep="FUT.N.short", 
          cat1="set" , data=all.small)



rm(all.small)
rm(hold.small, slm)
rm(model.small, model.small.add, model.small.x, model.small.y, hold.small.x, hold.small.y, 
   hold.small.add, hold.small2.x, hold.small2.y, hold.small2.add)

