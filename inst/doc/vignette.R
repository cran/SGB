## ------------------------------------------------------------------------
library(SGB)
data(carseg)
## Extract the compositions
uc <- as.matrix(carseg[,(1:5)])
## Define the log-ratio transformation matrix
Vc <- matrix(c( 1,0,0,0,
               -1,1,0,0,
               0,-1,1,0,
               0,0,-1,1,
               0,0,0,-1),ncol=4,byrow=TRUE)
colnames(Vc) <- c("AB","BC","CD","DE")
rownames(Vc) <- colnames(uc)
print(Vc)

## ------------------------------------------------------------------------
## Fit a model
Form1 <- Formula(AB | BC | CD | DE ~  log(expend) + I(PAC*log(expend)) +
	 log(sent) + log(FBCF) + log(price) + rates)
obj1 <- regSGB(Form1, data=list(carseg, uc, Vc), shape10 = 4.4, 
               control.outer = list(trace=FALSE))

## ------------------------------------------------------------------------
obj1

## ------------------------------------------------------------------------
## Results
summary(obj1)

## ------------------------------------------------------------------------
## Overall results
t1 <- table.regSGB(obj1)
print(t1)

## ------------------------------------------------------------------------
## Quality of fit
par(mfrow=c(3,2))
hzbeta(uc,obj1)
mtext("Marginal distribution of the z-transform of parts, model obj1",
	 line=-1,outer=TRUE)

## ------------------------------------------------------------------------
## Influence of log-ratio transformation
# setup a matrix Vilr with orthonormal columns
Vilr <- contr.helmert(5)
Vilr <- Vilr%*%diag(1/sqrt(diag(crossprod(Vilr))))
colnames(Vilr) <- c("A.B","AB.C","A_C.D","A_D.E")
rownames(Vilr) <- colnames(uc)
print(Vilr)

## ------------------------------------------------------------------------
Form2 <- Formula(A.B | AB.C | A_C.D | A_D.E ~  log(expend) + 
	I(PAC*log(expend)) + log(sent) + log(FBCF) + log(price) + rates)
obj2 <-  regSGB(Form2, data = list(carseg, uc, Vilr), shape10 = 4.4,
                control.outer = list(trace=FALSE))
t2 <- table.regSGB(obj2)
## Comparison
# overall statistics
cbind(t1,t2)

## ------------------------------------------------------------------------
# fitted parts
range(obj1[["meanA"]]/obj2[["meanA"]] )

## ------------------------------------------------------------------------
## stepSGB
# First lrt
step1 <- stepSGB(obj1, carseg, uc, bound = 2.1, control.outer = list(trace=FALSE))
round(step1[["tab"]])

## ------------------------------------------------------------------------
summary(step1[["reg"]][["iter4"]])

## ------------------------------------------------------------------------
# Second lrt
step2 <- stepSGB(obj2, carseg, uc, bound = 2.1, control.outer = list(trace=FALSE))
round(step2[["tab"]])

## ------------------------------------------------------------------------
summary(step2[["reg"]][["iter7"]])

## ------------------------------------------------------------------------
## gof  tests
npar <- length(obj1[["par"]])
D <- dim(uc)[2]
np2 <- npar-D+1

# K-S test on obj1
ks1 <- ks.SGB(uc, shape1=obj1[["par"]][1], shape2=obj1[["par"]][np2:npar],
              scale=obj1[["scale"]])[["tests"]][,1:2]
names(ks1) <- c("stat1","pval1")

# K-S test on obj2
ks2 <- ks.SGB(uc, shape1=obj2[["par"]][1], shape2=obj2[["par"]][np2:npar],
              scale=obj2[["scale"]])[["tests"]][,1:2]
names(ks2) <- c("stat2","pval2")

st14 <- step1[["reg"]][["iter4"]]
st27 <- step2[["reg"]][["iter7"]]

# K-S test on st14
ks14 <- ks.SGB(uc, shape1=st14[["par"]][1], shape2=st14[["par"]][np2:npar],
               scale=st14[["scale"]])[["tests"]][,1:2]
names(ks14) <- c("stat14","pval14")

# K-S test on st2
ks27 <- ks.SGB(uc, shape1=st27[["par"]][1], shape2=st27[["par"]][np2:npar],
               scale=st27[["scale"]])[["tests"]][,1:2]
names(ks27) <- c("stat27","pval27")

## Kolmogorov-Smirnov tests
round(cbind(ks1,ks2,ks14,ks27),3)

## ------------------------------------------------------------------------
usup <- carseg[1:3,1:5]
## Introduce some missing values
usup[1,2] <- NA
usup[2,2:3] <- NA
usup[3,] <- NA
usup <- usup/rowSums(usup,na.rm=TRUE)
usup
impute.regSGB(obj1,carseg[1:3,],usup)

## original values
carseg[1:3,1:5]

