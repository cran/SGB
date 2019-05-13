regSGB.formula <-
function(Formula, data= list(),  weight=rep(1,dim(d)[1]), 
      shape10 = 1, bound = 2.1, ind = NULL, shape1 = 1, Mean2=TRUE, control.optim = list(trace=0,fnscale=-1), 
      control.outer = list(itmax=1000, ilack.max=200, trace=TRUE, kkt2.check =TRUE, method = "BFGS"),...){
         
   if (length(data)!= 3) stop('data should be a list of 3 matrices')   
   d <- data[[1]]
   u <- as.matrix(data[[2]])
   V <- data[[3]]
   if (NROW(u) != NROW(d)) stop('Different number of rows in u and d')
   mT <- terms(Formula, data = d)
   D1 <- NCOL(u)-1
   lrtnames <- rownames(attr(mT, "factors"))[1:D1]
   if (any(lrtnames != colnames(V)) ){
      stop("Variable names of the left hand side of Formula do not match column names of matrix V")
      }
      
   F1 <- formula(Formula,lhs=0)
   mt <- terms(F1, data = d)

   mf <- model.frame(formula=F1, data=d)
   d1 <- model.matrix(attr(mf, "terms"), data=mf)
   object <- regSGB.default(d1, u, V,  weight=weight, shape10=shape10, bound=bound, ind=ind, Mean2=Mean2, shape1=shape1,
       control.optim=control.optim,
       control.outer=control.outer)
   object$call <- match.call()
   object$Formula <- eval(Formula)
   return(object)
   
  }
