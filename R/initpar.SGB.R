# initpar.SGB <-
# function(d,u,V, shape1=1, Mean2=TRUE){
#   lrt <- as.matrix(log(u))%*%as.matrix(V)
#   N <- dim(u)[1]
#   D <- dim(u)[2] 
#   D1 <- D-1
#   r <- NCOL(d)
#   R <- D1*r
# 
# # Initial a from scaled Dirichlet                 
# # Initial scale parameters bi, i=1,...N by linear regression
#   coefi <- lm.fit(d,lrt)$coefficients
#   nco <- rownames(coefi)
#   nco2 <- expand.grid(1:D1,nco)
#   ncovec <- paste(nco2$Var2,nco2$Var1,sep=".")
#   coefab <- c(shape1,as.vector(t(coefi))) 
#   names(coefab) <- c("shape1",ncovec)
# # Initial pl, l=1,...,D from Ng
#   shape2 <- compushape2(shape1,coefi,d,u,V)
#   
# # Initial pl, l=1,...,D: mean(shape20), so that initially E_A(U)=b0
#   if (Mean2) shape2 <- rep(mean(shape2),length(shape2))
#   names(shape2) <- paste("shape2",1:D,sep=".")
#   
#   x0 <- c(coefab,shape2)
#   return(x0)
# }
# 
# initpar_old.SGB <-
# function(d,u,V, shape1=1, Mean2=TRUE){
#   lrt <- as.matrix(log(u))%*%as.matrix(V)
#   N <- dim(u)[1]
#   D <- dim(u)[2] 
#   D1 <- D-1
#   r <- NCOL(d)
#   R <- D1*r
# 
# # Initial a from scaled Dirichlet                 
# # Initial scale parameters bi, i=1,...N by linear regression
#   dnam <- colnames(d)
#   fmla <- as.formula(paste(colnames(lrt)[1], " ~ ", paste(dnam, collapse= " + ")))
# 
#   coefi <- lm(fmla, data= data.frame(lrt,d))$coefficients
#   nco <- names(coefi)
#   nco[1] <- "Intercept"
# 
#   for (i in 2:D1){
#     fmla <- as.formula(paste(colnames(lrt)[i], " ~ ", paste(dnam, collapse= " + ")))
#     coefii <- lm(fmla, data= data.frame(lrt,d))$coefficients 
#     coefi <- rbind(coefi,coefii) # initial beta for ith lrt
#   } 
# 
#   nco2 <- expand.grid(1:D1,nco)
#   ncovec <- paste(nco2$Var2,nco2$Var1,sep=".")
#   coefab <- c(shape1,as.vector(coefi) )
#   names(coefab) <- c("shape1",ncovec)
# 
# # Initial pl, l=1,...,D from Ng
#   shape2 <- compushape2(shape1,coefi,d,u,V)
#   
# # Initial pl, l=1,...,D: mean(shape20), so that initially E_A(U)=b0
#   if (Mean2) shape2 <- rep(mean(shape2),length(shape2))
#   names(shape2) <- paste("shape2",1:D,sep=".")
#   
#   x0 <- c(coefab,shape2)
#   return(x0)
# }

initpar.SGB <-
function (d, u, V, weight=rep(1,dim(u)[1]),shape1 = 1, Mean2 = TRUE) 
{
    lrt <- as.matrix(log(u)) %*% as.matrix(V)
    N <- dim(u)[1]
    D <- dim(u)[2]
    D1 <- D - 1
    r <- NCOL(d)
    R <- D1 * r
    coefi <- lm.wfit(d, lrt,weight)$coefficients
    nco <- rownames(coefi)
    if(D==2) nco <- names(coefi)
    nco2 <- expand.grid(1:D1, nco)
    ncovec <- paste(nco2$Var2, colnames(V)[nco2$Var1], sep = ".")
    if (is.null(colnames(V)))  ncovec <- paste(nco2$Var2, nco2$Var1, sep = ".")
    coefab <- c(shape1, as.vector(t(coefi)))
    names(coefab) <- c("shape1", ncovec)
    shape2 <- compushape2(shape1, t(coefi), d, u, V)
    if (Mean2) 
        shape2 <- rep(mean(shape2), length(shape2))
    names(shape2) <- paste("shape2", colnames(u), sep = ".")
    if (is.null(colnames(u))) names(shape2) <- paste("shape2", 1:D, sep = ".")
    x0 <- c(coefab, shape2)
    return(x0)
}
