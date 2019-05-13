B2i <- function(bal,balnames=FALSE){

D <- dim(bal)[2]
D1 <- D-1

b2i <-
function(x){  
  lp <- length(x[x==1])
  lm <- length(x[x==-1])
  ilr <- ( x*(x==1)/lp + x*(x==-1)/lm )*sqrt(lp*lm/(lp+lm))
  return(ilr)
}

## compute the contrast matrix (normalized to ilr)
V <- b2i(bal[1,1:D])
for (i in 2:D1){
  x <-  b2i(bal[i,1:D])
  V <- rbind(V,x)
}  
V <- as.matrix(V)

logic <- (is.null(rownames(bal)) | !balnames)
if(logic) rownames(V) <- paste("ilr",1:D1,sep="")
else rownames(V) <- rownames(bal)
colnames(V) <- colnames(bal)

## matrix to transform the log compositions to ilr
Vc <- t(V) 
return(Vc)
}
