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
