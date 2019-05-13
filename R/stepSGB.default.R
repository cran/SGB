stepSGB.default <-
function (obj0, d, u, weight = rep(1, dim(d)[1]), shape10 = 1, 
    bound = 2.1, shape1 = NULL, Mean2 = TRUE, maxiter = 10, control.optim = list(trace = 0, fnscale = -1), 
    control.outer = list(itmax = 1000, ilack.max = 200, 
        trace = TRUE, kkt2.check = TRUE, method = "BFGS")) 
{
    if (any(is.na(u)) | any(is.na(d))) 
        stop("Missing values u or d are not allowed")
    if (NROW(u) != NROW(d)) 
        stop("Different number of rows in u and d")
    pmax <- ncol(u) * ncol(d)
    obj <- list()
    tab <- table.regSGB(obj0)
    obj[["full"]] <- obj0
    AIC0 <- tab["AIC", 1]
    print(paste("AIC =", AIC0), quote = FALSE)
    pval <- obj0[["summary"]][["p.value"]]
    lp <- length(pval)
    nc <- ncol(u)
    np <- lp - nc
    AIC1 <- AIC0
    ind <- order(pval[2:np], decreasing = TRUE) + 1
    if (!is.null(shape1)) 
        ind <- c(1, ind)
    iter <- 0
    while ((AIC1 <= AIC0) & (iter < maxiter)) {
        iter <- iter + 1
        AIC0 <- AIC1
        indi <- ind[1:iter]
        print("indices of fixed parameters:", quote = FALSE)
        print(indi)

        obj1 <- regSGB.default(d, as.matrix(u), obj0[["V"]], 
                weight, shape10 = shape10, bound = bound, ind = indi, 
                shape1 = shape1, Mean2 = Mean2, 
                control.optim = control.optim, 
                control.outer = control.outer)
        
        obj1[["Formula"]] <- obj0[["Formula"]]
        tab1 <- table.regSGB(obj1)
        tab <- cbind(tab, tab1)
        AIC1 <- tab1["AIC", ]
        print(paste("AIC =", AIC1), quote = FALSE)
        obj[[paste("iter", iter, sep = "")]] <- obj1
    }
    
    if (iter == maxiter) 
        warning("Maximum number of iterations exceeded")
    names(tab) <- c("full", paste("iter", 1:iter, sep = ""))
    object <- list()
    object$reg <- obj
    object$iter <- iter
    object$tab <- tab
    object$call <- match.call()
    class(object) <- "stepSGB"
    return(object)
}

