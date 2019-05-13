stepSGB <-
function (obj0, d, u, weight = rep(1, dim(d)[1]), shape10 = obj0[["par"]][1], 
    bound = 2.1, shape1 = NULL, Mean2 = TRUE, maxiter = 10, control.optim = list(fnscale = -1), 
    control.outer = list(itmax = 1000, ilack.max = 200, trace = TRUE, 
        kkt2.check = TRUE, method = "BFGS")) 
{
    if (NROW(u) != NROW(d)) 
        stop("Different number of rows in u and d")
    D1 <- NCOL(u) - 1
    Formula <- obj0[["Formula"]]
    if (!is.null(Formula)) {
        F1 <- formula(Formula, lhs = 0)
        mt <- terms(F1, data = d)
        if (any(rownames(attr(mt, "factors"))[1:D1] != colnames(obj0[["V"]]))) {
            warnings("Variable names of the right hand side of Formula do not match column names of matrix V")
        }
        mf <- model.frame(formula = F1, data = d)
        d1 <- model.matrix(attr(mf, "terms"), data = mf)
    }
    else d1 <- d
    object <- stepSGB.default(obj0, d1, as.matrix(u), weight = weight, 
        shape10 = shape10, bound = bound, shape1 = shape1, Mean2 = Mean2, 
        maxiter = maxiter, control.optim = control.optim, control.outer = control.outer)
    object$call <- match.call()
    object$Formula <- eval(Formula)
    return(object)
}

