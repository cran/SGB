coefmat <- function(object, digits = 3){
Form <- object[["Formula"]]
rn <- colnames(object[["V"]])
cn <- attr(terms.formula(Form),"term.labels")
if (attr(terms.formula(Form),"intercept") == 1) cn <- c("(Intercept)", cn)

nreg <- length(object[["par"]])- length(rn)-1
est <- object[["par"]][2:nreg]
star <- object[["summary"]][["signif"]][2:nreg]
resu <- paste(format(round(est,digits), nsmall = digits), star)
rema <- matrix(resu,nrow=length(rn))
row.names(rema) <- rn
colnames(rema) <- cn
t(rema)
}
