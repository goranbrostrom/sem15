kolla <- function(){
    ## This chrashes R (3.2.0) !
   library(eha)
   mort$Y <- with(mort, Surv(enter, exit, event))
   fit <- coxreg(Y ~ ses * I(birthdate - 1810), x = TRUE,
                 data = mort)
   X <- fit$x
   mort$s <- X[, 1]
   mort$bd <- X[, 2]
   mort$ia <- X[, 3]
   fit <- coxreg(Y ~ s + bd + ia, data = mort)
   dr <- drop1(fit, test = "Chisq")
   return(dr)
   beta <- seq(-56, 1, length = 100)
   ll <- numeric(100)
   for (i in 1:100){
       cat("i = ", i, "\n")
       offs <- beta[i] * X[, 1]
       ll[i] <- weibreg(Y ~ bd + ia + offset(offs), data = mort)$loglik[2]
   }
   plot(beta, ll, type = "l")
   abline(v = fit$coef[1], lty = 3)
   cbind(beta, ll)
}
