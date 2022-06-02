


# Distribuição Poisson 

p <- function(q, dist = "poisson", lower.tail = TRUE, rounding = 4, 
              porcentage = FALSE, gui = "plot", ...) {
  
  argaddit <- list(...)
  argdef <- formals(p)
  if (dist == "poisson") {
    
    if (!any(names(argaddit) == "lambda")) stop("Insira o argumento 'lambda'!", call. = FALSE)
    
    
    if (lower.tail) {
      plotcurve <- function(q, lambda) {
        curve(dpois(x, lambda = lambda), -6, 6, ylab = expression(f[T](t)), xlab="T")
        x <- seq(-6, q, by = 0.01)
        y <- seq(q, 6, by = 0.01)
        fx <- dpois(x, lambda = lambda)
        fy <- dpois(y, lambda = lambda)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(ppois(qq,  lambda = lambda, lower.tail = T), digits=rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(T<=q)==Pr~"\n\n"~lambda==lambda, list(q=qq, Pr=Pr, lambda = lambda)))
      }
      if (gui == "plot" ) {
        # Probability
        mu <- argaddit$mean
        sigma <- argaddit$sd
        prob <- ppois(q = q, lambda = lambda)
        # Plot
        plotcurve(q, lambda)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotcurve(qaux, muaux),
                               qaux = manipulate::slider(-6, 6, q),
                               muaux = manipulate::slider(lambda, lambda + 200, lambda))
      }
      
    } else{
      plotcurve <- function(q, lambda) {
        curve(dpois(x, lambda = lambda), -6, 6, ylab = expression(f[T](t)), xlab="T")
        x <- seq(q, 6, by=0.01)
        y <- seq(-6, q, by=0.01)
        fx <- dpois(x, lambda = lambda)
        fy <- dpois(y, lambda = lambda)
        polygon(c(x, rev(x)),
                c(fx, rep(0, length(fx))),
                col="red")
        polygon(c(y, rev(y)),
                c(fy, rep(0, length(fy))),
                col="gray90")
        abline(v=0, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(ppois(qq, lambda = lambda, lower.tail = F), digits=rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(T~`>`~q)==Pr~"\n\n"~lambda=lambda, list(q=qq, Pr=Pr, lambda = lambda)))
      }
      if (gui == "plot") {
        # Probability
        lambda <- argaddit$lambda
        lambda <- argaddit$lambda
        prob <- ppois(q = q, lambda = lambda)
        # Plot
        plotcurve(q,lambda = lambda)
      }
      if (gui == "rstudio") {
        manipulate::manipulate(plotcurve(qaux, lambdaaux),
                               qaux = manipulate::slider(-6, 6, q),
                               muaux = manipulate::slider(lambda, lambda + 200, lambda))
      }
      
    }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}
