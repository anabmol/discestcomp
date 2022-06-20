
# Distribuição Poisson

p <- function(q, dist = "poisson", lower.tail = TRUE, rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  argaddit <- list(...)
  argdef <- formals(p)
  if (dist == "poisson") {
    if (!any(names(argaddit) == "lambda")) stop("Insira o argumento 'lambda'!", call. = FALSE)
    if (lower.tail) {
      plotcurve <- function(q, lambda) {
        rmin <- lambda - 4 * sqrt(lambda)
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
        rmax <- ceiling(lambda + 4 * sqrt (lambda))
        x <- rmin:rmax
        x1 <- rmin:q
        x2 <- (q + 1):rmax
        probx <- dpois(x, lambda = lambda)
        probx1 <- dpois(x1, lambda = lambda)
        probx2 <- dpois(x2, lambda = lambda)

        #Área de plotagem
        xlim <- c(rmin, rmax)
        ylim <- c(min(probx), max(probx) + 0.2)

        #Área de plotagem
        plot.new()
        plot.window(xlim, ylim)

        #Eixos
        axis(1)
        axis(2)

        #Título e labels
        title(ylab = expression(p[X](x)), xlab="X")


        #Linhas e pontos da região desejada
        lines (x1, probx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
        points(x1, probx1, lwd = 2, col = "red", pch = 19)

        lines (x2, probx2, type = "h", lwd = 2)
        points(x2, probx2, lwd = 2, pch = 19)


        abline(v=lambda, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(ppois(qq, lambda = lambda, lower.tail = T), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(X<=q)==Pr~"\n\n"~lambda==lambd, list(q=qq, Pr=Pr, lambd = lambda)))
      }

      if (gui == "plot" ) {
        # Probability
        lambda <- argaddit$lambda
        prob <- ppois(q = q, lambda = lambda)
        # Plot
        plotcurve(q, lambda)
      }
      if (gui == "rstudio") {
        lambda <- argaddit$lambda
        prob <- ppois(q = q, lambda = lambda)
        manipulate::manipulate(plotcurve(q, lambda),
                               q = manipulate::slider(0, q+30, q),
                               lambda = manipulate::slider(lambda, lambda + 200, lambda))
      }

    } else{
      plotcurve <- function(q, lambda) {
        rmin <- lambda - 4 * sqrt(lambda)
        if (rmin < 0) rmin <- 0 else rmin <- round(rmin)
        rmax <- ceiling(lambda + 4 * sqrt (lambda))
        x <- rmin:rmax
        x1 <- rmin:q
        x2 <- (q + 1):rmax
        probx <- dpois(x, lambda = lambda)
        probx1 <- dpois(x1, lambda = lambda)
        probx2 <- dpois(x2, lambda = lambda)

        #Área de plotagem
        xlim <- c(rmin, rmax)
        ylim <- c(min(probx), max(probx) + 0.2)

        #Área de plotagem
        plot.new()
        plot.window(xlim, ylim)

        #Eixos
        axis(1)
        axis(2)

        #Título e labels
        title(ylab = expression(p[X](x)), xlab="X")


        #Linhas e pontos da região desejada
        lines (x1, probx1, type = "h", panel.first = grid(), lwd = 2, col = "red")
        points(x1, probx1, lwd = 2, col = "red", pch = 19)

        lines (x2, probx2, type = "h", lwd = 2)
        points(x2, probx2, lwd = 2, pch = 19)


        abline(v=lambda, lty=2)
        qq <- round(q, digits=2)
        qqaux <-round(q, digits=2)
        Pr <- round(ppois(qq, lambda = lambda, lower.tail = T), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(X<=q)==Pr~"\n\n"~lambda==lambd, list(q=qq, Pr=Pr, lambd = lambda)))
      }
      if (gui == "plot") {
        # Probability
        lambda <- argaddit$lambda
        prob <- ppois(q = q, lambda = lambda)
        # Plot
        plotcurve(q,lambda = lambda)
      }
      if (gui == "rstudio") {
        lambda <- argaddit$lambda
        prob <- ppois(q = q, lambda = lambda)
        manipulate::manipulate(plotcurve(q, lambda),
                               q = manipulate::slider(q, q+30, q),
                               lambda = manipulate::slider(lambda, lambda + 200, lambda))
      }
    }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
  }
}
