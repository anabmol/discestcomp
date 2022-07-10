th <- function(x, y = NULL, test = "ztest", h0, alternative = "two.sided", alpha = 0.05,
               exact = TRUE, correct = FALSE, paired = FALSE, plot = FALSE, ...) {
  argaddit <- list(...)
  if (missing(x)) {
    xfile <- file.choose(new = TRUE)
    x <- read.table(xfile, h = TRUE)
  }
  # Z test:
  if (test == "ztest") {
    if (!any(names(argaddit) == "sd")) {
      sdev <- readline("Insert the value of standard deviation population? ")
      sdev <- as.numeric(sdev)
    } else sdev <- argaddit$sd
    if (missing(h0)) {
      h0 <- readline("Insert the value of null hypothesis? ")
      h0 <- as.numeric(h0)
    }
    if (any(alternative == c("two.sided", "t", "T"))) {
      if (is.null(y)) {
        n <- length(x)
        ztest <- (mean(x) - h0) / (sdev /sqrt(n))
        ztab <- qnorm(1 - alpha)
        ztab <- c(-ztab, ztab)
        cat("    Z teste bilateral   \n")
        cat("==============\n")
        cat(paste("O valor do teste eh: ", ztest), "\n")
        cat("\n ==============\n")
        
        resultado <- list("Z_teste" = ztest, "Ponto_Critico (+-)" = ztab)
        print(resultado)
        
        cat(paste("Valor-p: ", 2 * pnorm(abs(ztest), lower.tail = FALSE)), "\n")
        
        cat("\n ==============\n")
        
        if (abs(ztest) >= ztab[2]) {
          cat("\n ==============")
          cat("\n Rejeita-se H0!")
        } else {
          cat("\n ==============")
          cat("\n Não se rejeita H0!")
        }
        
      }
    }
    
    if (any(alternative == c("less", "l", "L"))) {
      print("less")
    }
    
    if (any(alternative == c("greater", "g", "G"))) {
      print("greater")
    }
    #Área de plotagem
    ztab <- qnorm(1 - alpha)
    ztab <- c(-ztab, ztab)
    
    #Área de plotagem
    plot.new()
    plot.window(ztab,ztab)
    
    #Eixos
    axis(1)
    axis(2)
    
    #Título e labels
    title()
    
    
    #Linhas e pontos da região desejada
    lines (ztest, ztab, type = "h", panel.first = grid(), lwd = 2, col = "green")
    points(ztest,ztab, lwd = 2, col = "red", pch = 10)
    {
      if (gui == "plot" ) {
        # Probability
        sd <- argaddit$sd
        test <- th(ztest = ztest , sd = sd)
        # Plot
        plotcurve(ztest, sd)
    {
      if (gui == "rstudio") {
        sd <- argaddit$sd
        test <- th(ztest = ztest, sd = sd)
        manipulate::manipulate(plotcurve(ztest, sd),
                               x = manipulate::slider(1-alpha),
                              sd = manipulate::slider(-ztab, ztab))
          
    }
    }
  
  }
}

