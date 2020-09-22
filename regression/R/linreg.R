#'This is a description of linreg function
#
#'@title linreg
#'@name linreg
#'
#'@description This function creates a multiple regression model to handle linear regression.
#'
#'@details Two arguments with a formula and a data are handled by this function.
#'
#'@param formula A formula
#'
#'@param data A matrix
#'
#'@return An object with of class linreg as an S3 class
#'
#'@usage linreg(y ~ x, data)
#'
#'@example
#' linreg(Petal.Length ~ Species, data = iris)
#'
#'@import ggplot2
#'
#'@export


linreg <- function(formula, data){
  y1 <- all.vars(formula)[1]
  dy1 <- data[[y1]]
  mat2 <- model.matrix(formula, data)
  reco <- solve(t(mat2) %*% mat2) %*% t(mat2) %*% dy1
  fval <- mat2 %*% reco
  resi <- dy1 - fval
  resi<-as.vector(resi)
  dfre <- length(dy1) - length(all.vars(formula))
  reva <- (t(resi) %*% resi)[1,1] / dfre
  vreco <- reva * solve(t(mat2) %*% mat2)
  vreco1 <- diag(vreco)
  tcoe <- reco / sqrt(vreco1)
  pcoe <- pt(tcoe, dfre)
  rtex <- paste(deparse(match.call()))
  v <- list(a1 = t(reco), a2 = fval, a3 = resi, a4 =reva, a5 = vreco, a6 = vreco1, a7 = t(tcoe), a8 = t(pcoe), a9 = rtex, a10 = dfre)
attr(v, "class") <- "linreg"
  return(v)
}

linlist <- linreg(Petal.Length ~ Species, data = iris)

print.linreg <- function(x){
  coeff <- x$a1
  cat("Call:", "\n")
  cat(x$a9, "\n")
  cat("Coefficients:", "\n")
  print(coeff)
}

plot.linreg <- function(x){
  Fitted_value <- x$a2
  Residual <- x$a3
  Resivar <- sqrt(abs(Residual / sd(Residual)))
  gg3 <- data.frame(Fitted_value, Residual)
  gg4 <- data.frame(Fitted_value, Resivar)
  plot1 <- ggplot(gg3,aes(x = Fitted_value, y = Residual)) +
    geom_point(aes(x = Fitted_value, y = Residual), size = 5, shape = 1) +
    labs(x = "Fitted values\n lm(Petal.Length~Species)", y = "Residuals", title = "Residuals vs Fitted") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_summary(fun = median, color = "red", geom = "line", size=1)
    print(plot1)

    plot2 <- ggplot(gg4, aes(x = Fitted_value, y = Resivar)) +
    geom_point(size = 5, shape = 1) +
    labs(x = "Fitted values\n lm(Pental.Length~Species)", y = expression(sqrt("|Standardized residuals|")) ,    title = "Scale-Location") +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_summary(fun = mean, color = "red", geom = "line", size=1)
    print(plot2)
}
 resid <- function(x){
   UseMethod("resid")
}
resid.linreg <- function(x){
  cat(x$a3, "\n")
}
pred <- function(x){
  UseMethod("pred")
}
pred.linreg <- function(x){
  cat(x$a2, "\n")
}
coef <- function(x){
  UseMethod("coef")
}
coef.linreg <- function(x){
  cat(x$a1, "\n")
}
summary <- function(x){
  UseMethod("summary")
}
summary.linreg <- function(x){
  emotvect <- c("","","")
  mat1 <- (cbind(t(x$a1), sqrt(x$a6), t(x$a7), t(x$a8), emotvect))
  print.table(mat1)
  cat(sqrt(x$a4), "\n")
  cat('on')
  cat(x$a10, "\n")
}
