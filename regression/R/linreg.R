#'This is a description of linreg function
#'
#'
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
#'@usage linreg(formula, data)
#'
#'@examples linreg(Petal.Length ~ Species, data = iris)
#'
#'@import ggplot2
#'
#'@importFrom stats median model.matrix pt quantile sd
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
  pcoe <- 2*pt(-abs(tcoe), dfre)
  rtex <- match.call()
  v <- list(a1 = t(reco), a2 = fval, a3 = resi, a4 =reva, a5 = vreco, a6 = vreco1, a7 = t(tcoe), a8 = t(pcoe), a9 = rtex, a10 = dfre)
attr(v, "class") <- "linreg"
  return(v)
}

#linlist <- linreg(Petal.Length ~ Species, data = iris)

#'@rdname print
#'@export
print.linreg <- function(x){
  cat("Call:", "\n")
  print(x$a9)
  cat("Coefficients:", "\n")
  print(x$a1)
}

#'@rdname plot
#'@export
plot.linreg <- function(x){
  Fitted_value <- x$a2
  Residual <- x$a3
  Resivar <- sqrt(abs(Residual / sd(Residual)))
  outliers <- function(c) {
    Q1 <- quantile(c, probs=0.25)
    Q3 <- quantile(c, probs=0.75)
    IQR <- Q3-Q1
    index <- ifelse(c > Q3+1.5*IQR, c,
                    ifelse(c < Q1-1.5*IQR, c, NA))
    return(index)
  }
  outliers1 <- outliers(Residual)
  outliers2 <- outliers(Resivar)
  gg3 <- data.frame(Fitted_value, Residual, outliers1)
  gg4 <- data.frame(Fitted_value, Resivar, outliers2)

  plot1 <- ggplot(gg3,aes(x = Fitted_value, y = Residual)) +
    geom_point(aes(x = Fitted_value, y = Residual), size = 5, shape = 1) +
    labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_summary(fun = median, color = "red", geom = "line", size=1) +
    geom_text(aes(label=outliers1), hjust=1, na.rm=TRUE)
  print(plot1)

  plot2 <- ggplot(gg4, aes(x = Fitted_value, y = Resivar)) +
    geom_point(size = 5, shape = 1) +
    labs(x = "Fitted values", y = expression(sqrt("|Standardized residuals|")) ,    title = "Scale-Location") +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_summary(fun = mean, color = "red", geom = "line", size=1) +
    geom_text(aes(label=outliers2), hjust=1, na.rm=TRUE)
  print(plot2)
}

#'@rdname resid
#'@export
resid <- function(x){
  UseMethod("resid")
}
resid.linreg <- function(x){
  print(x$a3)
}

#'@rdname pred
#'@export
pred <- function(x){
  UseMethod("pred")
}
pred.linreg <- function(x){
  print(x$a2)
}
#'@rdname coef
#'@export
coef.linreg <- function(x){
  print(x$a1)
}

summary.linreg <- function(x){
  signif_code <- ifelse(x$a8<=0.001, "***",
                        ifelse(x$a8<=0.01, "**",
                               ifelse(x$a8<=0.1, "*", "")))
  print(data.frame(t(x$a1),sqrt(x$a6),t(x$a7),t(x$a8),t(signif_code)))
  cat("\nResidual standard error:", sqrt(x$a4), "on", x$a10, "degrees of freedom")
}
