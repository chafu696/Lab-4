#'This is a description of linreg function
#
#'@title linreg
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
#'@example 
#'
#'@export


linreg <- function(formula, data){
  y1 <- all.vars(formula)[1]
  dy1 <- data[[y1]]
  mat2 <- model.matrix(formula, data)
  reco <- solve(t(mat2) %*% mat2) %*% t(mat2) %*% dy1
  fval <- mat2 %*% reco
  resi <- dy1 - fval
  dfre <- length(dy1) - length(all.vars(formula))
  reva <- (t(resi) %*% resi)[1,1] / dfre
  vreco <- reva * solve(t(mat2) %*% mat2)
  vreco1 <- diag(vreco)
  tcoe <- reco / sqrt(vreco1)
  pcoe <- pt(tcoe, dfre)
  rtex <- paste(deparse(match.call()))
  v <- list(a1 = round(t(reco), 2), a2 = round(fval, 2), a3 = round(resi, 2), a4 = round(reva, 2), a5 = round(vreco, 2), a6 = round(vreco1, 2), a7 = t(tcoe), a8 = t(pcoe), a9 = rtex)
attr(v, "class") <- "linreg"
  return(v)
}

linlist <- linreg(Petal.Length ~ Species, data = iris)
print.linreg <- function(x){
  coeff <- linlist$a1
  cat("Call:", "\n")
  cat(linlist$a9, "\n")
  cat("Coefficients:", "\n")
  print(coeff)
}

plot.linreg <- function(x){
  Fitted_value <- linlist$a2
  Residual <- linlist$a3
  gg3 <- data.frame(Fitted_value, Residual)
  plot1 <- ggplot(gg3, aes(x = Fitted_value, y = Residual)) + geom_point(size = 5, shape = 1)+ scale_y_continuous(limits = c(-1.5,1.5), breaks=seq(-1.5,1.5,1)) + labs(x = "Fitted values\n lm(Pental.Length~Species)", y = "Residuals", title = "Residuals vs Fitted") + theme(plot.title = element_text(hjust = 0.5))+stat_summary(fun = median, color = "red", geom = "line", size=1)
}