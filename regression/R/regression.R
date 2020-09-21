#'This is a description of linreg function
#
#'@title linreg
#'
#'@description This function creates a multiple regression model to handle linear regression.
#'
#'@details Two arguments with a formula and a data are handled by this function.
#'
#'@param formu A formula
#'
#'@param ldata A matrix 
#'
#'@return An object with of class linreg as an RC class
#'
#'@example 
#'
#'@export


linreg <- function(formu, ldata){
  y1 <- all.vars(formu)[1]
  x1 <- all.vars(formu)[2]
  dy1 <- ldata[[y1]]
  dx1 <- ldata[[x1]]
  dafr <- data.frame(y1 = dy1, x1 = dx1)
  names(dafr) <- c(y1, x1)
  dmat <- model.frame(formu, dafr)
  mat1 <- model.matrix(formu, dmat)
  reco <- solve(t(mat1) %*% mat1) %*% t(mat1) %*% dy1
  fval <- mat1 %*% reco
  resi <- dy1 - fval
  dfre <- length(dy1) - length(all.vars(formu))
  reva <- (t(resi) %*% resi)[1,1] / dfre
  vreco <- reva * solve(t(mat1) %*% mat1)
  vreco1 <- diag(vreco)
  tcoe <- reco / sqrt(vreco1)
  pcoe <- pt(tcoe, dfre)
  rtex <- paste("linreg(formula =", y1, "~", x1, "ldata = iris)" )
  v <- list(a1 = round(t(reco), 2), a2 = round(fval, 2), a3 = round(resi, 2), a4 = round(reva, 2), a5 = round(vreco, 2), a6 = round(vreco1, 2), a7 = t(tcoe), a8 = t(pcoe), a9 = rtex)
attr(v, "class") <- "linreg"
  return(v)
}
linlist <- linreg(Petal.Length ~ Species, ldata = iris)
print.linreg <- function(x){
  coeff <- linlist$a1
  cat("\n")
  cat("\n")
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