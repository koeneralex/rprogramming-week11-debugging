## Libraries ------------------

library(ggplot2)


## Data -----------------------

v1 <- c(1:12)
m1 <- matrix(v1, nrow=3, ncol=4, byrow=TRUE)


## Function -------------------

tukey_multiple <- function(x) {
  
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x)){
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x)){ 
    outlier.vec[i] <- all(outliers[i,]) 
    }
  
  return(outlier.vec)
  
  }

tukey_multiple(v1)

tukey_multiple(m1)

tukey_multiple(100)


## Debugging -----------------

traceback()
  # Traceback tells us that the error occurs at line 3 of the function.

debug(tukey_multiple)
tukey_multiple()
  # Debug tell us the error occurs at line 3 of the function as well.

undebug(tukey_multiple)


## Fixing --------------------

tukey_multiple <- function(x) {
  
  outliers <- array(TRUE, dim=dim(x))
  for (j in 1:ncol(x)){
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x)){ 
    outlier.vec[i] <- all(outliers[i,]) 
  }
  
  return(outlier.vec)
  
}

