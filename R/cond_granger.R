#' Conditional granger causality
#'
#' Compute conditional granger causality of multivariate timeseries.
#'
#' @param data object containing all observations (rows) and variables (columns)
#' that are being considered. The variables should be ordered as follows: First
#' the variables that are supposed to granger cause a set of other variables
#' (>=1). Then the set of variables (>=1) that are Granger caused by the first set
#' of variables. Finally, a set of variables to condition on (>=1).
#' @param nx The number of variables (>=1) that Granger cause a set of other
#' variables (default = 1), conditioned on a third set of variables (>=1).
#' @param ny The number of variables (>=1) that are Granger caused by the first
#' nx variables (default = 1), conditioned on a third set of variables (>=1).
#' @param order Autoregressive order (>=1) of timeseries.
#' @param prob Logical. If TRUE, the F statistic is returned together with the p-value.
#'
#' @export
#' @examples
#' #Compute conditional granger causality of region x (nx =1) to regions y and z (ny=2),
#' #conditional on regions q and w for an AR(3) model.
#'
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time <= 5*60)
#' df <- pv1$df[, c("Nose_x", "LWrist_x", "RWrist_x")]
#' condGranger(df, nx=1, ny=1, order = 3)
condGranger <- function(data, nx=1, ny=1, order=1, prob=TRUE){

  data <- as.matrix(data)
  X <- t(data)
  X <- X-rowMeans(X)
  data <- t(X)

  x <- as.matrix(data[,1:nx])
  y <- as.matrix(data[,-(1:nx)])[,1:ny]

  if (sum(as.matrix(data[,-(1:(nx+ny))]))!=0) {

    z <- as.matrix(data[,-(1:(nx+ny))])
    nz <- ncol(z)

    # Compute F2
    x  <-  embed(x,order+1)
    x2 <- as.matrix(x[,1:nx])
    xlag <- as.matrix(x[,-(1:nx)])

    y  <-  embed(y,order+1)
    y2 <- as.matrix(y[,1:ny])
    ylag <- as.matrix(y[,-(1:ny)])

    z  <-  embed(z,order+1)
    z2 <- as.matrix(z[,1:nz])
    zlag <- as.matrix(z[,-(1:nz)])

    fitF <-lm(y2~cbind(ylag,zlag,xlag))
    fitR <- lm(y2~cbind(ylag,zlag))
    SSER <- sum(fitR$res^2)
    SSEF <- sum(fitF$res^2)
    f <- log(SSER/SSEF)
    prb <- anova(fitF,fitR)$Pr[2]

    if (prob == TRUE){
      out <- list(F = f, prob = prb)
    } else{
      out <- f
    }
    out
  } else {
    x  <-  embed(x,order+1)
    x2 <- as.matrix(x[,1:nx])
    xlag <- as.matrix(x[,-(1:nx)])

    y  <-  embed(y,order+1)
    y2 <- as.matrix(y[,1:ny])
    ylag <- as.matrix(y[,-(1:ny)])

    fitF <-lm(y2~cbind(ylag,xlag))
    fitR <- lm(y2~cbind(ylag))
    SSER <- sum(fitR$res^2)
    SSEF <- sum(fitF$res^2)
    f <- log(SSER/SSEF)
    prb <- anova(fitF,fitR)$Pr[2]

    if (prob==TRUE){
      out <- list(F = f, prob = prb)
    } else{
      out <- f
    }
    out
  }
}


condgrangertest.default <- function(x, y, order = 1, na.action = na.omit, ...)
{
  ## either x is a 2-column time series
  ## or x and y are univariate time series
  if((NCOL(x) == 2) && missing(y)) {
    xnam <- colnames(x)[1]
    ynam <- colnames(x)[2]
    x <- as.zoo(x)
    y <- x[,2]
    x <- x[,1]
  } else {
    xnam <- deparse(substitute(x))
    ynam <- deparse(substitute(y))
    x <- as.zoo(x)
    y <- as.zoo(y)
    stopifnot((NCOL(x) == 1), (NCOL(y) == 1))
  }

  ## compute lagged observations
  lagX <- do.call("merge", lapply(1:order, function(k) lag(x, -k)))
  lagY <- do.call("merge", lapply(1:order, function(k) lag(y, -k)))

  ## collect series, handle NAs and separate results again
  all <- merge(x, y, lagX, lagY)
  colnames(all) <- c("x", "y", paste("x", 1:order, sep = "_"), paste("y", 1:order, sep = "_"))
  all <- na.action(all)
  y <- as.vector(all[,2])
  lagX <- as.matrix(all[,(1:order + 2)])
  lagY <- as.matrix(all[,(1:order + 2 + order)])

  ## fit full model
  fm <- lm(y ~ lagY + lagX)

  ## compare models with waldtest
  rval <- waldtest(fm, 2, ...)

  ## adapt annotation
  attr(rval, "heading") <- c("Granger causality test\n",
                             paste("Model 1: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ") + Lags(", xnam, ", 1:", order,
                                   ")\nModel 2: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ")", sep = ""))

  return(rval)
}

