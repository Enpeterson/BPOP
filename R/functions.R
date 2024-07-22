
# functions.R
# (to be extended throughout the semester)

#------
GetSplines <- function( # Get B-splines
  x.i, ##<< Vector of x-values (without NAs) for which splines need to be calculated (determines the number of rows of B.ik)
  x0 = NULL, ##<< x-value which determines knot placement. By default, knot is placed half-interval before last observation
  I = 2.5, ##<< Interval length between two knots during observation period
  degree = 3 # currently tested only with degree 3
) {
  if (is.null(x0)) {
    x0 <- max(x.i)-0.5*I
  }
  # get knots, given that one knot needs to be in year0
  knots <- seq(x0-1000*I, x0+1000*I, I)
  while (min(x.i) < knots[1]) knots <- c(seq(knots[1]-1000*I, knots[1]-I,I), knots)
  while (max(x.i) > knots[length(knots)]) knots <- c(knots, seq(knots[length(knots)]+I,
                                                                knots[length(knots)]+1000*I, I))
  Btemp.ik <- bs(x.i, knots = knots[-c(1, length(knots))],  degree = degree,
                 Boundary.knots = knots[c(1, length(knots))])
  indicesofcolswithoutzeroes <- which(apply(Btemp.ik, 2, sum) > 0)
  # only remove columns with zeroes at start and end
  startnonzerocol <- indicesofcolswithoutzeroes[1]
  endnonzerocol <- indicesofcolswithoutzeroes[length(indicesofcolswithoutzeroes)]
  B.ik <- Btemp.ik[,startnonzerocol:endnonzerocol]
  colnames(B.ik) <- paste0("spline", seq(1, dim(B.ik)[2]))
  knots.k <- knots[startnonzerocol:endnonzerocol]
  names(knots.k) <- paste0("spline", seq(1, dim(B.ik)[2]))
  ##value<< List of B-splines containing:
  return(list(B.ik = B.ik, ##<< Matrix, each row is one observation, each column is one B-spline.
              knots.k = knots.k ##<< Vector of knots.
  ))
}
#---------------
# add CIs to a plot
AddCIs <- function(CI.low.t, # lower bound for seq.years.t
                   CI.up.t, # upper bound for seq.years.t
                   seq.years.t, col = 1){
  # add CIs to a plot.
  col = adjustcolor(col, alpha.f = 0.1)
  for (t in 2:length(seq.years.t))
    polygon(c(seq.years.t[t-1], seq.years.t[t-1], seq.years.t[t], seq.years.t[t],seq.years.t[t-1]),
            c(CI.low.t[t-1], CI.up.t[t-1], CI.up.t[t], CI.low.t[t], CI.low.t[t-1]),
            col=col, border = NA)
}

#---------------
PlotTrace <- function(#Traceplot for one parameter
  ### Trace plot for one parameter and add loess smoother for each chain
  parname, mcmc.array,##<< needs to be 3-dimensional array!
  n.chains= NULL, n.sim= NULL, main = NULL){
  if (is.null(main)) main <- parname
  if (is.null(n.sim)) n.sim <- dim(mcmc.array)[1]
  if (is.null(n.chains)) n.chains <- dim(mcmc.array)[2]
  plot(c(mcmc.array[,1,parname]), type = "l", ylab = parname,  main = main,
       ylim = c(min(mcmc.array[,,parname]),max(mcmc.array[,,parname])))
  for (chain in 1:n.chains){
    lines(c(mcmc.array[,chain,parname]), type = "l", col = chain)
  }
  for (chain in 1:n.chains){
    curve(predict(loess(c(mcmc.array[,chain,parname])~seq(1,n.sim)),x), lty = 2, lwd = 3, add = TRUE, type = "l", col = chain)
  }
}
#---------------
PlotPostOnly <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample
  post.samp, parname = NULL##<<used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
}

PlotPostWithNormalPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, priormean, priorsd, parname = NULL##<< used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
  curve(dnorm(x, mean = priormean, sd = priorsd), col = 2, lwd = 3, add = TRUE)
  abline(v = priormean, col = 2, lty = 2)
}
#PlotPostWithNormalPrior(post.samp = rnorm(100,0,1), priormean = -1, priorsd = 10, parname = "bla")

PlotPostWithUnifPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, priorlow, priorup, parname = NULL##<< used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "",xlim = c(minx, maxx))
  h <- 1/(priorup-priorlow)
  segments(priorlow, h, priorup, h, col = 2)
}
#PlotPostWithUnifPrior(post.samp = rnorm(100,0), priorlow = -2, priorup = 10, parname = "bla")

#------------------
# WAIC
# adapted from Vehtari and Gelman 2014
waic <- function(log_lik){ # log-like should be the log(p(y_i|theta^(s)) with i=1,..,n rows and s=1,2,..,S columns
  log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
                    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
              p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
              pointwise=pointwise, total=total, se=se))
}

colVars <- function(a){
  n <-   dim(a)[[1]];
  c <- dim(a)[[2]];
  return(.colMeans(((a - matrix(.colMeans(a, n, c), nrow = n,
                                ncol = c, byrow = TRUE)) ^ 2), n, c) * n / (n - 1))
}

