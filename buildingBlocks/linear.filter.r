train.linear.filter <- function(Y, alpha.1=0.25,
                                alpha.2=0.25, alpha.3=0.25,
                                alpha.4=0.25){
  # simple matrix filter
  model = list(
    Y = Y,
    alpha.1 = alpha.1,
    alpha.2 = alpha.2,
    alpha.3 = alpha.3,
    alpha.4 = alpha.4,
    global.mean = mean(Y),
    col.means = c(colMeans(Y)),
    row.means = c(rowMeans(Y))
  )
  model$F <- alpha.1 * model$Y + alpha.2 * rep(model$col.means, each=nrow(Y)) +
        alpha.3 * rep(model$row.means, times=ncol(Y)) +
        alpha.4 * model$global.mean
  return(model)
}

loo.I.lf <- function(model){
  dim.Y <- dim(model$Y)
  leverage <- model$alpha.1 + model$alpha.2 / ncol(Y)
        + model$alpha.3 / nrow(Y) + model$alpha.4 / (length(Y))
  return((model$F - model$Y * leverage) / (1 - leverage))
}

loo.I0.lf <- function(model){
  dim.Y <- dim(model$Y)
  leverage <- model$alpha.1 + model$alpha.2 / ncol(Y)
  + model$alpha.3 / nrow(Y) + model$alpha.4 / (length(Y))
  return(model$F - model$Y * leverage)
}