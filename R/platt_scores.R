#' Calculate Platt scores
#'
#' Platt scaling uses a logistic regression to convert the linear output of a two-step kernel ridge regression to approximate probabilities.
#'
#' @param x an object that contains or represents the outcome
#'
#' @return Depending on the input, a vector or matrix with the platt scores.
#'
#' @importFrom stats binomial glm.fit plogis
#'
#' @rdname platt_scores
#' @name platt_scores
#' @export
setGeneric("platt_scores",
           function(x, ...) standardGeneric("platt_scores"))

#' @rdname platt_scores
#' @export
setMethod("platt_scores",
          "tskrr",
          function(x){
            coefs <- x@platt
            if(!length(coefs))
              coefs <- add_platt(x)@platt

            .plattscore(fitted(x),coefs)
          })