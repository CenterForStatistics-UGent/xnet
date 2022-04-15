# internal functions for platt scoring

# orig: the original outcome
# pred: the predictions
.generate_coefs <- function(orig, pred){
  if(is.matrix(orig))
    orig <- as.vector(orig)
  if(is.matrix(pred))
    pred <- as.vector(pred)

  pred <- cbind(1,pred)
  glm.fit(pred, orig,
          family = binomial())$coefficients
}

.plattscore <- function(x, coefs){
  plogis(coefs[1] + coefs[2]*x)
}