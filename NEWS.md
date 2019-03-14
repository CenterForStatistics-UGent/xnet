## xnet devel (to become 0.1.6)

### Breaking changes

### New features

### bug fixes and minor improvements

* `linear_filter` gave totally wrong predictions due to a code error:  fixed.
* `linear_filter` returned a matrix when NAs were present: fixed.
* `fitted` now has an argument `labels` which allows to add the
labels to the returned object.

* In the testing procedures
    - testing skewed homogenous networks added.
    - testing validations added
    - testing symmetric calculations
    - testing processing of labels
    
* input testing for `tskrr` moved to its own function and is 
also used by `impute_tskrr` now.
    

## xnet 0.1.5

### Breaking changes

* class `tskrr`, `tskrrHeterogenous` and `tskrrHomogenous`:
    - the slot `has.orig` has been removed as it doesn't make sense to 
 keep the original kernel matrices. It is replaced by a slot `has.hat`
 allowing to store the hat matrices.
    - the slots `k.orig` and `g.orig` have been replaced by the slots
 `Hk` and `Hg` to store the hat matrices. These are more needed for
 fitting etc. 
 
* The function `has_original` has been removed and replaced by `has_hat`
* The argument `keep` of the function `tskrr` now stores the hat matrices
instead of the original kernel matrices.
* The function `tskrr` has lost its argument `homogenous`. It didn't make
sense to set that by hand.

### New features

* classes `tskrrHeterogenousImpute` and `tskrrHomogenousImpute` are added
  to allow for storing models with imputed predictions.

### bug fixes and minor improvements

## xnet 0.1.4

### breaking changes

* `get_loo_fun()` : 

    - argument `homogenous` removed in favor of `x`. This
allows for extension of the function based on either an object or
the class of that object. 
    - `x` becomes the first argument.

### New features

* There's a new function `linear_filter` that fits a linear filter over
an adjacency matrix. This function comes with a class `linearFilter`.
* `tune()` has a new argument `fun` that allows to specify a function
for optimization.
* functions `loss_mse()` and `loss_auc()` are provided for tuning.
* `update()` allows to retrain the model with new lambdas.

### bug fixes and minor improvements

* predictions were calculated wrongly in `tune()`: fixed.
* MSE was calculated wrongly in the previous version of `tune()`: fixed.

