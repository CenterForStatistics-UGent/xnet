## xnet 0.1.8

### Breaking changes

* For consistency, the arguments `K` and `G` for the function `predict()`
have been renamed `k` and `g` (lower case).
* `loo` now adds the labels to the output (except for linear filters)

### New features

* `tune` now allows for a one-dimensional grid search for heterogenous
networks. Set `onedim = TRUE` to avoid a full grid search.
* `has_onedim` tells whether the grid search was one dimensional or not.
This is a getter for the appropriate slote in the tskrrTune class.
* `plot_grid` allows you to plot the loss in function of the
searched grid after tuning a model. It deals with both 1D and
2D grids and can be used for quick evaluation of the optimal
lambda values.
* `residuals` allows you to calculate the residuals based on
the predictions or on the loo values of choice.
* There's a `plot` method available now for `tskrr` objects. It
allows to plot fitted values, residuals, original response and
the results of different loo settings, together with dendrograms
based on the kernel matrices.

### Bug fixes and minor improvements

* `predict` didn't give correct output when only `g` was passed.
fixed.
* `colnames` didn't get the correct labels for homogenous networks


## xnet 0.1.7

### Breaking changes

* Preliminary function `impute_loo` is removed from the 
package.
* `eigen2hat`, `eigen2map` and `eigen2matrix` had the second argument
renamed from `vec` to `val`. The old name implied that the second argument
took the vectors, which it doesn't!


### New features

* A `tskrrImpute` virtual class is added to represent imputed models.

### bug fixes and minor improvements

* `is_symmetric` didn't take absolute values to compare. Fixed.
* `show` methods for objects are cleaned up.
* `predict` gave nonsensical output. Fixed.

## xnet 0.1.6

### Breaking changes

* `valid_labels` now requires the K and G matrices to have the
same ordering of row and column names. Otherwise the matrix 
wouldn't be symmetric and can't be used.
* `linear_filter` now forces the alphas to sum up to 1.
* `tune` now returns an object of class `tskrrTuneHomogenous` or
`tskrrTuneHeterogenous`. 

### New features

* the class `tskrrTune` provides a more complete object with all
information of tuning. It is a superclass with two real subclasses,
`tskrrTuneHeterogenous` and `tskrrTuneHomogenous`.
* the function `tune` now allows to pass the matrices directly so
you don't have to create a model with `tskrr` first.

### bug fixes and minor improvements

* `linear_filter` gave totally wrong predictions due to a code error:  fixed.
* `linear_filter` returned a matrix when NAs were present: fixed.
* `fitted` now has an argument `labels` which allows to add the
labels to the returned object.
* `tskrr` now returns an error if the Y matrix is not symmetric or
skewed when fitting a homogenous network.
* `labels` now produces more informative errors and warnings.

* In the testing procedures
    - testing skewed homogenous networks added.
    - testing validations added
    - testing symmetric calculations
    - testing processing of labels
    - testing shortcuts
    - testing update function
    
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

