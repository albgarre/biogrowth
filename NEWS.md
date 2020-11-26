# biogrowth 0.1.2

* Documented S3 classes.
* Implemented additional S3 methods for all the fitting classes (vcov, deviance, predict, fitted, residuals, coef).
* Now, the full Ratkowsky model can be fitted under dynamic conditions. I had forgotten to add it in the helper.^
* Bug fix in distribution_to_logcount. If the growth rate was 0 in one of the simulations, approx would give an error. Added ties="ordered" to try and fix this.
* Bug fix in fit_secondary_model. The function was returning NA for the fitted parameters due to some error generating the output. 
* I got rid of the ugly warning messages in fit_secondary_model. They were due to bind_cols trying to fix names.
* Bug fix in plotting of dynamic predictions. The attribute for the y-axis label was not really used.

# biogrowth 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Included the full Ratkowsky model.
* Added the possibility to a single model to various curves with the `fit_multiple_growth` and `fit_multiple_growth_MCMC` functions.
* Included a new vignette with advanced plotting options.
* Implemented automatic checks about model parameters for primary models.
* Implemented automatic checks about model parameters for cardinal fits.
* Implemented S3 methods for residuals.
* Included S3 plotting method for secondary fits.
* Added new arguments to the S3 plotting methods with additional
aesthetic options.
* Defined a range in the Zwietering secondary model to avoid unreasonable
results.
* Updated vignette with new functions.
* Small changes in function documentation and vignettes for better clarity.

