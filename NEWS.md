# biogrowth 0.2.0

* Included the Richards and logistic growth models.
* predict_isothermal_growth now accepts both named vectors or list as arguments.
* The model definition for predict_stochastic_growth has been improved. Now they are 
defined in a single argument using a tibble which includes expected values, standard
deviations and scale where the normal distribution is defined. This makes it easier
to define (not so many arguments) and way more flexible (specially when it comes to 
adding new models).
* Using a flexible unit system was giving more issues than it solved. Especially when
making the Baranyi model under dynamic and static conditions equivalent. Set the 
unit system to log10 for population size and ln(units)/[time] for the growth rate.
* Set the default binwidth of plot.TimeDistribution to NULL (geom_histogram picks it).

# biogrowth 0.1.2

* Documented S3 classes.
* Implemented additional S3 methods for all the fitting classes (vcov, deviance, predict, fitted, residuals, coef).
* Now, the full Ratkowsky model can be fitted under dynamic conditions. I had forgotten to add it in the helper.^
* Bug fix in distribution_to_logcount. If the growth rate was 0 in one of the simulations, approx would give an error. Added ties="ordered" to try and fix this.
* Bug fix in fit_secondary_model. The function was returning NA for the fitted parameters due to some error generating the output. 
* I got rid of the ugly warning messages in fit_secondary_model. They were due to bind_cols trying to fix names.
* Bug fix in plotting of dynamic predictions. The attribute for the y-axis label was not really used.
* Reduced iterations in the vignette to reduce compilation time.

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

