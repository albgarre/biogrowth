# biogrowth 1.0.3

* added the CITATION to the paper in the JSS.

# biogrowth 1.0.2

* included a new vignette describing the use of custom distributions for forward uncertainty propagation.

# biogrowth 1.0.1

* included two new datasets: `refrigeratorSpain` and `greek_tractors`.

# biogrowth 1.0.0-3

* upgraded the HTML documentation to the format of R version 4

# biogrowth 1.0.0-2 

* fixed a bug in coef.GlobalGrowthComparison and coef.GrowthComparison

# biogrowth 1.0.0

* added a biogrowth-package help page
* updated the roxygen2 documentation to markdown style
* implemented predict_growth as an overall function for growth predictions
* implemented fit_growth as an overall function for growth fitting
* implemented time_to_size as an overall function for calculating the time to reach a given population size
* included lifecycle badges for every exported function
* renamed stochastic_growth to predict_growth_uncertainty
* implemented functions to calculate guesses for primary and secondary models
* implemented functions to show initial guesses of growth models
* implemented logLik and AIC calculations for model fits
* implemented functions for model comparison/selection
* reimplemented the predict method for global fits for something that makes more sense (it now returns a vector)
* fixed a bug in time_to_logcount for trilinear model. It was messing up the calculation in the horizontal parts
* included arguments logbase_mu and logbase_logN to deal with different unit systems
* created new vignettes with a more "chapter-like" style
* updated the README file


# biogrowth 0.2.3

* Included print methods for every class.

# biogrowth 0.2.2

* Added an alias to the mod-Gompertz function so ??modGompertz or ??gompertz finds it
* Added bounds for fit_multiple_growth in the vignette to avoid an error message in CRAN on macOS.

# biogrowth 0.2.1

* Included a times argument to the predict methods of dynamic fitting functions.
* Changed the base of the log-parameter transformation to 10 in predict_stochastic_growth.
* Changed the scale of plot.FitSecondaryGrowth.
* Include 2 new datasets: growth_pH_temperature and conditions_pH_temperature.
* Updated the labels of plot.FitSecondaryGrowth, so now it shows the transformation explicityly.
* Updated the example dataset conditions_pH_temperature to better show the models.
* Fixed a bug in the example for predict_stochastic_growth

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
* Added a new vignette about using predict_dynamic_growth() for static conditions.
* Improved parameter validation for fit_secondary_growth.
* Several improvements in the main vignette (new arguments, new functions, better descriptions...).

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

