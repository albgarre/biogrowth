
# library(tidyverse)
#
# env_conditions <- tibble(time = c(0, 5, 15),
#                          temperature = c(20, 30, 35),
#                          pH = c(7, 6.5, 5)
#                          )
#
# sec_models <- list(
#     temperature = list(model = "Zwietering",
#                        b = 1,
#                        xmin = 25,
#                        xopt = 35,
#                        n = 1),
#     pH = list(model = "CPM",
#               xmin = 5.5,
#               xopt = 6.5,
#               xmax = 7.5,
#               n = 2)
# )
#
# primary_pars <- list(mu_opt = .5,
#              Nmax = 1e8,
#              N0 = 1e0,
#              Q0 = 1e-3)
#
# predict_dynamic_growth(seq(0, 20, length = 1000),
#                        env_conditions, primary_pars,
#                        sec_models)




# predict_isothermal_growth("modGompertz", seq(0, 100, length = 1000),
#                           list(logN0 = 2, C = 6, mu = .2, lambda = 25)) %>%
#     .$simulation %>%
#     ggplot() +
#     geom_line(aes(time, logN))
#
# predict_isothermal_growth("Baranyi", seq(0, 100, length = 1000),
#                           list(logN0 = 2, logNmax = 8, mu = .2, lambda = 25)) %>%
#     .$simulation %>%
#     ggplot() +
#     geom_line(aes(time, logN))
#
# predict_isothermal_growth("Trilinear", seq(0, 100, length = 1000),
#                           list(logN0 = 2, logNmax = 8, mu = .2, lambda = 25)) %>%
#     .$simulation %>%
#     ggplot() +
#     geom_line(aes(time, logN))




# my_data <- tibble(time = c(0, 25, 50, 75, 100),
#                   logN = c(2, 2.5, 7, 8, 8))
#
# fit_isothermal_growth(my_data, "modGompertz",
#                       c(C = 6, mu = .2, lambda = 25),
#                       c(logN0 = 2)
# ) %>%
#     .$fit %>%
#     summary()
#
#
# fit_isothermal_growth(my_data, "Baranyi",
#                       c(logN0 = 2, logNmax = 8, mu = .2, lambda = 25)
# ) %>%
#     .$fit %>%
#     summary()
#
# fit_isothermal_growth(my_data, "Trilinear",
#                       c(logN0 = 2, logNmax = 8, mu = .2, lambda = 25)
# ) %>%
#     .$fit %>%
#     summary()











# env_conditions <- tibble(time = c(0, 5, 15),
#                          temperature = c(20, 30, 35),
#                          pH = c(7, 6.5, 5)
# )
#
# sec_models <- list(
#     temperature = list(model = "Zwietering",
#                        xmin = 25,
#                        xopt = 35,
#                        n = 1),
#     pH = list(model = "CPM",
#               xmin = 5.5,
#               xopt = 6.5,
#               xmax = 7.5,
#               n = 2)
# )
#
# primary_pars <- list(mu_opt = 5,
#                      Nmax = 1e4,
#                      N0 = 1e0,
#                      Q0 = 1e-3)
#
# my_data <- predict_dynamic_growth(seq(0, 20, length = 20),
#                                   env_conditions, primary_pars,
#                                   sec_models) %>%
#     .$simulation %>%
#     select(time, logN) %>%
#     mutate(., logN = logN + rnorm(nrow(.), 0, 0.2))
#
# sec_model_names <- c(temperature = "Zwietering",
#                      pH = "CPM")
#
# known_pars <- list(mu_opt = 4, Nmax = 1e4,
#                    temperature_n = 1,
#                    pH_xmax = 7.5,
#                    N0 = 1e0, Q0 = 1e-3,
#                    pH_n = 2, pH_xmin = 5.5
# )
#
# this_p <- list(temperature_xmin = 25, temperature_xopt = 35,
#                pH_xopt = 6.5)
#
# aa <- fit_dynamic_growth(my_data, env_conditions, unlist(this_p),
#                          unlist(known_pars), sec_model_names)
# aa
# summary(aa$fit_results)
#
# aa <- fit_MCMC_growth(my_data, env_conditions, unlist(this_p),
#                       unlist(known_pars), sec_model_names, niter = 100)
#
# aa
# plot(aa$fit_results)
# pairs(aa$fit_results)

# bb <- predict_MCMC_growth(aa, seq(0, 15, length = 50), env_conditions, niter = 10)
#
# bb$quantiles %>%
#     ggplot(aes(x = time)) +
#         geom_line(aes(y = q50)) +
#         geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .5)



#
# predict_stochastic_growth("modGompertz", seq(0, 30, length = 100), 1000,
#                           0, .2,
#                           2, .3,
#                           4, .4,
#                           6, .5,
#                           corr_matrix = matrix(c(1, 0.1, 0.1, .1,
#                                                  0.1, 1, 0.1, .1,
#                                                  .1, .1, 1, .1,
#                                                  .1, .1, .1, 1),
#                                                nrow = 4)
# ) %>%
#     .$quantiles %>%
#     ggplot(aes(x = time)) +
#     geom_line(aes(y = q50)) +
#     geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .5)







# my_data <- expand.grid(temp = seq(0, 40, length = 6),
#             pH = seq(5, 7, length = 6)) %>%
#     mutate(gamma_temp = CPM_model(temp,
#                                   5, 35, 37, 1),
#            gamma_pH = CPM_model(pH,
#                                 5.2, 6.5, 6.8, 2)) %>%
#     mutate(mu_opt = 1.2,
#            mu = mu_opt *gamma_temp*gamma_pH,
#            sq_mu = sqrt(mu)+ rnorm(nrow(.), 0, 0.2)) %>%
#     mutate(sq_mu = ifelse(sq_mu < 0, 0, sq_mu)) %>%
#     mutate(mu = sq_mu^2) %>%
#     select(temperature = temp, pH, mu)
#
# sec_model_names <- c(temperature = "Zwietering",
#                      pH = "CPM")
#
# known_pars <- list(mu_opt = 1.2,
#                    temperature_n = 1,
#                    pH_xmax = 6.8,
#                    pH_n = 2, pH_xmin = 5.2
# )
#
# this_p <- list(temperature_xmin = 5, temperature_xopt = 35,
#                pH_xopt = 6.5)
#
# get_secondary_residuals(this_p, my_data, known_pars, sec_model_names, "sqrt")
#
# aa <- fit_secondary_growth(my_data, this_p, known_pars, sec_model_names)














