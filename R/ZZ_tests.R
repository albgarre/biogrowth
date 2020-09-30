
# library(tidyverse)
#
# env_conditions <- tibble(time = c(0, 5, 40),
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
# primary_pars <- list(mu_opt = 2,
#              Nmax = 1e8,
#              N0 = 1e0,
#              Q0 = 1e-3)

# aa <- predict_dynamic_growth(seq(0, 50, length = 1000),
#                        env_conditions, primary_pars,
#                        sec_models)
#
# plot(aa, add_factor = "pH", ylims= c(-1, 7), label_y2 = "aa")
# time_to_logcount(aa, 5)




# aa <- predict_isothermal_growth("modGompertz", seq(0, 100, length = 1000),
#                           list(logN0 = 2, C = 6, mu = .2, lambda = 25))
#
# plot(aa)
# time_to_logcount(aa, 2.5)
#
# aa <- predict_isothermal_growth("Baranyi", seq(0, 100, length = 1000),
#                           list(logN0 = 2, logNmax = 8, mu = .2, lambda = 25))
#
# plot(aa)
#
# aa <- predict_isothermal_growth("Trilinear", seq(0, 100, length = 1000),
#                           list(logN0 = 2, logNmax = 8, mu = .2, lambda = 25))
# plot(aa)




# my_data <- tibble(time = c(0, 25, 50, 75, 100),
#                   logN = c(2, 2.5, 7, 8, 8))
#
# aa <- fit_isothermal_growth(my_data, "modGompertz",
#                       c(C = 6, mu = .2, lambda = 25),
#                       c(logN0 = 2)
#                       )
#
# summary(aa)
# plot(aa)
#
#
# fit_isothermal_growth(my_data, "Baranyi",
#                       c(logN0 = 2, logNmax = 8, mu = .2, lambda = 25),
#                       c()
# ) %>%
#     plot()
#
# fit_isothermal_growth(my_data, "Trilinear",
#                       c(logN0 = 2, logNmax = 8, mu = .2, lambda = 25),
#                       c()
# ) %>%
#     plot()





# env_conditions <- tibble(time = c(0, 5, 40),
#                          temperature = c(20, 30, 35),
#                          aw = c(.99, .95, .9)
#                          )
#
# sec_models <- list(
#     temperature = list(model = "CPM",
#                        b = 1,
#                        xmin = 25,
#                        xopt = 35,
#                        xmax = 42,
#                        n = 1),
#     aw = list(model = "CPM",
#               xmin = 5.5,
#               xopt = 6.5,
#               xmax = 7.5,
#               n = 1)
# )
#
# primary_pars <- list(mu_opt = 2,
#              Nmax = 1e8,
#              N0 = 1e0,
#              Q0 = 1e-3)
#
# aa <- predict_dynamic_growth(seq(0, 50, length = 1000),
#                        env_conditions, primary_pars,
#                        sec_models)





# env_conditions <- tibble(time = c(0, 5, 15),
#                          temperature = c(20, 30, 35),
#                          aw = c(.99, .95, .9)
# )
#
# sec_models <- list(
#     temperature = list(model = "CPM",
#                        xmin = 25,
#                        xopt = 35,
#                        xmax = 40,
#                        n = 1),
#     aw = list(model = "CPM",
#               xmin = .85,
#               xopt = .95,
#               xmax = 1,
#               n = 1)
# )
#
# primary_pars <- list(mu_opt = 5,
#                      Nmax = 1e4,
#                      N0 = 1e0,
#                      Q0 = 1e-3)
#
# example_dynamic_growth <- predict_dynamic_growth(seq(0, 15, length = 30),
#                                   env_conditions, primary_pars,
#                                   sec_models) %>%
#     .$simulation %>%
#     select(time, logN) %>%
#     mutate(., logN = logN + rnorm(nrow(.), 0, 0.2))
#
# ggplot(example_dynamic_growth) +
#     geom_point(aes(x = time, y = logN))
#
# example_env_conditions <- tibble(time = c(0, 5, 15),
#                                  temperature = c(20, 30, 35),
#                                  aw = c(.99, .95, .9)
#                                  )
#
# usethis::use_data(example_dynamic_growth)
# usethis::use_data(example_env_conditions)
#

# data("example_dynamic_growth")
# data("example_env_conditions")
#
# sec_model_names <- c(temperature = "CPM",
#                      aw= "CPM")
#
# known_pars <- list(mu_opt = 4, Nmax = 1e4,
#                    temperature_n = 1,
#                    aw_xmax = 1, aw_xmin = .9,
#                    N0 = 1e0, Q0 = 1e-3,
#                    aw_n = 1
# )
#
# this_p <- list(temperature_xmin = 25, temperature_xopt = 35,
#                temperature_xmax = 40,
#                aw_xopt = .95)
#
# aa <- fit_dynamic_growth(example_dynamic_growth, example_env_conditions,
#                          unlist(this_p),
#                          unlist(known_pars), sec_model_names)
# summary(aa)
# plot(aa)
#
# bb <- fit_MCMC_growth(my_data, env_conditions, unlist(this_p),
#                       unlist(known_pars), sec_model_names, niter = 20)
#
# summary(bb)
# plot(bb, add_factor = "temperature",
#      ylims = c(-1, 6))
#
# cc <- predict_MCMC_growth(bb, seq(0, 15, length = 50), env_conditions, niter = 10)
#
# plot(cc)
#
# cc$simulations
# distribution_to_logcount(cc, 1)



# aa <- predict_stochastic_growth("modGompertz", seq(0, 30, length = 100), 1000,
#                           0, .2,
#                           2, .3,
#                           4, .4,
#                           6, .5,
#                           corr_matrix = matrix(c(1, 0.1, 0.1, .1,
#                                                  0.1, 1, 0.1, .1,
#                                                  .1, .1, 1, .1,
#                                                  .1, .1, .1, 1),
#                                                nrow = 4)
# )
#
# plot(aa)
#
# bb <- distribution_to_logcount(aa, 4)
# plot(bb)







# example_cardinal <- expand.grid(temp = seq(0, 40, length = 8),
#             pH = seq(5, 7, length = 8)) %>%
#     mutate(gamma_temp = biogrowth:::CPM_model(temp,
#                                   5, 35, 37, 1),
#            gamma_pH = biogrowth:::CPM_model(pH,
#                                 5.2, 6.5, 6.8, 2)) %>%
#     mutate(mu_opt = 1.2,
#            mu = mu_opt *gamma_temp*gamma_pH,
#            sq_mu = sqrt(mu)+ rnorm(nrow(.), 0, 0.05)) %>%
#     mutate(sq_mu = ifelse(sq_mu < 0, 0, sq_mu)) %>%
#     mutate(mu = sq_mu^2) %>%
#     select(temperature = temp, pH, mu)
#
# usethis::use_data(example_cardinal)



# write_excel_csv(my_data, path = "example_cardinal.csv")
#
# data("example_cardinal")
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
# aa <- fit_secondary_growth(example_cardinal, this_p, known_pars, sec_model_names)
#
# aa$fit_results$residuals
# aa$data
#
# aa











