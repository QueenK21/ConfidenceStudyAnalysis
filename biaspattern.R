library('brms')
library('lme4')
library('bayestestR')
brms::ar

setwd("data/")
data_exp3 <- read.csv("exp3_re_for_dummycoding_brms_re_id.csv")

prior <- get_prior(oribias ~ orientation + (1|subjects), 
                   data = data_exp3, family = gaussian())

bias_priors <- c(set_prior("normal(0, 1)", class = "b"),
                set_prior("student_t(3, 0, 10)", class = "sd"),
                set_prior("normal(0, 10)", class = "Intercept"))

logitprior <- c(set_prior('cauchy(0, 2.5)', class = 'b'),
              set_prior('cauchy(0, 10)', class = 'Intercept'), 
              set_prior('cauchy(0, 10)', class = 'sd'))


oribiasM <- brm(oribias ~ orientation + (1|id), 
               data = data_exp3, 
               save_all_pars = "TRUE", 
               family = gaussian(link = "identity"),
               prior = bias_priors, 
               warmup = 1000,
               chains = 4,
               iter = 10000,
               control = list(adapt_delta = 0.98, max_treedepth = 15))

#oribiasM_q <- brm(oribias ~ orientation + I(orientation^2) + (1|subjects), 
#                data = data_exp3, 
#                save_all_pars = "TRUE",
#                family = gaussian(link = "identity"),
#                prior = bias_priors, 
#                warmup = 1000,
#                chains = 4,
#                iter = 10000,
#                control = list(adapt_delta = 0.98, max_treedepth = 15))

oribiasL <- brm(oribias ~ orthopoly_L + (1|subjects), 
                data = data_exp3, 
                save_all_pars = "TRUE", 
                family = gaussian(link = "identity"),
                prior = bias_priors, 
                warmup = 1000,
                chains = 4,
                iter = 10000,
                control = list(adapt_delta = 0.98, max_treedepth = 15))

oribiasQ <- brm(oribias ~ orthopoly_L + orthopoly_Q + (1|subjects), 
                data = data_exp3, 
                save_all_pars = "TRUE", 
                family = gaussian(link = "identity"),
                prior = bias_priors, 
                warmup = 1000,
                chains = 4,
                iter = 10000,
                control = list(adapt_delta = 0.98, max_treedepth = 15))


orinullM <- update(oribiasM, formula = ~. -orientation)

BF_oribias <- bayes_factor(oribiasM, orinullM)
BF_oribias

BF_trend_oribias <- bayes_factor(oribiasQ, oribiasL)
BF_trend_oribias

sdM <- brm(sd ~ orientation + (1|id), 
                data = data_exp3, 
                save_all_pars = "TRUE", 
                family = lognormal(link = "identity"),
                prior = bias_priors, 
                warmup = 1000,
                chains = 4,
                iter = 10000,
                control = list(adapt_delta = 0.98, max_treedepth = 15))

sdL <- brm(sd ~ orthopoly_L + (1|subjects), 
           data = data_exp3, 
           save_all_pars = "TRUE", 
           family = lognormal(link = "identity"),
           prior = bias_priors, 
           warmup = 1000,
           chains = 4,
           iter = 10000,
           control = list(adapt_delta = 0.98, max_treedepth = 15))

sdQ <- brm(sd ~ orthopoly_L + orthopoly_Q + (1|subjects), 
           data = data_exp3, 
           save_all_pars = "TRUE", 
           family = lognormal(link = "identity"),
           prior = bias_priors, 
           warmup = 1000,
           chains = 4,
           iter = 10000,
           control = list(adapt_delta = 0.98, max_treedepth = 15))

sdnullM <- update(sdM, formula = ~. -orientation)

BF_sd <- bayes_factor(sdQ, sdnullM)
BF_sd <- bayes_factor(sdM, sdnullM)
BF_sd

guessM <- brm(guessrate ~ orientation + (1|subjects), 
           data = data_exp3, 
           save_all_pars = "TRUE", 
           family = zero_inflated_beta("logit"),
           prior = logitprior, 
           warmup = 1000,
           chains = 4,
           iter = 10000,
           control = list(adapt_delta = 0.98, max_treedepth = 15))

guessnullM <- update(guessM, formula = ~. -orientation)

BF_guess <- bayes_factor(guessM, guessnullM)
BF_guess


swapM <- brm(swaprate ~ orientation + (1|subjects), 
              data = data_exp3, 
              save_all_pars = "TRUE", 
              family = zero_inflated_beta("logit"),
              prior = logitprior, 
              warmup = 1000,
              chains = 4,
              iter = 10000,
              control = list(adapt_delta = 0.98, max_treedepth = 15))

swapnullM <- update(swapM, formula = ~. -orientation)

BF_guess <- bayes_factor(swapM, swapnullM)
BF_guess


confbiasM <- brm(confbias ~ orientation + (1|subjects), 
                data = data_exp3, 
                save_all_pars = "TRUE", 
                family = gaussian(link = "identity"),
                prior = bias_priors, 
                warmup = 1000,
                chains = 4,
                iter = 10000,
                control = list(adapt_delta = 0.98, max_treedepth = 15))

#confbiasM_q <- brm(confbias ~ orientation + ori_quad + (1|subjects), 
#                  data = data_exp3, 
#                  save_all_pars = "TRUE", 
#                  family = gaussian(link = "identity"),
#                  prior = bias_priors, 
#                  warmup = 1000,
#                  chains = 4,
#                  iter = 10000,
#                  control = list(adapt_delta = 0.98, max_treedepth = 15))

confbiasL <- brm(confbias ~ orthopoly_L + (1|subjects), 
                 data = data_exp3, 
                 save_all_pars = "TRUE", 
                 family = gaussian(link = "identity"),
                 prior = bias_priors, 
                 warmup = 4000,
                 chains = 4,
                 iter = 40000,
                 control = list(adapt_delta = 0.98, max_treedepth = 15))

confbiasQ <- brm(confbias ~ orthopoly_L + orthopoly_Q + (1|subjects), 
                 data = data_exp3, 
                 save_all_pars = "TRUE", 
                 family = gaussian(link = "identity"),
                 prior = bias_priors, 
                 warmup = 4000,
                 chains = 4,
                 iter = 40000,
                 control = list(adapt_delta = 0.98, max_treedepth = 15))

confbnullM <- update(confbiasL, formula = ~. -orthopoly_L)

conf_comparision <- bayesfactor_models(confbiasL, confbiasQ, denominator = confbnullM)
bayesfactor_inclusion(conf_comparision, match_models = TRUE)



confbias_w_M <- brm(confbias_w_swap ~ orientation + (1|subjects), 
                 data = data_exp3, 
                 save_all_pars = "TRUE", 
                 family = gaussian(link = "identity"),
                 prior = bias_priors, 
                 warmup = 1000,
                 chains = 4,
                 iter = 10000,
                 control = list(adapt_delta = 0.98, max_treedepth = 15))

confbias_w_M_L <- brm(confbias_w_swap ~ orthopoly_L + (1|subjects), 
                    data = data_exp3, 
                    save_all_pars = "TRUE", 
                    family = gaussian(link = "identity"),
                    prior = bias_priors, 
                    warmup = 1000,
                    chains = 4,
                    iter = 10000,
                    control = list(adapt_delta = 0.98, max_treedepth = 15))

confbias_w_M_q <- brm(confbias_w_swap ~ orthopoly_L + orthopoly_Q + (1|subjects), 
                   data = data_exp3, 
                   save_all_pars = "TRUE", 
                   family = gaussian(link = "identity"),
                   prior = bias_priors, 
                   warmup = 1000,
                   chains = 4,
                   iter = 10000,
                   control = list(adapt_delta = 0.98, max_treedepth = 15))


confbnul_w_M <- update(confbias_w_M, formula = ~. -orientation)

BF <- bayes_factor(confbias_w_M, confbnul_w_M)
BF

confM <- brm(conf ~ orientation + (1|subjects), 
                 data = data_exp3, 
                 save_all_pars = "TRUE", 
                 family = gaussian(link = "identity"),
                 prior = bias_priors, 
                 warmup = 1000,
                 chains = 4,
                 iter = 10000,
                 control = list(adapt_delta = 0.98, max_treedepth = 15))

confM_q <- brm(conf ~ orientation + ori_quad + ori_cubic + (1|subjects), 
             data = data_exp3, 
             save_all_pars = "TRUE", 
             family = gaussian(link = "identity"),
             prior = bias_priors, 
             warmup = 1000,
             chains = 4,
             iter = 10000,
             control = list(adapt_delta = 0.98, max_treedepth = 15))

confnullM <- update(confbiasM, formula = ~. -orientation)

BF_oribias <- bayes_factor(confM, confM_q)
BF_oribias


resolM <- brm(conresol ~ orientation + (1|subjects), 
             data = data_exp3, 
             save_all_pars = "TRUE", 
             family = gaussian(link = "identity"),
             prior = bias_priors, 
             warmup = 1000,
             chains = 4,
             iter = 10000,
             control = list(adapt_delta = 0.98, max_treedepth = 15))

resolnullM <- update(resolM, formula = ~. -orientation)

BF <- bayes_factor(resolM, resolnullM)
BF
