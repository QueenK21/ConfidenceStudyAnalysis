library('brms')
library('lme4')
library('bayestestR')
brms::ar

setwd("data/")
rawConf_exp1 <- read.csv("rawConf_lme_new.csv")

rawprior <- c(set_prior('cauchy(0, 2.5)', class = 'b'),
              set_prior('cauchy(0, 10)', class = 'Intercept'), 
              set_prior('cauchy(0, 10)', class = 'sd'))

rawFullM <- brm(confidence ~ correctness * validity + (1|ID),
                data = rawConf_exp1,
                save_all_pars = "TRUE",
                family = bernoulli(link = "logit"),
                prior = rawprior,
                warmup = 4000,
                chains = 4,
                iter = 40000,
                control = list(adapt_delta = 0.98, max_treedepth = 15),
                seed = 9892)
                
rawMainM <- update(rawFullM, formula = ~ .-correctness:validity)

BFmainM <- update(fullM, formula = ~ .-Correctness:Validity)

CorrmainM <- update(rawFullM, formula = ~. -validity - correctness:validity)

ValmainM <- update(rawFullM, formula = ~. -correctness -correctness:validity)

null <- update(rawFullM, formula = ~. -correctness -validity -correctness:validity)

comparison <- bayesfactor_models(rawFullM, rawMainM, CorrmainM, ValmainM, denominator = null)

#update(comparison, reference = 2)

bayesfactor_inclusion(comparison, match_models = TRUE)


BF_int_main <- bayes_factor(rawFullM, rawMainM)
BF_int_main
