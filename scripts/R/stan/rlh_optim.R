library(cmdstanr)
library(loo)

file.exists(here::here("scripts", "R", "stan", "rlh_optim.stan"))

mod = cmdstan_model(
  here::here(
    "scripts", "R", "stan", "rlh_optim.stan"
    )
  )

