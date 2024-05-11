# Latent change
suppressPackageStartupMessages({
  library("tidyverse")
  library("sjPlot")
  library("sjstats")
  library("lme4")
  library("brms")
  library("effectsize")
  library("scales")
  library("DT")
  library("kableExtra")
  library("lme4")
  library("JWileymisc")
  library("multilevelTools")
  library("data.table")
  library("insight")
  library("mice")
  library("lcsm")
  library("lavaan")
})

# df_bysubj$mood_change <- df_bysubj$mood_post - df_bysubj$mood_init

# d <- readRDS("data/prep/groundhog_clean.RDS")
d <- rio::import(
  here::here("data", "prep", "prl", "groundhog_clean_rev_norev.csv")
)

out <- d[d$ema_number < 11, ] |> 
  group_by(is_reversal, user_id) |> 
  summarize(
    fdbk = mean(feedback)
  ) |> 
  ungroup()

plot(density(out$fdbk))

out_clean <- out[out$fdbk > 0.44, ]
good_user_id <- na.omit(unique(out_clean$user_id))

df <- d[d$user_id %in% good_user_id, ]

out2 <- df[df$ema_number < 11, ] |> 
  group_by(user_id) |> 
  summarize(
    acc = mean(feedback),
    n = n()
  )

plot(density(out2$acc))

out2_clean <- out2[out2$acc > 0.44, ]
good2_user_id <- na.omit(unique(out2_clean$user_id))

df3 <- df[df$user_id %in% good2_user_id, ]

# There are no enough data in the no-reversal condition
temp <- df3[df3$is_reversal == 0, ]
temp |> 
  group_by(user_id, ema_number) |> 
  summarize(
    pre = mean(mood_pre),
    post = mean(mood_post),
    ch = mean(mood_change),
    acc = mean(feedback)
  ) |> 
  as.data.frame()


out3 <- df3[(df3$ema_number < 11) & (df3$is_reversal == 1), ] |> 
  group_by(is_reversal, user_id, ema_number) |> 
  summarize(
    acc = mean(feedback),
    n = n(),
    mood_ch = mean(mood_change),
    moodpre = mean(mood_pre),
    moodpost = mean(mood_post),
    fs = mean(final_score)
  ) |> 
  ungroup()

out4 <- out3 |> 
  group_by(is_reversal, ema_number) |> 
  summarize(
  ch = mean(mood_ch, trim = 0.2),
  mpre = mean(moodpre),
  mpost = mean(moodpost),
  final = mean(fs)
  ) |> 
  ungroup()

out4 %>%
  ggplot(aes(x = ema_number, y = ch)) +
  geom_point() +
  geom_line() +
  theme(panel.grid = element_blank()) 
  
subset_it <- unique(out3$user_id)[1:9]
temp <- out3[out3$user_id %in% subset_it, ]

temp %>%
  ggplot(aes(x = ema_number, y = mood_ch)) +
  geom_point() +
  geom_line() +
  # coord_cartesian(ylim = c(1, 4)) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~user_id)


out3 |> # data set
  ggplot(aes(x = ema_number, y = mood_ch, group = user_id)) + # setting variables
  geom_point(size = .5) + # adding points to plot
  geom_line(alpha=0.2) + # adding lines to plot
  # setting the x-axis with breaks and labels
  scale_x_continuous(
    limits = c(1, 10),
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    name = "EMA Number"
  ) +
  # setting the y-axis with limits breaks and labels
  scale_y_continuous(
    limits = c(-60, 60),
    # breaks = c(10, 30, 50, 70, 90),
    name = "Mood Change"
  )


m <- lmer(mood_ch ~ ema_number + (ema_number | user_id), data = out3)
summary(m)

tmp <- meanDecompose(ema_number ~ user_id, data = out3)

plot(
  testDistribution(
    tmp[["ema_number by user_id"]]$X,
    extremevalues = "theoretical", ev.perc = .001
  ),
  varlab = "Between Person Math Scores"
)

plot(
  testDistribution(
    tmp[["ema_number by residual"]]$X,
    extremevalues = "theoretical", ev.perc = .001
  ),
  varlab = "Within Person Math Scores"
)

iccMixed(
  dv = "mood_ch",
  id = c("user_id"),
  data = out3
) |>
  print()

modelPerformance(m) 


dat <- out3 |>
  dplyr::select(user_id, ema_number, mood_ch)


dat_wide <- dat %>%
  pivot_wider(
    names_from = ema_number,      # La colonna che diventerà le nuove intestazioni delle colonne
    values_from = mood_ch         # La colonna da cui prendere i valori per riempire la tabella
  )

colnames(dat_wide) <- c(
  "user_id", "chmood1", "chmood2", "chmood3", "chmood4", "chmood5", 
  "chmood6", "chmood7", "chmood8", "chmood9", "chmood10"
)

# Convert all character columns to factors
dat_wide[] <- lapply(dat_wide, function(x) if(is.character(x)) factor(x) else x)

dat_wide8 <- dat_wide |> 
  dplyr::select(-c(chmood9, chmood10))

# Deterministic regression imputation
# imp <- mice(temp, method = "norm.predict", m = 1) # Impute data
imputed_data <- mice(dat_wide8, m=1, method='pmm', seed=1234)

d_imp <- complete(imputed_data)

plot_trajectories(
  data = dat_wide8,
  id_var = "user_id",
  var_list = x_var_list,
  xlab = "Time", ylab = "Value",
  connect_missing = FALSE,
  # random_sample_frac = 0.018,
  title_n = TRUE
)

x_var_list <- paste0("chmood", 1:8)
x_var_list

uni_lavaan_results <- fit_uni_lcsm(
  data = dat_wide8, 
  missing="fiml",
  var = x_var_list,
  model = list(alpha_constant = TRUE, 
               beta = TRUE, 
               phi = TRUE)
)

uni_lavaan_syntax <- fit_uni_lcsm(
  data = d_imp,
  var = x_var_list,
  model = list(
    alpha_constant = TRUE,
    beta = TRUE,
    phi = TRUE
  ),
  return_lavaan_syntax = TRUE
)

plot_lcsm(
  lavaan_object = uni_lavaan_results,
  lavaan_syntax = uni_lavaan_syntax,
  edge.label.cex = .9,
  lcsm_colours = TRUE,
  lcsm = "univariate"
)

extract_fit(uni_lavaan_results)

summary(uni_lavaan_results)

mod <- '
  # Specify latent true scores 
  lx1 =~ 1 * x1 
  lx2 =~ 1 * x2 
  lx3 =~ 1 * x3 
  lx4 =~ 1 * x4 
  lx5 =~ 1 * x5 
  lx6 =~ 1 * x6 
  lx7 =~ 1 * x7 
  lx8 =~ 1 * x8 
  # Specify mean of latent true scores 
  lx1 ~ gamma_lx1 * 1 
  lx2 ~ 0 * 1 
  lx3 ~ 0 * 1 
  lx4 ~ 0 * 1 
  lx5 ~ 0 * 1 
  lx6 ~ 0 * 1 
  lx7 ~ 0 * 1 
  lx8 ~ 0 * 1 
  # Specify variance of latent true scores 
  lx1 ~~ sigma2_lx1 * lx1 
  lx2 ~~ 0 * lx2 
  lx3 ~~ 0 * lx3 
  lx4 ~~ 0 * lx4 
  lx5 ~~ 0 * lx5 
  lx6 ~~ 0 * lx6 
  lx7 ~~ 0 * lx7 
  lx8 ~~ 0 * lx8 
  # Specify intercept of obseved scores 
  x1 ~ 0 * 1 
  x2 ~ 0 * 1 
  x3 ~ 0 * 1 
  x4 ~ 0 * 1 
  x5 ~ 0 * 1 
  x6 ~ 0 * 1 
  x7 ~ 0 * 1 
  x8 ~ 0 * 1 
  # Specify variance of observed scores 
  x1 ~~ sigma2_ux * x1 
  x2 ~~ sigma2_ux * x2 
  x3 ~~ sigma2_ux * x3 
  x4 ~~ sigma2_ux * x4 
  x5 ~~ sigma2_ux * x5 
  x6 ~~ sigma2_ux * x6 
  x7 ~~ sigma2_ux * x7 
  x8 ~~ sigma2_ux * x8 
  # Specify autoregressions of latent variables 
  lx2 ~ 1 * lx1 
  lx3 ~ 1 * lx2 
  lx4 ~ 1 * lx3 
  lx5 ~ 1 * lx4 
  lx6 ~ 1 * lx5 
  lx7 ~ 1 * lx6 
  lx8 ~ 1 * lx7 
  # Specify latent change scores 
  dx2 =~ 1 * lx2 
  dx3 =~ 1 * lx3 
  dx4 =~ 1 * lx4 
  dx5 =~ 1 * lx5 
  dx6 =~ 1 * lx6 
  dx7 =~ 1 * lx7 
  dx8 =~ 1 * lx8 
  # Specify latent change scores means 
  dx2 ~ 0 * 1 
  dx3 ~ 0 * 1 
  dx4 ~ 0 * 1 
  dx5 ~ 0 * 1 
  dx6 ~ 0 * 1 
  dx7 ~ 0 * 1 
  dx8 ~ 0 * 1 
  # Specify latent change scores variances 
  dx2 ~~ 0 * dx2 
  dx3 ~~ 0 * dx3 
  dx4 ~~ 0 * dx4 
  dx5 ~~ 0 * dx5 
  dx6 ~~ 0 * dx6 
  dx7 ~~ 0 * dx7 
  dx8 ~~ 0 * dx8 
  # Specify constant change factor 
  g2 =~ 1 * dx2 + 1 * dx3 + 1 * dx4 + 1 * dx5 + 1 * dx6 + 1 * dx7 + 1 * dx8 
  # Specify constant change factor mean 
  g2 ~ alpha_g2 * 1 
  # Specify constant change factor variance 
  g2 ~~ sigma2_g2 * g2 
  # Specify constant change factor covariance with the initial true score 
  g2 ~~ sigma_g2lx1 * lx1
  # Specify proportional change component 
  dx2 ~ beta_x * lx1 
  dx3 ~ beta_x * lx2 
  dx4 ~ beta_x * lx3 
  dx5 ~ beta_x * lx4 
  dx6 ~ beta_x * lx5 
  dx7 ~ beta_x * lx6 
  dx8 ~ beta_x * lx7 
  # Specify autoregression of change score 
  dx3 ~ phi_x * dx2 
  dx4 ~ phi_x * dx3 
  dx5 ~ phi_x * dx4 
  dx6 ~ phi_x * dx5 
  dx7 ~ phi_x * dx6 
  dx8 ~ phi_x * dx7 
'

new_var_names <- c("id", paste0("x", 1:8))
newdat <- dat_wide8
names(newdat) <- new_var_names

fit = sem(mod, newdat, missing="fiml")


fs = lavPredict(uni_lavaan_results)
head(fs)

lx_fs = fs[, 1:8]
dd <- as_tibble(lx_fs)

dd$id <- 1:dim(lx_fs)[1]
vlist = paste0("lx", 1:8)

plot_trajectories(
  data = dd,
  id_var = "id",
  var_list = vlist,
  xlab = "Time", ylab = "Value",
  connect_missing = FALSE,
  # random_sample_frac = 0.018,
  title_n = TRUE
)

dd_long <- dd %>%
  pivot_longer(cols = starts_with("lx"), names_to = "time", values_to = "value")

summary_data <- dd_long %>%
  group_by(time) %>%
  summarise(
    mean = mean(value),
    se = sd(value) / sqrt(n())  # Standard error calculation
  )


ggplot(summary_data, aes(x = time, y = mean, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(title = "Average Values Over Time with Standard Errors",
       x = "Time Point",
       y = "Mean Value") 

glimpse(dd_long)

hist(dd_long$value)

dd_long$time <- as.numeric(dd_long$time)
fm <- lmer(value ~ time + (1| id), data = dd_long)
summary(fm)

qqnorm(resid(fm))
qqline(resid(fm))

plot(fitted(fm), resid(fm))
abline(h = 0, col = "red")

plot(predict(fm), dd_long$value)

# -------------------------
# Condizione di no reversal
# -------------------------

# Il modello non converge. Ci sono troppo pochi soggetti.

