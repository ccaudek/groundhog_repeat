# This script runs a latent growth model for the participants' accuracy
# as a function of EMA session.
#
# Date: Thu May 16 05:00:59 2024

# -------------------
# Latent Growth Model
# -------------------

# Load necessary packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(lavaan)
  library(mice)
  library(lcsm)
  library(semPlot)
})

# Load data
d <- rio::import(
  here::here("data", "prep", "prl", "groundhog_clean_rev_norev.csv")
)

# scs_df <- rio::import(
#   here::here("data", "prep", "quest_scales", "scs_scores.csv")
# ) |>
#   dplyr::select(user_id, scs_total_score)
# 
# d1 <- left_join(d, scs_df, by = "user_id")
# 
# # Impute missing data
# imputed_data <- mice(d1, m = 1, method = 'pmm', seed = 1234)
# 
# d_imp <- complete(imputed_data)

# Select the first 10 EMA sessions and remove participants with low performance
out <- d %>%
  filter(ema_number < 11) %>%
  group_by(is_reversal, user_id) %>%
  summarize(fdbk = mean(feedback), .groups = 'drop')

plot(density(out$fdbk))

out_clean <- out %>%
  filter(fdbk > 0.44)

good_user_id <- na.omit(unique(out_clean$user_id))

df <- d %>%
  filter(user_id %in% good_user_id)

# Calculate accuracy and remove low performers
out2 <- df %>%
  filter(ema_number < 11) %>%
  group_by(user_id) %>%
  summarize(acc = mean(feedback), n = n(), .groups = 'drop')

plot(density(out2$acc))

out2_clean <- out2 %>%
  filter(acc > 0.44)

good2_user_id <- na.omit(unique(out2_clean$user_id))

df3 <- df %>%
  filter(user_id %in% good2_user_id)

# Summarize for reversal condition only
out3 <- df3 %>%
  filter(ema_number < 11, is_reversal == 1) %>%
  group_by(user_id, ema_number) %>%
  summarize(
    acc = mean(feedback),
    mood_ch = mean(mood_change),
    moodpre = mean(mood_pre),
    moodpost = mean(mood_post),
    fs = mean(final_score),
    .groups = 'drop'
  )

# Convert data to wide format for LGM
dat <- out3 %>%
  select(user_id, ema_number, acc)

dat_wide <- dat %>%
  pivot_wider(names_from = ema_number, values_from = acc, names_prefix = "acc")

colnames(dat_wide) <- c("user_id", paste0("acc", 1:10))

# Select the first 8 EMA points
dat_wide8 <- dat_wide %>%
  select(user_id, acc1:acc8)

lg_acc_lavaan_model <- '
  # latent variable definitions
      #intercept (note intercept is a reserved term)
      eta_1 =~ 1*acc1
      eta_1 =~ 1*acc2
      eta_1 =~ 1*acc3
      eta_1 =~ 1*acc4
      eta_1 =~ 1*acc5
      eta_1 =~ 1*acc6
      eta_1 =~ 1*acc7
      eta_1 =~ 1*acc8

      #slope 
      eta_2 =~ 0*acc1
      eta_2 =~ acc2
      eta_2 =~ acc3
      eta_2 =~ acc4
      eta_2 =~ acc5
      eta_2 =~ acc6
      eta_2 =~ acc7
      eta_2 =~ 1*acc8

  # factor variances
      eta_1 ~~ eta_1
      eta_2 ~~ eta_2

  # covariances among factors 
      eta_1 ~~ eta_2

  # factor means 
      eta_1 ~ 1
      eta_2 ~ 1

  # manifest variances (made equivalent by naming theta)
      acc1 ~~ theta*acc1
      acc2 ~~ theta*acc2
      acc3 ~~ theta*acc3
      acc4 ~~ theta*acc4
      acc5 ~~ theta*acc5
      acc6 ~~ theta*acc6
      acc7 ~~ theta*acc7
      acc8 ~~ theta*acc8
  # manifest means (fixed at zero)
      acc1 ~ 0*1
      acc2 ~ 0*1
      acc3 ~ 0*1
      acc4 ~ 0*1
      acc5 ~ 0*1
      acc6 ~ 0*1
      acc7 ~ 0*1
      acc8 ~ 0*1
' #end of model definition



# Fit the latent growth model
fit <- sem(
  lg_acc_lavaan_model, 
  data = dat_wide8, 
  missing = "fiml", 
  meanstructure = TRUE
)

# Summarize the model fit
summary(fit, fit.measures = TRUE, standardized = TRUE)

print(fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr")))

semPaths(fit, what = "path", whatLabels = "par")

# Change the class from integer64 to integer
dat_wide8$user_id <- as.numeric(dat_wide8$user_id)

acc_predicted <- as.data.frame(
  cbind(dat_wide8$user_id, lavPredict(fit))
)

#naming columns
names(acc_predicted) <- c("id", "eta_1", "eta_2")

head(acc_predicted)

#calculating implied manifest scores
acc_predicted$acc1 <- 1 * acc_predicted$eta_1 + 0 * acc_predicted$eta_2
acc_predicted$acc2 <- 1 * acc_predicted$eta_1 + 0.273 * acc_predicted$eta_2
acc_predicted$acc3 <- 1 * acc_predicted$eta_1 + 0.620 * acc_predicted$eta_2
acc_predicted$acc4 <- 1 * acc_predicted$eta_1 + 1.006 * acc_predicted$eta_2
acc_predicted$acc5 <- 1 * acc_predicted$eta_1 + 0.902 * acc_predicted$eta_2
acc_predicted$acc6 <- 1 * acc_predicted$eta_1 + 0.923 * acc_predicted$eta_2
acc_predicted$acc7 <- 1 * acc_predicted$eta_1 + 1.039 * acc_predicted$eta_2
acc_predicted$acc8 <- 1 * acc_predicted$eta_1 + 1.0 * acc_predicted$eta_2

# reshaping wide to long
acc_predicted_long <- reshape(
  data = acc_predicted,
  timevar = c("ema_number"),
  idvar = "id",
  varying = c(
    "acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7", "acc8"
  ),
  direction = "long", sep = ""
)

head(acc_predicted_long)

acc_predicted_long <-
  acc_predicted_long[order(acc_predicted_long$id, acc_predicted_long$ema_number), ]

head(acc_predicted_long)


ggplot(
  data = acc_predicted_long, # data set
  aes(x = ema_number, y = acc, group = id)
) + # setting variables
  # geom_point(size=.5) + #adding points to plot
  geom_line(alpha = 0.15) 


# eof --





















# ---------------------------------------------------------
# The predicted values are not bounded in the [0, 1] range
# ---------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(lavaan)
  library(mice)
  library(semPlot)
})

# Load data
d <- rio::import(here::here("data", "prep", "prl", "groundhog_clean_rev_norev.csv"))

# Select the first 10 EMA sessions and remove participants with low performance
out <- d %>%
  filter(ema_number < 11) %>%
  group_by(is_reversal, user_id) %>%
  summarize(fdbk = mean(feedback), .groups = 'drop')

plot(density(out$fdbk))

out_clean <- out %>%
  filter(fdbk > 0.44)

good_user_id <- na.omit(unique(out_clean$user_id))

df <- d %>%
  filter(user_id %in% good_user_id)

# Calculate accuracy and remove low performers
out2 <- df %>%
  filter(ema_number < 11) %>%
  group_by(user_id) %>%
  summarize(acc = mean(feedback), n = n(), .groups = 'drop')

plot(density(out2$acc))

out2_clean <- out2 %>%
  filter(acc > 0.44)

good2_user_id <- na.omit(unique(out2_clean$user_id))

df3 <- df %>%
  filter(user_id %in% good2_user_id)

# Summarize for reversal condition
out3 <- df3 %>%
  filter(ema_number < 11, is_reversal == 1) %>%
  group_by(user_id, ema_number) %>%
  summarize(
    acc = mean(feedback),
    mood_ch = mean(mood_change),
    moodpre = mean(mood_pre),
    moodpost = mean(mood_post),
    fs = mean(final_score),
    .groups = 'drop'
  )

# Convert data to wide format for LGM
dat <- out3 %>%
  select(user_id, ema_number, acc)

dat_wide <- dat %>%
  pivot_wider(names_from = ema_number, values_from = acc, names_prefix = "acc")

colnames(dat_wide) <- c("user_id", paste0("acc", 1:10))

# Select the first 8 EMA points
dat_wide8 <- dat_wide %>%
  select(user_id, acc1:acc8)

# Impute missing data
imputed_data <- mice(dat_wide8, m = 1, method = 'pmm', seed = 1234)

d_imp <- complete(imputed_data)

lg_acc_lavaan_model <- '
  # latent variable definitions
      # intercept (note intercept is a reserved term)
      eta_1 =~ 1*acc1
      eta_1 =~ 1*acc2
      eta_1 =~ 1*acc3
      eta_1 =~ 1*acc4
      eta_1 =~ 1*acc5
      eta_1 =~ 1*acc6
      eta_1 =~ 1*acc7
      eta_1 =~ 1*acc8

      # slope 
      eta_2 =~ 0*acc1
      eta_2 =~ acc2
      eta_2 =~ acc3
      eta_2 =~ acc4
      eta_2 =~ acc5
      eta_2 =~ acc6
      eta_2 =~ acc7
      eta_2 =~ 1*acc8

  # factor variances
      eta_1 ~~ eta_1
      eta_2 ~~ eta_2

  # covariances among factors 
      eta_1 ~~ eta_2

  # factor means 
      eta_1 ~ 1
      eta_2 ~ 1

  # manifest variances (made equivalent by naming theta)
      acc1 ~~ theta*acc1
      acc2 ~~ theta*acc2
      acc3 ~~ theta*acc3
      acc4 ~~ theta*acc4
      acc5 ~~ theta*acc5
      acc6 ~~ theta*acc6
      acc7 ~~ theta*acc7
      acc8 ~~ theta*acc8
  # manifest means (fixed at zero)
      acc1 ~ 0*1
      acc2 ~ 0*1
      acc3 ~ 0*1
      acc4 ~ 0*1
      acc5 ~ 0*1
      acc6 ~ 0*1
      acc7 ~ 0*1
      acc8 ~ 0*1
' # end of model definition

# Fit the latent growth model
fit <- sem(lg_acc_lavaan_model, data = dat_wide8, missing = "fiml", meanstructure = TRUE)

# Summarize the model fit
summary(fit, fit.measures = TRUE, standardized = TRUE)

print(fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea")))

# Plot the path diagram
semPaths(fit, what = "path", whatLabels = "par")

# Change the class from integer64 to integer
dat_wide8$user_id <- as.numeric(dat_wide8$user_id)

# Predict factor scores (latent intercept and slope)
acc_predicted <- as.data.frame(
  cbind(dat_wide8$user_id, lavPredict(fit))
)

# Naming columns
names(acc_predicted) <- c("id", "eta_1", "eta_2")

# Calculating implied manifest scores
acc_predicted$acc1 <- 1 * acc_predicted$eta_1 + 0 * acc_predicted$eta_2
acc_predicted$acc2 <- 1 * acc_predicted$eta_1 + 1 * acc_predicted$eta_2
acc_predicted$acc3 <- 1 * acc_predicted$eta_1 + 2 * acc_predicted$eta_2
acc_predicted$acc4 <- 1 * acc_predicted$eta_1 + 3 * acc_predicted$eta_2
acc_predicted$acc5 <- 1 * acc_predicted$eta_1 + 4 * acc_predicted$eta_2
acc_predicted$acc6 <- 1 * acc_predicted$eta_1 + 5 * acc_predicted$eta_2
acc_predicted$acc7 <- 1 * acc_predicted$eta_1 + 6 * acc_predicted$eta_2
acc_predicted$acc8 <- 1 * acc_predicted$eta_1 + 7 * acc_predicted$eta_2

# Applying logistic transformation to restrict values to [0, 1]
acc_predicted <- acc_predicted %>%
  mutate_at(vars(starts_with("acc")), ~1 / (1 + exp(-.)))

# Reshaping wide to long
acc_predicted_long <- reshape(
  data = acc_predicted,
  timevar = c("ema_number"),
  idvar = "id",
  varying = c("acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7", "acc8"),
  direction = "long", sep = ""
)

# Ordering the data
acc_predicted_long <- acc_predicted_long[order(acc_predicted_long$id, acc_predicted_long$ema_number), ]

# Plot the predicted values
ggplot(data = acc_predicted_long, aes(x = ema_number, y = acc, group = id)) +
  geom_line(alpha = 0.15) +
  labs(title = "Predicted Accuracy Over Time",
       x = "Time (EMA Number)",
       y = "Predicted Accuracy") 



