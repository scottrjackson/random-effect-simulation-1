library(tidyverse)
library(lme4)
library(lmerTest)
library(car)

## Practice Exercise #1 ##############################################################
# 1. Function for simulating random-intercept data

simulate_mem_randomintercept <- function(beta_1,
                                         random_intercept_mean,
                                         random_intercept_sd,
                                         error_sd,
                                         n_subjects) {
    subjects <- paste0("s", 1:n_subjects)
    conditions <- 0:1
    df <- expand.grid(subjects, conditions, stringsAsFactors = FALSE)
    colnames(df) <- c("subject", "condition")

    ranefs_bysubject <- data.frame(subject = subjects,
                                   intercept_bysubject = rnorm(length(subjects),
                                                               random_intercept_mean,
                                                               random_intercept_sd),
                                   stringsAsFactors = FALSE)
    df <- df %>% left_join(ranefs_bysubject, by = "subject")

    df$y <- df$intercept_bysubject +      # random intercepts
            beta_1 * df$condition +       # effect of condition
            rnorm(nrow(df), 0, error_sd)  # residual errors

    return(df)
}

# 2. Test out function

# a. Data with 10 subjects

simdata_s10 <- simulate_mem_randomintercept(beta_1 = 150,
                                            random_intercept_mean = 400,
                                            random_intercept_sd = 50,
                                            error_sd = 25,
                                            n_subjects = 10)
print(simdata_s10)
summary(simdata_s10)

# b. Data with 1,000 subjects, then fit lmer(), check parameters

simdata_s1000 <- simulate_mem_randomintercept(beta_1 = 150,
                                              random_intercept_mean = 400,
                                              random_intercept_sd = 50,
                                              error_sd = 25,
                                              n_subjects = 1000)
summary(lmer(y ~ condition + (1|subject), data = simdata_s1000), corr = FALSE)

# 3. Alternate version of simulation function

simulate_mem_randomintercept_v2 <- function(beta_0,
                                            beta_1, # this replaces the random intercept mean
                                            random_intercept_sd,
                                            error_sd,
                                            n_subjects) {
    subjects <- paste0("s", 1:n_subjects)
    conditions <- 0:1
    df <- expand.grid(subjects, conditions, stringsAsFactors = FALSE)
    colnames(df) <- c("subject", "condition")

    ranefs_bysubject <- data.frame(subject = subjects,
                                   intercept_bysubject = rnorm(length(subjects),
                                                               0, # mean of zero since the ranef is a difference
                                                               random_intercept_sd),
                                   stringsAsFactors = FALSE)
    df <- df %>% left_join(ranefs_bysubject, by = "subject")

    df$y <- (beta_0 + df$intercept_bysubject) + # here's where we add the fixed effect and random effect
            beta_1 * df$condition +             # effect of condition
            rnorm(nrow(df), 0, error_sd)        # residual errors

    return(df)
}

# 4. Check that v2 works the same

simdata_s1000_v2 <- simulate_mem_randomintercept_v2(beta_0 = 400,
                                                    beta_1 = 150,
                                                    random_intercept_sd = 50,
                                                    error_sd = 25,
                                                    n_subjects = 1000)
summary(lmer(y ~ condition + (1|subject), data = simdata_s1000_v2), corr = FALSE)

simdata_s5_v2 <- simulate_mem_randomintercept_v2(400, 150, 50, 25, 5)
print(simdata_s5_v2)
summary(simdata_s5_v2)
summary(simdata_s1000_v2)

# 5. Back to `sleep`
mysleep <- sleep

# a. 
sleep_ttest <- t.test(extra ~ group, data = mysleep)

# b. 
sleep_lm <- lm(extra ~ group, data = mysleep)

# c. 
sleep_anova <- aov(extra ~ 1 + group, data = mysleep)
sleep_anova2 <- anova(sleep_lm)

# d. 
sleep_paired_ttest <- t.test(extra ~ 1 + group, data = mysleep, paired = TRUE)

# e.
sleep_anova_within <- aov(extra ~ 1 + group + Error(ID), data = mysleep)

# f. 
sleep_mem <- lmer(extra ~ 1 + group + (1|ID), data = mysleep)

# 6. 
# the "unpaired"/"omnibus" analyses are all the same
print(sleep_ttest)
summary(sleep_lm)
summary(sleep_anova)
print(sleep_anova2)

# the paired/within/ranef analyses are all the same
print(sleep_paired_ttest)
summary(sleep_anova_within)
summary(sleep_mem, corr = FALSE)

## Practice Exercise #2 ############################################################

# 1. Function for by-subject and by-item random effects

simulate_mem_bysubject_byitem <- function(beta_0, # still have a single fixed effect
                                          beta_1, 
                                          random_subject_sd, # still have subject ranef sd 
                                          random_item_sd,    # added item ranef sd
                                          error_sd,
                                          n_subjects,
                                          n_items) { # added number of items
    subjects <- paste0("s", 1:n_subjects)
    items <- paste0("i", 1:n_items)
    conditions <- 0:1
    df <- expand.grid(subjects, items, conditions, stringsAsFactors = FALSE)
    colnames(df) <- c("subject", "item", "condition")

    # still create by-subject random effects table
    ranefs_bysubject <- data.frame(subject = subjects,
                                   intercept_bysubject = rnorm(length(subjects),
                                                               0,
                                                               random_subject_sd),
                                   stringsAsFactors = FALSE)
    
    # add another random effects table, this one by item
    ranefs_byitem <- data.frame(item = items,
                                intercept_byitem = rnorm(length(items),
                                                         0,
                                                         random_item_sd),
                                stringsAsFactors = FALSE)

    # merge both ranef tables with data
    df <- df %>% left_join(ranefs_bysubject, by = "subject") %>%
        left_join(ranefs_byitem, by = "item")

    df$y <- (beta_0 + df$intercept_bysubject +
                      df$intercept_byitem)   +  # need to add both ranefs to fixed intercept
            beta_1 * df$condition +             # effect of condition
            rnorm(nrow(df), 0, error_sd)        # residual errors

    return(df)
}

# 2. Simulate small data set and print
simulate_mem_bysubject_byitem(beta_0 = 500,
                              beta_1 = 100,
                              random_subject_sd = 150,
                              random_item_sd = 75,
                              error_sd = 50,
                              n_subjects = 3,
                              n_items = 3)

# 3. Simulate larger data set and fit model
sim_bysubj200_byitem200 <- simulate_mem_bysubject_byitem(beta_0 = 500,
                                                         beta_1 = 100,
                                                         random_subject_sd = 150,
                                                         random_item_sd = 75,
                                                         error_sd = 50,
                                                         n_subjects = 200,
                                                         n_items = 200)
summary(sim_bysubj200_byitem200)
sim_crossed_fit <- lmer(y ~ 1 + condition + (1|subject) + (1|item),
                        data = sim_bysubj200_byitem200)
summary(sim_crossed_fit, corr = FALSE)


## Practice Exercise #3 ############################################################

# 1. Make function for random slope & intercept

sim_ranef_int_slope <- function(beta_0, beta_1,
                                beta_0_ranef_bysubj_sd, beta_1_ranef_bysubj_sd,
                                error_sd, n_subjects, n_items) {

    subjects <- paste0("s", 1:n_subjects)
    items <- paste0("i", 1:n_items)
    conditions <- 0:1

    df <- expand.grid(subjects, items, conditions,
                      stringsAsFactors = FALSE)
    colnames(df) <- c("subject", "item", "condition")

    ranefs_bysubject <- data.frame(subject = subjects,
                                   beta_0_ranef_bysubj = rnorm(length(subjects),
                                                               0,
                                                               beta_0_ranef_bysubj_sd),
                                   beta_1_ranef_bysubj = rnorm(length(subjects),
                                                               0,
                                                               beta_1_ranef_bysubj_sd),
                                   stringsAsFactors = FALSE)
    df <- df %>% left_join(ranefs_bysubject, by = "subject") %>%
        mutate(y = (beta_0 + beta_0_ranef_bysubj) +
                   (beta_1 + beta_1_ranef_bysubj) * condition +
                   rnorm(nrow(.), 0, error_sd))
    return(df)
}

# 2. Test out function

simdata_int_slope <- sim_ranef_int_slope(beta_0 = 450,
                                         beta_1 = 120,
                                         beta_0_ranef_bysubj_sd = 200,
                                         beta_1_ranef_bysubj_sd = 90,
                                         error_sd = 70,
                                         n_subjects = 300,
                                         n_items = 300)
head(simdata_int_slope)
summary(simdata_int_slope)
fit_int_slope <- lmer(y ~ 1 + condition + (1 + condition | subject),
                      data = simdata_int_slope)
summary(fit_int_slope, corr = FALSE)

# 3. Make function for both by-item and by-subject random intercepts & slopes

sim_ranef_crossed_int_slope <- function(beta_0, beta_1,
                                        beta_0_ranef_bysubj_sd, beta_1_ranef_bysubj_sd,
                                        # just add by-item parameters
                                        beta_0_ranef_byitem_sd, beta_1_ranef_byitem_sd,
                                        error_sd, n_subjects, n_items) {

    subjects <- paste0("s", 1:n_subjects)
    items <- paste0("i", 1:n_items)
    conditions <- 0:1

    df <- expand.grid(subjects, items, conditions,
                      stringsAsFactors = FALSE)
    colnames(df) <- c("subject", "item", "condition")

    ranefs_bysubject <- data.frame(subject = subjects,
                                   beta_0_ranef_bysubj = rnorm(length(subjects),
                                                               0,
                                                               beta_0_ranef_bysubj_sd),
                                   beta_1_ranef_bysubj = rnorm(length(subjects),
                                                               0,
                                                               beta_1_ranef_bysubj_sd),
                                   stringsAsFactors = FALSE)
    ranefs_byitem <- data.frame(item = items,
                                beta_0_ranef_byitem = rnorm(length(items),
                                                            0,
                                                            beta_0_ranef_byitem_sd),
                                beta_1_ranef_byitem = rnorm(length(items),
                                                            0,
                                                            beta_1_ranef_byitem_sd),
                                stringsAsFactors = FALSE)
    df <- df %>% left_join(ranefs_bysubject, by = "subject") %>%
        left_join(ranefs_byitem, by = "item") %>%
        mutate(y = (beta_0 + beta_0_ranef_bysubj + beta_0_ranef_byitem) +
                   (beta_1 + beta_1_ranef_bysubj + beta_1_ranef_byitem) * condition +
                   rnorm(nrow(.), 0, error_sd))
    return(df)
}

# 4. Test out crossed ranef function

# a. non-zero ranefs
simdata_crossed_int_slope <- sim_ranef_crossed_int_slope(beta_0 = 450,
                                                         beta_1 = 120,
                                                         beta_0_ranef_bysubj_sd = 150,
                                                         beta_1_ranef_bysubj_sd = 80,
                                                         beta_0_ranef_byitem_sd = 100,
                                                         beta_1_ranef_byitem_sd = 30,
                                                         error_sd = 50,
                                                         n_subjects = 300,
                                                         n_items = 300)
head(simdata_crossed_int_slope)
summary(simdata_crossed_int_slope)

system.time(fit_crossed_int_slope <- lmer(y ~ 1 + condition +
                                             (1 + condition | subject) +
                                             (1 + condition | item),
                                          data = simdata_crossed_int_slope))
system.time(fit_crossed_int_slope_uncorr <- lmer(y ~ 1 + condition +
                                                    (1 + condition || subject) +
                                                    (1 + condition || item),
                                                 data = simdata_crossed_int_slope))
summary(fit_crossed_int_slope)
summary(fit_crossed_int_slope_uncorr)

# b. zero by-item variance
simdata_crossed_int_slope_zeroitem <-
    sim_ranef_crossed_int_slope(beta_0 = 450,
                                beta_1 = 120,
                                beta_0_ranef_bysubj_sd = 150,
                                beta_1_ranef_bysubj_sd = 80,
                                beta_0_ranef_byitem_sd = 0,
                                beta_1_ranef_byitem_sd = 0,
                                error_sd = 50,
                                n_subjects = 300,
                                n_items = 300)

system.time(fit_zeroitemranef <- lmer(y ~ 1 + condition +
                                          (1 + condition || subject) +
                                          (1 + condition || item),
                                      data = simdata_crossed_int_slope_zeroitem))
summary(fit_zeroitemranef)

# c. zero by-subject variance
simdata_crossed_int_slope_zerosubj <-
    sim_ranef_crossed_int_slope(beta_0 = 450,
                                beta_1 = 120,
                                beta_0_ranef_bysubj_sd = 0,
                                beta_1_ranef_bysubj_sd = 0,
                                beta_0_ranef_byitem_sd = 100,
                                beta_1_ranef_byitem_sd = 30,
                                error_sd = 50,
                                n_subjects = 300,
                                n_items = 300)

system.time(fit_zerosubjranef <- lmer(y ~ 1 + condition +
                                          (1 + condition || subject) +
                                          (1 + condition || item),
                                      data = simdata_crossed_int_slope_zerosubj))
summary(fit_zerosubjranef)

## Practice Exercise #4 ##############################################################
# 1. Simulate some random-intercept data

simdata_randint <-
    sim_ranef_crossed_int_slope(beta_0 = 100,
                                beta_1 = 50,
                                beta_0_ranef_bysubj_sd = 75,
                                beta_1_ranef_bysubj_sd = 0,
                                beta_0_ranef_byitem_sd = 0,
                                beta_1_ranef_byitem_sd = 0,
                                error_sd = 50,
                                n_subjects = 10,
                                n_items = 100)

# 2. Fit no-pooling model, compare estimates to simulation ranefs
simdata_randint$subject_sum <- as.factor(simdata_randint$subject)
contrasts(simdata_randint$subject_sum) <- contr.sum(levels(simdata_randint$subject_sum))
fit_nopool <- lm(y ~ 1 + condition + subject_sum, data = simdata_randint)
summary(fit_nopool)
simdata_randint %>% select(subject, beta_0_ranef_bysubj) %>% unique()

# combine and plot
nopool_estimates <- summary(fit_nopool)$coef %>% as.data.frame %>%
                                      mutate(effect = rownames(.)) %>%
                                      filter(grepl("subject", effect)) %>%
                                      mutate(subject = gsub("subject_sum", "s", effect)) %>%
                                      select(subject, Estimate)
print(nopool_estimates)
sim_params <- simdata_randint %>% select(subject, beta_0_ranef_bysubj) %>% unique()
colnames(sim_params) <- c("subject", "sim_param")
print(sim_params)

combined_sim_nopool <- nopool_estimates %>% left_join(sim_params, by = "subject")

ggplot(combined_sim_nopool, aes(sim_param, Estimate)) + geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()

# 3. Same with MEM, also compare Estimates and BLUPs
fit_mem <- lmer(y ~ 1 + condition + (1|subject), data = simdata_randint)
summary(fit_mem)

partialpool_blups <- ranef(fit_mem)$subject %>% as.data.frame() %>%
                                  mutate(subject = rownames(.)) %>%
                                  select(subject, `(Intercept)`)
colnames(partialpool_blups) <- c("subject", "BLUP")
print(partialpool_blups)

# 4. Compare BLUPs and Estimates vs. "ground truth" parameters
combined_sim_nopool_partialpool <- combined_sim_nopool %>% left_join(partialpool_blups, by = "subject")

ggplot(combined_sim_nopool_partialpool, aes(sim_param, Estimate)) + geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()

windows() # or quartz() or x11()
ggplot(combined_sim_nopool_partialpool, aes(sim_param, BLUP)) + geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()

windows() # or quartz() or x11()
ggplot(combined_sim_nopool_partialpool, aes(BLUP, Estimate)) + geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_minimal()

# BLUP "offset" a result of simulation variance
combined_sim_nopool_partialpool %>% mutate(blup_sim_diff = sim_param - BLUP) %>%
    summarize(mean_blup_sim_diff = mean(blup_sim_diff))
mean(simdata_randint$beta_0_ranef_bysubj)
summary(fit_mem)$coef
