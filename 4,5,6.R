library(tidyverse)
library(moderndive)
library(infer)
library(gridExtra)
hunde <- read.table("../hunde.txt", sep = "\t", header = T)

hunde %>% ggplot(aes(x = race, y = maxLA)) + 
  geom_boxplot(outlier.color =  "white") +
  geom_jitter( width = 0.25) +
  theme_classic()

hunde %>% ggplot(aes(x = wgt, y = maxLA, color = race)) +
  geom_point() +
  theme_classic()

hunde %>% group_by(race) %>% 
  summarise(n = n(),
            min_wgt = min(wgt),
            max_wgt = max(wgt),
            min_maxLA = min(maxLA),
            max_maxLA = max(maxLA))


lm_0 <- lm(maxLA ~ 1, data = hunde)
X0 <- model.matrix(lm_0)
n <- nrow(X0)

set.seed(2022)
B <- 10000
my_boot <- tibble(residuals = residuals(lm_0)) %>%
  rep_sample_n(size = n, replace = TRUE, reps = B) %>%
  mutate(y = fitted(lm_0) + residuals)

lm_fit <- lm(maxLA ~ race, data = hunde)
X <- model.matrix(lm_fit)

F_test <- function(lm_null, lm_full) {
  p <- lm_full$rank
  q <- lm_null$rank
  
  lm_full$df.residual * sum((lm_full$fitted.values -
                               lm_null$fitted.values)^2)/(sum(lm_full$residuals^2) *
                                                            (p - q))
}

my_res <- my_boot %>%
  summarize(F = F_test(lm.fit(X0, y), lm.fit(X, y)))

F_obs <- F_test(lm.fit(X0, hunde$maxLA), lm.fit(X, hunde$maxLA))
p_value <- sum(my_res$F > F_obs) / B
p_value

ggplot(data = my_res) + 
  geom_histogram(aes(x = F, y = ..density..),
                 color = "white", fill = "steelblue", bins = 100) + 
  labs(x = "F") +
  theme_bw() +
  geom_vline(xintercept = F_obs, color = "red")
#+ 
  stat_function(fun = df, args = list(df1 = 25 - 16, df2 = 50 - 25), 
                geom = "area", fill = "pink", color = "blue", alpha = 0.25)