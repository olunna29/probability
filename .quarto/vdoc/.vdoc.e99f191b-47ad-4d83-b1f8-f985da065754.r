#
#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
#| cache: true
throw_dice <- function(n, weights = rep(1/6, 6)) {
  dice1 <- sample(1:6, size = n, replace = TRUE, prob = weights)
  dice2 <- sample(1:6, size = n, replace = TRUE, prob = weights)
  dice1 + dice2
}
#
#
#
#
set.seed(10)

weights <- c(0.25, rep(0.75 / 5, 5))
sample_sizes <- c(10, 20, 50, 100, 1000)

simulations <- tibble(
  n = sample_sizes,
  sums = map(n, ~ throw_dice(.x, weights = weights))
)

simulations |>
  unnest(sums) |>
  mutate(n = factor(n, levels = sample_sizes)) |>
  ggplot(aes(x = sums)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  scale_x_continuous(breaks = 2:12) +
  facet_wrap(~ n, scales = "free_y", ncol = 5) +
  labs(
    title = "Histograms for Different Numbers of Rolls with an Unfair Die",
    subtitle = "Larger samples better reflect the biased distribution of sums.",
    x = "Sum",
    y = "Count"
  )
#
#
#
#
fair_probs <- tibble(
  face = 1:6,
  probability = rep(1 / 6, 6),
  type = "Fair die"
)

loaded_probs <- tibble(
  face = 1:6,
  probability = c(rep(0.14, 5), 0.30),
  type = "Loaded die"
)

probabilities <- bind_rows(fair_probs, loaded_probs)

probabilities |>
  ggplot(aes(x = factor(face), y = probability, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ type) +
  labs(
    title = "Theoretical Probability Distributions",
    subtitle = "The loaded die increases the probability of one face.",
    x = "Face",
    y = "Probability"
  )
#
#
#
#| cache: true
log_lr <- function(rolls) {
  p_loaded <- c(rep(0.14, 5), 0.30)
  p_fair <- rep(1 / 6, 6)
  log(p_loaded[rolls] / p_fair[rolls])
}
#
#
#
#
set.seed(10)

simulate_stopping_time <- function() {
  rolls <- sample(1:6, 200, replace = TRUE, prob = c(0.14, 0.14, 0.14, 0.14, 0.14, 0.30))
  posterior <- cumsum(log_lr(rolls))
  posterior_prob <- 1 / (1 + exp(-posterior))
  which(posterior_prob >= 0.95)[1]
}

stopping_times <- map_int(1:1000, ~ simulate_stopping_time())

tibble(stopping_time = stopping_times) |>
  ggplot(aes(x = stopping_time)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  labs(
    title = "Distribution of the First Roll to Cross a 0.95 Posterior",
    subtitle = "Based on 1,000 repeated experiments with the loaded die.",
    x = "Roll Number",
    y = "Count"
  )
#
#
#
#
#
