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
set.seed(10)
throws <- throw_dice(20)

tibble(sum = throws) |>
  ggplot(aes(x = sum)) +
  geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
  scale_x_continuous(breaks = 2:12) +
  labs(
    title = "Distribution of 20 Dice Rolls",
    subtitle = "With only 20 rolls, the sample does not yet reflect the true distribution.",
    x = "Sum",
    y = "Count"
  )
#
#
#
#
#
#
