## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(latent2likert) # Load the package
set.seed(12345) # Ensure reproducible results

# Generate responses for Course A and Course B
responses_A <- rlikert(size = 10, n_items = 1, n_levels = 4, mean = 0, sd = 1)
responses_B <- rlikert(size = 20, n_items = 1, n_levels = 4, mean = 1, sd = 1)

## -----------------------------------------------------------------------------
n_levels <- 4
n_groups <- 2
categories <- c("Poor", "Fair", "Good", "Excellent")

# Create a data frame to summarize the responses
response_data <- data.frame(
  Course = rep(c("A", "B"), each = n_levels),
  Response = factor(rep(categories, n_groups), levels = categories),
  Proportion = c(
    response_prop(responses_A, n_levels),
    response_prop(responses_B, n_levels)
  )
)

# Filter out rows with zero proportions
response_data <- response_data[response_data$Proportion > 0, ]
response_data

## -----------------------------------------------------------------------------
# Pre- and post-assessments of skills for Course A
pre_A <- rlikert(size = 10, n_items = 3, n_levels = 5, mean = c(-1, 0, 1))
post_A <- rlikert(size = 10, n_items = 3, n_levels = 5, mean = c(0, 0, 2))

# Pre- and post-assessments of skills for Course B
pre_B <- rlikert(size = 20, n_items = 3, n_levels = 5, mean = c(-1, 0, 1))
post_B <- rlikert(size = 20, n_items = 3, n_levels = 5, mean = c(0, 0, 0))

## -----------------------------------------------------------------------------
# Combine pre and post responses into a list
pre_post <- list(pre_A, post_A, pre_B, post_B)

# Number of items and response levels
n_items <- 3
n_levels <- 5

# Define skills assessed
skills <- c("Programming", "Searching online", "Solving problems")

# Generate repeated skill labels for questions
questions <- rep(rep(skills, each = n_levels), 4)
questions <- factor(questions, levels = skills)

# Create a data frame to summarize the responses
response_data <- data.frame(
  Course = rep(c("Course A", "Course B"), each = 2 * n_items * n_levels),
  Question = questions,
  Time = as.factor(rep(c(
    rep("before", n_items * n_levels),
    rep("after", n_items * n_levels)
  ), 2)),
  Response = rep(seq_len(n_levels), 2 * n_items * 2),
  Proportion = as.vector(sapply(pre_post, function(d) {
    as.vector(t(response_prop(d, n_levels)))
  }))
)

head(response_data)

## -----------------------------------------------------------------------------
data(part_bfi)
head(part_bfi)

## -----------------------------------------------------------------------------
vars <- c("A1", "A2", "A3", "A4", "A5")
items_male <- part_bfi[part_bfi$gender == 0, vars]
items_female <- part_bfi[part_bfi$gender == 1, vars]

## -----------------------------------------------------------------------------
params_male <- estimate_params(data = items_male, n_levels = 6)
params_female <- estimate_params(data = items_female, n_levels = 6)

## -----------------------------------------------------------------------------
set.seed(12345) # Ensure reproducible results

new_items_male <- rlikert(
  size = nrow(items_male),
  n_items = 5,
  n_levels = 6,
  mean = params_male["mean", ],
  sd = params_male["sd", ],
  corr = cor(items_male)
)

new_items_female <- rlikert(
  size = nrow(items_female),
  n_items = 5,
  n_levels = 6,
  mean = params_female["mean", ],
  sd = params_female["sd", ],
  corr = cor(items_female)
)

## -----------------------------------------------------------------------------
# Combine new items and gender in new data frame
new_data <- data.frame(rbind(new_items_male, new_items_female))
new_data$gender <- c(rep(0, nrow(items_male)), rep(1, nrow(items_female)))
head(new_data)

# Reverse the first item because it has negative correlations
part_bfi$A1 <- (min(part_bfi$A1) + max(part_bfi$A1)) - part_bfi$A1
new_data$Y1 <- (min(new_data$Y1) + max(new_data$Y1)) - new_data$Y1

# Create agreeableness scale scores
part_bfi$agreeable <- rowMeans(part_bfi[, vars])
new_data$agreeable <- rowMeans(new_data[, c("Y1", "Y2", "Y3", "Y4", "Y5")])

