library(tidyverse)
library(ISLR2)
library(splines)
library(broom)
library(glue)

mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

make_split <- function(df, train_prop = 0.9, seed = 123) {
  set.seed(seed)
  n <- nrow(df)
  train_idx <- sample(seq_len(n), size = floor(train_prop * n), replace = FALSE)
  list(
    train = df[train_idx, , drop = FALSE],
    test = df[-train_idx, , drop = FALSE],
    train_idx = train_idx
  )
}

make_folds <- function(n, k = 10, seed = 1234) {
  set.seed(seed)
  sample(rep(1:k, length.out = n))
}

make_equally_spaced_knots <- function(x, n_knots) {
  if (n_knots == 1) {
    return(median(range(x)))
  }
  seq(min(x), max(x), length.out = n_knots + 2)[2:(n_knots + 1)]
}

fit_global_poly2 <- function(train_df) {
  lm(mpg ~ poly(horsepower, 2, raw = TRUE), data = train_df)
}

fit_regression_spline <- function(train_df, n_knots) {
  hp <- train_df$horsepower
  knots <- make_equally_spaced_knots(hp, n_knots)
  lm(
    mpg ~ bs(
      horsepower,
      knots = knots,
      Boundary.knots = range(hp) + c(-5, 5)
    ),
    data = train_df
  )
}

fit_smoothing_spline <- function(train_df) {
  smooth.spline(x = train_df$horsepower, y = train_df$mpg, cv = FALSE)
}

fit_local_poly <- function(train_df, degree = 1) {
  loess(mpg ~ horsepower, data = train_df, degree = degree, family = "gaussian")
}

predict_model <- function(model, new_df, model_type, ...) {
  if (model_type %in% c("poly2", "reg_spline", "loess")) {
    return(as.numeric(predict(model, newdata = new_df)))
  }
  if (model_type == "smooth_spline") {
    return(as.numeric(predict(model, x = new_df$horsepower)$y))
  }
  stop("Unknown model_type")
}

cv_regression_splines <- function(train_df, fold_seed = 99, k_folds = 10, knot_grid = 1:10) {
  folds <- make_folds(nrow(train_df), k = k_folds, seed = fold_seed)

  results <- map_dfr(knot_grid, function(knot_count) {
    fold_errors <- map_dbl(1:k_folds, function(fold_id) {
      inner_train <- train_df[folds != fold_id, , drop = FALSE]
      inner_valid <- train_df[folds == fold_id, , drop = FALSE]

      model <- fit_regression_spline(inner_train, n_knots = knot_count)
      preds <- predict_model(model, inner_valid, model_type = "reg_spline")
      mse(inner_valid$mpg, preds)
    })

    tibble(
      knot_count = knot_count,
      cv_mse_mean = mean(fold_errors),
      cv_mse_sd = sd(fold_errors)
    )
  })

  best_row <- results %>% arrange(cv_mse_mean, knot_count) %>% slice(1)

  list(summary = results, best_k = best_row$knot_count[[1]], best_cv_mse = best_row$cv_mse_mean[[1]])
}

cv_function_basis_models <- function(train_df, best_k, fold_seed = 101, k_folds = 10) {
  folds <- make_folds(nrow(train_df), k = k_folds, seed = fold_seed)
  model_names <- c("poly2", "smooth_spline", "reg_spline")

  res <- map_dfr(model_names, function(model_name) {
    fold_errors <- map_dbl(1:k_folds, function(fold_id) {
      inner_train <- train_df[folds != fold_id, , drop = FALSE]
      inner_valid <- train_df[folds == fold_id, , drop = FALSE]

      model <- switch(
        model_name,
        poly2 = fit_global_poly2(inner_train),
        smooth_spline = fit_smoothing_spline(inner_train),
        reg_spline = fit_regression_spline(inner_train, n_knots = best_k)
      )

      preds <- predict_model(model, inner_valid, model_type = model_name)
      mse(inner_valid$mpg, preds)
    })

    tibble(
      model_name = model_name,
      cv_mse_mean = mean(fold_errors),
      cv_mse_sd = sd(fold_errors)
    )
  })

  best <- res %>% arrange(cv_mse_mean, model_name) %>% slice(1)
  list(summary = res, best_model = best$model_name[[1]], best_cv_mse = best$cv_mse_mean[[1]])
}

cv_local_models <- function(train_df, fold_seed = 202, k_folds = 10, degrees = c(1, 2)) {
  folds <- make_folds(nrow(train_df), k = k_folds, seed = fold_seed)

  res <- map_dfr(degrees, function(deg) {
    fold_errors <- map_dbl(1:k_folds, function(fold_id) {
      inner_train <- train_df[folds != fold_id, , drop = FALSE]
      inner_valid <- train_df[folds == fold_id, , drop = FALSE]

      model <- fit_local_poly(inner_train, degree = deg)
      preds <- predict_model(model, inner_valid, model_type = "loess")
      mse(inner_valid$mpg, preds)
    })

    tibble(
      degree = deg,
      cv_mse_mean = mean(fold_errors),
      cv_mse_sd = sd(fold_errors)
    )
  })

  best <- res %>% arrange(cv_mse_mean, degree) %>% slice(1)
  list(summary = res, best_degree = best$degree[[1]], best_cv_mse = best$cv_mse_mean[[1]])
}

fit_selected_model <- function(train_df, model_label, best_k = NULL, best_degree = NULL) {
  switch(
    model_label,
    poly2 = fit_global_poly2(train_df),
    smooth_spline = fit_smoothing_spline(train_df),
    reg_spline = fit_regression_spline(train_df, n_knots = best_k),
    loess_deg1 = fit_local_poly(train_df, degree = 1),
    loess_deg2 = fit_local_poly(train_df, degree = 2),
    stop("Unknown model_label")
  )
}

predict_selected_model <- function(model, model_label, new_df) {
  if (model_label == "poly2") return(predict_model(model, new_df, "poly2"))
  if (model_label == "smooth_spline") return(predict_model(model, new_df, "smooth_spline"))
  if (model_label == "reg_spline") return(predict_model(model, new_df, "reg_spline"))
  if (model_label %in% c("loess_deg1", "loess_deg2")) return(predict_model(model, new_df, "loess"))
  stop("Unknown model_label")
}

run_single_experiment <- function(split_seed = 123, cv_seed_base = 1000) {
  auto_df <- ISLR2::Auto %>% drop_na(horsepower, mpg)
  split <- make_split(auto_df, train_prop = 0.9, seed = split_seed)
  train_df <- split$train
  test_df <- split$test

  spline_cv <- cv_regression_splines(train_df, fold_seed = cv_seed_base + 1)
  function_cv <- cv_function_basis_models(train_df, best_k = spline_cv$best_k, fold_seed = cv_seed_base + 2)
  local_cv <- cv_local_models(train_df, fold_seed = cv_seed_base + 3)

  best_function_label <- function_cv$best_model
  best_local_label <- glue("loess_deg{local_cv$best_degree}")
  benchmark_label <- "poly2"

  best_function_model <- fit_selected_model(train_df, best_function_label, best_k = spline_cv$best_k)
  best_local_model <- fit_selected_model(train_df, best_local_label)
  benchmark_model <- fit_selected_model(train_df, benchmark_label)

  test_results <- tibble(
    paradigm = c("function_basis", "local_regression", "global_polynomial"),
    selected_model = c(best_function_label, best_local_label, benchmark_label),
    test_mse = c(
      mse(test_df$mpg, predict_selected_model(best_function_model, best_function_label, test_df)),
      mse(test_df$mpg, predict_selected_model(best_local_model, best_local_label, test_df)),
      mse(test_df$mpg, predict_selected_model(benchmark_model, benchmark_label, test_df))
    )
  )

  list(
    train_df = train_df,
    test_df = test_df,
    spline_cv = spline_cv,
    function_cv = function_cv,
    local_cv = local_cv,
    test_results = test_results
  )
}

run_repeated_experiments <- function(n_repeats = 10, split_seed_start = 5000, cv_seed_start = 9000) {
  map_dfr(seq_len(n_repeats), function(i) {
    out <- run_single_experiment(
      split_seed = split_seed_start + i,
      cv_seed_base = cv_seed_start + 10 * i
    )

    out$test_results %>%
      mutate(
        repetition = i,
        best_k = out$spline_cv$best_k,
        best_function_model = out$function_cv$best_model,
        best_local_degree = out$local_cv$best_degree
      )
  })
}

plot_test_mse_distributions <- function(repeated_results) {
  ggplot(repeated_results, aes(x = paradigm, y = test_mse, fill = paradigm)) +
    geom_boxplot(alpha = 0.75, outlier.shape = 16) +
    geom_jitter(width = 0.08, alpha = 0.7, size = 2) +
    labs(
      title = "Distribuciones del ECM de prueba por paradigma",
      x = "Paradigma",
      y = "ECM de prueba"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}
