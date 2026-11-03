context("test-values")

# Fake feature_calculations object with known structure

make_features <- function(sets, n_per_group = 20, seed = 1){
  set.seed(seed)
  ids <- paste0("ts", 1:(2 * n_per_group))
  groups <- rep(c("A", "B"), each = n_per_group)
  out <- do.call(rbind, lapply(names(sets), function(s){
    do.call(rbind, lapply(sets[[s]], function(f){
      data.frame(id = ids, group = groups, feature_set = s, names = f,
                 values = stats::rnorm(length(ids)) + (groups == "B"))
    }))
  }))
  structure(out, class = c("feature_calculations", "data.frame"))
}

test_that("classify(by_set = TRUE) gives each set only its own features", {

  feats <- make_features(list(quantiles = c("q1", "q2"), fftquantiles = c("quantile_1", "quantile_2", "quantile_3")))

  # Classifier that records the predictors it was given

  seen <- new.env()
  seen$predictors <- list()

  recorder <- function(formula, data){
    seen$predictors[[length(seen$predictors) + 1]] <- sort(setdiff(colnames(data), "group"))
    e1071::svm(formula, data = data, kernel = "linear", scale = FALSE)
  }

  suppressMessages(classify(feats, classifier = recorder, by_set = TRUE, n_resamples = 1, add_all_features = TRUE))

  got <- unique(seen$predictors)
  expect_equal(length(got), 3)
  expect_true(list(sort(c(paste0("allfeatures_quantiles_", c("q1", "q2")), paste0("allfeatures_fftquantiles_", c("quantile_1", "quantile_2", "quantile_3"))))) %in% got)
  expect_true(list(paste0("fftquantiles_", c("quantile_1", "quantile_2", "quantile_3"))) %in% got)
  expect_true(list(paste0("quantiles_", c("q1", "q2"))) %in% got)
})

test_that("fit_models computes precision and recall from the right margins", {

  # Classifier with fixed predictions for the test set

  fixed_preds <- c("A", "B", "A", "B", "C", "C")
  registerS3method("predict", "fixedpred", function(object, newdata, ...){
    factor(fixed_preds, levels = object$lev)
  })
  fixed <- function(formula, data){structure(list(lev = levels(data$group)), class = "fixedpred")}

  train <- data.frame(group = factor(c("A", "A", "B", "B", "C", "C")), f1 = c(1, 2, 3, 4, 5, 6))
  test <- data.frame(group = factor(c("A", "A", "A", "B", "B", "C")), f1 = c(1, 2, 3, 4, 5, 6))
  iters <- data.frame(seed = 1, feature_name = "f1")

  res <- suppressMessages(theftdlc:::fit_models(list(list(Train = train, Test = test)), iters, 1,
                                                is_null_run = FALSE, classifier = fixed))

  # Truth A: 2 of 3 predicted A; truth B: 1 of 2 predicted B; truth C: 1 of 1 predicted C.
  # Each class is predicted twice, with 2, 1, and 1 correct

  precision <- c(2 / 2, 1 / 2, 1 / 2)
  recall <- c(2 / 3, 1 / 2, 1 / 1)

  expect_equal(res$accuracy, 4 / 6)
  expect_equal(res$mean_precision, mean(precision))
  expect_equal(res$mean_recall, mean(recall))
  expect_equal(res$mean_f1_score, mean(2 * precision * recall / (precision + recall)))
})

# Fake classify() output with known accuracies

fake_results <- function(){
  set.seed(5)
  x <- 0.80 + stats::rnorm(30, sd = 0.03)
  y <- 0.75 + stats::rnorm(30, sd = 0.03)
  z <- 0.50 + stats::rnorm(30, sd = 0.03)
  res <- data.frame(model_type = rep(c("Main", "Main", "Null", "Null"), each = 30),
                    resample = rep(1:30, 4),
                    feature_set = rep(c("setx", "sety", "setx", "sety"), each = 30),
                    accuracy = c(x, y, z, z + 0.01))
  res$mean_precision <- res$accuracy
  res$mean_recall <- res$accuracy
  res$mean_f1_score <- res$accuracy
  list(TrainTestSizes = c(train_size = 75, test_size = 25), ClassificationResults = res)
}

test_that("compare_features uses the corrected resampled t-test with the train and test sizes", {

  res <- fake_results()
  cr <- res$ClassificationResults
  d <- cr$accuracy[cr$model_type == "Main" & cr$feature_set == "setx"] - cr$accuracy[cr$model_type == "Main" & cr$feature_set == "sety"]

  # Nadeau and Bengio (2003): t = mean(d) / sqrt(var(d) * (1 / J + n_test / n_train))

  t_expected <- mean(d) / sqrt(stats::var(d) * (1 / 30 + 25 / 75))
  p_expected <- 2 * stats::pt(-abs(t_expected), 29)

  out <- suppressMessages(compare_features(res, metric = "accuracy", by_set = TRUE, hypothesis = "pairwise"))
  expect_equal(out$t_statistic, t_expected)
  expect_equal(out$p.value, p_expected)

  dn <- cr$accuracy[cr$model_type == "Main" & cr$feature_set == "setx"] - cr$accuracy[cr$model_type == "Null" & cr$feature_set == "setx"]
  tn_expected <- mean(dn) / sqrt(stats::var(dn) * (1 / 30 + 25 / 75))

  out_null <- suppressMessages(compare_features(res, metric = "accuracy", by_set = TRUE, hypothesis = "null"))
  expect_equal(out_null$t_statistic[out_null$feature_set == "setx"], tn_expected)
  expect_equal(out_null$p.value[out_null$feature_set == "setx"], 1 - stats::pt(tn_expected, 29))
})

test_that("every metric and hypothesis combination of compare_features runs", {

  res <- fake_results()
  res_feat <- res
  res_feat$ClassificationResults <- res$ClassificationResults %>%
    dplyr::rename(names = "feature_set")

  for(metric in c("accuracy", "precision", "recall", "f1")){
    for(hypothesis in c("null", "pairwise")){
      out <- suppressMessages(compare_features(res, metric = metric, by_set = TRUE, hypothesis = hypothesis))
      expect_true(all(is.finite(out$t_statistic)))
      out <- suppressMessages(compare_features(res_feat, metric = metric, by_set = FALSE, hypothesis = hypothesis))
      expect_true(all(is.finite(out$t_statistic)))
    }
  }
})

test_that("interval(type = 'se') matches the two-sided t confidence interval", {

  res <- fake_results()
  x <- res$ClassificationResults$accuracy[res$ClassificationResults$model_type == "Main" &
                                            res$ClassificationResults$feature_set == "setx"]

  out <- interval(res, metric = "accuracy", by_set = TRUE, type = "se", interval = 0.95)
  ci <- stats::t.test(x, conf.level = 0.95)$conf.int

  expect_equal(out$.lower[out$feature_set == "setx"], ci[1])
  expect_equal(out$.upper[out$feature_set == "setx"], ci[2])
})

test_that("project(low_dim_method = 'PCA') centers the data", {

  feats <- make_features(list(setx = c("f1", "f2", "f3", "f4")))
  feats$values <- feats$values + 10

  pca <- project(feats, norm_method = "MinMax", low_dim_method = "PCA")
  expected <- stats::prcomp(pca$ModelData, center = TRUE, scale. = FALSE)

  expect_equal(pca$ModelFit$sdev, expected$sdev)
  expect_equal(abs(unname(pca$ModelFit$x[, 1])), abs(unname(expected$x[, 1])))
})

test_that("same-named features from different sets are normalized separately", {

  feats <- make_features(list(setx = "x", sety = "x"))
  feats$values[feats$feature_set == "sety"] <- feats$values[feats$feature_set == "sety"] + 1000

  pca <- project(feats, norm_method = "zScore", low_dim_method = "PCA")
  expect_equal(unname(apply(pca$ModelData, 2, stats::sd)), c(1, 1))

  # k-means centers weighted by cluster size give the column means of the normalized data

  clusts <- cluster(feats, norm_method = "zScore", clust_method = "kmeans", k = 2)
  col_means <- colSums(clusts$ModelFit$centers * clusts$ModelFit$size) / sum(clusts$ModelFit$size)
  expect_equal(unname(col_means), c(0, 0))
})

test_that("shrink uses every class's coefficients in multiclass problems", {

  # Each class differs from the others on its own feature

  set.seed(1)
  n <- 30
  groups <- rep(c("A", "B", "C"), each = n)
  ids <- paste0("ts", seq_along(groups))
  vals <- list(fa = stats::rnorm(3 * n) + 4 * (groups == "A"),
               fb = stats::rnorm(3 * n) + 4 * (groups == "B"),
               fc = stats::rnorm(3 * n) + 4 * (groups == "C"))

  feats <- do.call(rbind, lapply(names(vals), function(f){
    data.frame(id = ids, group = groups, feature_set = "setx", names = f, values = vals[[f]])
  }))
  feats <- structure(feats, class = c("feature_calculations", "data.frame"))

  set.seed(2)
  mod <- glmnet::cv.glmnet(as.matrix(as.data.frame(vals)), groups, family = "multinomial", type.measure = "class")
  coefs <- stats::coef(mod)
  nonzero <- sapply(coefs, function(x){as.vector(x[-1, 1]) != 0})
  rownames(nonzero) <- names(vals)

  set.seed(2)
  one <- shrink(feats, threshold = "one")
  set.seed(2)
  all <- shrink(feats, threshold = "all")

  expect_equal(sort(unique(one$names)), sort(names(vals)[rowSums(nonzero) > 0]))
  expect_equal(sort(unique(all$names)), sort(names(vals)[rowSums(nonzero) == ncol(nonzero)]))
  expect_equal(length(unique(one$names)), 3)
})

test_that("filter_duplicates keeps exactly one copy of each duplicated feature and nothing else is removed", {

  feats <- make_features(list(catch22 = c("CO_FirstMin_ac", "DN_HistogramMode_5"),
                              feasts = c("diff1_acf1", "acf1", "coef_hurst", "trend_strength"),
                              tsfeatures = c("diff1_acf1", "x_acf1", "firstmin_ac", "firstzero_ac", "hurst", "stability"),
                              User = "mean"))

  n_series <- length(unique(feats$id))

  for(seed in 1:10){
    out <- filter_duplicates(feats, seed = seed)
    pairs <- unique(paste(out$feature_set, out$names))

    # Duplicate groups present: diff1_acf1 (feasts, tsfeatures), acf1/x_acf1, coef_hurst/hurst, CO_FirstMin_ac/firstmin_ac

    expect_equal(sum(c("feasts diff1_acf1", "tsfeatures diff1_acf1") %in% pairs), 1)
    expect_equal(sum(c("feasts acf1", "tsfeatures x_acf1") %in% pairs), 1)
    expect_equal(sum(c("feasts coef_hurst", "tsfeatures hurst") %in% pairs), 1)
    expect_equal(sum(c("catch22 CO_FirstMin_ac", "tsfeatures firstmin_ac") %in% pairs), 1)

    # Features without a duplicate in the data are untouched

    expect_true(all(c("catch22 DN_HistogramMode_5", "feasts trend_strength", "tsfeatures firstzero_ac",
                      "tsfeatures stability", "User mean") %in% pairs))

    expect_equal(nrow(out), nrow(feats) - 4 * n_series)
  }
})
