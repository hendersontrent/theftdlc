context("test-cluster")

test_that("default cluster", {
  feature_clusters <- cluster(feature_matrix, k = 6)
  expect_equal(2, length(feature_clusters))
})
