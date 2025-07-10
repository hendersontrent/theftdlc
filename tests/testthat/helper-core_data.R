library(theft)
tmp <- theft::simData
tmp <- tmp[tmp$process %in% c("Gaussian Noise", "AR(1)"), ]

feature_matrix <- calculate_features(data = tmp,
                                     feature_set = "catch22",
                                     catch24 = FALSE)
