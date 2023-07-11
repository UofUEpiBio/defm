## code to prepare `valentesns` dataset goes here

library(data.table)

valentesns <- fread("data-raw/sns_model_data.csv")

id <- valentesns$id
X  <- valentesns[, .(
  Hispanic, year, exposure_smoke, exposure_drink, exposure_mj,
  sibsmoke, Grades, adultdrink, has_sib, Female)] |> as.matrix()

Y  <- valentesns[, .(alcohol, tobacco, mj)] |> as.matrix()

X <- cbind(X, model.matrix(~ I(as.factor(X[, 2])))[,-1])

valentesnsList <- list(
  id = id,
  X = X,
  Y = Y
)

valentesns <- as.data.frame(valentesns)

usethis::use_data(valentesns, valentesnsList, overwrite = TRUE)
