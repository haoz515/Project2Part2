#' a function for cross-validation
#'
#' This function predict output body_mass_g using covariates bill_length_mm,
#' bill_depth_mm, and flipper_length_mm
#'
#' @param k a numeric value specify the number of folds
#' @keywords prediction
#'
#' @return a numeric with the cross-validation error
#'
#' @examples
#' data(my_penguins)
#' my_rf_cv(5)
#'
#' @import randomForest
#'
#'
#' @export
my_rf_cv <- function(k){
  my_penguins <- tidyr::drop_na(my_penguins)
  # fold and split the data
  data <- my_penguins %>% dplyr::select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm)
  fold <- sample(rep(1:k, length = nrow(my_penguins)))
  data <- data %>% dplyr::mutate("split" = fold)
  # create the vector to store mse
  mse <- c()
  # iterate through k times
  for(i in 1:k) {
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    # use random forest to predict
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm
                          + bill_depth_mm
                          + flipper_length_mm, data = data_train, ntree = 100)
    prediction <- predict(model, data_test[,-1])
    # store the mse in a vector
    mse[i] <- mean((data_test$body_mass_g - prediction)^2)
  }
  # return the mean of the vector mse
  out <- mean(mse)
  return(out)
}
