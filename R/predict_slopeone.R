#' Predict ratings for multiple users and items given known ratings
#'
#' @param model \code{data.table} of produced by \code{\link{build_slopeone}}
#' @param target \code{data.table} of (user_id, item_id) to predict ratings
#' @param ratings \code{data.table} of known ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return a \code{data.table} containig (user_id, item_id, predicted_rating)
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#'
#' predictions <- predict_slopeone(model, MovieLense[(!in_train), c(1, 2), with = FALSE], ratings$ratings)
#'
#' @import data.table
#'
#' @export

predict_slopeone <-function(model, targets, ratings, ...) {
  setkey(ratings, user_id)
  adply(targets,
        1,
        function(row) {
          data.frame(
            predicted_rating=predict_slopeone_for_user(
              model, row$item_id, ratings[J(row$user_id), ]))
        }, ...)
}
