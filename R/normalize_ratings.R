#' Normalize rating table
#'
#' @param ratings \code{data.table} of ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return List of 4 objects:
#' \itemize{
#'   \item global - global rating mean
#'   \item user - \code{data.table} of users' means (id, mean)
#'   \item item - \code{data.table} of items' means (id, mean)
#'   \item ratings - \code{data.table} of normalized ratings
#' }
#'
#' @details
#' Factor rating table into global mean + user mean + item mean. This is usually a preliminary step before building a model
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#'
#' @import data.table
#'
#' @export

normalize_ratings <- function(ratings, ...) {
  result <- list()
  result$global <- ratings[, mean(rating)]
  result$user <- ratings[, list(mean_rating=mean(rating)), by='user_id']
  result$item <- ratings[, list(mean_rating=mean(rating)), by='item_id']

  ratings$rating <- ratings$rating - result$global
  setkey(result$user, user_id)
  ratings$rating <- ratings$rating - result$user[J(ratings$user_id), ]$mean_rating
  setkey(result$item, item_id)
  ratings$rating <- ratings$rating - result$item[J(ratings$item_id), ]$mean_rating
  result$ratings <- ratings
  return(result)
}
