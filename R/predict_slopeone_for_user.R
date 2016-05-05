#' Predict score for target_item_id given the known ratings of a single user
#'
#' @param model \code{data.table} of produced by \code{\link{build_slopeone}}
#' @param target_item_id target item id to predict rating
#' @param ratings \code{data.table} of user's known ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return predicted rating score
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
#' predict_slopeone_for_user(model, "Volcano (1997)", ratings$ratings[user_id == "1", c(2, 3), with = FALSE])
#'
#' @import data.table
#'
#' @export

predict_slopeone_for_user <- function(model, target_item_id, ratings) {
  # If target_id is already rated by the user, return that rating.
  already_rated <- subset(ratings, ratings$item_id == target_item_id)
  if (NROW(already_rated) == 1) {
    return(already_rated$rating)
  } else if (NROW(already_rated) > 1) {
    warning(paste(target_item_id,
                  ' is already rated by user, but there are multiple ratings.'))
    return(already_rated[1, ]$rating)
  }
  if (NROW(model) == 0) {
    return(NA)
  }
  # Compute weighted average ratings.
  ratings <- rename(ratings, c('item_id'= "item_id1"))
  ratings <- cbind(ratings, item_id2=target_item_id)
  setkey(ratings, item_id1, item_id2)
  joined <- model[ratings, ]
  joined <- joined[complete.cases(joined), ]
  if (NROW(joined) == 0) {
    return(NA)
  }
  return(sum(joined[, (b + rating) * support]) /
           sum(joined[, sum(support)]))
}
