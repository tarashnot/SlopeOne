#' Build slope one model
#'
#' @param ratings \code{data.table} of ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return a \code{data.table} of (item_id1, item_id2, b, support) where b represents the average rating difference of 'item 2 rating' - 'item 1 rating'. support represents number of ratings used to compute b
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
#' @import data.table plyr
#'
#' @export

build_slopeone <- function(ratings, ...) {
  if (NROW(ratings) == 0) {
    return(data.table(data.frame(item_id1=c(), item_id2=c(), b=c(), support=c())))
  }
  score_diff_per_user <- dlply(ratings, .(user_id), function(rows) {
    if (NROW(rows) > 1) {
      # Get diffs for all item_id pairs.
      pair_rows_nums <- subset(
        expand.grid(rows_num1=1:NROW(rows), rows_num2=1:NROW(rows)),
        rows_num1 != rows_num2 &
          rows[rows_num1, 'item_id'] != rows[rows_num2, 'item_id'])
      data.table(
        item_id1=rows[pair_rows_nums$rows_num1, 'item_id'],
        item_id2=rows[pair_rows_nums$rows_num2, 'item_id'],
        diff=rows[pair_rows_nums$rows_num2, 'rating']
        - rows[pair_rows_nums$rows_num1, 'rating'])
    }
  }, ...)
  # ddply is slow when merging data frames within list while rbindlist is
  # much faster.
  score_diff_per_user <- rbindlist(score_diff_per_user)
  if (NROW(score_diff_per_user) == 0) {
    return(data.table(data.frame(item_id1=c(), item_id2=c(), b=c(), support=c())))
  }
  score_diff_per_user$item_id1 <- as.character(score_diff_per_user$item_id1)
  score_diff_per_user$item_id2 <- as.character(score_diff_per_user$item_id2)
  # Compute average score diff between item 1 and item 2.
  model <- score_diff_per_user[,
                               list(b=mean(diff), support=NROW(diff)),
                               by='item_id1,item_id2']
  setkey(model, item_id1, item_id2)
  return(model)
}
