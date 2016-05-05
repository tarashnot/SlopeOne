#' MovieLense Dataset (100k)
#'
#' The 100k MovieLense ratings data set. The data was collected through the MovieLens web site (movielens.umn.edu) during the seven-month period from September 19th, 1997 through April 22nd, 1998. The data set contains about 100,000 ratings (1-5) from 943 users on 1664 movies.
#'
#' @format a \code{\link{data.table}} with 99392 rows and 3 variables:
#' \describe{
#'   \item{user_id}{ID of user, character}
#'   \item{item_id}{Name/ID of movie, character}
#'   \item{rating}{rating of movie by user, integer in range 1-5}
#' }

#' @source \url{GroupLens Research, http://www.grouplens.org/node/73}
"MovieLense"
