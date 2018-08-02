#' @title enumerate combination
#' @description This function is based on the output of
#' \code{analyseShoppingCart()}. Using the mathematical partition function from
#' the package \code{partitions} every combination is enumerated. Afterwards
#' the combinations are filtered so only combinations are returned which make
#' sense for the practical utilization.
#' @param listFromASC list as it comes from \code{analyseShoppingCart()}
#' @param intermediateSteps logical; if TRUE, additionally a list is returned
#' which contains the intermediate steps. More in details; default is FALSE
#' @detalis
#' If \code{intermediateSteps} is set to \code{TRUE}, the output of this
#' function will change to a list of length 2. The first element will be the
#' same matrix as it is produced normally. The second element is a list
#' containing the intermediate steps which are done inside of this function.
#' @return
#' Depending on \code{intermediateSteps} a matrix or a list is returned. Matrix
#' represents the combinations which shall be used for calculating the discount.
#' @examples
#' ### setup
#' books <- dplyr::tibble(
#' itemID = 1:5,
#' name = c(
#' "Stein der Weisen",
#' "Kammer des Schreckens",
#' "Gefangene von Askaban",
#' "Feuerkelch",
#' "Orden des Phönix"
#' )
#' )

#' set.seed(1)
#' shoppingCart <- dplyr::sample_n(books, 8, replace = TRUE)
#' shoppingCart <- dplyr::arrange(shoppingCart, itemID)
#' ls <- analyseShoppingCart(shoppingCart, itemID, name)
#' ### end of setup
#' enumerateCombinations(ls)
#' @export

enumerateCombinations <- function(listFromASC, intermediateSteps = FALSE) {



}
