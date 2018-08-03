#' @title enumerate combination
#' @description This function is based on the output of
#' \code{analyseShoppingCart()}. Using the mathematical partition function from
#' the package \code{partitions} every combination is enumerated. Afterwards
#' the combinations are filtered so only combinations are returned which make
#' sense for the practical utilization.
#' @param listFromASC list as it comes from \code{analyseShoppingCart()}
#' @param intermediateSteps logical; if TRUE, additionally a list is returned
#' which contains the intermediate steps. More in details; default is FALSE
#' @details
#' If \code{intermediateSteps} is set to \code{TRUE}, the output of this
#' function will change to a list of length 2. The first element will be the
#' same matrix as it is produced with \code{intermediateSteps == FALSE}. The
#' second element is a list containing the intermediate steps which are done
#' within this function. These steps include:
#' 1. Enumerate every possible partition using
#'    \code{partitions::restrictedparts()}.
#' 2. filter1: Since we have only up to five different items in the shopping
#'    cart and discount is given for the items' dissimilarity, the greatest
#'    'discount set' can be the number of different items in the shopping cart.
#' 3. filter2:
#' @return
#' Depending on \code{intermediateSteps} a matrix or a list is returned. Matrix
#' represents the combinations which shall be used for calculating the discount.
#' @examples
#' ```
#' ### setup
#' books <- dplyr::tibble(
#'   itemID = 1:5,
#'   name = c(
#'     "Stein der Weisen",
#'     "Kammer des Schreckens",
#'     "Gefangene von Askaban",
#'     "Feuerkelch",
#'     "Orden des Phönix"
#'   )
#' )

#' set.seed(1) # for reproducibility
#' shoppingCart <- dplyr::sample_n(books, 8, replace = TRUE)
#' shoppingCart <- dplyr::arrange(shoppingCart, itemID)
#' ls <- analyseShoppingCart(shoppingCart, itemID, name)
#' ### end of setup
#' enumerateCombinations(ls)
#' ```
#' @export

enumerateCombinations <- function(listFromASC, intermediateSteps = FALSE) {

  # at first an important security check if the list is really the one which is
  # expected
  stopifnot(
    c(
      "itemsInTotal", "numberOfDifferentItems", "maxNumberPerItem",
      "numberOfMaxima", "minNumberPerItem"
    ) %in% names(listFromASC)
  )

  # numberOfSummands <- ifelse(
  #   listFromASC$maxNumberPerItem >= listFromASC$numberOfDifferentItems,
  #   listFromASC$maxNumberPerItem,
  #   listFromASC$numberOfDifferentItems
  # )
  ### enumerate all combinations
  everyCombination <- as.matrix(partitions::restrictedparts(
    listFromASC$itemsInTotal,
    # numberOfSummands
    listFromASC$maxNumberPerItem
  ))

  ### let's filter
  # No bigger set of combinations is possible than we have different items in
  # the shopping cart. That means that the first summand need to be lower or
  # equal to the number of different items.
  filterDifferentItems <- which(
    everyCombination[1,] <= listFromASC$numberOfDifferentItems
  )
  # Filter impossible combinations, eg the combination 3* 1st book, 3*2nd
  # book, 3*3rd book and 1*4th book: 4 discount + 2*3 discount, but not possible
  # is 2*4 discount + 1*2 discount
  ls <- listFromASC # to make name shorter
  filterPossibleCombinations <- which(
    everyCombination[ls$maxNumberPerItem,] >= ls$numberOfMaxima
  )
  # 'Discount sets' of size 'number of different items' can only occur as often
  # as the number of minima. Eg the combination 5,4,1 may result in the sums
  # 3,2,2,2,1 and 2,2,2,2,2 but not in 3,3,2,1,1
  filterMoreCombinations <- which(
    everyCombination[1 + ls$minNumberPerItem,] < ls$numberOfDifferentItems
  )
  # only the intersection of those filters is interesting
  intersection <- dplyr::intersect(
    filterDifferentItems, filterPossibleCombinations
  )
  intersection <- dplyr::intersect(
    intersection, filterMoreCombinations
  )
  alternatives <- everyCombination[
    , # take every row = every summand, but ...
    intersection # only these columns
  ]
  # bug fix: output shall always be a matrix
  if (length(intersection) == 1) alternatives <- as.matrix(alternatives)

  if (isTRUE(intermediateSteps)) {
    intermediateSteps <- list(
      everyCombination = everyCombination,
      filterForDifferentItems = filterDifferentItems,
      filterForPossibleCombinations = filterPossibleCombinations,
      filterMoreCombinations = filterMoreCombinations,
      intersection = intersection
    )

    return(list(
      alternatives = alternatives,
      intermediateSteps = intermediateSteps
    ))
  } else {
    return(alternatives)
  }

}
