#' @title extract discount sets
#' @description Based on \code{analyseShoppingCart()} and
#' \code{enumerateCombinations()} this function provides filtered but possible
#' discount sets.
#' @param alternatives \code{matrix} as it comes from
#' \code{enumerateCombinations()}
#' @param intermediateSteps logical; default is \code{FALSE}
#' @return If \code{intermediateSteps} is set to \code{TRUE}, a list will be
#' returned; otherwise a matrix
#' @examples
#' ```
#' # These books are available in the shop
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
#'
#' set.seed(1)
#' shoppingCart <- dplyr::sample_n(books, 15, replace = TRUE) %>%
#'   dplyr::arrange(itemID)
#'
#' ls <- analyseShoppingCart(shoppingCart, itemID, name)
#' alternatives <- enumerateCombinations(ls)
#' extractDiscountSets(alternatives)
#' ```
#' @export
#' @importFrom magrittr '%>%'

extractDiscountSets <- function(alternatives, intermediateSteps = FALSE) {

  colnames(alternatives) <- paste0("V", 1:ncol(alternatives))
  checkCorrectness <- alternatives

  for (i in seq_len(ncol(checkCorrectness)) ) {
    # for debugging
    # if (i >= 2) break

    # 'n' are the numbers we substract from each column, but only 'n' rows
    n <- checkCorrectness[, i]
    for (j in seq_len(nrow(checkCorrectness)) ) {
      # print(n[j])
      # print(checkCorrectness[, i])
      checkCorrectness[1:n[j], i] <- checkCorrectness[1:n[j], i] - 1
      # print(checkCorrectness[, i])
      if (-1 %in% checkCorrectness[, i]) break

      # sorting is important. Otherwise substraction doesn't work correctly
      checkCorrectness[, i] <- sort(checkCorrectness[, i], decreasing = T)
      # print(checkCorrectness[, i])
    }
  }

  incorrectDiscountSets <- checkCorrectness %>%
    as.data.frame() %>%
    # dplyr::bind_cols(summands = paste0("summand", 1:nrow(checkCorrectness))) %>%
    tidyr::gather(key = "combination", value = "value") %>%
    dplyr::filter(value < 0) %>%
    dplyr::distinct(combination) %>%
    dplyr::pull(combination)

  correctDiscountSets <- which(colnames(alternatives) != incorrectDiscountSets)
  correctDiscountSets <- alternatives[, correctDiscountSets]

  if (isTRUE(intermediateSteps)) {
    intermediateSteps <- list(
      "checkCorrectness" = checkCorrectness,
      "incorrectDiscountSets" = incorrectDiscountSets
    )
    return(list(
      "correctDiscountSets" = correctDiscountSets,
      "intermediateSteps" = intermediateSteps
    ))
  } else return(correctDiscountSets)

}
