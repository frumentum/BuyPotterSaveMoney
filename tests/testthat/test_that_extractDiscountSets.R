context("test_that_extractDiscountSets")

# These books are available in the shop
books <- dplyr::tibble(
  itemID = 1:5,
  name = c(
    "Stein der Weisen",
    "Kammer des Schreckens",
    "Gefangene von Askaban",
    "Feuerkelch",
    "Orden des Phönix"
  )
)

set.seed(1)
shoppingCart <- dplyr::sample_n(books, 15, replace = TRUE) %>%
  dplyr::arrange(itemID)

ls <- analyseShoppingCart(shoppingCart, itemID, name)
alternatives <- enumerateCombinations(ls)

test_that(
  "extract only the possible discount sets",
  {
    possibleDiscountSets <- extractDiscountSets(alternatives)

    # the output should be a matrix again
    expect_is(possibleDiscountSets, "matrix")

    # and the number of columns should be lower or equal to 'alternatives'
    expect_lte(ncol(alternatives), ncol(possibleDiscountSets))

    # if we let us show the intermediate steps, we see that every possible
    # discount set solution works
    discountSteps <- extractDiscountSets(alternatives, intermediateSteps = T)
    expect_is(discountSteps, "list")
    expect_length(discountSteps, 2)
    expect_is(discountSteps$possibleDiscountSets, "matrix")
    expect_equal(discountSteps$possibleDiscountSets, possibleDiscountSets)
    expect_is(discountSteps$intermediateSteps, "matrix")
    expect_equal(ncol(discountSteps$intermediateSteps), ncol(alternatives))
  }
)
