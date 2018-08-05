context("text_that_calculateBestDiscount")

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
correctDiscountSets <- extractDiscountSets(alternatives)

discountTibble <- dplyr::tibble(
  set = 1:5,
  discount = c(0, 0.05, 0.1, 0.2, 0.25)
)

test_that(
  "calculating the discount works",
  {
    bestDiscount <- calculateBestDiscount(
      correctDiscountSets, discountTibble, pricePerItem = 8
    )
    # the output shall be a numeric vector containing discount, price in total,
    # and so on
    expect_is(bestDiscount, "numeric")
    partsOfOutput <- c(
      "priceInTotal", "discountAbs", "discountPercent", "priceWithoutDiscount"
    )
    expect_equal(partsOfOutput, names(bestDiscount))

    # similiar to the other functions, calculateBestDiscount has an argument
    # intermediateSteps which can be set to TRUE to show all intermediate steps
    bestDiscountSteps <- calculateBestDiscount(
      correctDiscountSets, discountTibble, pricePerItem = 8,
      intermediateSteps = TRUE
    )
    expect_is(bestDiscountSteps, "list")
    expect_length(bestDiscountSteps, 2)
    expect_equal(bestDiscountSteps$bestDiscount, bestDiscount)
    iS <- bestDiscountSteps$intermediateSteps
    expect_is(iS, "list")
  }
)
