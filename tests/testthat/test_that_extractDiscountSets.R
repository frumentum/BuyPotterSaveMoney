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
randomNumbers <- sample(1:10, 5, replace = T)
shoppingCart <- dplyr::bind_cols(books, number = randomNumbers)
ls <- analyseShoppingCart(shoppingCart)

alternatives <- enumerateCombinations(ls)

test_that(
  "extract only the possible discount sets",
  {
    correctDiscountSets <- extractDiscountSets(alternatives)

    # the output should be a matrix again
    expect_is(correctDiscountSets, "matrix")

    # and the number of columns should be lower or equal to 'alternatives'
    expect_lte(ncol(correctDiscountSets), ncol(alternatives))

    # if we let us show the intermediate steps, we see that every possible
    # discount set solution works
    discountSteps <- extractDiscountSets(alternatives, intermediateSteps = T)
    expect_is(discountSteps, "list")
    expect_length(discountSteps, 2)
    expect_is(discountSteps$correctDiscountSets, "matrix")
    expect_equal(discountSteps$correctDiscountSets, correctDiscountSets)
    expect_is(discountSteps$intermediateSteps, "list")
    iS <- discountSteps$intermediateSteps
    expect_is(iS$checkCorrectness, "matrix")
    expect_is(iS$incorrectDiscountSets, "character")
    # incorrect discount sets is the length difference between the number of
    # columns of 'alternatives' and the correct number of discount sets
    expect_length(
      iS$incorrectDiscountSets, ncol(alternatives) - ncol(correctDiscountSets)
    )
    expect_equal(ncol(iS$checkCorrectness), ncol(alternatives))
  }
)

test_that(
  "Is a shopping cart of size 80 too big?",
  {
    set.seed(1) # for reproducibility
    shoppingCart <- dplyr::bind_cols(
      books,
      number = c(15, 15, 10, 20, 20)
    )

    timeMeasurement <- system.time({
      ls <- analyseShoppingCart(shoppingCart)

      alternatives <- enumerateCombinations(ls)

      discountSets <- extractDiscountSets(alternatives)
    })

    expect_lte(timeMeasurement["elapsed"], 5) # less than 5 seconds
  }
)
