context("text_that_calculatePrice")

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

set.seed(1) # for reproducibility
shoppingCart <- dplyr::bind_cols(
  books,
  number = sample(0:10, 5, replace = T)
)


test_that(
  "one function returns best price with information about discount",
  {
    discountTibble <- dplyr::tibble(
      set = 1:5,
      discount = c(0, 5, 10, 20, 25)
    )

    priceInfo <- calculatePrice(
      shoppingCart, discountTibble, pricePerItem = 8
    )

    # including the intermediate steps
    priceInfoDetailed <- calculatePrice(
      shoppingCart, discountTibble, pricePerItem = 8, intermediateSteps = TRUE
    )

    # test basic structure first
    expect_is(priceInfo, "numeric")
    expect_is(priceInfoDetailed, "list")
    expect_equal(priceInfo, priceInfoDetailed$bestDiscount)
    iS <- priceInfoDetailed$intermediateSteps
    expect_is(iS, "list")

    expect_length(iS, 9)


    ## some price tests
    shoppingCart <- dplyr::bind_cols(
      books,
      number = c(1,1,4,1,1)
    )
    priceInfo <- calculatePrice(shoppingCart, discountTibble, pricePerItem = 8)

    expect_equal(unname(priceInfo["priceWithoutDiscount"]), 8*8)
  }
)
