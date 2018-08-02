# following tests are for the wrapper function 'analyseShoppingCart'
test_that(
  "analyseShoppingCart returns a list with four information, combination
  3,1,1,3", {
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
    # create first test shopping cart with the combination 3,1,1,3
    shoppingCart <- dplyr::bind_rows(
      books[2, ],
      books[2, ],
      books[2, ],
      books[3, ],
      books[4, ],
      books[5, ],
      books[5, ],
      books[5, ]
    )

    # analyse this shopping cart
    tmpResult <- analyseShoppingCart(shoppingCart, itemID, name)
    # shall be class list...
    expect_is(tmpResult, "list")
    # shall contain four integer informtion...
    expect_length(tmpResult, 4)
    expect_is(unlist(tmpResult), "integer")
    # these information are named as characters...
    namesOfInformation <- names(unlist(tmpResult))
    expect_is(namesOfInformation, "character")
    # ... and their names should be
    expect_equal(namesOfInformation[1], "itemsInTotal")
    expect_equal(namesOfInformation[2], "numberOfDifferentItems")
    expect_equal(namesOfInformation[3], "maxNumberPerItem")
    expect_equal(namesOfInformation[4], "numberOfMaxima")
  }
)
