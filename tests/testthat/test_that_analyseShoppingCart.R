context("test_that_analyseShoppingCart")

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
    expect_length(tmpResult, 7)
    expect_is(unlist(tmpResult), "integer")
    # these information are named as characters...
    namesOfInformation <- names(unlist(tmpResult))
    expect_is(namesOfInformation, "character")
    # ... and their names should be
    expect_equal(namesOfInformation[1], "itemsInTotal")
    expect_equal(namesOfInformation[2], "numberOfDifferentItems")
    expect_equal(namesOfInformation[3], "maxNumberPerItem")
    expect_equal(namesOfInformation[4], "numberOfMaxima")
    expect_equal(namesOfInformation[5], "minNumberPerItem")
    expect_equal(namesOfInformation[6], "numberOfMinima")
    # expect_equal(namesOfInformation[7], "numberOfEveryItem")

    # and are the numbers the right ones?
    expect_equal(tmpResult[[1]], 8) # 8 books in total
    expect_equal(tmpResult[[2]], 4) # 4 different books
    expect_equal(tmpResult[[3]], 3) # 3 books are the maximum ...
    expect_equal(tmpResult[[4]], 2) # ... and two times 3 books were bought
    expect_equal(tmpResult[[5]], 1) # and books 3 and 5 were bought only ones
    expect_equal(tmpResult[[6]], 2) # and 2 times only 1 book was bought
    expect_equal(tmpResult[[7]], c(3,3,1,1)) # 3*1st + 3*2nd + 1*3rd + 1*4th
  }
)
