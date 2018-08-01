library(tidyverse)
books <- tibble(
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
shoppingCart <- sample_n(books, 8, replace = TRUE) %>%
  arrange(itemID)

groupedShoppingCart <- count(shoppingCart, itemID, name)

totalBooks <- sum(groupedShoppingCart$n)
maxNumberPerBook <- max(groupedShoppingCart$n)
numberOfMaxima <- groupedShoppingCart %>%
  filter(n == maxNumberPerBook) %>%
  nrow()
differentBooks <- nrow(groupedShoppingCart)

alternatives <- restrictedparts(totalBooks, maxNumberPerBook)
# no bigger set possible than we have different books
filterForDifBooks <- which(alternatives[1,] <= differentBooks)
# and filter impossible combinations
filterForPosComb <- which(alternatives[3,] >= numberOfMaxima)
alternatives <- alternatives[, intersect(filterForDifBooks, filterForPosComb)]

fourCombn <- combn(shoppingCart$name, 4) %>%
  as.data.frame()
colnames(fourCombn) <- paste0("V", 1:ncol(fourCombn))
gatheredFourCombn <- gather(fourCombn, "column", "title", V1:V70)
filteredFourCombn <- gatheredFourCombn %>%
  group_by(column) %>%
  summarise(dif = length(unique(title))) %>%
  filter(dif == 4)

fullFilteredFourCombn <- gatheredFourCombn %>%
  filter(column %in% filteredFourCombn$column)

fullFilteredFourCombn2 <- fourCombn[, which(colnames(fourCombn) %in% filteredFourCombn$column)]
