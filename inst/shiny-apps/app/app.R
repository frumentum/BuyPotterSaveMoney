library(shiny)
library(dplyr)
library(tidyr)
library(partitions)

#### setup ####
# These books are available in the shop
books <<- dplyr::tibble(
  itemID = 1:5,
  name = c(
    "Der Stein der Weisen",
    "Die Kammer des Schreckens",
    "Der Gefangene von Askaban",
    "Der Feuerkelch",
    "Der Orden des Phönix"
  )
)

discountInfos <<- dplyr::tibble(
  set = 1:5,
  discount = c(0, 5, 10, 20, 25)
)

pricePerItem <<- 8
#### ebd of setup ###

################################################################################
###################### shiny UI ################################################
################################################################################
ui <- fluidPage(
  br(),
  fluidRow(
    column(
      2, offset = 1, img(src = "derSteinDerWeisen_skaliert.jpg")
    ),
    column(
      2, img(src = "dieKammerDesSchreckens.jpeg")
    ),
    column(
      2, img(src = "derGefangeneVonAskaban_skaliert.jpg")
    ),
    column(
      2, img(src = "derFeuerkelch.jpeg")
    ),
    column(
      2, img(src = "derOrdenDesPhönix.jpeg")
    )
  ),
  fluidRow(
    column(
      2, offset = 1,
      numericInput(
        "item1", label = paste("Harry Potter und", books$name[1]),
        min = 0, max = 20, value = 0, step = 1
      )
    ),
    column(
      2,
      numericInput(
        "item2", label = paste("Harry Potter und", books$name[2]),
        min = 0, max = 20, value = 0, step = 1
      )
    ),
    column(
      2,
      sliderInput(
        "item3", label = paste("Harry Potter und", books$name[3]),
        min = 0, max = 20, value = 0, step = 1
      )
    ),
    column(
      2,
      sliderInput(
        "item4", label = paste("Harry Potter und", books$name[4]),
        min = 0, max = 20, value = 0, step = 1
      )
    ),
    column(
      2,
      sliderInput(
        "item5", label = paste("Harry Potter und", books$name[5]),
        min = 0, max = 20, value = 0, step = 1
      )
    )
  ),
  fluidRow(
    column(
      3, offset = 2,
      uiOutput("goShoppingCart")
    )
  ),
  br(), # insert empty row
  fluidRow(
    column(
      6, offset = 3,
      verbatimTextOutput("price")
    )
  )
)

################################################################################
######################### shiny server #########################################
################################################################################
server <- function(input, output, server) {

  # create action button "moveToShoppingCart"
  output$goShoppingCart <- renderUI({
    if (input$item1 != 0 ||
        input$item2 != 0 ||
        input$item3 != 0 ||
        input$item4 != 0 ||
        input$item5 != 0) {
      actionButton(
        "moveToShoppingCart", "in den Warenkorb legen", icon = icon("send")
      )
    }
  })

  # make 'moveToShoppingCart' reactive and identify the shopping cart
  identifyShoppingCart <- eventReactive(input$moveToShoppingCart, {
    shoppingCart <- books %>%
      dplyr::bind_cols(
        number = c(input$item1, input$item2, input$item3, input$item4,
                   input$item5)
      )

    bestDiscountDetailed <- calculatePrice(
      shoppingCart, discountInfos, pricePerItem, intermediateSteps = TRUE
    )

    # change column names to display
    colnames(shoppingCart) <- c("Position", "Bezeichnung", "Anzahl")
    shoppingCart <- shoppingCart %>%
      dplyr::filter(Anzahl != 0)
    bestDiscountDetailed$shoppingCart <- shoppingCart

    return(bestDiscountDetailed)
  })


  # create a modal dialog to show the shopping cast
  observeEvent(identifyShoppingCart(), {
    showModal(modalDialog(
      title = "Warenkorb",
      fluidRow(column(12, tableOutput("shoppingCartTable"))),
      hr(), # draw a horizontal line
      fluidRow(column(9, offset = 3, htmlOutput("withoutDiscount"))),

      footer = tagList(
        modalButton("Warenkorb ändern"),
        actionButton("buy", "Kaufen")
      )
    ))
  })

  output$shoppingCartTable <- renderTable({
    identifyShoppingCart()$shoppingCart
  })

  output$withoutDiscount <- renderText({
    infos <- identifyShoppingCart()$bestDiscount
    oldPrice <- unname(infos["priceWithoutDiscount"])
    newPrice <- unname(infos["priceInTotal"])
    paste(
      "Gesamtpreis:",
      "<font color=\"#FF0000\"><s>", oldPrice, "</s></font>",
      "      ", # empty space
      "<font color=\"#00ff00\" size=\"20px\">", newPrice, "</font>"
    )
  })
}

shinyApp(ui, server)
