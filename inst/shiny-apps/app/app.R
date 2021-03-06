library(shiny)
library(dplyr)
library(tidyr)
library(partitions)
library(BuyPotterSaveMoney)

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
  includeCSS("styles.css"), # for styling the header
  headerPanel(
    title = "Willkommen im Zauberland des Harry Potter!",
    windowTitle = "Harry Potters Zauberland"
  ),
  br(), # 3 break lines
  br(),
  br(),
  h3(
    "Um Produkte zum Warenkorb hinzuzufügen, einfach die gewünschte Menge in",
    "die dafür vorgesehenen Felder eingeben und anschließend",
    em("in den Warenkorb legen"), "!"
  ),
  br(),
  fluidRow(
    column(
      2, offset = 1, img(src = "derSteinDerWeisen_skaliert.jpg")
    ),
    column(
      2, img(src = "dieKammerDesSchreckens.jpeg")
    ),
    column(
      2, img(src = "derGefangeneVonAskaban.jpeg")
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
        min = 0, max = 19, value = 0, step = 1
      )
    ),
    column(
      2,
      numericInput(
        "item2", label = paste("Harry Potter und", books$name[2]),
        min = 0, max = 19, value = 0, step = 1
      )
    ),
    column(
      2,
      numericInput(
        "item3", label = paste("Harry Potter und", books$name[3]),
        min = 0, max = 19, value = 0, step = 1
      )
    ),
    column(
      2,
      numericInput(
        "item4", label = paste("Harry Potter und", books$name[4]),
        min = 0, max = 19, value = 0, step = 1
      )
    ),
    column(
      2,
      numericInput(
        "item5", label = paste("Harry Potter und", books$name[5]),
        min = 0, max = 19, value = 0, step = 1
      )
    )
  ),
  fluidRow(
    column(
      3, offset = 2,
      uiOutput("goShoppingCart")
    )
  ),
  br(), # 3 break lines
  br(),
  br(),
  h4(
    "Rabattaktion: Bei zwei verschiedenen Büchern gibt es 5% auf diese zwei",
    "Bücher. Bei drei verschiedenen Büchern gibt es 10%, bei vier gibt es 20%",
    "und bei fünf verschiedenen Büchern sogar 25%."
  ),
  h3("Musst du kaufen, kannst du sparen!")
)

################################################################################
######################### shiny server #########################################
################################################################################
server <- function(input, output, session) {

  # create a reactive counter which counts the number of shoppings +
  # a reactive logical information if the input is correct
  rV <- reactiveValues(
    counter = NULL,
    inputIsOK = NULL
  )


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

  # security check that no error can occur
  observeEvent(input$moveToShoppingCart, {
    inputs <- c(input$item1, input$item2, input$item3, input$item4, input$item5)
    if (sum(inputs > 19) != 0) {
      rV$inputIsOK <- NULL # reset first
      rV$inputIsOK <- FALSE
    } else {
      rV$inputIsOK <- NULL # reset first
      rV$inputIsOK <- TRUE
    }
  })

  # if input is not OK, inform the user. Otherwise analyse the shopping cart.
  # make 'moveToShoppingCart' reactive and identify the shopping cart
  identifyShoppingCart <- eventReactive(rV$inputIsOK, {
    if (isTRUE(rV$inputIsOK)) {
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
    } else return("showErrorMessage")
  })


  # create a modal dialog to show the shopping cast
  observeEvent(identifyShoppingCart(), {
    if (isTRUE(rV$inputIsOK)) {
      showModal(modalDialog(
        title = "Warenkorb",
        fluidRow(
          column(7, tableOutput("shoppingCartTable")),
          column(5, htmlOutput("showDiscount", style="transform: rotate(40deg)"))
        ),
        hr(), # draw a horizontal line
        fluidRow(column(9, offset = 3, htmlOutput("price"))),

        footer = tagList(
          modalButton("Warenkorb ändern"),
          actionButton("buy", "Kaufen")
        )
      ))
    } else {
      showModal(modalDialog(
        h2("Aufgrund eines doofen Programierfehlers ist es derzeit leider",
           "nicht möglich, 20 Bücher oder mehr von einem Buch in den Warenkorb",
           "zu legen."),
        h3("Grund: Im ersten Schritt der Rabattberechnung wurden aus der",
           "Gesamtanzahl an Büchern", em("n"), "im Warenkorb alle möglichen",
           "Partitionen mit", em("k"), "Summanden berechnet, wobei",
           em("k"), "gleich der maximalen Anzahl eines Buches entspricht.",
           "Da alle Ergebnisse in einer Matrix abgespeichert werden, führt",
           "dies sehr schnell zu hohen Datenmengen, was letztlich zum",
           "Prozessabruch führt. Leider fiel mir dieser Fehler erst zu spät",
           "auf."),
        footer = modalButton("Na gut!")
      ))
    }
  })

  # render shopping cart table
  output$shoppingCartTable <- renderTable({
    identifyShoppingCart()$shoppingCart
  })
  # render price
  output$price <- renderText({
    infos <- identifyShoppingCart()$bestDiscount
    oldPrice <- unname(infos["priceWithoutDiscount"])
    newPrice <- unname(infos["priceInTotal"])
    paste(
      "Gesamtpreis:",
      "<font color=\"#FF0000\"><s>", oldPrice, "€", "</s></font>",
      "<font color=\"#ffffff\">","xxxxxx", "</font>", # empty space
      "<font color=\"#00ff00\" size=\"20px\">", newPrice, "€", "</font>"
    )
  })
  # render the discount in percent
  output$showDiscount <- renderText({
    infos <- identifyShoppingCart()$bestDiscount
    discountPercent <- round(unname(infos["discountPercent"]), digits = 1)
    paste(
      "Du sparst",
      "<font color=\"#cd853f\" size=\"20px\">", discountPercent, "%", "</font>"
    )
  })

  ################## observe 'buy'-button ######################################
  # observe buy-button
  observeEvent(input$buy, {
    # update counter
    if ( is.null(rV$counter) ) {
      rV$counter <- 1
    } else {
      rV$counter <- rV$counter + 1
    }

    # reset all numerical inputs for the items
    updateNumericInput(session, inputId = "item1", value = 0)
    updateNumericInput(session, inputId = "item2", value = 0)
    updateNumericInput(session, inputId = "item3", value = 0)
    updateNumericInput(session, inputId = "item4", value = 0)
    updateNumericInput(session, inputId = "item5", value = 0)

    removeModal(session)
  })

  observeEvent(rV$counter, {
    if (rV$counter %in% c(3, 10, 15)) {
      showModal(modalDialog(
        fluidRow(
          column(12, htmlOutput("welcome", style = "font-style: italic"))
        ),

        fluidRow(
          column(6, img(src = "bitcoin_skaliert.png")),
          column(6, img(src = "myPublicKey.png"))
        ),
        footer = modalButton(
          ifelse(rV$counter != 15,
                 "Vielleicht ein andermal!",
                 "Einladen kann nie schaden!")
        )
      ))
    }
  })

  output$welcome <- renderText({
    if (rV$counter == 3)
      brick <- "Oh, ein Osterei. Dem Entwickler vielleicht einen Kaffee kaufen?"
    if (rV$counter == 10) brick <- "Dem Entwickler einen Kaffee kaufen!"
    if (rV$counter == 15)
      brick <- paste(
        "Oder den Entwickler einstellen?"
      )
    paste("<h2>", brick, "</h2>")
  })
}

shinyApp(ui, server)
