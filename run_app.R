try(Sys.setlocale("LC_CTYPE", "Spanish_Guatemala.utf8"), silent = TRUE)
options(shiny.port = 7777, shiny.host = "127.0.0.1")
shiny::runApp(".", launch.browser = FALSE)

