library(shiny)
library(shinydashboard)
library(dplyr)

# dput(head(mtcars)[1:3, 1:3])

df <-
  structure(
    list(
      mpg = c(21, 21, NA),
      cyl = c(6, NA, 4),
      disp = c(NA, 160, 108)
    ),
    .Names = c("mpg", "cyl", "disp"),
    row.names = c("Mazda RX4",
                  "Mazda RX4 Wag", "Datsun 710"),
    class = "data.frame"
  )


join_df <- function(df) {
  # stopifnot(!is.data.frame(df))
  
  df <- df %>%
    na.omit() %>%
    mutate(id = rownames(df))
  
  to_join <- head(mtcars)[1:3, 4:6] 
  to_join <- to_join %>%
    mutate(id = rownames(to_join))
  
  
  df %>%
    dplyr::left_join(to_join, by = "id")
}


header <- dashboardHeader(
  title = "Example Shiny app"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               textInput("text1", "Enter text:", value = "", placeholder = "letters only"), 
               actionButton("start", "Start"),
               textOutput("output_text1")
           ),
           box(width = NULL,
               actionButton("process_df_btn", "Process 'mtcars' DF")
           )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

server <- function(input, output, session) {
  
  observeEvent(input$start, {
    txt <- req(input$text1)
    
    # letters only
    req(nchar(gsub("[a-zA-Z]", "", txt)) == 0)
    
    output$output_text1 <- renderText(sprintf("Entered text: %s", txt))
  })
  
  observeEvent(input$process_df_btn, {
    
    # nicely handle error without crashing the app
    tryCatch({
      join_df(df)
    }, error = function(e) {
      
      showModal(modalDialog(
        title = "Error occured!",
        e
      ))
      
      req(FALSE)
    })
    
    print("...do something else...")
    
  })
  
}

shinyApp(ui = ui, server = server)
