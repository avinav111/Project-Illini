library(tidyverse)
library(shiny)

football = read_csv(file = "data/football.csv")


ui <- navbarPage(
  title = "Illini Football",
  tabPanel(
    title = "Graphs",
    titlePanel(title = "UIUC Football Head vs Head Data:" ), 
    sidebarLayout(
      sidebarPanel(selectInput(
        inputId = "team", 
        label = "TEAM:", 
        choices = sort(unique(football$Opponent))),
        selectInput(
          inputId = "year",
          label = "SEASON",
          choices = football$Season),
        checkboxInput(
          inputId = "size", label = "See overall illini record vs team"
        )
        ),
      mainPanel(plotOutput("plot"))
        )
    ),
  tabPanel(title = "Table", dataTableOutput("table")),
  tabPanel(title = "About", includeMarkdown("about.Rmd")) 
)

server <- function(input, output) {
    
  football_team = reactive({
    football |>
      filter(Opponent == input$team)
  })
  
  observeEvent(
    eventExpr = input$team, 
    handlerExpr = {
      updateSelectInput(inputId = "year", choices = football_team()$Season)
    }
  )
  
    output$plot <- renderPlot({
      graph = football |>
        filter(Opponent == input$team) |>
        filter(Season == input$year) |>
        group_by(Opponent) |>
        select(PointDiff, Season, Result) |>
        ggplot() + aes(x = Season, y = PointDiff, color = Result) |>
        geom_point(size = 10) 
      
      if(input$size){
        graph = football|>
          filter(Opponent == input$team) |>
          group_by(Opponent) |>
          select(PointDiff, Season, Result) |>
          ggplot() + aes(x = Season, y = PointDiff, color = Result) |>
          geom_point(size = 10) 
      }
      graph
    })
    
    output$table = renderDataTable({
      football_team()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
