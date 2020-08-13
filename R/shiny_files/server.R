#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
  # server connection
  #!!! let user input eventually
  con <- DBI::dbConnect(drv    = RSQLite::SQLite(),
                        dbname = "H:/projects/stats_projects/learnedr/learned_league.sqlite")

  qs <- DBI::dbGetQuery(con,'
                     select *
                     from "questions"
                     ')

  output$display_question <-
    renderText({
      qs$question[1]
    })

  output$correct_answer <-
    renderText({
      qs$answer[1]
    })

  values <- reactiveValues(variable = NA)

  observe({
    if(input$submit > 0) {
      values$variable <- isolate(input$variable)
    }
  })

  output$text <- renderText({values$variable})
})
