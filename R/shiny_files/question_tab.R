question_tab <-
  tabPanel(title = "question tab",
    # display question
    textOutput("question"),

    # accept my answer
    textInput("variable", "Add Recommendation", ""),
    actionButton("submit", "Add"),
    textOutput("text"),

    # display answer
    textOutput("answer")

    # display image/sound

    # answer

    # other?
  )
