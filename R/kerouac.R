#' kerouac
#'
#' Port in R/shiny of The Most Dangerous Writing App by Manuel Elbert.
#' @references \code{http://www.themostdangerouswritingapp.com/}

#' @import shiny
#' @docType package
#' @name kerouac
NULL

#' Kerouac typing challenge
#'
#' If you stop typing for `grace` seconds, during the `duration` of the challenge,
#' then all your prose will be lost. So type, you fool!
#' @example
#' \dontrun{
#' kerouac()
#' }
#' @export
kerouac <- function(){
  # UI --------
  ui <- shinyUI(fluidPage(
    tags$head(
      tags$style(type="text/css",
                 "textarea { font-family:monospace;}")),
    # options row ------
    fluidRow(
      column(10, offset=1,
             # Application title
             titlePanel("Kerouac", windowTitle = "Beat, write, beat, write"),
             fluidRow(
               column(2,
                      br(),
                      actionButton("start", "Start typing ", icon=icon("road", lib="glyphicon"))),

               column(4,
                      radioButtons("duration",
                                   "Countdown",
                                   choices=list("10sec"=1/6, "2min"=2, "5min"=5, "20min"=20),
                                   inline=TRUE,
                                   selected=2)),

               column(4,
                      radioButtons("grace",
                                   "Grace period",
                                   choices=list("5sec"=5, "10sec"=10),
                                   inline=TRUE,
                                   selected=5))))),

    # textarea row ------
    fluidRow(
      column(10, offset=1,
             # Prose area
             htmlOutput("banner"),
             #textOutput("debug"),
             p("Type your prose below:"),
             tags$textarea(id="prose", rows=20, cols=80, ""),
             # Countdowns
             textOutput("time_remaining", inline=FALSE),
             #textOutput("time_lastedit", inline=FALSE),
             textOutput("charcount", inline=FALSE),
             hr(),
             span("Based on the genious ",  a(href="themostdangerouswritingapp.com", "Most Dangerous Writing App"),
                  "by", a(href="twitter.com/maebert", "Manuel Elbert"),
                  "(", a(href="github.com/maebert/themostdangerouswritingapp", "source"), ")"),
             br(),
             span("Ported in R during a rainy day by", a(href="http://www.twitter.com/vincentbonhomme", "Vincent Bonhomme"),
                  "(", a(href="github.com/vbonhomme/kerouac", "source"), ")")
      ))
  )
  )

  # Server --------
  server <- shinyServer(function(input, output, session) {

    # reactive for options, all in seconds -----
    duration <- reactive(as.numeric(input$duration)*60)
    grace    <- reactive(as.numeric(input$grace))

    # time of first edit -----
    # starts countdown when the button is pressed
    time_firstedit <- eventReactive(input$start, {
      output$banner <- renderUI(HTML('<br /><div class="alert alert-info"><center><bold>Type, you fool!<bold></center></div>'))
      updateTextInput(session, "prose", value="")
      Sys.time()
    })

    # remaining time --------
    # reactive, in seconds
    time_remaining <- reactive({
      invalidateLater(1000, session)
      (duration() - as.numeric(difftime(Sys.time(), time_firstedit(), units="sec")))})

    # renders it
    output$time_remaining <- renderText({
      invalidateLater(100, session)
      if (time_firstedit()>0 & !any(fail(), success()))
        c(paste0("Remaining: ", time_remaining() %/% 60, ":", round(time_remaining() %% 60)),
          rep("*", round(grace() - as.numeric(difftime(Sys.time(), time_lastedit(), units="sec")))))
      else
        ""
    })

    # lastedit countdown -----
    # any edit will reset this countdown
    time_lastedit <- eventReactive(list(grace(), duration(), input$start, input$prose), Sys.time())
    # renders it
    # output$time_lastedit <-  renderText({
    #   invalidateLater(100, session)
    #   if (time_firstedit()>0 & !any(fail(), success()))
    #     rep("*", round(grace() - as.numeric(difftime(Sys.time(), time_lastedit(), units="sec"))))
    #   else
    #     ""
    # })

    # counts the number of char -----
    charcount <- reactive(nchar(input$prose))

    # renders it
    output$charcount <- renderText({
      if (time_firstedit()>0 & !any(fail(), success()))
        paste(charcount(), "characters")
      else
        ""
    })

    ### fail flag -------
    fail <- reactive({
      invalidateLater(100, session)
      all((Sys.time() - time_lastedit()) > grace(), !success())
    })

    # success flag --------
    success <- reactive({
      invalidateLater(100, session)
      all(time_remaining() <= 0, charcount()>0)
    })

    # banner spitting ------
    observe({

      #if (!success() & !fail())
      #  output$banner <- renderUI(HTML('<br /><div class="alert alert-info"><center><bold>Type, you fool!<bold></center></div>'))
      if (success())
        output$banner <- renderUI(HTML('<br /><div class="alert alert-success"><center><bold>Very well done!</bold></center></div>'))
      if (fail()) {
        updateTextInput(session, "prose", value="")
        output$banner <- renderUI(HTML('<br /><div class="alert alert-danger"><center><bold>You miserably failed.<bold></center></div>'))
      }
    })

    # output$debug <- renderText(paste("firstedit:", time_firstedit(), "\n",
    #                                  "lastedit:", time_lastedit(), "\n",
    #                                  "fail:", fail(), "\n",
    #                                  "success:", success()))

  })

  # Run the application
  shinyApp(ui = ui, server = server)
}

