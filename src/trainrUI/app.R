#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(httr)
library(jsonlite)
library(rredis)
library(shiny)
library(shinyAce)
library(shinyjs)
library(shinythemes)
library(shinyTree)
library(trainr)
library(uuid)

source("util.R")

save_to_redis <- function(key, value) {
    rredis::redisSet(key, value)
    rredis::redisExpire(key, 2.6e6)
}

# Define UI for application that draws a histogram
ui <- shiny::tags$html(
    shiny::tags$body(
        fluidPage(
            theme = shinytheme("sandstone"),
            shiny::tags$head(
                shinyjs::useShinyjs(),
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
            ),
            fluidRow(
                class = "title",
                column(
                    12,
                    style = "display: flex; align-items: flex-start;",
                    div(titlePanel("Learn R with Tests!"), style = "flex-grow: 1;"),
                    actionButton(
                        "progress",
                        label = NULL,
                        class = "progress-icon btn-small btn-circle center-block"
                    ),
                    div(
                        style = "margin-left: 10px",
                        actionButton(
                            "help",
                            label = NULL,
                            class = "question-mark-icon btn-small btn-circle center-block"
                        )
                    )
                )
            ),
            fluidRow(
                class = "main",
                column(
                    6,
                    h2("Source Code"),
                    aceEditor(
                        "code",
                        mode = "r",
                        theme = "solarized_dark",
                        value = "",
                        autoComplete = "live",
                        height = "80%"
                    ),
                    actionButton("eval", "Check Exercise"),
                    actionButton("next_ex", "Next Exercise")
                ),
                column(
                    6,
                    h2("Output"),
                    htmlOutput("output"),
                    div(
                        id = "output-loading",
                        class = "loading-container",
                        div(class = "animated loading-bar")
                    )
                )
            )
        )
    ),
    shiny::tags$footer()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # Show loader on Ace editor
    shinyjs::addClass("code", "loading")

    # Set up a user-specific folder
    token <- isolate(getQueryString(session))
    TRAINR_DIR <- if (is.null(token$session)) {
        session_id <- uuid::UUIDgenerate()
        updateQueryString(paste0("?session=", session_id), mode = "replace")
        session_id
    } else {
        token$session
    }

    # If we can connect to Redis, try to pull down an exercise listing for this
    # session key
    redis_connected <- tryCatch(
        {
            rredis::redisConnect("redis")
            TRUE
        },
        error = function(e) {
            message("Could not connect to Redis:")
            message(e)
            FALSE
        }
    )


    # If a session key has a listing stored in Redis, use that one, otherwise
    # create a new folder/listing and put that listing in Redis
    stored_listing <- if (redis_connected) rredis::redisGet(TRAINR_DIR)
    if (is.data.frame(stored_listing)) {
        dir.create(TRAINR_DIR, showWarnings = F)
        saveRDS(stored_listing, paste0(TRAINR_DIR, "/.exercises"))
    } else {
        trainr::setup_dir(TRAINR_DIR, type = "shiny")
        listing <- readRDS(paste0(TRAINR_DIR, "/.exercises"))
        save_to_redis(TRAINR_DIR, listing)
    }


    # Pre-load the editor window with the current file contents
    initial_exercise_lines <- paste(
        trainr::current_exercise_lines(TRAINR_DIR),
        collapse = "\n"
    )
    updateAceEditor(session, "code", value = initial_exercise_lines)
    shinyjs::removeClass("code", "loading")



    # When the "Check Exercise" button is clicked, pass the exercise lines
    # entered off to the trainrAPI code runner and output the results
    observeEvent(input$eval, {
        shinyjs::html(
            "output",
            html = "<span style=\"color:#268bd2;\"><h3>Checking Exercise...</h3></span>"
        )
        shinyjs::addClass("output-loading", "loading")

        input_lines <- stringr::str_split(input$code, "\n", simplify = T)
        current_exercise_data <- as.list(subset(
            readRDS(paste0(TRAINR_DIR, "/.exercises")),
            current,
            chapter:exercise
        ))

        result <- httr::POST(
            "api:8000/eval",
            body = list(
                lines = jsonlite::toJSON(input_lines),
                chapter = jsonlite::toJSON(current_exercise_data$chapter),
                lesson = jsonlite::toJSON(current_exercise_data$lesson),
                exercise = jsonlite::toJSON(current_exercise_data$exercise)
            )
        )

        if (httr::status_code(result) == 200) {
            result_list <- jsonlite::fromJSON(httr::content(result, "text"))
            output <- paste(result_list$msg, collapse = "</br>")

            shinyjs::html(
                "output",
                html = "<span style=\"color:#268bd2;\"><h3>Result:</h3></span>"
            )
            shinyjs::html("output", html = output, add = TRUE)

            if (result_list$success) trainr::mark_current_exercise_complete(TRAINR_DIR)
        } else {
            shinyjs::html(
                "output",
                html = "<span style=\"color:#dc322f;\"><h3>Error!</h3></span>"
            )
            err_msg <- paste0(
                "</br><span style=\"color:#dc322f;\"><b>",
                "Something went wrong, if the problem persists, ",
                "please report to the system administrator.",
                "</b></span>"
            )
            shinyjs::html("output", html = err_msg, add = TRUE)
        }
        shinyjs::removeClass("output-loading", "loading")
    })


    # When the "Next Exercise" button is clicked
    observeEvent(input$next_ex, {
        shinyjs::html(
            "output",
            html = "<span style=\"color:#268bd2;\"><h3>Loading Next Exercise...</h3></span>"
        )
        shinyjs::addClass("code", "loading")
        shinyjs::addClass("output-loading", "loading")

        current_listing <- trainr::update_current_exercise(TRAINR_DIR)
        current_exercise_lines <- paste(
            trainr::current_exercise_lines(TRAINR_DIR),
            collapse = "\n"
        )
        updateAceEditor(session, "code", value = current_exercise_lines)
        if (redis_connected) save_to_redis(TRAINR_DIR, current_listing)

        shinyjs::removeClass("code", "loading")
        shinyjs::removeClass("output-loading", "loading")
        shinyjs::html(
            "output",
            html = "<span style=\"color:#859900;\"><h3>Exercise Loaded</h3></span>",
            add = TRUE
        )
    })


    # Show the help modal
    observeEvent(input$help, {
        showModal(modalDialog(
            title = "Saving Sessions",
            "If you'd like to save your progress, just bookmark this site.",
            "Your session is linked to your session URL parameter. Sessions ",
            "remain active for 30 days after last accessed."
        ))

    })

    # Show the progress modal
    observeEvent(input$progress, {
        showModal(modalDialog(
            title = "My Progress",
            shinyTree("tree", stripes = TRUE, multiple = FALSE, animation = FALSE)
        ))

        output$tree <- renderTree({
            ex_list <- get_exercise_listing(TRAINR_DIR)
            text_added <- add_descriptions(ex_list)
            tree_list(text_added)
        })
    })


    session$onSessionEnded(function() {
        unlink(TRAINR_DIR, recursive = TRUE)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
