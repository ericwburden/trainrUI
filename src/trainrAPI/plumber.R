#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(trainr)

TRAINR_DIR <- trainr::setup_dir(getwd(), "shiny")

#* @apiTitle Run Arbitrary R code!

#* Use trainr to evaluate R code and return message lines
#* @param lines:[character]
#* @param chapter:character
#* @param lesson:character
#* @param exercise:character
#* @post /eval
function(lines, chapter, lesson, exercise) {
  message("\n\nResponse at:", Sys.time())
  message("chapter: ", chapter)
  message("lesson: ", lesson)
  message("exercise: ", exercise, "\n")
  original_file_path <- system.file(
    paste("lessons", chapter, lesson, exercise, sep = "/"),
    package = "trainr"
  )
  message("original_file_path", original_file_path, "\n")
  original_file_lines <- readLines(original_file_path)
  original_test_lines <- trainr::get_test_lines(original_file_lines)

  message("Test Lines:", paste(original_test_lines, collapse = "\n"))
  message("\n\nExercise Lines:", paste(lines, collapse = "\n"))

  result <- trainr::check_exercise_shiny(lines, original_test_lines)
  if (is.null(result)) { list(success = F, msg = "Error!") } else { result }
}


#* Returns a response if API is reachable
#* @get /_ping
function() {
  "OK"
}
