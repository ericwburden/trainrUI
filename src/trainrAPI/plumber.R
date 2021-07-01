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

#* @apiTitle Run Arbitrary R code!

#* Use trainr to evaluate R code and return message lines
#* @param lines:[character]
#* @post /eval
function(lines) {
  result <- trainr::check_exercise_shiny(lines)
  if (is.null(result)) { list(success = F, msg = "Error!") } else { result }
}


#* Returns a response if API is reachable
#* @get /_ping
function() {
  "OK"
}
