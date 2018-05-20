#' Plan a journey with Open Trip Planner
#'
#' R interface to the Open Trip Planner journey planning API,
#' See [docs.opentripplanner.org](http://docs.opentripplanner.org/en/latest/) for details.
#' Function to return distance for walk, cycle or car - desn't make sense for transit (bus or rail)
#'
#' @details
#' Requires a local instance of Open Trip Planner.
#' See the OTP [tutorials](http://docs.opentripplanner.org/en/latest/Basic-Tutorial/) for set up details.
#'
#' @param from Longitude/Latitude pair, e.g. `c(-0.134649,51.529258)` or SF points of class "sfc_POINT" "sfc" and length one
#' @param to Longitude/Latitude pair, e.g. `c(-0.088780,51.506383)` or SF points of class "sfc_POINT" "sfc" and length one
#' @param modes Boolean value which returns raw list from the geojson if TRUE (FALSE by default).
#' @inheritParams json2sf_otp
#' @seealso json2sf_otp
#' @export
#' @examples
#' \dontrun{
#' from = c(-0.134649,51.529258) # Euston Station
#' to = c(-0.088780,51.506383) # Bridge House
#' r1 = journey(from, to)
#' }
PlanRoute <- function(from,
                      to,
                      modes = "TRANSIT,WALK",
                      baseURL = "http://localhost:8080/otp/routers/default"){

  # need to check modes are valid

  # setup router URL with /plan
  routerUrl <- paste0(baseURL, '/plan')

  # Use GET from the httr package to make API call and place in req - returns json by default
  req <- GET(routerUrl,
             query = list(
               fromPlace = from,
               toPlace = to,
               mode = modes
             ))
  # convert response content into text
  text <- content(req, as = "text", encoding = "UTF-8")


  # parse text to json
  asjson <- jsonlite::fromJSON(text)

  # Check for errors - if no error object, continue to process content
  if (is.null(asjson$error$id)) {
    # set error.id to OK
    response = json2sf_otp(asjson)
    return (response)
  } else {
    # there is an error - return the error code and message
    response <-
      list("errorId" = asjson$error$id,
           "errorMessage" = asjson$error$msg)
    return (response)
  }
}


