#' Plan a journey with Open Trip Planner
#'
#' R interface to the Open Trip Planner journey planning API,
#' See [docs.opentripplanner.org](http://docs.opentripplanner.org/en/latest/) for details.
#'
#' @details
#' Requires a local instance of Open Trip Planner.
#' See the OTP [tutorials](http://docs.opentripplanner.org/en/latest/Basic-Tutorial/) for set up details.
#'
#' @param from Longitude/Latitude pair, e.g. `c(-0.134649,51.529258)` or SF points of class "sfc_POINT" "sfc" and length one
#' @param to Longitude/Latitude pair, e.g. `c(-0.088780,51.506383)` or SF points of class "sfc_POINT" "sfc" and length one
#' @param save_raw Boolean value which returns raw list from the geojson if TRUE (FALSE by default).
#' @inheritParams json2sf_otp
#' @seealso json2sf_tapi
#' @export
#' @examples
#' \dontrun{
#' from = c(-0.134649,51.529258) # Euston Station
#' to = c(-0.088780,51.506383) # Bridge House
#' r1 = journey(from, to)
#' }
# function to return isochrone (only works correctly for walk and/or transit modes - limitation of OTP)
otpIsochrone <-
  function(otpcon,
           from,
           modes,
           cutoff,
           batch = TRUE,
           date = '2017/06/12',
           time = '09:00:00'
  )
  {
    # convert modes string to uppercase - expected by OTP
    modes <- toupper(modes)

    routerUrl <- paste(otpcon, '/isochrone', sep = "")
    # need to check modes are valid
    # Use GET from the httr package to make API call and place in req - returns json by default
    req <- GET(
      routerUrl,
      query = list(
        fromPlace = from,
        mode = modes,
        cutoffSec = cutoff,
        batch = TRUE,
        date = date,
        time= time
      )
    )
    # convert response content into text
    text <- content(req, as = "text", encoding = "UTF-8")

    # Check that geojson is returned

    if (grepl("\"type\":\"FeatureCollection\"", text)) {
      status <- "OK"
    } else {
      status <- "ERROR"
    }
    response <-
      list("status" = status,
           "response" = text)
    return (response)
  }
