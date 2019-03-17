#' internal function that connects to an OTP instance, retrieves raw data from
#' PlannerResource, and performs error handling.
otp_plan_internal <- function(otpcon = NA,
                     fromPlace = NA,
                     toPlace = NA,
                     mode = "CAR",
                     date_time = Sys.time(),
                     arriveBy = FALSE,
                     maxWalkDistance = 800,
                     walkReluctance = 2,
                     transferPenalty = 0,
                     minTransferTime = 0,
                     full_elevation = FALSE)
{
  # Check Valid Inputs
  checkmate::assert_class(otpcon, "otpconnect")
  checkmate::assert_numeric(fromPlace,
                            lower =  -180,
                            upper = 180,
                            len = 2)
  fromPlace <- paste(fromPlace, collapse = ",")
  checkmate::assert_numeric(toPlace,
                            lower =  -180,
                            upper = 180,
                            len = 2)
  toPlace <- paste(toPlace, collapse = ",")
  mode <- toupper(mode)
  checkmate::assert_subset(
    mode,
    choices = c("TRANSIT", "WALK", "BICYCLE", "CAR", "BUS", "RAIL"),
    empty.ok = F
  )
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m/%d/%Y")
  time <- format(date_time, '%I:%M:%S')
  #time <- tolower(time)

  if (arriveBy) {
    arriveBy <- 'true'
  } else{
    arriveBy <- 'false'
  }

  # Construct URL
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/plan")

  req <- httr::GET(
    routerUrl,
    query = list(
      fromPlace = fromPlace,
      toPlace = toPlace,
      mode = mode,
      date = date,
      time = time,
      maxWalkDistance = maxWalkDistance,
      walkReluctance = walkReluctance,
      arriveBy = arriveBy,
      transferPenalty = transferPenalty,
      minTransferTime = minTransferTime
    )
  )

  # convert response content into text
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  # parse text to json
  asjson <- jsonlite::fromJSON(text)

  # Check for errors - if no error object, set status to "OK" and return asjson
  if (is.null(asjson$error$id)) {
    response <- list("status" = "OK", "json" = asjson)
    return(response)
  } else {
    # there is an error - set status to "ERROR" and return the error code and message
    response <-
      list(
        "status" = "ERROR",
        "errorId" = asjson$error$id,
        "errorMessage" = asjson$error$msg
      )
    return(response)
  }
}
