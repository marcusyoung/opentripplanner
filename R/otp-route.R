#' Get the geometry of a route from the OTP
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.529258,-0.134649)`
#' @param toPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.506383,-0.088780,)`
#' @mode Character vector of modes of travel valid values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL default CAR
#' @date_time POSIXct, a date and time, defaults to current date and time
#' @arriveBy Logical, Whether the trip should depart or arrive at the specified date and time, default FALSE
#' @maxWalkDistance Numeric
#' @walkReluctance Numeric
#' @transferPenalty Numeric
#' @minTransferTime Numeric
#' @export
#'
otp_plan <- function(otpcon = NA,
           fromPlace = NA,
           toPlace = NA,
           mode = "CAR",
           date_time = Sys.time(),
           arriveBy = FALSE,
           maxWalkDistance = 800,
           walkReluctance = 2,
           transferPenalty = 0,
           minTransferTime = 0)
{
  # Check Valid Inputs
  if(!"otpconnect" %in% class(otpcon)){
    message("otpcon is not a valid otpconnect object")
    stop()
  }
  if(class(fromPlace) != "numeric" & length(fromPlace) != 2){
    message("fromPlace is not a valid latitude, longitude pair")
    stop()
  }else{
    fromPlace <- paste(fromPlace, collapse = ",")
  }
  if(class(toPlace) != "numeric" & length(toPlace) != 2){
    message("toPlace is not a valid latitude, longitude pair")
    stop()
  }else{
    toPlace <- paste(toPlace, collapse = ",")
  }
  if(!all(mode %in% c("TRANSIT","WALK","BICYCLE","CAR","BUS","RAIL"))){
    message("modes is not a valid, can be a charactor vector of any of these values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL")
    stop()
  }else{
    modes <- paste(modes, collapse = ",")
  }
  if(!all(class(date_time) %in% c("POSIXct","POSIXt") )){
    message("date_time is not a valid, must be object of class POSIXct POSIXt")
    stop()
  }else{
    date = substr(as.character(date_time),1,10)
    time = substr(as.character(date_time),12,16)
  }
  if(arriveBy){
    arriveBy = 'true'
  }else{
    arriveBy = 'false'
  }

  # Construct URL
  routerUrl <- paste0(ifelse(otpcon$ssl, 'https://', 'http://'),otpcon$hostname,
                      ":",otpcon$port,"/otp/routers/",otpcon$router,"/plan")

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

  # Check for errors - if no error object, continue to process content
  if(is.null(asjson$error$id)){
    response <- otp_json2sf(asjson)
    return(response)
  } else {
    # there is an error - return the error code and message
    response <-
      list("errorId" = asjson$error$id,
           "errorMessage" = asjson$error$msg)
    return(response)
  }


}

#' Convert Google Encoded Polyline into sf object
#'
#' @param line character - polyline
#' @export
#' @examples
#' None
polyline2linestring <- function(line){
  line = gepaf::decodePolyline(line)
  line = as.matrix(line[,2:1])
  line = sf::st_linestring(line)
  return(line)
}

#' Convert output from Open Trip Planner into sf object
#'
#' @param obj Object from transportapi.com read-in with
#' @export
#' @examples
#' None
otp_json2sf = function(obj) {
  requestParameters = obj$requestParameters
  plan = obj$plan
  debugOutput = obj$debugOutput

  itineraries = obj$plan$itineraries

  itineraries$startTime = as.POSIXct(itineraries$startTime / 1000 , origin = '1970-01-01', tz = "GMT")
  itineraries$endTime = as.POSIXct(itineraries$endTime / 1000 , origin = '1970-01-01', tz = "GMT")


  legs = list()
  #Loop over itineraries
  for(i in seq(1:nrow(itineraries))){
    leg = itineraries$legs[[i]]
    # split into parts
    vars = leg
    vars$from = NULL
    vars$to = NULL
    vars$steps = NULL
    vars$legGeometry = NULL

    # Come Bakc to
    #from = leg$from
    #to = leg$to
    #steps = leg$steps

    # Extract geometry
    legGeometry = leg$legGeometry
    lines = lapply(legGeometry$points, polyline2linestring)
    lines = sf::st_sfc(lines, crs = 4326)

    vars$geometry = lines
    vars = sf::st_sf(vars)
    vars$route_option = i

    #return to list
    legs[[i]] = vars
  }

  legs <- legs[!is.na(legs)]
  suppressWarnings(legs <- dplyr::bind_rows(legs))
  #rebuild the sf object
  legs <- as.data.frame(legs)
  legs$geometry <- sf::st_sfc(legs$geometry)
  legs <- sf::st_sf(legs)
  sf::st_crs(legs) <- 4326

  legs$startTime = as.POSIXct(legs$startTime / 1000 , origin = '1970-01-01', tz = "GMT")
  legs$endTime = as.POSIXct(legs$endTime / 1000 , origin = '1970-01-01', tz = "GMT")

  itineraries$legs = NULL
  itineraries = itineraries[legs$route_option,]
  itineraries = dplyr::bind_cols(itineraries,legs)

  itineraries = sf::st_as_sf(itineraries)
  sf::st_crs(itineraries) = 4326

  return(itineraries)
}
