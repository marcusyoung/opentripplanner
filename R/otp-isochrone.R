#' Get the geometry of a route from the OTP
#'
#' @param otpcon OTP connection object produced by otp_connect()
#' @param fromPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.529258,-0.134649)`
#' @param toPlace Numeric vector, Latitude/Longitude pair, e.g. `c(51.506383,-0.088780,)`
#' @param mode Character vector of modes of travel valid values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL, default CAR
#' @param date_time POSIXct, a date and time, defaults to current date and time
#' @param arriveBy Logical, Whether the trip should depart or arrive at the specified date and time, default FALSE
#' @param maxWalkDistance Numeric passed to OTP
#' @param walkReluctance Numeric passed to OTP
#' @param transferPenalty Numeric passed to OTP
#' @param minTransferTime Numeric passed to OTP
#' @param full_elevation Logical, should the full elevation profile be returned, defualt FALSE
#'
#' @export
#'
#' @detials
#' This function returns a SF data.frame with one row for each leg of the jounrey
#' (a leg is defined by a change in mode). For transit more than one route option may be returned
#' and is indicated by the route_option column.
#'
#'
#' Elevation
#' OTP supports elevation data, and can return the elevation profile of the route if available.
#' OTP returns the elevation profile separately from the XY coordinates, this means there is not
#' direct match between the number of XY points and the number of Z points.  OTP also only returns
#' the elevation profile for the first leg of the route (this appears to be a bug).
#' As default the otp_plan function matches the elevation profile to the XY coordinates to return
#' a SF linestring with XYZ coordinates. If you require a more detailed elevation profile,
#' the full_elevation parameter will return a nested data.frame with three columns.
#' first and second are returned from OTP, while distance is the cumulative distance along the
#' route and is derived from First.
#'
otp_isochrone <- function(otpcon = NA,
                     fromPlace = NA,
                     mode = "CAR",
                     date_time = Sys.time(),
                     arriveBy = FALSE,
                     maxWalkDistance = 800,
                     walkReluctance = 5,
                     transferPenalty = 0,
                     minTransferTime = 600,
                     cutoffSec = c(600,1200,1800,2400, 3000, 3600))
{
  # Check Valid Inputs
  if(!"otpconnect" %in% class(otpcon)){
    message("otpcon is not a valid otpconnect object")
    stop()
  }
  if(class(fromPlace) != "numeric" | length(fromPlace) != 2){
    message("fromPlace is not a valid latitude, longitude pair")
    stop()
  }else{
    if(fromPlace[1] <= 90 & fromPlace[1] >= -90 &  fromPlace[2] <= 180 & fromPlace[2] >= -180){
      fromPlace <- paste(fromPlace, collapse = ",")
    }else{
      message("fromPlace coordinates excced valid values +/- 90 and +/- 180 degrees")
      stop()
    }

  }
  if(!all(mode %in% c("TRANSIT","WALK","BICYCLE","CAR","BUS","RAIL"))){
    message("mode is not a valid, can be a character vector of any of these values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL")
    stop()
  }else{
    mode <- paste(mode, collapse = ",")
  }
  if(!all(class(date_time) %in% c("POSIXct","POSIXt") )){
    message("date_time is not a valid, must be object of class POSIXct POSIXt")
    stop()
  }else{
    date <- substr(as.character(date_time),1,10)
    time <- substr(as.character(date_time),12,16)
  }
  if(class(cutoffSec) != "numeric"){
    message("cutoffSec is not valid vector of numbers")
    stop()
  }
  if(arriveBy){
    arriveBy <- 'true'
  }else{
    arriveBy <- 'false'
  }

  # Construct URL
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl,"/isochrone")

  query <- list(
    fromPlace = fromPlace,
    mode = mode,
    date = date,
    time = time,
    maxWalkDistance = maxWalkDistance,
    walkReluctance = walkReluctance,
    arriveBy = arriveBy,
    transferPenalty = transferPenalty,
    minTransferTime = minTransferTime
  )
  cutoffSec <-  as.list(cutoffSec)
  names(cutoffSec) = rep("cutoffSec",length(cutoffSec))
  query <- c(query,cutoffSec)

  req <- httr::GET(
    routerUrl,
    query = query
  )

  # convert response content into text
  text <- httr::content(req, as = "text", encoding = "UTF-8")

  if(nchar(text) < 200){
    warning("Failed to get isochrone, returning error message")
    return(text)
  }else{
    # parse to sf
    response = sf::st_read(text, quiet = T)
    response$id = seq(1:nrow(response))
    return(response)
  }

}

