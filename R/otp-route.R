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
           minTransferTime = 0,
           full_elevation = FALSE)
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
  if(class(toPlace) != "numeric" | length(toPlace) != 2){
    message("toPlace is not a valid latitude, longitude pair")
    stop()
  }else{
    if(toPlace[1] <= 90 & toPlace[1] >= -90 &  toPlace[2] <= 180 & toPlace[2] >= -180){
      toPlace <- paste(toPlace, collapse = ",")
    }else{
      message("fromPlace coordinates excced valid values +/- 90 and +/- 180 degrees")
      stop()
    }

  }
  if(!all(mode %in% c("TRANSIT","WALK","BICYCLE","CAR","BUS","RAIL"))){
    message("mode is not a valid, can be a charactor vector of any of these values TRANSIT, WALK, BICYCLE, CAR, BUS, RAIL")
    stop()
  }else{
    mode <- paste(mode, collapse = ",")
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
    response <- otp_json2sf(asjson, full_elevation)
    return(response)
  } else {
    # there is an error - return the error code and message
    response <-
      list("errorId" = asjson$error$id,
           "errorMessage" = asjson$error$msg)
    warning("A routing error has occured, returing error message")
    return(response)
  }


}


#' Convert Google Encoded Polyline into sf object
#'
#' OTP returns the 2d route as a polylinebean and the elevation profile as vector of numbers
#' But the number of points for each is not the same, as 2D line only has a point at change of driections
#' While elevation is regually spaced. If elevation is supplied the correct heights are matched
#'
#' @param line character - polyline
#' @param elevation numeric - vector of elevations
#' @export
#' @examples
#' None
polyline2linestring <- function(line, elevation = NULL){
  line = gepaf::decodePolyline(line)
  line = as.matrix(line[,2:1])
  linestring = sf::st_linestring(line)
  if(exists("elevation")){
    # Some modes don't have elevation e.g TRANSIT, check for this
    if(all(is.na(elevation))){
      ele = rep(0,length(linestring)/2)
    }else{
      point = sf::st_cast(sf::st_sfc(linestring),"POINT")
      len = sf::st_length(linestring)
      dist = sf::st_distance(point[seq(1,length(point)-1)],point[seq(2,length(point))], by_element = T)
      dist = dist /len
      dist = cumsum(dist) #proprotion of the elivation data to get the heights from
      val = round(dist *length(elevation),0)
      ele = elevation[c(1,val)]
    }
    linestring3D = cbind(line, ele)
    linestring3D = sf::st_linestring(linestring3D, dim = "XYZ")
    return(linestring3D)
  }else{
    return(line)
  }

}





#' Convert output from Open Trip Planner into sf object
#'
#' @param obj Object from the OTP API to process
#' @param full_elevation logical should the full elevation profile be returned (if available)
#' @export
#' @examples
#' None
otp_json2sf = function(obj, full_elevation = FALSE) {
  requestParameters = obj$requestParameters
  plan = obj$plan
  debugOutput = obj$debugOutput

  itineraries = plan$itineraries

  itineraries$startTime = as.POSIXct(itineraries$startTime / 1000 , origin = '1970-01-01', tz = "GMT")
  itineraries$endTime = as.POSIXct(itineraries$endTime / 1000 , origin = '1970-01-01', tz = "GMT")


  legs = list()
  #Loop over itineraries
  for(i in seq(1,nrow(itineraries))){
    leg = itineraries$legs[[i]]
    # split into parts
    vars = leg
    vars$from = NULL
    vars$to = NULL
    vars$steps = NULL
    vars$legGeometry = NULL

    # Extract geometry
    legGeometry = leg$legGeometry$points

    # Check for Elevations
    steps = leg$steps
    elevation = lapply(seq(1,length(legGeometry)), function(x){leg$steps[[x]]$elevation})
    if(sum(lengths(elevation))>0){
      # We have Elevation Data
      # Extract the elevation values
      elevation = lapply(seq(1,length(legGeometry)), function(x){dplyr::bind_rows(elevation[[x]])})
      elevation = lapply(seq(1,length(legGeometry)), function(x){if(nrow(elevation[[x]]) == 0){NA}else{elevation[[x]]$second }})
      lines = list()
      for(j in seq(1,length(legGeometry))){
        lines[[j]] = polyline2linestring(line = legGeometry[j], elevation = elevation[[j]])
      }
    }else{
      lines = polyline2linestring(legGeometry)
    }

    lines = sf::st_sfc(lines, crs = 4326)

    vars$geometry = lines
    vars = sf::st_sf(vars)
    vars$route_option = i

    #Add full elevation if required
    if(full_elevation){
      vars$elevation = elevation
    }

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
