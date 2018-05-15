#' Convert Google Encoded Polyline into sf object
#'
#' @param line character - polyline
#' @export
#' @examples
#' None
polyline2linestring <- function(line){
  line = gepaf::decodePolyline(line)
  line = as.matrix(line[,2:1])
  line = st_linestring(line)
  return(line)
}

#' Convert output from Open Trip Planner into sf object
#'
#' @param obj Object from transportapi.com read-in with
#' @export
#' @examples
#' None
json2sf_otp = function(obj) {
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
    lines = st_sfc(lines, crs = 4326)

    vars$geometry = lines
    vars = st_sf(vars)
    vars$route_option = i

    #return to list
    legs[[i]] = vars
  }

  legs <- legs[!is.na(legs)]
  suppressWarnings(legs <- dplyr::bind_rows(legs))
  #rebuild the sf object
  legs <- as.data.frame(legs)
  legs$geometry <- st_sfc(legs$geometry)
  legs <- st_sf(legs)
  st_crs(legs) <- 4326

  legs$startTime = as.POSIXct(legs$startTime / 1000 , origin = '1970-01-01', tz = "GMT")
  legs$endTime = as.POSIXct(legs$endTime / 1000 , origin = '1970-01-01', tz = "GMT")

  itineraries$legs = NULL
  itineraries = itineraries[legs$route_option,]
  itineraries = dplyr::bind_cols(itineraries,legs)

  itineraries = sf::st_as_sf(itineraries)
  st_crs(itineraries) = 4326

  return(itineraries)
}




