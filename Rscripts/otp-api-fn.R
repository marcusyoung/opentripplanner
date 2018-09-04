# This is a set of functions used to query the OTP API - the beginnings of a comprehensive API wrapper for OTP


# Load the required libraries
library(curl)
library(httr)
library(jsonlite)


# otp connect function - just creates the URL currently

otpConnect <-
  function(hostname = 'localhost', router = 'default', port = '8080', ssl = 'false')
  {
    return (paste0(ifelse(ssl, "https://", "http://"), hostname, ":", port, "/otp/routers/", router))
  }

# Function to return distance for walk, cycle or car - desn't make sense for transit (bus or rail)
otpTripDistance <-
  function(otpcon,
           from,
           to,
           modes)
  {
    # convert modes string to uppercase - expected by OTP
    modes <- toupper(modes)

    # need to check modes are valid

    # setup router URL with /plan
    routerUrl <- paste0(otpcon, '/plan')

    # set parameters for API query
    params <- list(
      fromPlace = from,
      toPlace = to,
      mode = modes)

    # Use GET from the httr package to make API call and place in req - returns json by default
    req <- GET(routerUrl, query = params)

    # convert response content into text
    text <- content(req, as = "text", encoding = "UTF-8")

    # parse text to json
    asjson <- jsonlite::fromJSON(text)

    # Check for errors - if no error object, continue to process content
    if (is.null(asjson$error$id)) {
      # set error.id to OK
      error.id <- "OK"
      if (modes == "CAR") {
        # for car the distance is only recorded in the legs objects. Only one leg should be returned if mode is car and we pick that -  probably need error check for this
        response <-
          list(
            "errorId" = error.id,
            "duration" = asjson$plan$itineraries$legs[[1]]$distance
          )
        return (response)
        # for walk or cycle
      } else {
        response <-
          list("errorId" = error.id,
               "duration" = asjson$plan$itineraries$walkDistance)
        return (response)
      }
    } else {
      # there is an error - return the error code and message
      response <-
        list("errorId" = asjson$error$id,
             "errorMessage" = asjson$error$msg)
      return (response)
    }
  }


# Function to make an OTP API lookup and return trip time in simple or detailed form. The parameters from, to, modes, date and time must be specified in the function call other parameters have defaults set and are optional in the call.
otpTripTime <-
  function(otpcon,
           from,
           to,
           modes,
           detail = FALSE,
           date,
           time,
           maxWalkDistance = 800,
           walkReluctance = 2,
           arriveBy = 'false',
           transferPenalty = 0,
           minTransferTime = 0,
           walkSpeed = 1.4,
           bikeSpeed = 4.3,
           maxTransfers = 10,
           wheelchair = FALSE,
           preWaitTime = 60)
  {
    # convert modes string to uppercase - expected by OTP
    modes <- toupper(modes)

    routerUrl <- paste0(otpcon, '/plan')

    # set parameters for API query
    params <- list(
      fromPlace = from,
      toPlace = to,
      mode = modes,
      date = date,
      time = time,
      maxWalkDistance = maxWalkDistance,
      walkReluctance = walkReluctance,
      arriveBy = arriveBy,
      transferPenalty = transferPenalty,
      minTransferTime = minTransferTime,
      walkSpeed = walkSpeed,
      bikeSpeed = bikeSpeed,
      maxTransfers = maxTransfers,
      wheelchair = wheelchair)

    # Use GET from the httr package to make API call and place in req - returns json by default. Not using numItineraries due to odd OTP behaviour - if request only 1 itinerary don't necessarily get the top/best itinerary, sometimes a suboptimal itinerary is returned. OTP will return default number of itineraries depending on mode. This function returns the first of those itineraries.
    req <- GET(routerUrl, query = params)

    # convert response content into text
    text <- content(req, as = "text", encoding = "UTF-8")

    # parse text to json
    asjson <- jsonlite::fromJSON(text)

    # Check for errors - if no error object, continue to process content
    if (is.null(asjson$error$id)) {
      # set error.id to OK
      error.id <- "OK"
      # get first itinerary
      df <- asjson$plan$itineraries[1,]

      #Check if start of journey is within reasonable wait time
      required_start_time <- as.POSIXct(paste0(date," ",format(strptime(time, "%I:%M %p"), format="%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")
      journey_start_time <- df$startTime
      journey_start_time <- as.POSIXct(journey_start_time / 1000, origin = "1970-01-01")
      if (as.numeric(difftime(journey_start_time,required_start_time, units="mins")) < as.integer(preWaitTime)){

        # check if need to return detailed response
        if (detail == TRUE) {
          # need to convert times from epoch format
          df$start <-
            strftime(as.POSIXct(df$startTime / 1000, origin = "1970-01-01"), format="%H:%M:%S")
          df$end <-
            strftime(as.POSIXct(df$endTime / 1000, origin = "1970-01-01"), format="%H:%M:%S")

          # subset the dataframe ready to return
          ret.df <-
            subset(
              df,
              select = c(
                'start',
                'end',
                'duration',
                'walkTime',
                'transitTime',
                'waitingTime',
                'transfers'
              )
            )

          # convert seconds into minutes where applicable
          ret.df[, 3:6] <- round(ret.df[, 3:6] / 60, digits = 2)

          # rename walkTime column as appropriate - this a mistake in OTP
          if (modes == "CAR") {
            names(ret.df)[names(ret.df) == 'walkTime'] <- 'driveTime'
          } else if (modes == "BICYCLE") {
            names(ret.df)[names(ret.df) == 'walkTime'] <- 'cycleTime'
          }

          # If legs are null then use the input data to build the dataframe

          if (is.null(df$legs)){
            df2 <- data.frame(matrix(ncol = 12, nrow = 1))
            df2_col_names <- c("startTime",
                               "endTime",
                               "distance",
                               "name",
                               "from_lat",
                               "from_lon",
                               "to_lat",
                               "to_lon",
                               "mode",
                               "agencyName",
                               "routeShortName",
                               "duration")

            colnames(df2) <- df2_col_names

            df2$startTime <- df$start
            df2$endTime <- df$end
            df2$name <- 'Journey'
            df2$from_lat <- gsub(",.*$", "", from)
            df2$from_lon <- sub('.*,\\s*', '', from)
            df2$to_lat <- gsub(",.*$", "", to)
            df2$to_lon <- sub('.*,\\s*', '', to)
            df2$mode <- mode
            df2$agencyName <- mode
            df2$routeShortName <- mode
            df2$duration <- df$duration
            df2$distance <- round(distm(c(as.numeric(df2$from_lon),
                                          as.numeric(df2$from_lat)),
                                        c(as.numeric(df2$to_lon),
                                          as.numeric(df2$to_lat)),
                                        fun = distHaversine)/1000, digits = 2)
          } else {
            df2 <- df$legs[[1]]

            df2$startTime <-
              strftime(as.POSIXct(df2$startTime / 1000, origin = "1970-01-01"), format="%H:%M:%S")

            df2$endTime <-
              strftime(as.POSIXct(df2$endTime / 1000, origin = "1970-01-01"), format="%H:%M:%S")

            df2$distance <- round(df2$distance, digits=2)

            df2$duration <- round(df2$duration/60, digits = 2)
          }

          #Constructs dataframe of lats and longs from origin to destination

          output_table_nrow <- nrow(df2)
          output_table_col_names <- c("startTime",
                                      "endTime",
                                      "distance",
                                      "name",
                                      "from_lat",
                                      "from_lon",
                                      "to_lat",
                                      "to_lon",
                                      "mode",
                                      "agencyName",
                                      "routeShortName",
                                      "duration")

          output_table <- data.frame(matrix(ncol = 12, nrow = output_table_nrow))
          colnames(output_table) <- output_table_col_names

          for (i in 1:output_table_nrow){
            output_table$startTime[i] <- df2$startTime[i]
            output_table$endTime[i] <- df2$endTime[i]
            output_table$distance[i] <- df2$distance[i]
            output_table$name[i] <- df2$from$name[i]
            output_table$from_lat[i] <- df2$from$lat[i]
            output_table$from_lon[i] <- df2$from$lon[i]
            output_table$to_lat[i] <- df2$to$lat[i]
            output_table$to_lon[i] <- df2$to$lon[i]
            output_table$mode[i] <- df2$mode[i]
            if (df2$mode[i] == 'CAR' || df2$mode[i] == 'WALK' || df2$mode[i] == 'BICYCLE'){
              output_table$agencyName[i] <- df2$mode[i]
              output_table$routeShortName[i] <- df2$mode[i]
            } else {
              output_table$agencyName[i] <- df2$agencyName[i]
              output_table$routeShortName[i] <- df2$routeShortName[i]
            }
            output_table$duration[i] <- df2$duration[i]
          }

          output_table$name <- sub('[.]', '_', make.names(output_table$name, unique=TRUE)) # Makes sure there are no duplicates in names

          #Constructs a SpatialLinesDataFrame to be handled by the addPolylines function (without it leaflet colors don't work properly)

          poly_lines <-
            apply(output_table,1,function(x){
              points <- data.frame(lng=as.numeric(c(x["from_lon"],
                                                    x["to_lon"])),
                                   lat=as.numeric(c(x["from_lat"],
                                                    x["to_lat"])),
                                   stringsAsFactors = F)
              coordinates(points) <- c("lng","lat")
              Lines(Line(points),ID=x["name"])
            })

          row.names(output_table) <- output_table$name

          poly_lines <- SpatialLinesDataFrame(SpatialLines(poly_lines),output_table)

          # Output the response

          response <-
            list("errorId" = error.id,
                 "itineraries" = ret.df,
                 "trip_details" = df2,
                 "output_table" = output_table,
                 "poly_lines" = poly_lines)
          return (response)
        } else {
          # detail not needed - just return travel time in seconds
          response <-
            list("errorId" = error.id, "duration" = df$duration)
          return (response)
        }
      } else {
        # there is an error - return the error code and message
        response <-
          list("errorId" = asjson$error$id,
               "errorMessage" = asjson$error$msg)
        return (response)
      }
    }
  }

# Light version of otpTripTime for Choropleth use
otpChoropleth <-
  function(otpcon,
           from,
           to,
           modes,
           detail = FALSE,
           date,
           time,
           maxWalkDistance = 800,
           walkReluctance = 2,
           arriveBy = 'false',
           transferPenalty = 0,
           minTransferTime = 0,
           walkSpeed = 1.4,
           bikeSpeed = 4.3,
           maxTransfers = 10,
           wheelchair = FALSE)
  {
    # convert modes string to uppercase - expected by OTP
    modes <- toupper(modes)

    routerUrl <- paste0(otpcon, '/plan')

    params = list(
      fromPlace = from,
      toPlace = to,
      mode = modes,
      date = date,
      time = time,
      maxWalkDistance = maxWalkDistance,
      walkReluctance = walkReluctance,
      arriveBy = arriveBy,
      transferPenalty = transferPenalty,
      minTransferTime = minTransferTime,
      walkSpeed = walkSpeed,
      bikeSpeed = bikeSpeed,
      maxTransfers = maxTransfers,
      wheelchair = wheelchair)

    # Use GET from the httr package to make API call and place in req - returns json by default. Not using numItineraries due to odd OTP behaviour - if request only 1 itinerary don't necessarily get the top/best itinerary, sometimes a suboptimal itinerary is returned. OTP will return default number of itineraries depending on mode. This function returns the first of those itineraries.
    req <- GET(routerUrl, query = params)

    # convert response content into text
    text <- content(req, as = "text", encoding = "UTF-8")

    # parse text to json
    asjson <- jsonlite::fromJSON(text)

    # Check for errors - if no error object, continue to process content
    if (is.null(asjson$error$id)) {
      # set error.id to OK
      error.id <- "OK"
      # get first itinerary
      df <- asjson$plan$itineraries[1,]
      # check if need to return detailed response
      if (detail == TRUE) {
        # need to convert times from epoch format
        df$start <-
          strftime(as.POSIXct(df$startTime / 1000, origin = "1970-01-01"), format="%H:%M:%S")
        df$end <-
          strftime(as.POSIXct(df$endTime / 1000, origin = "1970-01-01"), format="%H:%M:%S")

        # subset the dataframe ready to return
        ret.df <-
          subset(
            df,
            select = c(
              'start',
              'end',
              'duration',
              'walkTime',
              'transitTime',
              'waitingTime',
              'transfers'
            )
          )

        # convert seconds into minutes where applicable
        ret.df[, 3:6] <- round(ret.df[, 3:6] / 60, digits = 2)
        # rename walkTime column as appropriate - this a mistake in OTP
        if (modes == "CAR") {
          names(ret.df)[names(ret.df) == 'walkTime'] <- 'driveTime'
        } else if (modes == "BICYCLE") {
          names(ret.df)[names(ret.df) == 'walkTime'] <- 'cycleTime'
        }

        # Output the response

        response <-
          list("errorId" = error.id, "itineraries" = ret.df)
        return (response)
      } else {
        # detail not needed - just return travel time in seconds
        response <-
          list("errorId" = error.id, "duration" = df$duration)
        return (response)
      }
    } else {
      # there is an error - return the error code and message
      response <-
        list("errorId" = asjson$error$id,
             "errorMessage" = asjson$error$msg)
      return (response)
    }
  }


# function to return isochrone (only works correctly for walk and/or transit modes - limitation of OTP)
otpIsochrone <-
  function(otpcon,
           from,
           modes,
           cutoff = 3600,
           batch = TRUE,
           date,
           time,
           maxWalkDistance = 800,
           walkReluctance = 2,
           walkSpeed = 1.4,
           bikeSpeed = 4.3,
           minTransferTime = 0,
           maxTransfers = 10,
           wheelchair = FALSE,
           arriveBy = 'false'
  )
  {
    # convert modes string to uppercase - expected by OTP
    modes <- toupper(modes)

    routerUrl <- paste0(otpcon, '/isochrone')

    # set parameters for API query
    params <- list(
      fromPlace=from,
      mode=modes,
      batch=batch,
      date=date,
      time=time,
      maxWalkDistance=maxWalkDistance,
      walkReluctance=walkReluctance,
      walkSpeed=walkSpeed,
      bikeSpeed=bikeSpeed,
      minTransferTime=minTransferTime,
      maxTransfers=maxTransfers,
      wheelchair=wheelchair,
      arriveBy=arriveBy)

    # api accepts multiple cutoffSec args:
    # http://docs.opentripplanner.org/en/latest/Intermediate-Tutorial/#calculating-travel-time-isochrones
    params <- append(params, as.list(setNames(cutoff, rep("cutoffSec", length(cutoff)))))

    # Use GET from the httr package to make API call and place in req - returns json by default
    req <- GET(routerUrl, query = params)

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
