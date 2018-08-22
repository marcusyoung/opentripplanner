#' Build an OTP Graph
#'
#' OTP is run in Java and requires Java commands to be typed into the command line.
#' The function allows the parameters to be defined in R and automatically passed to Java.
#' This function builds a OTP graph from the Open Street Map and other files.
#'
#' @param otp A character string, path to the OTP .jar file
#' @param dir A character string, path to a directory containing the necessary files, see details
#' @param memory A positive integer. Amount of memory to assign to the OTP in GB, default is 2
#' @param router A character string for the name of the router, must match with contents of dir, default "current"
#' @return
#' Returns and log messages produced by OTP, and will return the message "Graph built" if sucessfull
#' @details
#' The OTP .jar file can be downloaded from https://repo1.maven.org/maven2/org/opentripplanner/otp/
#'
#' To build an OTP graph requires the following files to be in the directory
#' specified by the dir variable.
#'
#' /graphs - A sub-directory
#'   /current - A sub-directory with the name of the OTP router used in 'router' variaible
#'     osm.pbf - Required, pbf file containing the Open Street Map
#'     router-config.json - Required, json file containing configations settings for the OTP
#'     gtfs.zip - Optional, and number of GTFS files with transit timetables
#'     terrain.tif - Optional, GeoTiff image of terrain map
#'
#' The function will accept any file name for the .jar file, but it must be the only .jar file in that directory
#' OTP can support multiple routers (e.g. different regions), each router must have its own sub-directory in the graphs directory
#' @examples
#' otp_build_graph("C:/otp")
#' @export
otp_build_graph <- function(otp = NULL,
                            dir = NULL,
                            memory = 2,
                            router = "current",
                            analyst = TRUE)
{
  # Run Checks
  jar_file <- otp_checks(otp = otp, dir = dir, router = router, graph = F)
  message("Basic checks completed, building graph, this may take a few minutes")

  # Set up OTP
  text <- paste0('java -Xmx',
                    memory,
                    'G -jar "',
                    otp,
                    '/',
                    jar_file,
                    '" --build "',
                    dir,
                    '/graphs/',
                    router,
                    '"')

  if(analyst){
    text <- paste0(text," --analyst")
  }


  set_up <- try(system(text, intern = TRUE))

  # Check for errors
  if(grepl("ERROR",set_up[2])){
    message("Failed to build graph with message:")
    message(set_up[2])
  }else{
    message("Graph built")

  }
  return(set_up)


}


#' Set up an OTP instance.
#'
#' OTP is run in Java and requires Java commands to be typed into the command line.
#' The function allows the parameters to be defined in R and automatically passed to Java.
#' This function sets up a local instance of OTP, for remote versions see documentation.
#'
#' The function assumes you have run otp_build_graph()
#'
#' @param dir A character string path to the directory containing the OTP .jar file and the OTP graph
#' @param memory A positive integer. Amount of memory to assign to the OTP in GB, default is 2
#' @param router A character vector for the name of the routers, must match with contents of dir, default "current"
#' Only a single router is currently supported
#' @param port A positive integer. Optional, default is 8080.
#' @param secure_port A positive integer. Optional, default is 8081.
#' @param analyist Logical. Should the analyist features be loaded? Default FALSE
#' @param wait Logical, Should R wait until OTP has loaded before running next line of code, default TRUE
#' @return
#' This function does not return a value to R.
#' If wait is TRUE R will wait until OTP is running (maximum of 5 minutes)
#' @examples
#' otp_setup("C:/otp","C:/data")
#' otp_setup("C:/otp","C:/data", memory = 5, analyst = TRUE)
#' @export
otp_setup <- function(otp = NULL,
                      dir = NULL,
                      memory = 2,
                      router = "current",
                      port = 8080,
                      securePort = 8081,
                      analyst = FALSE,
                      wait = TRUE)
{
  # Run Checks
  jar_file <- otp_checks(otp = otp, dir = dir, router = router, graph = T)

  # Set up OTP
  text <- paste0('java -Xmx',
                    memory,
                    'G -jar "',
                    otp,
                    '/',
                    jar_file,
                    '" --router ',
                    router,
                    ' --graphs "',
                    dir,
                    '/graphs"',
                    ' --server --port ',
                    port,
                    ' --securePort ',
                    securePort
  )

  if(analyst){
    text <- paste0(text," --analyst")
  }

  set_up <- try(system(text, intern = FALSE, wait = FALSE))


  # Check for errors
  if(grepl("ERROR",set_up[2])){
    message("Failed to build graph with message:")
    message(set_up[2])
  }

  message(paste0(Sys.time()," OTP is loading and may take a while to be useable"))

  if(wait){
    Sys.sleep(30)

    # Check if connected
    for(i in 1:10){
      #message(paste0("Attempt ",i))
      otpcon <- try(otp_connect(hostname = "localhost",
                                             router = router,
                                             port = port,
                                             ssl = FALSE,
                                             check = TRUE), silent = T)
      if("otpconnect" %in% class(otpcon)){
        message(paste0(Sys.time()," OTP is ready to use Go to localhost:",port," in your browser to view the OTP"))
        browseURL(paste0(ifelse(otpcon$ssl,"https://","http://"),"localhost:",port))
        break
      }else{
        if(i < 10){
          Sys.sleep(30)
        }else{
          message(paste0(Sys.time()," OTP is taking an unusually long time to load, releasing R to your control"))
        }

      }
    }
  }

}

#' Stop and OTP Instance
#'
#' OTP is run in Java and requires Java commands to be typed into the command line.
#' The function allows the parameters to be defined in R and automatically passed to Java.
#' This function stops an already running OTP instance
#'
#' The function assumes you have run otp_setup()
#'
#' @export
otp_stop <- function()
{
  if(Sys.info()[['sysname']] == "Windows"){
    readline(prompt="This will force Java to close, Press [enter] to continue, [escape] to abort")
    system("Taskkill /IM java.exe /F", intern = TRUE)
  }else{
    message("This function currently only works in Windows")
  }

}


#' Basic OTP Setup Checks
#'
#' Checks to run before setting up the OTP
#'
#' @param dir A character string path to a folder containing the necessary files, see details
#' @param router A character string for the name of the router, must match with contents of dir, default "current"
#' @param graph Logical, check for graph, default = FALSE
#'
otp_checks <- function(otp = NULL, dir = NULL, router = NULL, graph = FALSE)
{
  ### Checks
  # Check we have the directory defined
  if(!exists("otp")){
    warning("Path to the Open Trip Planner is not defined")
    stop()
  }
  if(!exists("dir")){
    warning("Path to the files to build graph are not defined")
    stop()
  }
  # Check that the folder exists
  if(!dir.exists(otp)){
    warning(paste0("Unable to find directory: ",otp))
    stop()
  }
  if(!dir.exists(dir)){
    warning(paste0("Unable to find directory: ",dir))
    stop()
  }
  # Check that the jar exists
  jar_file <- list.files(otp, recursive = F)
  jar_file <- jar_file[grepl("\\.jar",jar_file)]
  if(length(jar_file) == 0){
    warning(paste0("Unable to find .jar file in ",dir))
    stop()
  }

  # Check for the graphs folder
  if(!dir.exists(paste0(dir,"/graphs"))){
    warning(paste0("The graphs sub-folder could not be found in: ",dir))
    stop()
  }
  # Check for the router folder
  if(!dir.exists(paste0(dir,"/graphs/",router))){
    warning(paste0("The router sub-folder named '",router,"', could not be found in: ",dir,"/graphs"))
    stop()
  }

  # Check we have correct verrsion of Java
  java_version <- try(system("java -version", intern = TRUE))
  if(class(java_version) == "try-error"){
    warning("R was unable to detect a version of Java")
    stop()
  }else{
    java_version <- java_version[1]
    java_version <- strsplit(java_version,"\"")[[1]][2]
    java_version <- strsplit(java_version,"\\.")[[1]][1:2]
    java_version <- as.numeric(paste0(java_version[1],".",java_version[2]))
    if(java_version < 1.8){
      warning("OTP requires Java version 1.8 or later")
      stop()
    }
  }

  # Check that the graph exists
  if(graph){
    graph_file <- list.files(paste0(dir,"/graphs/",router), recursive = F)
    graph_file <- graph_file[grepl("Graph.obj",graph_file)]
    if(length(graph_file) == 0){
      warning(paste0("Unable to find Graph.obj in ",dir,"/graphs/",router))
      stop()
    }
  }


  ### End of Checks
    return(jar_file)
}
