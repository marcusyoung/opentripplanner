#' Build an OTP Graph
#'
#' OTP is run in Java and requires Java commands to be typed into the command line.
#' The function allows the parameters to be defined in R and automatically passed to Java.
#' This function builds a OTP graph from the Open Street Map and other files.
#'
#' @param dir A character string path to a directory containing the necessary files, see details
#' @param memory A positive integer. Amount of memory to assign to the OTP in GB, default is 2
#' @param router A character string for the name of the router, must match with contents of dir, default "current"
#' @details To build an OTP graph requires the following files to be in the directory
#' specified by the path variable.
#'
#' otp.jar - The OTP file, can be downloaded from https://repo1.maven.org/maven2/org/opentripplanner/otp/
#' /graphs - A sub-directory
#'   /current - A sub-directory with the name of the OTP router used in 'router' variaible
#'     osm.pbf - Required, pbf file containing the Open Street Map
#'     router-config.json - Required, json file containing configations settings for the OTP
#'     gtfs.zip - Optional, and number of GTFS files with transit timetables
#'     terrain.tif - Optional, GeoTiff image of terrain map
#'
#' The function will accept any file name for the .jar file, but it must be the only .jar file in that directory
#' OTP can support multiple routers (e.g. different regions), each router must have its own sub-directory in the graphs directory
#'
#' @examples
#' otpcon <- otp_connect(dir = "C:/temp", memory = 2, router = "current")
#' @export
#'
otp_build_graph <- function(dir = NULL,
                            memory = 2,
                            router = "current")
{
  # Run Checks
  jar_file <- otp_checks(dir = dir, router = router, graph = F)
  message("Basic checks completed, building graph, this may take a few minutes")

  # Set up OTP
  set_up <- try(system(paste0("java -Xmx",
                              memory,
                              "G -jar ",
                              dir,
                              "/",
                              jar_file,
                              " --build ",
                              dir,
                              "/graphs/",
                              router)
                       , intern = TRUE))

  # Check for errors
  if(grepl("ERROR",set_up[2])){
    message("Failed to build graph with message:")
    message(set_up[2])
  }



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
#' @param check Logical. If TRUE connection object is only returned if OTP
#'     instance and router are confirmed reachable. Optional, default is TRUE.

#' @examples
#' otpcon <- otp_connect()
#' otpcon <- otp_connect(router = "UK2018",
#'                       ssl = TRUE)
#' otpcon <- otp_connect(hostname = "ec2.us-west-2.compute.amazonaws.com",
#'                       router = "UK2018",
#'                       port = 8888,
#'                       ssl = TRUE)
#' @export
otp_setup <- function(dir = NULL,
                      memory = 2,
                      router = "current",
                      port = 8080,
                      securePort = 8081)
{
  # Run Checks
  jar_file <- otp_checks(dir = dir, router = router, graph = T)

  # Set up OTP
  set_up <- try(system(paste0("java -Xmx",
                              memory,
                              "G -jar ",
                              dir,
                              "/",
                              jar_file,
                              " --router ",
                              router,
                              " --graphs ",
                              dir,
                              "/graphs",
                              " --server --port ",
                              port,
                              " --securePort ",
                              securePort
                              )
                       , intern = FALSE, wait = FALSE))


  # Check for errors
  if(grepl("ERROR",set_up[2])){
    message("Failed to build graph with message:")
    message(set_up[2])
  }
  Sys.sleep(30)
  message("OTP is loading and may take a minute to be useable")
  message(paste0("Go to localhost:",port," in your browser to view the OTP"))

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
#' @export
#'
otp_checks <- function(dir = NULL, router = NULL, graph = FALSE)
{
  ### Checks
  # Check we have the directory defined
  if(!exists("dir")){
    warning("Path to the Open Trip Planner is not defined")
    stop()
  }
  # Check that the folder exists
  if(!dir.exists(dir)){
    warning(paste0("Unable to find directory: ",dir))
    stop()
  }
  # Check that the jar exists
  jar_file = list.files(dir, recursive = F)
  jar_file = jar_file[grepl("\\.jar",jar_file)]
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
      warning("OPT requires Java version 1.8 or later")
      stop()
    }
  }

  # Check that the graph exists
  if(graph){
    graph_file = list.files(paste0(dir,"/graphs/",router), recursive = F)
    graph_file = graph_file[grepl("Graph.obj",graph_file)]
    if(length(graph_file) == 0){
      warning(paste0("Unable to find Graph.obj in ",dir,"/graphs/",router))
      stop()
    }
  }


  ### End of Checks
    return(jar_file)
}
