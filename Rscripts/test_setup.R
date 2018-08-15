dir = "F:/otp-tutorial-master/otp-tutorial-master/demo"
memory = 10
devtools::install_github("mem48/opentripplanner-malcolm")
library(opentripplanner)
source("R/otp-setup.R")
source("R/otp-route.R")
library(tmap)
tmap_mode("view")
#otp_build_graph(dir,memory = memory, analyst = TRUE)
otp_setup(dir,
          memory = 30,
          router = "current",
          port = 8801,
          securePort = 8802,
          analyst = TRUE)

otpcon <- otp_connect("localhost","current",8801)
#fromPlace = c(53.50101, -2.25443)
#toPlace = c(53.46281,-2.23967)


fromPlace = c(53.53541, -2.13066)
toPlace = c(53.40749, -2.32635)

# fails for transit where there are multiple legs to each itinary


route = otp_plan(otpcon, fromPlace = fromPlace, toPlace = toPlace, mode = c("CAR"), full_elevation = T)

route = route[route$route_option == 1,]
qtm(sf::st_zm(route), lines.col = "mode", lines.lwd = 3)
# plot the elevation
coords = st_coordinates(route)
lens = st_distance(x = st_cast(route$geometry, "POINT")[1:(nrow(coords)-1)], y = st_cast(route$geometry, "POINT")[2:(nrow(coords))],  by_element = T)
lens = cumsum(lens)
lens = c(0,lens)



plot(route$elevation[[1]]$second ~ route$elevation[[1]]$distance, col="black", type="o", pch = 9, cex = 0.5, xlab = "Route Distance (m)", ylab = "Elevation (m)", main = "Comparison of full elevation (black) and extracted elevation (red)")
lines(coords[,3] ~ lens, col="red", type="o", xlim=c(0,max(lens)), xlab = "Distance (m)", ylab = "Elevation (m)", pch = 9, cex = 0.5)


otp_stop()

