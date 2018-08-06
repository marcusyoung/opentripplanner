dir = "F:/otp-tutorial-master/otp-tutorial-master/demo"
memory = 60
library(opentripplanner)
library(tmap)
tmap_mode("view")
otp_build_graph(dir,memory = memory)
otp_setup(dir,
          memory = 2,
          router = "current",
          port = 8801,
          securePort = 8802)

otpcon <- otp_connect("localhost","current",8801)
fromPlace = c("53.43306", "-2.23881")
toPlace = c("53.52970", "-2.26456")

route = otp_plan(otpcon, fromPlace = fromPlace, toPlace = toPlace )

qtm(sf::st_zm(route))
# plot the elevation
coords = st_coordinates(route)
plot(coords[,3] / 0.3048)

otp_stop()

