dir = "F:/otp-tutorial-master/otp-tutorial-master/demo"
memory = 60
#otp_build_graph(dir,memory = memory)
otp_setup(dir,
          memory = 2,
          router = "current",
          port = 8801,
          securePort = 8802)

otpcon <- otp_connect("localhost","current",8801)
fromPlace = c("53.49703","-2.25306")
toPlace = c("53.47252","-2.24413")

route = otp_plan(otpcon, fromPlace = fromPlace, toPlace = toPlace )
qtm(route)


otp_stop()
