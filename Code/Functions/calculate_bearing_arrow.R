calculate_bearing_arrow <- function(context, lon_from, lat_from, lon_to, lat_to) {
  
  # Convert all coordinates to radians
    # From
  lat1 <- lat_from * pi / 180
  lon1 <- lon_from * pi / 180
    # To
  lat2 <- lat_to * pi / 180
  lon2 <- lon_to * pi / 180
  
  # Calculate auxiliary variables
  d_lon <- lon2 - lon1
  y <- sin(d_lon) * cos(lat2)
  x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(d_lon)
  
  # Calculate Bearing using atan2
  # note that atan2 returns values between -pi and pi
  bearing_rad <- atan2(y, x)
  
  # Convert result to degreees
  bearing_deg <- bearing_rad * 180 / pi
  
  # atan2 returns values in the range -180° to +180°
  # Normalize to 0-360
  bearing_deg_norm <- (bearing_deg + 360) %% 360
}

# Create binding for arrow function

register_scalar_function(
  name = "calculate_bearing_arrow",
  fun = calculate_bearing_arrow,
  in_type = schema(
    lon_from = float64(),
    lat_from = float64(),
    lon_to = float64(),
    lat_to = float64()
  ),
  out_type = float64(),
  auto_convert = TRUE
)