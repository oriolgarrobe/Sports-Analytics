angle <- function(a, b, c){
  ### Calculate the angle A between sides b and c in a triangle (a is the goalline, A is the shot taker's angle)
  # INPUTS
  # a = side opposite of the angle we want to calculate
  # b = side next to the angle we want to calculate
  # c = other side next to the angle we want to calculate
  
  # OUTPUTS
  # angle = angle in degrees
  
  angle_rad = acos((b^2 + c^2 - a^2) / (2*b*c))
  angle_deg = angle_rad * 180 / pi
  
  return(angle_deg)
}
