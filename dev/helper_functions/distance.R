distance <- function(loc1, loc2){
  #### Calculates the Euclidean distance between loc1 and loc2
  
  # INPUTS
  # loc1 = location 1 (vector of x,y coordinates)
  # loc2 = location 2 (vector of x,y coordinates)
  
  # OUTPUTS
  # distance = Euclidean distance
  
  distance = sqrt((loc1[1] - loc2[1])^2 + (loc1[2] - loc2[2])^2)
  return(distance)
}
