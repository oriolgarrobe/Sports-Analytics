triangle_area <- function(a,b,c){
  ### Calculate the area of triangle abc
  # INPUTS
  # a = point 1
  # b = point 2
  # c = point 3
  
  # OUTPUT
  # area = area of triangle
  
  # Source
  # https://www.geeksforgeeks.org/check-whether-a-given-point-lies-inside-a-triangle-or-not/
  
  area = abs((a[1] * (b[2]-c[2]) + b[1] * (c[2]-a[2]) + c[1] * (a[2]-b[2])) / 2)
  return(area)
  
}

a = c(1,8)
c = c(2,1)
b = c(8,9)
triangle_area(a,b,c)
