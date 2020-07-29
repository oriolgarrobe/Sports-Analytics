is_in_triangle <- function(a, b, c, p){
  ### Evaluate if point p is inside the triangle abc
  # INPUTS
  # a = point 1
  # b = point 2
  # c = point 3
  # p = point to evaluate
  
  # OUTPUT
  # is_in_triangle = Boolean
  
  # Source
  # https://www.geeksforgeeks.org/check-whether-a-given-point-lies-inside-a-triangle-or-not/
  
  # calculate area(abc)
  A = triangle_area(a,b,c)
  
  # calculate partial triangle areas
  A1 = triangle_area(a,b,p)
  A2 = triangle_area(b,c,p)
  A3 = triangle_area(c,a,p)
  
  # if the partial sums equal the total -> p is inside the triangle
  is_in_triangle <- isTRUE(all.equal(A1 + A2 + A3, A))
  
  return(is_in_triangle)
}

a = c(1,8)
c = c(2,1)
b = c(8,9)
p = c(2,2)
is_in_triangle(a,b,c,p)

