gauss_kernel <- function(split_angle, shot_angle, adjusted = FALSE){
  ### Calculates (shot angle adjusted) the Gaussian kernel for the ratio of the angle split by the goalkeeper and the shot itself
  ### To be used as goalkeeper positioning
  # INPUTS
  # split_angle = the angle of shot between the upper goalpost and the goalkeeper
  # shot_angle = full shot angle
  # adjusted = Boolean, if TRUE, the function calculates a shot angle adjusted kernel, where tighter shot angles are less strictly evaluated
  #            (if the shot angle is only 10 for example, it's not considered very bad positioning if the goalkeeper only splits the angle 9 to 1)
  
  # OUTPUTS
  # kernel = Gaussian kernel
  
  if (adjusted) {
    # create a kernel that gives a higher value for small values (shot angles)
    # kernel width (smoothing coefficient)
    h_adjust = 20
    kernel_adjust =  exp(-((shot_angle)/h_adjust)^2) + 0.2
    # create a kernel that gives more value to values close to 0.5 (meaning that the angle is split in half)
    kernel =  exp(-((split_angle/shot_angle - 0.5)/kernel_adjust)^2)
  } else {
    # create a kernel that gives more value to values close to 0.5 (meaning that the angle is split in half)
    h = 0.2
    kernel = exp(-((split_angle/shot_angle - 0.5)/h)^2)
  }
  return(kernel)
}

# 
# 
# gauss_kernel_adjusted <- function(split_angle, shot_angle){
#   h_adjust = 20
#   kernel_adjust =  exp(-((shot_angle)/h_adjust)^2)+0.2
#   
#   kernel =  exp(-((split_angle/shot_angle - 0.5)/kernel_adjust)^2)
#   return(kernel)
# }

# test

# x = seq(0,180, by = 0.01)
# h3 = 20
# kernel3 =  exp(-((x)/h3)^2)+0.2
# plot(x, kernel3, type = 'l', main = "Gaussian kernel", xlab = "", ylim = c(0,2), ylab = "kernel value")
# abline(v = 37, col = 'blue')
# 
# 
# shot_angle2 = 180
# perc_diff = seq(0,1, by = 0.001)
# h2 = 0.2
# kernel2 =  exp(-((perc_diff - 0.5)/h2)^2)
# plot(split_angle2, kernel2, type = 'l', main = "Gaussian kernel", xlab = "", ylim = c(0,1), ylab = "kernel value")
# abline(v = 0.5, col = 'blue')
