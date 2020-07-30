gauss_kernel <- function(split_angle, shot_angle){
  
  h = 0.2
  kernel = exp(-((split_angle/shot_angle - 0.5)/h)^2)
  return(kernel)
}



gauss_kernel_adjusted <- function(split_angle, shot_angle){
  h_adjust = 20
  kernel_adjust =  exp(-((shot_angle)/h_adjust)^2)+0.2
  
  kernel =  exp(-((split_angle/shot_angle - 0.5)/kernel_adjust)^2)
  return(kernel)
}

x = seq(0,180, by = 0.01)
h3 = 25
kernel3 =  exp(-((x)/h3)^2)+0.2
plot(x, kernel3, type = 'l', main = "Gaussian kernel", xlab = "", ylim = c(0,2), ylab = "kernel value")
abline(v = 37, col = 'blue')




shot_angle2 = 180
perc_diff = seq(0,1, by = 0.001)
h2 = 0.3
kernel2 =  exp(-((perc_diff - 0.5)/h2)^2)
plot(split_angle2, kernel2, type = 'l', main = "Gaussian kernel", xlab = "", ylim = c(0,1), ylab = "kernel value")
abline(v = 0.5, col = 'blue')
