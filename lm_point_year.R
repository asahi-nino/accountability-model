# lm on each row of district points

years <- as.numeric(colnames(dist_points)[2:6])

f = function(x, output) {
  name = x[1]
  
  total_points = x[2:6]
  
  non_na_points = total_points[!(is.na(total_points) | total_points=="E")]
  non_na_years = years[!(is.na(total_points) | total_points=="E")]
  
  year_points_lm <- lm(non_na_points ~ non_na_years)
  
  coef <- year_points_lm$coefficients
  
  intercept <- ifelse(is.na(coef[1]), 0, coef[1])
  slope <- ifelse(is.na(coef[2]), 0, coef[2])
  
  # plot(non_na_points ~ non_na_years, main = name)
  # abline(a = intercept, b = slope)
  
  #cat(name,"intercept: ", coef[1],"- slope: ", coef[2], "\n")
  # cat(name, " ", non_na_years, " ", non_na_points, "\n")
  
  slope
}

master_table$growth <- apply(dist_points[,1:6], 1, f)
