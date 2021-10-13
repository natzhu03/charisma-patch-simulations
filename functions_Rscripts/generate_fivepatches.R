#this function generates five patches with the centers fixed

generate_fivepatches <- function(freq, height, width) {
  
  all_orange <- matrix(sample(2, height*width, replace = TRUE, prob = c(0, 1)), nrow = width, ncol = height)
  colors <- c("blue", "orange")
  dir <- "/Users/nataliezhu/Desktop/Patch Generation Images/"
  
  #five is both the width and height of each of the 5 patches
  
  (height*width) -> total_area
  total_area * freq -> patch_total_area
  patch_total_area/5 -> patch_area
  sqrt(patch_area) -> five 
  
for(i in 1:length(freq)) {
  png(file = paste0(dir, "five_patches_distribution", freq[i], ".png") ,
      width = width, height = height)
  m <- all_orange
  
  #generates center patch 1
  start_row = height/2 - five[i]/2 
  start_col = width/2 - five[i]/2   
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  
  #generates top left patch 2
  start_row = height/4 - five[i]/2 
  start_col = width/4 - five[i]/2
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  
  #generates top right patch 3
  start_row = height/4 - five[i]/2 
  start_col = (3*width)/4 - five[i]/2
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  
  #generates bottom left patch 4
  start_row = (3*height)/4 - five[i]/2 
  start_col = width/4 - five[i]/2
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  
  #generates bottom right patch 5
  start_row = (3*height)/4 - five[i]/2 
  start_col = (3*width)/4 - five[i]/2
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }

  image(1:(nrow(m)), 1:ncol(m), m, col = colors, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  dev.off()
  }
}


