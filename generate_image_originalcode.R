freq <- c(.1, .05, .04, .03, .02, .01) #these are the frequencies of the patches we want to generate
single <- c(162, 114, 102, 89, 72, 51.2) #this is the height and width of the single patches (sqrt (freq*total image area)
five <- c(72, 51, 46, 40, 32, 23) #this is the height and width of the five patches for the random distribution generation

all_orange <- matrix(sample(2, 262144, replace = TRUE, prob = c(0, 1)), nrow = 512) #we wanted the image to be 512x512 pixels
colors <- c("blue", "orange")
dir <- "/Users/nataliezhu/Desktop/R!/"


for(i in 1:length(freq)) {
  #generates random distributions
  png(file = paste0(dir, "random_distribution", freq[i], ".png") ,
      width=512, height=512)
  m <- matrix(sample(2, 262144, replace = TRUE, prob = c(freq[i], 1 - freq[i])), ncol = 512)
  image(1:(nrow(m)), 1:ncol(m), m, col = colors, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  dev.off()
  
  #generates single patch distributions
  png(file = paste0(dir, "single_patch_distribution", freq[i], ".png") ,
      width=512, height=512)
  start_row <- floor(runif(1, 20, 320))
  start_col <- floor(runif(1, 20, 320))
  m <- all_orange
  for(k in start_row:(start_row + single[i])) {
    for(j in start_col:(start_col + single[i])) {
      m[k, j] <- 1
    }
  }
  image(1:(nrow(m)), 1:ncol(m), m, col = colors, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  dev.off()
  
  #generates multiple patch distributions
  png(file = paste0(dir, "clustered_distribution", freq[i], ".png") ,
      width=512, height=512)
  m <- all_orange
  
  start_row <- floor(runif(1, 20, 80))
  start_col <- floor(runif(1, 20, 220))
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  
  start_row <- floor(runif(1, 20, 80))
  start_col <- floor(runif(1, 320, 420))
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  
  start_row <- floor(runif(1, 180, 210))
  start_col <- floor(runif(1, 50, 190))
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  
  start_row <- floor(runif(1, 180, 210))
  start_col <- floor(runif(1, 270, 400))
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  
  start_row <- floor(runif(1, 350, 380))
  start_col <- floor(runif(1, 0, 400))
  for(k in start_row:(start_row + five[i])) {
    for(j in start_col:(start_col + five[i])) {
      m[k, j] <- 1
    }
  }
  #each of the five previous blocks of code are for one patch. kevin hardcoded the range of possible values the patch starting and ending point could take on so that they won't overlap.
  
  image(1:(nrow(m)), 1:ncol(m), m, col = colors, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  dev.off()
}