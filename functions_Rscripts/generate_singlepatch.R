#this function generates a single (square) centered patch 

generate_singlepatch <- function(freq, height, width) {
  all_orange <- matrix(sample(2, height*width, replace = TRUE, prob = c(0, 1)), nrow = width)
  colors <- c("blue", "orange")
  dir <- "/Users/nataliezhu/Desktop/Patch Generation Images/"  
 
for(i in 1:length(freq)) {
  png(file = paste0(dir, "single_patch_distribution", freq[i], ".png") ,
      width = width, height = width)
  
single <- sqrt(freq*height*width)  

#single is the height and width of the patch

  #i think we have to switch width to match up with start_col, not start_row?
  start_row  = width/2 - single[i]/2 
  start_col = height/2 - single[i]/2   
  #the code above ensures that the patch is centered
  
  m <- all_orange
  for(k in start_row:(start_row + single[i])) {
    for(j in start_col:(start_col + single[i])) {
      m[k, j] <- 1
    }
  }
  image(1:(nrow(m)), 1:ncol(m), m, col = colors, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  dev.off()
  }
}