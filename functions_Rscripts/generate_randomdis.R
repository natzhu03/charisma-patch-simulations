generate_randomdis <- function(freq, height, width) {
  colors <- c("blue", "orange")
  dir <- "/Users/nataliezhu/Desktop/Patch Generation Images/" 
  
  for(i in 1:length(freq)) {
    png(file = paste0(dir, "random_distribution", freq[i], ".png") ,
        width = width, height = height)  
    m <- matrix(sample(2, height*width, replace = TRUE, prob = c(freq[i], 1 - freq[i])), ncol = width)
    image(1:(nrow(m)), 1:ncol(m), m, col = colors, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    dev.off()
  }
}