# load library
library(EBImage)
 
set.seed(2020) # seed for randomization 

# load the R logo and save the rgb values:
img <- readImage("Rlogo.png")
img.2 <- img[,,1:3]
 
# count nº R packages available now in CRAN
library(rvest)
pkgs <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")
mylines <- pkgs %>% 
  html_nodes("tr") %>%
  xml_text()
nb_pkgs <- length(which(sapply(mylines, nchar)>5))
print(paste("There are", nb_pkgs, "packages available in CRAN as of", Sys.Date()))
#### helper functions ####
 
# functions for color simplification:
num.to.let <- function(x1){
  ref.dat <- data.frame(num = 10:15, let = LETTERS[1:6])
  out <- as.character(x1)
  if(x1 %in% 10:15){out = as.character(ref.dat$let[which(ref.dat$num == x1)])}
  return(out)
}
 
rgb.func = function(vec){
  #note: vec is a triple of color intensities
  g1 <-floor(255*vec[2])
  r1 <-floor(255*vec[1])
  b1 <-floor(255*vec[3])
 
  x1 <-r1 %/% 16
  x2 <-r1 %% 16
  x3 <-g1 %/% 16
  x4 <-g1 %% 16
  x5 <-b1 %/% 16
  x6 <-b1 %% 16
 
  x1 <-num.to.let(x1)
  x2 <-num.to.let(x2)
  x3 <-num.to.let(x3)
  x4 <-num.to.let(x4)
  x5 <-num.to.let(x5)
  x6 <-num.to.let(x6)
 
  out<- paste("#",x1,x2,x3,x4,x5,x6, sep = "")
  return(out)
 
}
 
 
im.func.1 <- function(image, k.cols = 5, samp.val = 3000){
  # creating a dataframe:
  test.mat <- matrix(image,ncol = 3)
  df <- data.frame(test.mat)
  colnames(df) <- c("r","g","b")
  df$y <- rep(1:dim(image)[1],dim(image)[2])
  df$x <- rep(1:dim(image)[2], each = dim(image)[1])
 
  samp.indx <- sample(1:nrow(df),samp.val)
  work.sub <- df[samp.indx,]
 
  # extracting colors:
  k2 <- kmeans(work.sub[,1:3],k.cols)
 
  # adding centers back:
  fit.test <- fitted(k2)
 
  work.sub$r.pred <- fit.test[,1]
  work.sub$g.pred <- fit.test[,2]
  work.sub$b.pred <- fit.test[,3]
 
  return(work.sub)
 
}
 
add.cols <- function(dat){
  apply(dat,1,rgb.func)
}
 
# general plotting function
plot.func <- function(dat){
  # assumes dat has colums x, ym cols
  plot(dat$y,max(dat$x) - dat$x, col = dat$cols,
       axes=FALSE,
       xlab = "An R for each CRAN package",
       ylab = "set.seed(2020)",
	   col.lab="papayawhip" ,
     cex.lab=1.5,
     pch="R",
	   cex= 0.9
	   )
  axis(side = 1, col= "papayawhip", col.axis= "papayawhip")
  axis(side = 2, col= "papayawhip", col.axis= "papayawhip")
  mtext("Curso de R - 2020", col="papayawhip", line=2.2, cex=2.5)
  mtext("Máster en Biodiversidad: Conservación y Evolución ", 
        col="papayawhip", line=1, cex=1.2)
  mtext("Universitat de València", 
        col="papayawhip", line=0, cex=1.2)
  }
 
#### simplify colors; sample n points ###
 
temp <- im.func.1(img.2, samp.val = 25000, k = 12)
temp$cols <- add.cols(temp[,6:8])
 
final <- temp[sample(1:nrow(temp), nb_pkgs),]
 
 
#### generate plot ####
par(bg = 'black') 
plot.func(final)

