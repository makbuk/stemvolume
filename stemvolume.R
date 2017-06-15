setwd("/home/makbuk/prog/r/stemvolume/")

#Predicting Volume of a Tree
#http://rstudio-pubs-static.s3.amazonaws.com/138191_9169a18ae3d34e1492d1df67a810e5d5.html
#https://github.com/avinayan?tab=repositories

require(xlsx)
library(ggplot2)

treesdata <- read.xlsx("oak_data.xls", sheetIndex = 1)
#радиус, радиус в квадрате
treesdata$r_m <- (treesdata$d_sm/200)
treesdata$rsquared_m2 <- (treesdata$d_sm/200)^2

head(treesdata)
summary(treesdata)
str(treesdata)

#create empty data.frame
treesvolume <- data.frame(d_sm=double(),
                          h_m=double(),
                          volume_m3=double())

uniplot <- unique(treesdata$plot)
#nlevels(uniplot)
for (i in 1:nlevels(uniplot)) {
  unitrees <- unique(subset(treesdata$ntree, treesdata$plot == uniplot[i]))
  #length(unitrees)
  for (j in 1:length(unitrees)) {
    td <- subset(treesdata, treesdata$plot == uniplot[i] & treesdata$ntree == unitrees[j])
    td$tvolume_m3 <- NA
    #tree volume m3
    tvolume_m3 <- 0
    for (k in 1:nrow(td)) {
      if (k == nrow(td)) {
        height_cone_m <- td$h_m[k] - td$h_m[k-1]
        tvolume_m3 <- tvolume_m3 + 1/3*pi*td$rsquared_m2[k-1]*height_cone_m
        td[k,10] <- 1/3*pi*td$rsquared_m2[k-1]*height_cone_m
      } else if (k > 1) {
        height_cone_m <- td$h_m[k] - td$h_m[k-1]
        #another formula
        #tvolume_m3 <- tvolume_m3 + 1/3*pi*height_cone_m*(td$rsquared_m2[k-1]+td$r_m[k-1]*td$r_m[k]+td$rsquared_m2[k])  
        #td$tvolume_m3[k] <- 1/3*pi*height_cone_m*(td$rsquared_m2[k-1]+td$r_m[k-1]*td$r_m[k]+td$rsquared_m2[k])  
        tvolume_m3 <- tvolume_m3 + pi*((td$rsquared_m2[k-1]+td$rsquared_m2[k])/2)*height_cone_m
        td[k,10] <- pi*((td$rsquared_m2[k-1]+td$rsquared_m2[k])/2)*height_cone_m
      }
      
    }
    
    
    tvrow <- nrow(treesvolume) + 1
    
    treesvolume[tvrow, 1] <- c(subset(td$d_sm, td$sort == "1.3м"))
    treesvolume[tvrow, 2] <- subset(td$h_m, td$sort == "верх")
    treesvolume[tvrow, 3] <- tvolume_m3
    
    #tree graphic model
    png(file = paste("trees/", paste(as.character(uniplot[i]),as.character(unitrees[j]), sep = " - "),".png" , sep = ""))
    plot(td$d_sm, td$h_m, type = "l", xlim = range(c(-td$d_sm, td$d_sm)), 
      xlab = "Diametr, sm", ylab = "Height, m")
      lines(-td$d_sm, td$h_m)
      title(paste(as.character(uniplot[i]),as.character(unitrees[j]), sep = " - "),
      sub=paste("Volume", format(tvolume_m3, digits=2, nsmall=2), " m3"))
    dev.off()

  }#for j
rm(td)  
}#for i


write.csv(treesvolume, file = "treesvolume.csv", row.names = TRUE)

treestable <- read.csv("oak_tables.csv")

model <- lm(volume_m3 ~ d_sm * h_m, data=treesvolume)
summary(model)

treestable[, 4] <- data.frame(predict(model,treestable))

write.csv(treestable, file = "oak_tables_result.csv", row.names = TRUE)
