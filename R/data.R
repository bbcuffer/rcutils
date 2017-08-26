gskslidecols <- rbind(lineblue=c(0,0,102),
                orange=c(255,153,0),
                green=c(50,182,50),
                paleblue=c(184,209,237),
                grey=c(128,128,128),
                lightorange=c(255,204,0),
                brown=c(114,99,77))

# light blue background for CofE slides    240, 245, 249 

slide.palette <-
  rgb(gskslidecols[,1], gskslidecols[,2],
      gskslidecols[,3], max=255, names=rownames(gskslidecols))

sgsk.palette <-
  rgb(c(0, 51, 55, 255, 51, 255),
      c(0, 153, 51, 204, 204, 102),
      c(0, 255, 153, 0, 51, 0),
      names=c("black", "darkblue", "lightblue", "yellow", "green", "orange"),
      maxColorValue=255)[c(1,2,6,3:5)]


slide.par <-
  list(mar=c(2,2.3,0,0)+.2, las=1, lwd=3,
       col.axis=1, col.lab=1, col.main=1, col.sub=1, col=1, fg=1)

paper.par <- list(mar=c(4,4,0,0)+.2, lwd=2, las=1)

gray.palette <- gray(c(0,.3,.45,.6,.75, .9))
