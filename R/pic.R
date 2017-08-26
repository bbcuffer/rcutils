pic <- function(fn, h=5, w=5, ps=14){
  type <- getOption('pic.format')
  file <- paste(getOption('pic.dir'),'/',fn,'.', type, sep='')
  if(is.null(getOption('pic.format'))){
    quartz(height=h, width=w, pointsize=ps)
  }
  else if(getOption('pic.format')=="wmf"){
    win.metafile(filename=file, height=h, width=w,
                 pointsize=ps)
  }
  else if(getOption('pic.format')=="eps"){
    postscript(file=file, height=h, width=w, pointsize=ps,
               onefile=F, horizontal=F, paper="special")
  }
  else if(getOption('pic.format')=="pdf"){
    pdf(file=file, height=h, width=w, pointsize=ps,
               onefile=F)
  }
}
 
## options(pic.format=NULL)
## options(pic.format='eps')
## options(pic.format='wmf')
## options(pic.format='pdf')
## options(pic.dir='C:/rc/Work/908/APV109141/Historical_Controls/Papers/SIM')
## pic("eggs2", w=34*12/72.27, h=4, ps=11)
## par(paper.par)
## plot(1:10, col=1:10, cex=1:10, pch=1:10)
## dev.off()
