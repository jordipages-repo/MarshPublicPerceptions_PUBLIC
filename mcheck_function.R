mcheck <- function (obj, ... ) {  
  rs<-resid(obj)
  fv<-fitted(obj)
  par(mfrow=c(1,2))
  require(car)
  plot(fv,rs,xlab="Fitted values",ylab="Residuals")
  abline(h=0, lty=2)
  lines(smooth.spline(fv, rs), col = "red")
  qqPlot(rs,xlab="Normal scores",ylab="Ordered residuals")
  par(mfrow=c(1,1))
  invisible(NULL)}
