Power = function(){
  print(2^3)
  
}

Power2 = function(x,a){
  print(x^a)
}

Power3 = function(x,a){
  return(x^a)
}

x = c(1:10)
plot(x,Power3(x,2))

plot(x,Power3(x,2), xlab="x", ylab = "X^2", main = "X vs X^2", type="l")

plot(x,Power3(x,2),log="xy", xlab="x", ylab = "X^2", main = "X vs X^2", type="l")
plot(x,Power3(x,2),log="x" ,xlab="x", ylab = "X^2", main = "X vs X^2", type="l")
plot(x,Power3(x,2),log = "y", xlab="x", ylab = "X^2", main = "X vs X^2", type="l")


PlotPower = function(x,a){
  plot(x=x,y=x^3, type="l")
}
