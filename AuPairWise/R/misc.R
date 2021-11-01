##########################################
lm.studentized  <- function(x,y)
{
  z = lm(y ~ x )
  z = rstudent(z)
  return( rank(abs(z)) )
}

##########################################
model.fx <- function(x, fx)
{
  x = fx(x)
  replace = ( x == -Inf)
  x[replace] = 0
  return (x)
}

##########################################
makeTransparent <- function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
