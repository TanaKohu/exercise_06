Remove <- function(deltaX, fragments)
{

  for (element in fragments) {
    index_to_remove <- which(deltaX == element)[1]
    if (!is.na(index_to_remove)) {
      deltaX <- deltaX[-index_to_remove]
    }
  }

  return(deltaX)

}


PartialDigestProblem <- function(deltaX)
{
  width <- max(deltaX)
  deltaX <- deltaX[deltaX!= width]
  print(deltaX)
  X <- c(0, width)

  Place(deltaX, X, width)
}


Place <- function(deltaX, X, width) {
  if (length(deltaX) == 0) {
    print('Pozicie')
    print(X)
    return()

  } else {
    y <- max(deltaX)
    fragmenty <-abs(X-y)
    condition <- all(fragmenty %in% deltaX)
    
    if (condition)
    {
      X <- c(X, y)
      deltaX <- Remove(deltaX, fragmenty)
      Place(deltaX, X, width)
      X <- X[X!= y]
      deltaX <- c(deltaX, fragmenty)
    }

      #  nova premenna fragmenty kde  X -(width - y)
      #  pytame sa ci je to podmnozinou delta x

    fragmenty2 <-abs(X-(width - y))
    condition2 <- all(fragmenty2 %in% deltaX)
    
    if (condition2){
      X <- c(X, width - y)
      deltaX <- Remove(deltaX, fragmenty2)
      X <- Place(deltaX, X, width)
      X <- X[X!= (width-y)]
      deltaX <- c(deltaX, fragmenty2)
    }
    return()
  }
}


# Example usage
deltaX <- c(2,2,3,3,4,5,6,7,8,10)
result <- PartialDigestProblem(deltaX)

