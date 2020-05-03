makeCacheMatrix <- function(x = matrix()) {      ##define function with argument which has a default matrix
  j <- NULL                                      ##initialize j as null
  set <- function(y){                            ##define the set function 
  x <<- y                                        ##asign y to x
  j <<- NULL                                     ##reset j to null 
  }
  get <- function()x                             ##define the get function
  setInverse <- function(inverse) j <<- inverse  ##assigns value to setInverse
  getInverse <- function() j                     ##assigns value to getInverse where called
  list(set = set, get = get,                     ##
  setInverse = setInverse, 
  getInverse = getInverse)
}



cacheSolve <- function(x, ...) {  ##to return a matrix that is the inverse of x
  j <- x$getInverse()       
  if(!is.null(j)){                ##to check if j is null
  message("getting cached data")  ##prints message if j is null
  return(j)                       ##returns j
  }
  mat <- x$get()
  j <- solve(mat,...)             ##assign j with the arguments of function solve
  x$setInverse(j)
  j
}