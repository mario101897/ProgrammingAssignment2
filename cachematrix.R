## This two functions are supposed to store the inverse of a matrix so the 
## program don´t do the same calculation when is not necessary, saving time and 
## memory 

## makeCacheMatrix creates an special object to store the matrix inverse value

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y){
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) invM <<- solveMatrix
  getInverse <- function() invM
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes the inverse matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getInverse()
  if(!is.null(invM)){
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data)
  x$setInverse(invM)
  invM      
}


