## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  #Sets the value of the matrix
  setMatrix <- function(xPrime){
    x <<- xPrime
    inverse <<- NULL
  }
  
  #Returns the matrix
  getMatrix <- function(){
    x
  }
  
  #Sets the inverse of the matrix
  setInverse <- function(theInverse){
    inverse <<- theInverse
  }
  
  #Returns the inverse of the matrix
  getInverse <- function(){
    inverse
  }
  
  #A "cached" matrix with its inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #Gets the inverse of the matrix in the "cached" matrix list
  theInverse <- x$getInverse()
  
  #If theInverse is NOT NULL, then the inverse has been cached and returns the cached inverse
  if(!is.null(theInverse)){
    message("Getting cached inverse")
    return(theInverse)
  }
  
  #Get the matrix in the list
  theMatrix <- x$getMatrix()
  #Get the inverse of the matrix and assign it to theInverse
  message("Solving for inverse")
  theInverse <- solve(theMatrix)
  #sets the inverse in the list to theInverse
  x$setInverse(theInverse)
  #return the freshly calculated inverse
  theInverse
}
