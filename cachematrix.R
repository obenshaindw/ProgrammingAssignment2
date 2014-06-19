## This R file contains two main functions designed to work with matrixes
## Using the makeCacheMatrix() function, one can create an object to work with matrixes for saving/getting a matrix and its inverse.
## Using the cacheSolve() function, one can see if a makeCacheMatrix object has a stored inverse of the matrix; if not will calculate/store it.

## makeCacheMatrix function creates a vector that is a list of sub functions contained within the vector
## Example: cacheMatrix <- makeCacheMatrix()
##   creates an object cacheMatrix with 4 functions (set, get solveInverse, getInverse).

## Examples for using functions provided in the body of the function. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## The set() function is called with a matrix as the argument:
  ##  - sets the value of x inside the caheMatrix object
  ##  - clears out the value of m inside the object and sets it to null
  ## Example:
  ##  cacheM <- makeCaheMatrix()
  ##  datamatrix <- matrix(1:4, ncol=2)
  ##  cacheM$set(datamatrix)
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## The get() function is called with no arguments:
  ##  - returns the matrix stored by the set() function.
  ## Example:
  ##  matrixFromCache <- cacheM$get()
  
  get <- function() {
    x
  }
  
  ## The solveInverse() function is called with a matrix as an argument:
  ##  - calculates the inverse of the matrix and stores it to the x variable inside the cacheMatrix object.
  ## Example:
  ##  cacheM$solveInverse(datamatrix)
  
  solveInverse <- function(solve) {
    m <<- solve
  }
  
  ## The getInverse() function is called with no arguments:
  ##  - returns the inverse of the matrix and stored in the x variable inside the cacheMatrix object.
  ## Example:
  ##  cacheM$getInverse()
  
  getInverse <- function() {
    m
  }
  
  list (set = set, 
        get = get, 
        solveInverse = solveInverse, 
        getInverse = getInverse)

}


## This function takes a cacheMatrix object as an argument.
## If the inverse of the matrix has already been calculated, it returns the value through the $getInverse() function.
## If no inverse has been calculated, is calculates the inverse using the $get() and $solveInverse() functions.

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x' or NULL, using the $getInverse() function. 
  m <- x$getInverse()
  
  ## If $getInverse() does not return null, the inverse has lready been calculated and it is returned.
  if( !is.null(m) ){
    message("Retrieving inverse of matrix from cache.")
    return(m)
  }
  
  ## If this block is getting executed, then $getInverse() returned NULL, so the inverse of the matrix must be calculated.
  
  ## $get() gets the matrix stored in the cacheMatrix object 
  data <- x$get()
  
  ## $solveInverse takes a matrix as an argument and caches the result to m.
  x$solveInverse(data)
  
  ## gets inverse of matrix from cahceMatrix. 
  inverse <- x$getInverse()
  inverse
}

