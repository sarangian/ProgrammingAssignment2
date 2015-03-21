## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##}


## Write a short comment describing this function

##cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#}
#R ProgrammingAssignment2 : Caching the Inverse of a Matrix
#
#makeCacheMatrix (<square matrix>): This function creates a special "matrix" object that can cache its inverse.
# 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                              ##creating object m, holds inv matrix
  set <- function(y) {
    x <<- y                              ## set a matrix data
    m <<- NULL                           ## clear previously cached inverse matrix in object m
  }
  get <- function() x                    ## simply return the object x; the input matrix
  setInverse <- function(inv) m <<- inv  ## assign matrix (inv) from environment to m
  getInverse <- function() m             ## simply return the matrix m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)          ## return a named-list of 4 functions
}


#cacheSolve(<makeCacheMatrix object>) :This function computes the inverse of the special "matrix" returned by makeCacheMatrix().
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {                     # If the inverse has already been calculated or not empty (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
    message("getting cached data")      ## print a message for user 
    return(m)                           ##return cached inverse matrix object m and end the function
  }
  data <- x$get()                      #otherwise m is NULL and hence obtain the original matrix in another object named 'data'                                     
  m <- solve(data, ...)                ##calculated inverse of matrix data using solve(); passing data, as well as ... arguments
  x$setInverse(m)                      ## set the calculated inverse matrix to makeCacheMatrix object
  m                                    ### return calculated inverse matrix ,that is the inverse of 'x'
       
}

