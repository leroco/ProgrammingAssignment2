###
## The following function creates a special "matrix", 
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

## Caching a matrix invloves storing the object for recall later on.
## this conserves memory for large repeated processes.
## We will create a matrix and and cache it to a variable eg Cache1.
## Cache1$get()to retrieve it 
#################################################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # sets the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # gets the value of the matrix
  get <- function() x
  # sets the inverse
  setinv <- function(inv_) inv <<- inv_
  # gets the inverse
  getinv <- function() inv
  
  # create the list of all the functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)     
}

###
### cacheSolve function
### generates the inverse of a  cached matrix
### check if the inverse is already cached
###  x here is  the cache that was created in the MakeCacheMatrix function
###  the inverse then gets pulled back into the cache
######
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
     }
  #  Solve will calculate the inverse if there is nothing in the cached inverse matrix
  data <- x$get()
  inv <- solve(data)
  #  cache the inverse
  x$setinv(inv)
  inv
}