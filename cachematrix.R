## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# `makeCacheMatrix` creates an alternative matrix object that can store its inverse
# 
# The variables stored in its scope are:
# - `inv_mx` : inverted matrix
# - `mx`     : matrix
# 
# The functions are: (more details commented line by line on the function)
# - `set`/`get`       acting on the `mx` value. The set function taks care 
#                     of resetting `inv_mx`, given the new `mx` value
# - `setInv`/`getInv` act on the `inv_mx` value
# 
# all these functions work on the variables in the scope of makeCacheMatrix 
# via the '<<-' operator 
makeCacheMatrix <- function(mx = matrix()) { # the constructor can set directly `mx`
  inv_mx <- NULL             # initializes inv_mx/inverted matrix
  set <- function(y) {
    mx <<- y                 # `mx`(matrix), in the scope of makeCacheMatrix, is assigned the input value y
    inv_mx <<- NULL          # as `mx` is new, `inv_mx`(matrix) value is reset
  }
  get <- function() mx                                         # gets the `mx`(matrix)
  setInv <- function(invertedMatrix) inv_mx <<- invertedMatrix # sets the `inv_mx`(inverted matrix)
  getInv <- function() inv_mx                                  # gets the `inv_mx`(inverted matrix)
  list(set = set, get = get,  # creates the list of functions
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

# `cacheSolve` returns the inverse of the input matrix
# it receives as input an object created with makeCacheMatrix
# if the inverse is cached, then it returns the cached value
# if the inverse is not cached, then it computes it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mx <- x$getInv()         # retrieves the inverted matrix value stored in x 
  if(!is.null(inv_mx)) {       # it returns the cached inverted matrix if available
    message("getting cached data")
    return(inv_mx)                      
  }
  mx <- x$get()              # otherwise, it gets the value of the matrix
  inv_mx <- solve(mx, ...)    # then it computes the inverse
  x$setInv(inv_mx)           # sets the inverse value in x with the setInv function
  inv_mx                     # returns the computed inverse matrix
}