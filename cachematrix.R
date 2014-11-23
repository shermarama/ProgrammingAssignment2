## The functions allow the inverse of a matrix to be calculated and stored, and
## to be retreived without recalculating if it's already been worked out. 

## This function generates a function called makeCacheMatrix which acts on the 
## input matrix x. It also generates:
## - n, an empty object
## - the function set, which places the values for x and n outside the function
## - several other functions to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
      n <- NULL
      set <- function(y) {
            x <<- y
            n <<- NULL
      }
      get <- function() x
      setinv <- function(solve) n <<- solve
      getinv <- function() n
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## cacheSolve calls the getinv function from the first function and puts the
## result in n. It then checks to see whether n is empty, that is, whether the 
## inverse has already been calculated. If it has, it returns a message and 
## the cached data. If it hasn't, it gets the matrix x from its external 
## location and stores it in the object 'data', inverts it using solve, stores
## the result in n and returns n. 

cacheSolve <- function(x, ...) {
      n <- x$getinv()
      if(!is.null(n)) {
            message("getting cached data")
            return(n)
      }
      data <- x$get()
      n <- solve(data, ...)
      x$setinv(n)
      n
}


