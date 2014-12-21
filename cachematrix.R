## Function to transform a regular matrix into an 'enhanced' version with additional
## properties, in this particular case it is able to store values assigned from within 
## other enviroments into its scope (ie. cache them)

makeCacheMatrix <- function(x = matrix()) {
      ## internal property to store inverse values
      i <- NULL
      ## setter method that can reset the values
      set <- function(y) {
          x <<- y
          i <<- NULL
      }
      ## getter method (returns the original simple matrix)
      get  <- function() x
      ## set method for the value to cache inside this object
      setcache  <- function(z) i  <<- z
      ## get method that returns current cache
      getcache  <- function() i
      ## function output
      list(set = set, 
           get = get,
           setcache = setcache,
           getcache = getcache)
}

## Enhanced version on 'Solve', making use of the makeCacheMatrix object
## It is able to calculate the inverse of a matrix, but will only perform the actual 
## calculation if the original object has no cache value, otherwise it uses cache
## instead

cacheSolve <- function(x, ...) {
  ## retrieves cache first
  m <- x$getcache()
  ## checks if not NULL
  if(!is.null(m)) {
    message("getting cached data")
    ## returns cache
    return(m)
  }
  ## if null caculates invers
  m <- solve(x$get())
  ## sets cache
  x$setcache(m)
  ## returns inverse of matrix x
  m  
}
