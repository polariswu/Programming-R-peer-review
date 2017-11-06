## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: This function computes the inverse of the special "matrix" returned by makeCacheMatrix command

makeCacheMatrix <- function(x = matrix()) 
  {
    i <- NULL #Define function to set the value of the matrix.
    set <- function(y) {
      x <<- y
      i <<- NULL ##clear cache
    }   # Define function to get the value of the matrix
    get <- function() x
    setinverse <- function(inverse) i <<- inverse # Define function to get the inverse
    getinverse <- function() i  # Return a list with the above functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() ## This fetches the cached value for the inverse
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }# If the cache was not empty, we can just return it. And now the cache was empty. We need to calculate it, cache it, and then return it.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
