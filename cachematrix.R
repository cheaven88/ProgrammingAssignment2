## The intention of these functions is to cache the costly computation of inversing a matrix

## makeCacheMatrix will create a matrix object where it's inverse can be cached
makeCacheMatrix <- function(x = numeric()) {
  #Set the null variable for the matrix
  m <- NULL
  #Function that sets up storing the variables in a seperate enviornment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the inverse of the matrix and store the inverse in the cache
  setinverse <- function(solve) m <<- solve
  #Get the value of the inverse from the cache to display
  getinverse <- function() m
  #Store the values in a list for the cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve will compute the inverse of what is returned by makeCacheMatrix
##The input should be the stored results of makeCacheMatrix
cacheSolve <- function(x, ...) {
  #Checks to see if the inverse of the object already exists.
  #If the inverse already exists, it will obtain the inverse from memory
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #If the inverse doesn't already exist, compute the inverse.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  #Return the inverse of the object.
  m
}
