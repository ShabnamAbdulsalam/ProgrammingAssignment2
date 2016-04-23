######################################################################################################################
## Assignment: Caching the Inverse of a Matrix

## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
######################################################################################################################

makeCacheMatrix <- function(x = matrix())
  {

    matrix_cache <- NULL
    set<-function(y)
      {
        x<<-y
        matrix_cache <<- NULL
      }
  
    get <- function() { x }
  
    setInverse <- function(inverse) 
      {
        matrix_cache <<- inverse
      }
 
    getInverse <- function() 
      {
        matrix_cache
      }
 
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

######################################################################################################################
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" should retrieve the inverse from the cache.
######################################################################################################################

cacheSolve <- function(x, ...) 
  {
    matrix_cache <- x$getInverse()
    if( !is.null(matrix_cache) ) 
      {
         message("getting cached data")
         return(matrix_cache)
      }
  
    data <- x$get()
    matrix_cache <- solve(data, ...)
    x$setInverse(matrix_cache)
    matrix_cache        
  }

######################################################################################################################
