## The first function, makeCacheMatrix creates a special matrix that can cache its inverse.
## The second function, cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.


## The function makeCacheMatrix creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m_invert <- NULL
    set <- function(y){
        x <<- y
        m_invert <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {m_invert <<- inverse}
    getinverse <- function() {m_invert}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special matrix returned above.
## If inverse has already been calculated and the matrix not changed, 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_invert <- x$getinverse()
        if(!is.null(m_invert)){
            message("getting cached data")
            return(m_invert)
        }
        data <- x$get()
        m_invert <- solve(data, ...)
        x$setinverse(m_invert)
        m_invert
}
