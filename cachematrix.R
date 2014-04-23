## These functions calculate the inverse of a matrix 'x' and cache the result
## so in case it is needed again,  it can be looked up in the cache rather than recomputed.
##

## MakeCacheMatrix takes an 'x' matrix as an argument
## and creates a "special" matrix object (actually, a list) that can cache its inverse. 
## This list are the functions that set and get the matrix, and that set and get the inverse.

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL  
        set <- function(y) {  
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m  
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated it retrieve the inverse from the cache..
## If it hasn't, it computes it and save it in the cache.
## Finally, it returns the inverse matrix of 'x'

cacheSolve <- function(x, ...) { 
        m <- x$getinverse() 
        if(!is.null(m)) { 
                message("getting cached data")
                return(m) ## returns the matrix stored in the cache. No computation is needed
        }
        data <- x$get() 
        m <- solve(data, ...) 
        x$setinverse(m) 
        m ## returns the inverse matrix of 'x' after computing and saving it in the cache
}
