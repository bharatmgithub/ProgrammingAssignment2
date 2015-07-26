#Both the functions create, store and recall a matrix and its inverse from the cache 
#makeCacheMatrix function creates a special "matrix" object that can cache its inverse through 4 functions: set,get, setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }  # storing in cache
        get <- function() x #getting matrix
        setinverse <- function(solve) m <<- solve #set inverse 
        getinverse <- function() m #get inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        }


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then
# the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() #query the x matrix's cache
        if(!is.null(m)) {   #loop to check whether the inverse already exists or not
                message("getting cached data") # sent message suggesting matrix value from cache
                return(m)
        }
        data <- x$get()#get the matrix from  makeCacheMatrix function 
        m <- solve(data, ...)#compute inverse
        x$setinverse(m)#store the inverse matrix in cache by applying the makeCacheMatrix set function
        m 
}
        
        


