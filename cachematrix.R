
# The following two functions are used to cache the inverse of a matrix rather than computing it repeatedly. 

# First function: makeCacheMatrix creates a special matrix object first, then containing a function to
        # 1. set the value of the matrix
        # 2. get the value of the matrix
        # 3. set the value of inverse of the matrix
        # 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #initialize the inverse matrix to be NULL first
        set <- function(y) {
                x <<- y  #set the matrix x to a new matrix y
                inv <<- NULL  #reset the inverse matrix to NULL; it essentially invalidates the cached inverse matrix since you now have a new matrix
        }
        get <- function() x  #return the matrix x
        setinverse <- function(inverse) inv <<- inverse #sets the value of inverse to inv, and it is also the cached inverse matrix and will be called by the second function cacheSolve.
        getinverse <- function() inv #returns the inverse 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   #returns the specical matrix that contains all the functions just defined 
}



#Second function: cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {#it takes in the matrix created in the function above 
        inv <- x$getinverse() #get the inverse of the matrix
        if(!is.null(inv)) { #if inverse is not empty, then we can just retrieve it from the cache
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #But if inverse is empty, meaning cache is empty, then we take the matrix and perform inverse on the spot
        inv <- solve(data)
        x$setinverse(inv) #then we set the inverse matrix 
        inv #return inverse 
}

