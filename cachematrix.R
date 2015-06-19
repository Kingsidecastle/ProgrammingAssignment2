## The first function, makeCacheMatrix, is designed to create a special
## "matrix" object that can cache its inverse. The second function, cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix above

## makeCacheMatrix is modled directly from the assignment example using a
## matrix instead of a vector. Where possible, the code was reused to reflect
## the accuracy and simplicity of the example. A matrix is first established
## and set to NULL. Then the function defines a new function that will cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
               
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve adopts the same code present within the assignment example
## substituting calls for the mean for calls for the inverse. It is a
## simple look that checks to see if a cached value is stored. If so
## then it is returned. If not, the solve function is used to take data
## supplied interactively and compute the inverse.

cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        if (!is.null(inv)){
                message("Retreiving cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)    
        x$setinv(inv)
        return(inv)
}
