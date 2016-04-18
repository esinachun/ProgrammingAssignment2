## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #1. set the value of the matrix
    inv <- NULL
    set <- function(y) {
        ## '<<-' assign a value to an object in an environment outside of current
        x <<- y
        inv <<- NULL
    }
    #2. Get the value of the matrix
    get = function() x
    
    #3. cachethe matrix's inverse
    setinv = function(inverse) inv <<-inverse
    getinv = function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## inverse of the original goes to makeCacheMatrix
        inv = x$getinv()
        
        ## if the inverse is already calculated
        if(!is.null(inv)) {
            #release the warning msg and skip the calculation.
            message("getting cached data")
            return(inv)
        }
        
        ## if there was no calculation yet, calculate the inverse.
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
