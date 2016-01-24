## Programming Assignment Week 3
## Axel Augustinus

makeCacheMatrix <- function(x = matrix()) {
        ## input: x = a square invertible matrix
        ## output: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        inv <- NULL # required so that getinv never gives an error
        set <- function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get <- function() x # returns matrix x
        setinv <- function(inverse) inv <<- inverse # assigns value 'inverse' to variable 'inv' 
        getinv <- function() inv # returns inv
        list(set=set, get=get, setinv=setinv, getinv=getinv) # returns $set, $get, $setinv and $getinv
}

cacheSolve <- function(x, ...) {
        ## input: x = output of makeCacheMatrix()
        ## output: inverse of the original matrix input to makeCacheMatrix()
        
        inv <- x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat <- x$get()
        inv <- solve(mat, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}