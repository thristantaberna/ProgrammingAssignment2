# This first function, makeCacheMatrix creates a vector entered by the user.
#It essentially, sets and gets the value of the vector as entered.
#It also sets and gets the inverse of the entered vector.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinverse = getinverse)
}


## The function below calculates the inverse of the vector replicated above.
## It first checks if the vector's inverse has already been calculated, if so it skips
## the calculation. Otherwise, it proceeds with the calculation using solve.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}