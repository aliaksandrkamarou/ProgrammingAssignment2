## makeCacheMatrix creates matrix object which:
##set the value of the matrix
##get the value of the matrix
##set the value of the solve 
##get the value of the solve 


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## cacheSolve takes an special matrix object as input
## and returns cached value of solved matrix
## or (if it  hasn't already been calculated) calculates solved matrix
## and saves solved value 

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
        ## Return a matrix that is the inverse of 'x'
}
