## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function implements setter and getter fir the inverse of the matrix
# In case iverse is never calculated it returns NULL
# otherwise the inverse itself

makeCacheMatrix <- function(x = matrix())
    {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    }


## Write a short comment describing this function

# this function utilizes the makeCacheMatrix and lexical scoping via << operator
# if inverse is already calculated it just fetches it from "m"
# otherwise calculates and sets it via setInverse

cacheSolve <- function(x, ...)
    {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    }
