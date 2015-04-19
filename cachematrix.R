## Functions for creating a special matrix type allowing to cache the
## result of its corresponding inverse. 
## @seealso https://github.com/dietersteiner/ProgrammingAssignment2/blob/master/README.md

## Return a special cache matrix object for a given matrix and define
## basic access functions for the original matrix data and its inverse
## 
## @param x A matrix.
## @return cacheMatrix.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(m) {
                x <<- m
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Calculates the corresponding inverse matrix for a cacheMatrix object.
## If the inverse has already been computed once, the cached result is
## returned; otherwise the inverse is calculated, cached, and returned.
## 
## @param x A cacheMatrix object (as returned from makeCacheMatrix).
## @return inv The inverse of the matrix defined in the cacheMatrix object.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) { ## result already cached
                message("returning cached data")
        } else { ## compute inverse and cache result
                matrix <- x$get()
                inv <- solve(matrix) 
                ## cache result
                x$setinv(inv)
        }
        inv
}

