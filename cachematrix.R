## Assignment from the Coursera Course R programming, title: Caching the Inverse of a Matrix
## Functions to create: makeCacheMatrix and cacheSolve 


## makeCacheMatrix: a function that creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}

## cacheSolve: a function that computes the inverse of the special 
## "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse
## from the cache. 

cacheSolve <- function(x, ...) {

        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setMatrix(m)
        m
}