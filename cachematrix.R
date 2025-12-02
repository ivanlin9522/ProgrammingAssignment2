## makeCacheMatrix: 
## Creates a special "matrix" object that can store a matrix and cache its inverse.
makeCacheMatrix <- function(m = matrix()) {

        ## Cached inverse (initially NULL)
        i <- NULL

        ## Setter: updates the matrix and resets the cached inverse
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }

        ## Getter: returns the stored matrix
        get <- function() {
                m
        }

        ## Setter for the inverse
        setInverse <- function(inverse) {
                i <<- inverse
        }

        ## Getter for the inverse
        getInverse <- function() {
                i
        }

        ## Return a list of methods for external access
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}


## cacheSolve:
## Computes the inverse of the matrix stored in the special object created by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## cacheSolve retrieves the inverse from the cache instead of recomputing it.
cacheSolve <- function(x, ...) {

        ## Check if inverse is already cached
        m <- x$getInverse()

        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## Retrieve the matrix from the object
        data <- x$get()

        ## Compute the inverse
        m <- solve(data, ...)

        ## Cache the inverse for future use
        x$setInverse(m)

        ## Return the computed inverse
        m
}
