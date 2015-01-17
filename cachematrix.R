## The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a matrix containing a function to
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    ## Initialise the matrix.
    inverseMatrix <- NULL
    # set the given matrix
    set <- function(matrix) {
            m <<- matrix
            inverseMatrix <<- NULL
    }

    # Get the given matrix
    get <- function()  {
      m
    }

    # find and set the inverse of the matrix
    setInverse <- function(solve) {
      inverseMatrix <<- solve
    }

    # Get the inverted matrix
    getInverse <- function() {
      inverseMatrix
    }

    # Return the list
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse
        )
}


## Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly

## The following function calculates the inverse of the special "marix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the  matrix and sets the value of the inverse in the cache via the `setInverse`
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached inverse matrix")
                return(inverseMatrix)
        }

        originalMatrix <- x$get()
        inverseMatrix <- solve(originalMatrix, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
