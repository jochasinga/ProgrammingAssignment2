## makeCacheMatrix returns a list of four functions to inspect and
## cache the inverse of matrix x in variable inv
## cacheSolve simply calculate the inverse matrix for variable inv
## unless inv is already not empty (NULL)

## makeCacheMatrix is a wrapper function for four functions: set, get, 
## setInverse, getInverse. set function is used to set matrix x, while
## x can be assigned directly with makeCacheMatrix(x) as well. get is
## used to get the original matrix x, setInverse is used to set the inverse
## of x to inv, getInverse simply returns the inv

makeCacheMatrix <- function(x = matrix()) {
    ## 1. Set the value of the matrix
    # Set an empty matrix to store inverse of x
    inv <- NULL 
    # Set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## 2. Get the value of the matrix
    get <- function() x
    ## 3. Set the value of the inverse matrix
    setInverse <- function(inverse) inv <<- inverse
    ## 4. Get the value of the inverse matrix
    getInverse <- function() inv
    # Return a lazy list of four lazy functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve is a wrapper function for solve, but also help to inspect whether
## inv is empty, and if so, it goes on to get the original matrix x, calculate
## its inverse, assign to inv and return it.

cacheSolve <- function(x, ...) {
    # Invoke getInverse from last function and assign to inv
    inv <- x$getInverse()
    # If inv isn't empty
    if(!is.null(inv)) {
        # Print the notification and just return inv
        message("getting cache data")
        return(inv)
    }
    # Else, get the original matrix, 
    badAssMatrix <- x$get()
    # solve its inverse, assign that to inv,
    inv <- solve(badAssMatrix, ...)
    x$setInverse(inv)
    # Return a matrix that is the inverse of 'x'
    inv
}
