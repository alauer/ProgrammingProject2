makeCacheMatrix <- function(matrix = matrix()) {

    inverse <- NULL
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    get <- function() matrix
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse

    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

cacheSolve <- function(special_matrix, ...) {
    inverse <- special_matrix$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- special_matrix$get()
    inverse <- solve(data, ...)
    special_matrix$set_inverse(inverse)
    inverse
}