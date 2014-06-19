## Creates a matrix listing a function that
## a) sets matrix value
## b) gets matrix value
## c) sets inverse matrix value
## d) gets inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
    
    ## set stored inverse value to null
    inverse.value <- null
    
    ## set matrix value
    
    matrix.value <- function(y) {
        x <<- y
        inverse.value <<- null
    }
    ## pulling value from matrix
    pull.matrix <- function() x
    ## setting inverse value
    set.inverse <- function(inv_) inverse.value <<- inv_
    ## pulling inverse value
    pull.inverse <- function () inverse.value
    
    ## print a list of all above functions
    list(matrix.value = matrix.value, pull.matrix = pull.matrix, set.inverse = set.inverse, pull.inverse = pull.inverse)

}


## Calculates the inverse of above matrix, first verifying if calculation has done.
## If calculation is done, pulls inverse from cache.
## Else calculates the inverse of the matrix and sets value


cacheSolve <- function(x, ...) {
        ## Verification
        inverse.value <- x$pull.inverse()
        if(!is.null(inverse.value)) {
            message("pulling cached data")
            return(inverse.value)
        }
        ##if not cached
        data <- x$pull.matrix()
        ## calculate inverse
        inverse.value <- solve(data, ...)
        ## then cache the inverse and print
        x$set.inverse(inverse.value)
        inverse.value
}
