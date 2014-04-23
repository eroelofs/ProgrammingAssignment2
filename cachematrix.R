#makeCacheMatrix - this function creates a special matrix object.
#The special matrix object will have the functions $get_inverse (to retrieve a previously inversed matrix)
#and $set_inverse (to inverse a matrix and store the inversed matrix).
makeCacheMatrix <- function(x = matrix()) {
        solved_x <- NULL        
        set <- function(y) {
                x <<- y
                solved_x <<- NULL
        }                
        get <- function() {
               x      
        } 
        set_inverse <- function(solved_input) {
                solved_x                
        } <<- solved_input        
        get_inverse <- function() {
                solved_x
        }                
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

#cacheSolve - this function requires the special matrix object as input (created with makeCacheMatrix).
#When cacheSolve is called, it will attempt to retrieve a cached version of the inversed matrix object.
#If $get_inverse was able to retrieve a cached object, it will print the message "getting cached data"
#and it will return the previously inversed (solved) special matrix object.
#if $get_inverse returns nothing (null), it will solve the input matrix object and store it into cache.
cacheSolve <- function(x, ...) {
        solved_x <- x$get_inverse()
        if (!is.null(solved_x)) {
                message("getting cached data")
                return(solved_x)
        } else {
                solved_x <- solve(x$get())
                x$set_inverse(solved_x)
                return(solved_x)
        }
}
