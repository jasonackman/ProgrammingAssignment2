## The following functions are used in tandem to provide
## a caching mechanism to store to transparently store
## the results of matrix inversions / solutions.


## This function create a special vector
## that stores a matrix along with its
## matrix inversion / solution.
makeCacheMatrix <- function(x = matrix()) {
    
    # Declare and initialize the solution variable.
    s <- NULL
    
    # Define set function to set x variable and clear solution variable.
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # Define get function to return value of matrix.
    get <- function() x
    
    # Define setsolution function to set value of solution.
    setsolution <- function(solve) s <<- solve
    
    # Define getsolution function to get value of solution.
    getsolution <- function() s
    
    # Returns list object which contains all functions for this function environment.
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = getsolution)
}


## This function calculates the solution of the special matrix created
## with the above function. Before calculating the solution, it will see
## if we have already calculated and stored the solution. If so, we just
## return that value. Otherwise, we calculate the solution, cache it, and
## return that value.
cacheSolve <- function(x, ...) {
    # Get the cached solution.
    s <- x$getsolution()
    
    # If the cached solution is not null, just return that since
    # we have already calculated it.
    if (!is.null(s)) {
        message("getting cached data.")
        return(s)
    }
    
    # Solve the matrix
    s = solve(x$get())
    
    # Cache the solution
    x$setsolution(s)
    
    # Return solution
    s
}
