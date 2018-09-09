#Below are two functions that are used to create a 
#special object that stores a matrix and cache's its inverse.

# These function, makeCacheMatrix, creates a special "matrix",
# which is really a list containing a function to 
#set the value of the matrix
#get the value of the matrix
#set the inverse
#get the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
	set <- function(y) {
        x <<- y    # Set the value in cache
        m <<- NULL # Clear the cache
	} 
    get <- function() x #get the value of the matrix
    setInverse <- function(inverse) m <<- inverse #set inverse
    getInverse <- function() m #get inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# These function, calculates the inverse of the special "matrix"
# It checks if the inverse has already been calculated.
# If so, gets the inverse and skips the computation.
# Otherwise, calculates the inverse and set the vlue in cache via setInverse

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()  #get matrix
        m <- solve(data) #calculate inverse
        x$setInverse(m)     #cache the result
        m                #return inverse
}
