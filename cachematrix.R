
## functions do: The first function builds a "special" vector with 4 function references. the four functions are:
## 1. setMatrix - used to set the Matrix we'll work with
## 2. getMatrix - used to return the Matrix we work with
## 3. setInverse - set the Inverse of the matrix directly as to imitate cache memory opeartion with big-O(1) time to get the inverse
## 4. getInverse - returns the Inverse of the matrix from memory without any further computiton at big-O(1) time
## The second funtion handles the caching needed to be done in case it has not been already computed. If needed, 
## computes and returned the inverse as needed

## The Function builds a "special" vector with 4 function references as explained above
makeCacheMatrix <- function(x = matrix()) {
 		m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inversedMatrix) m <<- inversedMatrix
        getInverse <- function() m
        list(set = setMatrix , get = getMatrix ,
             setInverse = setInverse ,
             getInverse = getInverse )
}


# Function checks if there's already a cached copy of the Inverse, if so gets it and, prints out a proper message
# and returns the Inversed mastrix, otherwise it computes it and places it on cache for that matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse ()
        if(!is.null(m)) {
                message("getting cached inverse matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse (m)
        m
}
