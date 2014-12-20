##############################################################
## Function makeCacheMatrix accepts a square matrix and
## allows getting and caching the matrix and its inverse.
## 
## Function cacheSolve gets the output 'x' of makeCAcheMatrix,
## checks if it contains a non-null inverse and returns it.
## Otherwise, it computes the inverse, updates 'x' and
## returns the inverse.
##############################################################

######################################################
##               Function makeCacheMatrix
########################################################
## Argument - a square matrix
##
## Output   - a list containing the following functions:
##
##   set - sets the value of the matrix
##   get - gets the value of the matrix
##   setinverse - sets the value of the matrix inverse 
##   getinverse - gets the value of the matrix inverse
########################################################

makeCacheMatrix <- function(x = matrix()) {
  
        inv <- NULL                                      ## Inverse initialization
        set <- function(y) {                             ## Assigns matrix 'y' to 'x'
                x <<- y                                  ## and resets the inverse
                inv <<- NULL
        }
        get <- function() x                              ## Matrix restitution
        setinverse <- function(inverse) inv <<- inverse  ## Inverse assignment
        getinverse <- function() inv                     ## Inverse restitution
        list(set = set, get = get,                       ## List of functions
                  setinverse = setinverse,
                  getinverse = getinverse)
}


#####################################################
##               Function cacheSolve
#####################################################
## Argument - output 'x' of makeCacheMatrix
##
## Output   - inverse of the matrix associated to 'x'
#####################################################

cacheSolve <- function(x, ...) {

        inv <- x$getinverse()                    ## Gets the matrix inverse in 'x'
        if(!is.null(inv)) {                      ## Returns the inverse when not NULL
                message("getting cached data")   
                return(inv)
        }                                        ## When the inverse in 'x' is NULL
        data <- x$get()                          ##   gets the matrix in 'x'
        inv <- solve(data, ...)                  ##   calculates the inverse
        x$setinverse(inv)                        ##   assigns the inverse to 'x'
        inv                                      ##   returns the inverse
}

