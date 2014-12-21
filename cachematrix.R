## Put comments here that give an overall description of what your
## functions do

# USE:
#
# create matrix x
# > x <- matrix(1:4, 2, 2)
#
# create special matrix from x
# > m1 <- makeCacheMatrix(x)
#
# calculate inverse of the matrix m1 and cache it
# > inv <- cacheSolve(m1)
#
# display matrix data:
# > m1$get()
#
# display cached inverse (if solved - i.e. cacheSolve is previously called on given matrix)
# > m1$getinv()
#
# confirm that cacheSolve returns inverse matrix (must solve matrix m1 first)
#
# m1$get() %*% m1$getinv()
#

## creates matrix with getters and setters and setinv, getinv methods for access to cached inverse of given matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## given matrix created with function makeCacheMatrix, fetches it's cached inverse matrix calculates its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

