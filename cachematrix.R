##makeCacheMatrix initializes the function object that will be used to consume
## the cache mechanism
makeCacheMatrix <- function(x = matrix()) {
        ##m can be a null matrix, we will initialize one for the user
        m <- NULL
        ##set will let us specify the matrix we want to work with this function
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        ##the get function is used to retrieve the original matrix from the cache
        get <- function() x
        ##the setMatrix function sets the calculated value in the cache
        setMatrix <- function(solve) m <<- solve
        ##the getMatrix function attempts to retrieve whatever matrix is on the cache
        getMatrix <- function() m
        ##then, we list the 4 possible functions to be used
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}

##the cacheSolve function is used to access the cache
##and in case there is already a value there, return it
##otherwise it lets the makeCacheMatrix object to compute the inverse of the matrix
##via the solve function and returns the output of the computation
cacheSolve <- function(x = matrix(), ...) {
        ##first, try to get the solution from the cache
        m <- x$getMatrix()
        ##if there was something in the cache, return it
        if(!is.null(m)){
          print('Cache data was used')
          return(m)
        }
        ##otherwise, the makeCacheMatrix object will calculate it
        ##and store it
        print('Calculating matrix')
        ##get the matrix to work on
        matrix <- x$get()
        ##solve the matrix
        m <- solve(matrix, ...)
        ##set it in the cache
        x$setMatrix(m)
        ##return the computation result to the caller
        return(m)
}
