## HW Week 3: cachematrix.R
## Function MakeCachematrix() creates a list of setter and getter functions. 
##   to cache the calculated inverse of a matrix to avoid repetitive calculations of the inverse..
## Function cacheSolve: returns the inverse of a matrix after firsst checking for a cached value in the 
##   list created by makeCacheMatrix, then calculating the inverse if the cached value is NA.
##
## Function: makeCacheMatrix(): return a list of functions that point to stored values for x and inverse x. 
makeCacheMatrix <- function(x = matrix()) {
    InvMat <- matrix(data = NA)   ## InvMat stores the cached inverse matrix. Initialize to NA.
    get <- function() x           ## get() retrieves argument matrix x (of makeCacheMatrix)
    
    setMatrix <- function(IM) InvMat <<- IM  ## setMatrix() assigns its argument to InvMat in parent environment.
    getMatrix <- function() InvMat           ## getMatrix() retrieves InvMat
    list(get = get, setMatrix = setMatrix, getMatrix = getMatrix)  ## returns the list of set/get functions
}

## Function: cacheSolve(): return a matrix that is the inverse of "x"
cacheSolve <- function(x, ...) {
    cachedIM <- x$getMatrix()             ## calls getMatrix() function to retrieve cached value from InvMat
    if( !is.na(cachedIM[1,1]) ) {
        message("Getting cached Inverse")
        return(cachedIM)                  ## if cachedIM[1,1] <> NA return cached value as inverse, otherwise ...
    }
    evalMatrix <- x$get()                 ## retrieve value of original matrix by calling $get() 
    IVM <- solve(evalMatrix)              ## evaluate inverse of evalMatrix, assign to IVM
    x$setMatrix(IVM)                      ## assign IVM to InvMat by calling FUN setMatrix(IVM)
    return(IVM)                           ## return IVM as respons (inverse of input matrix)
}

## Test the routines:
testMatrix <- matrix(c(2,0,0,0, 0,2,0,0, 0,0,2,0, 0,0,0,2), ncol = 4)   ## create input matrix to invert
testMatrix2 <- matrix(c(1,0,0,0, 2,3,0,0, 4,5,6,0, 0,0,0,7), ncol = 4)  ## create another input matrix
out <- makeCacheMatrix(testMatrix)     ## construct function list to set/get inverse of testMatrix
cacheSolve(out)                        ## calculate inverse of testMatrix the first time
cacheSolve(out)                        ## get cached inverse of testMatrix
out2 <- makeCacheMatrix(testMatrix2)   ## construct function list to set/get inverse of testMatrix2
cacheSolve(out2)                       ## calculate inverse of testMatrix2
cacheSolve(out2)                       ## get cached inverse of testMatrix2
cacheSolve(out)                        ## get cached inverse of testMatrix again


