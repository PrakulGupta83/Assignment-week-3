makeCacheMatrix <- function(m = matrix()) {
        s <<- NULL
        m <<- t
        get <- function () m
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list (get = get, setsolve = setsolve, getsolve = getsolve)
}

cacheSolve <- function(m, ...) {
        s<-m$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        tempmat <- m$get()
        s <- solve(tempmat) 
        m$setsolve(s)
        s
}