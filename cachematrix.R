## cachematrix.R includes two functions that calculate the inverted matrix given an invertible square matrix as input.
##
##  makeCacheMatrix() creates a data structure with function calls that may be called on the object derived from the
##  input invertible square matrix 'x'
##  
##  cacheSolve() makes function calls on the object output from makeCacheMatrix(), and based upon the state of 
## the 'inv' reference, EITHER
##  (a) calculates the inverted matrix with solve() and returns a reference to it if 'inv' = NULL, which indicates that 
##  the inverted matrix had either not been previously calculated or had been modified
##  OR
##  (b) returns a reference to the previously calculated and cached copy

##  Function makeCacheMatrix() - creates a data structure with function calls that may be called on the 'x' object
##  and a reference to `inv`, which points to a previously inverted matrix result or to NULL if the latter nonexistent.
##  Argument:  x, reference to an invertible square matrix;  no checking is performed:  'x' is square? 'x' is singular?
##  Returns:  list of references to public functions set_m(), get_m(), setInv_m(), getInv_m()
##	set_m():  sets value of invertible square matrix
##	get_m():  gets value of invertible square matrix
##	setInv_m():  inverts input square matrix
##	getInv_m():  gets value of inverted square matrix
##
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL  # set inverse reference to NULL upon instantiation
	set_m <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get_m <- function() return( x )  # getter returns value of x to get_m reference
	setInv_m <- function(inverse) inv <<- inverse  # setter writes inverse to setInv_m reference
	getInv_m <- function() return( inv )  # returns value of inverse to getInv_m reference
	# return list with references
	return( list(set = set_m, get = get_m, setInv = setInv_m, getInv = getInv_m) )
}

##
##  Function cacheSolve() - returns a reference to an inverted matrix depending upon state of the `inv` variable.
##  Either the cached copy is returned, or if not previously cached, calculated with solve().  In the latter case, the
##  `inv` reference is updated with the inverted matrix to improve performance of subsequent calls.
##  Argument:  x, reference to object returned by makeCacheMatrix().  See description above.
##  Returns:  reference to inverted matrix

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	# if inv not NULL, return from cache and exit
	if( ! is.null( inv ) ) {
		message("\nreturning cached data\n")
		return( inv )
	}
	# otherwise, inv not NULL, so run solve() for inverse
	inv <- solve( x$get(), ...)
	x$setInv( inv ) # set and return inverse
	return ( inv )
}
#
# Test function
#
test <- function( nrow=6, benchmark=FALSE) {
	# generate square matrix with random values
	m<-matrix(data=rnorm(nrow^2, mean=0, sd=10), nrow=nrow, ncol=nrow )
	m.constr <- makeCacheMatrix( m )  # process with constructor
	if(! benchmark) {
		orig <- round( m.constr$get(), 3)  # original matrix from constructor data
		invisible(cacheSolve( m.constr )) # calculate inverse matrix, but suppress print
		m.inv <- cacheSolve( m.constr ) # this should come from cache
		m1 <- "original invertible matrix:"
		orig  # display original matrix
		m2 <- "cached inverted matrix:"
		round(m.inv, 3)  # display inverted matrix
		m.inv.inv <- cacheSolve( makeCacheMatrix( m.inv ) )
		m3 <- "inverse of cached inverted matrix - compare with original matrix"
		m.inv.inv
		return( list( m1=m1, orig=orig, m2=m2, cache_inv=round(m.inv, 3), m3=m3, inv_cache_inv=round(m.inv.inv, 3)  ) )
		
	} else {
		m1 <- "system time for inverse matrix calculation"
		u.start <- system.time(cacheSolve( m.constr ))[1] # user time for inverse matrix calculation
		m2 <- "system time to fetch cached inverse matrix"
		u.stop <- system.time(cacheSolve( m.constr ))[1] # user time for cached inverse matrix fetch
		return( list( m1=m1, u.start=u.start, m2=m2, u.start=u.stop )  )
	}
}
