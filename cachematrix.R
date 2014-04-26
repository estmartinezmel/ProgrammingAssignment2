## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a = matrix()) { ##creation of a matrix object 
	inv <- NULL   ##inicialization of the matrix for get the inverse
	set <- function(b) {  ##function of set matrix data.##if matrix change, inverse is initialized again 
		a <<- b
		inv <<- NULL   
	}
	get <- function() a	 ##return the matrix
	setinv <- function(inverse) inv <<- inverse  ##set of matrix inverse
	getinv <- function() inv ##return the inverse of matrix, if matrix inverse hasn't calculated yet then return null
	list(set = set,get = get, ##list of functions
		setinv = setinv,
		getinv = getinv)
}

cacheSolve <- function(a) { ##function that calcule and return the inverse of matrix 'a'
	inv <- a$getinv() ## first get the inverse of matrix
        if(!is.null(inv)) { ## if inverse has already calculated then return the message with the inverse matrix
                message("GETTING CACHED DATA")
                return(inv)
        }
        data <- a$get() ##else then get data of a 
        inv <- solve(data) ##calcule inverse of adquired data
        a$setinv(inv) ##set the inverse of object a
        inv	##return the inverse
}
