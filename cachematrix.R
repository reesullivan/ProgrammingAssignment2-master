## compute inverses of matrices
## save in a location to speed processing for repeat matrices


## creates the cache of matrix inverses
makeCacheMatrix <- function(x = matrix()) 
{

     #assume x a square matrix nrow = ncol
     #the inverse matrix will have the same dimensions
   	

     #create a null matrix to start
     #this is where the inverse will be stored
     m <- matrix(nrow= nrow(x), ncol= nrow(x))


     #set is a function that defines x and m
     # x is what was input and m is reset to null
	set <- function(y)
	{
	          x <<- y
                m <<- matrix(nrow=nrow(y), ncol=nrow(y))
	}

      #get is the value of x from the function
	get <- function() x


      #define setinv as a function that assigns the INVERSE to m
      #set m as the inverse of the matrix
	setinv <- function(solve)m <<- solve


      #getinv returns the value of m, the inverse
	getinv <- function() m
	

      #the lookup list
	list(set=set, get=get,  setinv = setinv, getinv = getinv)

}



## use cacheSolve to create and store inverses, 
# if they are not already in the cache
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) 
{

	#m is the value of getinv from the list for matrix x  
      m <-x$getinv()


	#if there is no matrix x in the list, m is null. 
      #if there is a matrix that matches m in the list, m is not null 
       
	if(!is.null(m)) 
	{
		#m is not null, so there is a match
		#retrive that inverse
		message("getting cached data")
 
		# return the cached inverse
		return(m)
	}

     
      #otherwise, WHEN m is null, compute the inverse of the matrix x 
   
      #the data is the matrix x that was input
	g <- x$get()


      #m is the inverse computed from the matrix
	m <- solve(g)


      #add the new m and x value to the cache
	x$setinv(m)

 
      #return the computed inverse 
	return(m)
}


