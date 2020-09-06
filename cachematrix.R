#makeCachematrix and cacheSolve are two functions in this code.

makeCachematrix<-function(x=matrix()){
    inv<-NULL                 #initialize inverse as NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x           #Function for inputting matrix
    setInverse<-function(inverse)(inv <<- inverse)
    getInverse<-function(){inv}    #Function for getting inverse
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

cacheSolve<-function(x,...){     #this is to cache data
     inv<-x$getInverse()
     if(!is.null(inv)){               #checking if inverse in NULL
         message("getting cached data")
         return(inv)           #returns inverse value
     }
     mat<-x$get()
     inv<-solve(mat,...)     #calculates inverse of the matrix
     x$setInverse(inv)
     inv
}
#input
pmatrix<-makeCachematrix(matrix(10:13,nrow=2,ncol=2))
pmatrix$get()
pmatrix$getInverse()
cacheSolve(pmatrix)
pmatrix$getInverse()

