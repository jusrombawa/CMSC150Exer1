#ROMBAWA, JUSTIN AARON S.
#CMSC 150 AB-1L Exer 1

#finds if matrix input is square matrix
#does not take into account multi-way arrays yet
SquareMatrix = function(matA)
{
  b = dim(matA)
  
  if(b[1] == b[2]) {
    return(TRUE)
  } else if(b[1] != b[2]) {
    return(FALSE)
  }
}

#returns minor of a matrix with respect to i and j
#no need to check if square
#input: matrix, int, int
MatrixMinor = function(matA,i,j)
{
  matA = matA[-i,-j]

  return(matA)
}

#returns cofactor of a matrix with respect to i and j
#square not required
#input: matrix, int, int
MatrixCofactor = function(matA,i,j)
{
  sign = (-1)**(i+j)
  minA = MatrixMinor(matA,i,j)
  detA = det(minA)
  cofA = sign*detA
  return(cofA)
}

#returns adjoint of matrix (matrix of cofactors)
#square required, returns NULL and prints NA otherwise
MatrixAdjoint = function(matA)
{
  checkSquare = SquareMatrix(matA)
  #iterators for nested while loop, one-indexing in case I forget
  x = 1
  y = 1
  #length of each side of matrix
  len = dim(matA)[1]
  #adjoint matrix of A, size len x len, temporarily fill with zeroes
  adj = matrix(0,len,len)
  
  if(checkSquare == FALSE)
  {
    print("NA")
    return(NULL)
  } else {
      
    while(x <= len)
    {
      while(y <= len)
      {
        #take adjoint at [x,y] then increment y
        adj[x,y] = MatrixCofactor(matA,x,y)
        y = y+1
      }
      #reset y to 1 then increment x
      y = 1
      x = x+1
    }
    return(adj)
  }
}

#returns inverse of matrix
#requires square, returns null and prints NA otherwise
MatrixInverse = function(matA)
{
  checkSquare = SquareMatrix(matA)
  determinant = det(matA)
  
  #convert matA from column major to row major
  temp = c(matA)
  len = dim(matA)[1]
  matA = matrix(temp,len,len,byrow = TRUE)
  
  if(checkSquare == FALSE)
  {
    print("NA")
    return(NULL)
  } else if(checkSquare == TRUE && determinant == 0)
  {
    print("Determinant is equal to zero, inverse does not exist")
    return(NULL)
  } else  {
    inverse = MatrixAdjoint(matA)/determinant
    return(inverse)
  }
}
