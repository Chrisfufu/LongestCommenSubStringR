
LongestCommenSubString <- function(aString, bString){
  LCS = matrix(data = 0,nrow = nchar(aString)+1, ncol = nchar(bString)+1)


  for(i in 1:nchar(aString)+1){
    a<-str_sub(aString, i-1, i-1)
    for (j in 1:nchar(bString)+1){
      b<-str_sub(bString, j-1, j-1)
      if(a==b){
        LCS[i,j] = LCS[i-1,j-1]+1
      }
      else{
        LCS[i,j] = 0
      }
    }
  }

  result = -1

  for (i in 1:nchar(aString)+1){
    for (j in 1:nchar(bString)+1){
      if (result<LCS[i,j]){
        result = LCS[i,j]
        aIndex = i
      }
    }
  }
  if (result > 1){
    commenSubString <- str_sub(aString, aIndex-result, aIndex-1)
  }
  else{
    commenSubString =-1
  }

  return(commenSubString)
}
