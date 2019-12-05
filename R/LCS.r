#' Longest Commen Substring
#'
#' it takes two strings
#' @param aString and bString are two strings
#' @return the Longest Commen SubString.
#' @import stringr
#' @export
#'

library(stringr)
LCS <- function(aString, bString, maxLength){
  LongestCommenSubString = matrix(data = 0,nrow = nchar(aString)+1, ncol = nchar(bString)+1)


  for(i in 1:nchar(aString)+1){
    a<-str_sub(aString, i-1, i-1)
    for (j in 1:nchar(bString)+1){
      b<-str_sub(bString, j-1, j-1)
      if(a==b){
        LongestCommenSubString[i,j] = LongestCommenSubString[i-1,j-1]+1
      }
      else{
        LongestCommenSubString[i,j] = 0
      }
    }
  }

  result = -1

  for (i in 1:nchar(aString)+1){
    for (j in 1:nchar(bString)+1){
      if (result<LongestCommenSubString[i,j]){
        result = LongestCommenSubString[i,j]
        aIndex = i
      }
    }
  }
  if (result > maxLength){
    commenSubString <- str_sub(aString, aIndex-result, aIndex-1)
  }
  else{
    commenSubString =-1
  }

  return(commenSubString)
}
