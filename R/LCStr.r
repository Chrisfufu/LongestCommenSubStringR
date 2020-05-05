#' Longest Common Substring
#'
#' it takes two strings
#' @param aString and bString are two strings
#' @return the Longest Common SubString.
#' @import stringr
#' @export
#'



LCStr <- function(aString, bString, minLen){

  LCS = matrix(data = 0,nrow = nchar(aString)+1, ncol = nchar(bString)+1)
  lengthOfSubstring = -1
  finalIndex = -1

  for(i in 1:nchar(aString)+1){
    a<-stringr::str_sub(aString, i-1, i-1)
    for (j in 1:nchar(bString)+1){
      b<-stringr::str_sub(bString, j-1, j-1)
      if(a==b){
        LCS[i,j] = LCS[i-1,j-1]+1
        if (lengthOfSubstring < LCS[i,j]){
          lengthOfSubstring = LCS[i,j]
          finalIndex = i
        }
      }
      else{
        LCS[i,j] = 0
      }
    }
  }

  if (lengthOfSubstring > minLen){
    return (stringr::str_sub(aString, finalIndex-lengthOfSubstring, finalIndex-1))
  }
  else{
    return('no result')
  }
}
