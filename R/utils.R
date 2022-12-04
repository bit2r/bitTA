get_os <- function() {
  system_info <- Sys.info()
  if (!is.null(system_info)) {
    os <- system_info["sysname"]
    if (os == "Darwin") 
      os <- "osx"
  }
  else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) 
      os <- "osx"
    if (grepl("linux-gnu", R.version$os)) 
      os <- "linux"
  }
  tolower(os)
}

#' 한글의 종성 여부 체크
#' @description 한글의 종성 여부
#' @param x character. 종성 여부를 확인할 문자.
#' @param last logical. 마지막 음절만 체크여부. 기본값은 FALSE로 전체 음절을 체크함. 
#' @details 
#' 첫 번째 한글 글자는 ‘가’로 유니코드로는 AC00입니다. ‘가’를 시작 위치를 상대적인 위치 값을 구합니다.
#' 종성이 없는 글자 이후에 27개의 종성이 있는 글자가 옵니다. 그리고 다시 종성이 없는 글자가 옵니다. 
#' 따라서 28로 나눴을 때 나머지가 없으면 종성이 없는 글자입니다.
#' @return logical. 음절별 종성포함 여부를 의미하는 벡터. 한글이 아닌 음절의 경우에는 FALSE.
#' @examples
#' \donttest{
#' has_final_consonant("홍길동")
#' has_final_consonant("홍길동", last = FALSE)
#' 
#' has_final_consonant("텍스트 분석")
#' }
#' @export
has_final_consonant <- function(x, last = FALSE) {
  has_final <- (utf8ToInt(x) - strtoi(0xAC00)) %% 28 != 0
  
  idx <- gregexpr("[가-힣]", x) %>% 
    unlist()
  
  is_korean <- logical(nchar(x))
  is_korean[idx] <- TRUE
  
  has_final <- as.logical(has_final * is_korean)
  
  if (last) {
    return(has_final[nchar(x)])
  } 
  
  has_final
}


