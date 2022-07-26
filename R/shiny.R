#' Text Data Explorer
#'
#' @description 정규표현식 기반의 텍스트 데이터  탐색 작업을 위한 Shiny 앱 호출
#' @return 없음
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @seealso \code{\link{morpho_mecab}}
#' @examples
#' \dontrun{
#'  library(bitTA)
#'
#'  ## 텍스트 데이터 탐색기(Shiny Web Application) 호출
#'  explore_docs()
#' }
#' @export
#'
explore_docs <- function() {
  library(shiny)

  runApp(system.file("shiny/explore_docs", package="bitTA"))
}
