#' Text Data Quality Correction Worker
#'
#' @description 정규표현식 기반의 텍스트 데이터  품질 보정 작업을 위한 Shiny 앱 호출
#' @return 없음
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @seealso \code{\link{morpho_mecab}}, \code{\link{morpho_hann}}
#' @examples
#' \dontrun{
#'  library(bitTA)
#'
#'  ## 텍스트 데이터  품질 보정 작업기(Shiny Web Application) 호출
#'  doc_quality()
#' }
#' @export
#'
doc_quality <- function() {
  library(shiny)

  runApp(system.file("shiny/doc_quality", package="bitTA"))
}
