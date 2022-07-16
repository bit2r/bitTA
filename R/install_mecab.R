is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

is_mac <-function() {
  identical(tolower(Sys.info()[["sysname"]]), "darwin")
}

#' 은전한닢 형태소분석기와 사전 설치
#' @description 은전한닢 형태소분석기인 mecab-ko와 은전한닢 형태소분석기 사전인 
#' mecab-ko-dic을 사용자 환경에 설치한다.
#' @details Linux와 Mac은 소스를 가져다 컴파일하며, Windows는 바이너리를 가져다 복사한다.
#' @author 유충현
#' Maintainer: 유충현 <choonghyun.ryu@gmail.com>
#' @examples
#' \dontrun{
#' install_mecab_ko()
#' }
#' @export
#' @importFrom glue glue
#' @importFrom rstudioapi askForPassword
install_mecab_ko <- function(x) {
  script_path <- system.file("script", package = "bitTA")
  install_script <- glue::glue("/bin/sh {script_path}/install_mecab.sh")
  
  if (.Platform$GUI %in% "RStudio") {
    input <- rstudioapi::askForPassword("sudo password")
  } else {
    input <- readline("Enter your password: ")
  }
  
  if (is_linux() | is_mac()) {
    # system2(command = "/bin/sh", args = install_script)
    system(glue::glue("sudo -kS {install_script}"), input = input)
  } else if (is_windows()) {
    # TO-DO
  }  
}