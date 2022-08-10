.bitTAEnv <- new.env()

.onAttach <- function(libname, pkgname) {
  # RmecabKo의 .onLoad() 참조
  if (is_windows()) {
    op <- options()
    mecabOptions <- mecab_libs()[1]
    if(!(names(mecabOptions) %in% names(op))) options(mecabOptions)
  }
  
  if (!is_mecab_installed()) {
    message("To use bitTA, you need to install mecab-ko and mecab-ko-dic.\nYou can install it with install_mecab_ko().")
  }
}
