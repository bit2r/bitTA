.bitTAEnv <- new.env()

.onAttach <- function(libname, pkgname) {
  if (!is_mecab_installed()) {
    message("To use bitTA, you need to install mecab-ko and mecab-ko-dic.\nYou can install it with install_mecab_ko().")
  }
}
