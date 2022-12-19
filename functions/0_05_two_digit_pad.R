two_digit_pad <- function(x) {
  stringr::str_pad(x, 2, 'left', '0')
}
