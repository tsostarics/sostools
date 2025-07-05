
#' Convert a string to binary character vector
#'
#' Given a string, convert each character to binary representation. Limited to
#' 8 bit representations, which is more than enough for ascii characters.
#'
#' @param input_string String to encode
#'
#' @return Named character vector of binary encodings
string_to_binary <- function(input_string) {
  # Function to convert a single character to binary
  char_to_binary <- function(char) {
    ascii_value <- as.integer(charToRaw(char))
    binary_string <- intToBinary(ascii_value)

    # Ensure the binary string is 8 bits long
    binary_string <- sprintf("%08s", binary_string)
    return(binary_string)
  }

  # Function to convert integer to binary string
  intToBinary <- function(value) {
    if (value == 0) {
      return("0")
    }
    binary <- ""
    while (value > 0) {
      binary <- paste0(value %% 2, binary)
      value <- value %/% 2
    }
    return(binary)
  }

  # Apply char_to_binary to each character in the input string
  binary_vector <- vapply(strsplit(input_string, NULL)[[1]],
                          char_to_binary,
                          character(1))

  # Return the result as a character vector
  gsub(" ", "0", binary_vector)
}

#' Convert binary character vector to string
#'
#' Given a character vector of 8 bit binary representations, convert to a string
#'
#' @param binary_vector Character vector of binary encoded values
#'
#' @return String conversion of binary vector
binary_to_string <- function(binary_vector) {
  # Function to convert a binary string to a single character
  binary_to_char <- function(binary_string) {
    # Convert binary string to an integer
    ascii_value <- strtoi(binary_string, base = 2)
    # Convert integer to its corresponding character
    char <- rawToChar(as.raw(ascii_value))
    return(char)
  }

  # Apply binary_to_char to each binary string in the vector
  characters <- vapply(binary_vector, binary_to_char, character(1))

  # Combine all characters into a single string
  result_string <- paste0(characters, collapse = "")

  result_string
}

#' Read a watermarked png
#'
#' Loads a png file and converts the top left corner's binary representation
#' (assuming pure white vs off-white encoding from `encode_png`) to a string
#'
#' @param path Path to png file
#' @param nbits number of bits, nearly always 8
#'
#' @return String encoded in the png file
#' @export
read_watermarked_png <- function(path,
                                 nbits = 8L) {
  rlang::is_installed("png")
  img <- png::readPNG(path)
  bitstring_length <- seq_len(nbits)

  binary_to_string(
    vapply(seq_len(26L),
           \(row) {
             paste0(as.integer(img[row,bitstring_length,1L] != 1L), collapse = "")
           }, FUN.VALUE = 'char')
  )

}

#' Watermark a png
#'
#' Reads in a png file and watermarks the top left corner with the current
#' system time. Watermark is 26x8 pixels large.
#'
#' @param from Path to png file
#' @param to Output path file, set to NA to overwrite file specified by `from`
#'
#' @return output filepath
#' @export
watermark_png <- function(from, to = NA) {
  rlang::is_installed("png")

  if (is.na(to))
    to <- from

  to_watermark <- trimws(string_to_binary(as.character(Sys.time())))

  img <- png::readPNG(from)

  for (i in seq_along(to_watermark)) {
    row_values <- 1- (strsplit(to_watermark[i],"")[[1]] == 1) / 255

    for (j in seq_along(row_values)) {
      img[i,j,1:3] <- row_values[j]
    }
  }

  png::writePNG(img, to)
  to
}

#' Watermark a png figure
#'
#' Given a named character vector with an element named 'png', watermark the
#' file and return the path. Intended to serve in a pipeline after `save_plot`
#'
#' @param files Character vector containing an element named png
#' @param strict Logical, set to TRUE to throw an error if no element named
#' png is found, otherwise a warning is given.
#'
#' @return files
#' @export
watermark_png_figure <- function(files, strict = FALSE) {
  if (is.na(files["png"])) {
    msg <- "Vector does not contain an element named png"
    if (strict) {
      stop(msg)
    }

    warning(msg)
    return(files)
  }

  watermark_png(files['png'])

  invisible(files)
}


#' Scan png files in a directory
#'
#' Read the watermarks of all the png files in a directory and optionally
#' its subdirectories and displays diagnostics about the watermarks.
#'
#' @param dir Directory path
#' @param recursive Logical, default `TRUE`, see `list.files`
#'
#' @return Invisibly returns the watermarks for the files.
#' @export
scan_watermarked_directory <- function(dir, recursive = TRUE) {
  rlang::is_installed("RcppSimdJson")
  rlang::is_installed("png")

  png_files <- list.files(dir, pattern = ".png$", full.names = TRUE, recursive = recursive)

  tld <- basename(dirname(png_files))


  cli::cli_progress_bar(total = length(png_files))
  watermarks <- vapply(seq_along(png_files),
                       \(i) {
                         watermark <- read_watermarked_png(png_files[i])
                         cli::cli_progress_update(.envir = parent.frame(2))
                         watermark
                       }, character(1))
  cli::cli_process_done()
  names(watermarks) <- file.path(tld, basename(png_files))
  watermarks

  which_invalid <- !RcppSimdJson::is_valid_utf8(watermarks)
  valid_files <- watermarks[!which_invalid]
  which_blank <- names(which(valid_files == ""))
  which_systime <- names(valid_files[grepl("[0-9]{4}(-[0-9]{2}){2} ([0-9]{2}[:.]){3}[0-9]+", valid_files)])
  which_uncertain_format <- !names(valid_files) %in% c(which_blank, which_systime)

  if (any(which_invalid)){
    cli::cli_alert_danger("These watermarks are not valid utf-8:")
    cli_bullets2(names(which_invalid), "x")
  }

  if (length(which_blank) > 0){
    cli::cli_alert_warning("These watermarks are blank:")
    cli_bullets2(which_blank, "!")
  }

  if (any(which_uncertain_format)) {
    cli::cli_alert_info("I'm not what these watermarks are:")

    print(knitr::kable(watermarks[names(valid_files)[which_uncertain_format]],
                       col.names = c("Filename", "Watermark")))
  }

  if (length(which_systime) > 0){
    cli::cli_alert_success("These watermarks are good:")
    print(knitr::kable(watermarks[which_systime],
                       col.names = c("Filename", "Watermark")))
  }

  invisible(watermarks)
}

# Helper for formatting cli bullets
cli_bullets2 <- function(x, btype = "*") {
  names(x) <- rep(btype, length(x))
  cli::cli_bullets(x)
}
