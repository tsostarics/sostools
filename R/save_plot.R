#' Save plot helper
#'
#' Saves a given plot as an svg, pdf, and png file with given parameters.
#' Recommended to use `ggview` to settle on the appropriate width, height, and
#' scale, then pass the parameters to this function
#'
#' Default behavrior is to create a `Figures/` directory in the current working
#' directory.
#'
#' @param p Plot
#' @param filename String, filename to use, no file extension
#' @param dpi integer, dpi to use for png file. Default uses option SOSDISS2_DPI,
#' which is set to 300 on load.
#' @param use_subfolders Logical, defaults to FALSE, if TRUE will create
#' pdf, png, and svg subfolders in the Figures/ directory and save each figure
#' to the appropriate subdirectory. Useful when you have a LOT of figures.
#' @param ... Additional names parameters to pass
#' @param rootdir Directory to save files to, defaults to Figures. If this does
#' not already exist it will be created. You can set this to a subdirectory
#' so that when you set `use_subfolders` to TRUE it will create the correct
#' subfolders where you want them.
#' @param img_type character vector of image file types to save. Defaults
#' to c("pdf", "png", "svg"). Usually used to save only png files of
#' diagnostic plots that dont need vector versions.
#' @param .skip_save Logical, whether to skip saving the plot. Useful if you're
#' rerunning a script or rmd/qmd doc and don't want to re-save plots. You can
#' also use `option(SOSTOOLS_SKIP_PLOT_SAVE = FALSE)` to set this globally for
#' the current session.
#' @param postprocess A named list of functions to apply to the specific
#' output files AFTER they are saved. Eg for watermarking or cropping purposes.
#' The names must correspond to the individual filetypes, all of which must
#' also be specified in `img_type`
#'
#' @return Invisibly returns the paths to the saved plots
#' @export
save_plot <- function(p,
                      filename,
                      rootdir = "Figures",
                      dpi = getOption("SOSDISS2_DPI"),
                      use_subfolders=TRUE,
                      ...,
                      img_type = c("pdf", "png", "svg"),
                      .skip_save = getOption("SOSTOOLS_SKIP_PLOT_SAVE"),
                      postprocess) {

  stopifnot(all(img_type %in% c("pdf", "png", "svg")))

  # Quit early if the postprocessing list isn't named or has incorrect names
  if (!missing(postprocess)) {
    pp_names <- names(postprocess)
    stopifnot(!is.null(pp_names) && all(pp_names %in% img_type) && !any(duplicated(pp_names)))
    stopifnot(!all(vapply(postprocess, is.function, logical(1))))
  }

  if (!dir.exists(rootdir))
    dir.create(rootdir)

  if (use_subfolders) {
    # Ensure the subfolders exist before we try to write to them
    for (ext in img_type) {
      if (!dir.exists(file.path(rootdir, ext)))
        dir.create(file.path(rootdir, ext))
    }
    plot_path <- \(ext) file.path(rootdir,ext, paste0(filename, ".", ext))
  } else {
    plot_path <- \(ext) file.path(rootdir, paste0(filename, ".", ext))
  }

  if (!.skip_save) {

    # Save SVG file
    if ("svg" %in% img_type)
      ggplot2::ggsave(plot = p, filename = plot_path("svg"),
                      device = grDevices::svg,
                      ...)
    # Save PDF file
    if("pdf" %in% img_type)
      ggplot2::ggsave(plot = p, filename = plot_path("pdf"),
                      device = grDevices::cairo_pdf,
                      ...)
    # grDevices::dev.off() # Closes devices so we can resume plotting in main session

    if ("png" %in% img_type)
      # Save PNG file
      rlang::inject(
        ggplot2::ggsave(plot = p, filename = plot_path("png"),
                        device = grDevices::png, dpi = dpi,
                        # We need to use the cairo png device, which requires
                        # passing type = 'cairo' to the graphics device via ...
                        ...=!!!c(type = "cairo", rlang::dots_list(...))))
  }

  paths <- vapply(img_type, plot_path, "char")

  # If any postprocessing functions are set, do them now on the saved files
  if (!missing(postprocess)) {
    for (filetype in names(postprocess)) {
      fx <- postprocess[[filetype]]
      fx(paths[filetype])
    }
  }


  invisible(paths)
}

#' A function to crop white margins of a PNG image
#'
#' This is taken with some slight modifications from here:
#' https://github.com/statnmap/SDMSelect/blob/master/R/ToolFunctions.R
#'
#' @param x path to the PNG image
#' @param new_margin number of white pixels lines to keep
#' @param overwrite Logical, default FALSE, whether to overwrite the original
#' file. If FALSE, a new file with _crop appended to the filename will be saved.
#'
#' @export
crop_white_margins <- function(x, new_margin = 15, overwrite = FALSE)
{
  # Cut the saved image to remove excessive whitespace, leaving the specified new margin
  img <- png::readPNG(x)
  nudge <- 1L + as.integer(new_margin)

  img.test.row <- apply(img, 3, function(layer) {
    apply(layer, 1, function(i) {(sum(i != 1) > 0)})
  }) |>
    apply(1, function(i) {(sum(i) > 0)})

  n_rows <- length(img.test.row)
  half_rows <- n_rows %/% 2
  row_indices <- seq.int(half_rows, n_rows)
  rowMin <- max(min(which(img.test.row[seq_len(half_rows)])) - nudge, 1)
  rowMax <- min(max(seq_len(n_rows)[row_indices][which(img.test.row[row_indices])]) +nudge, n_rows)

  img.test.col <- apply(img, 3, function(layer) {
    apply(layer, 2, function(i) {(sum(i != 1) > 0)})
  }) |>
    apply(1, function(i) {(sum(i) > 0)})

  n_cols <- length(img.test.col)
  half_cols <- n_cols %/% 2
  col_indices <- seq.int(half_cols, n_cols)
  colMin <- max(min(which(img.test.col[seq_len(half_cols)])) - nudge, 1)
  colMax <- min(max(seq_len(n_cols)[col_indices][which(img.test.col[col_indices])]) + nudge,n_cols)

  # Remove rows and cols with white pixels from the original image
  img <- img[seq.int(rowMin,rowMax), seq.int(colMin,colMax),]
  if (overwrite){
    outpath <- x
  } else {
    outpath <- paste0(gsub(".png$", "", x), "_crop.png")
  }

  png::writePNG(img, target = outpath)

  rm(img)

  invisible(outpath)
}
