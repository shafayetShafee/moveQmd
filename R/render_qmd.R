get_fname <- function(file) {
  fs::path_ext_remove(fs::path_file(file))
}

has_path <- function(file) {
  fs::path_dir(file) != "."
}

render_qmd <- function(input_file, output_file = NULL, ...) {
  if (has_path(input_file)) {
    stop("Using file path as input_file is not allowed. Use only the file name")
  }
  # browser()
  input_file_name <- get_fname(input_file)
  dep_files <- paste0(input_file_name, "_files")

  if (!is.null(output_file)) {
    output_dir <- fs::path_dir(output_file)
    output_ext <- fs::path_ext(output_file)
    if(is.null(output_ext)) stop("Couldn't determine the output-file extension from output-file name")
    output_file<- fs::path_file(output_file)
  }
  quarto::quarto_render(input = input_file, output_file = output_file, ...)

  if(!is.null(output_dir)) {
    if(!fs::dir_exists(output_dir)) {
      stop(paste0("Output Directory ", output_dir, "doesn't exists"))
    }
    fs::file_move(output_file, output_dir)
    if (fs::dir_exists(dep_files)) {
      dep_path <- fs::path(output_dir, dep_files)
      if(fs::dir_exists(dep_path)) fs::dir_delete(dep_path)
      fs::dir_copy(dep_files, output_dir)
      fs::dir_delete(dep_files)
    }

    msg <- paste0(output_file, " moved to ", fs::path_abs(output_dir))
    message(msg)
  }
}


render_qmd("index.qmd", "E:/myoutput/output.html", output_format = "all")
render_qmd("index.qmd", "E:/myt/output.html")
