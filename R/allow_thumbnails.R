#' @export
allow_thumbnails <- function(x, options) {
  if (!is.null(options$thumb)) {
    filename <- sprintf("%s.full.pdf", strsplit(basename(x), "\\.")[[1]][1])
    absolute_path <- file.path(dirname(x), filename)
    
    # generate the full resolution pdf
    pdf(absolute_path, width = options$thumb$width, height = options$thumb$height)
    eval(parse(text = options$code))
    dev.off()
    
    # add an html link to the low resolution png
    options$fig.link = absolute_path
  }
  
  knitr:::hook_plot_md_base(x, options)
}

#'@export
thumbnail <- function(title, img, href, caption = TRUE) {
  library(htmltools)
  div(class = "col-sm-4",
      a(class = "thumbnail", title = title, href = href,
        img(src = img),
        div(class = ifelse(caption, "caption", ""),
            ifelse(caption, title, "")
        )
      )
  )
}