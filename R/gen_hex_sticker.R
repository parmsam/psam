#' @title Generate a hex sticker from an image file
#' @description Create and open a hex sticker using a reference image file
#' @param ref_image file path to reference image
#' @param file_name output image file, Default: 'hex.png'
#' @param pkg_name package name to include in hex sticker
#' @param git_name github repo name to include in hex sticker, Default: NULL
#' @param ft main font, Default: 'bangers'
#' @param ft1 annotation font, Default: 'rob'
#' @param txt_main package name text color, Default: 'black'
#' @param txt_repo repo text color, Default: 'black'
#' @return string with path to image file
#' @details function based on based on http://gradientdescending.com/how-to-generate-a-hex-sticker-with-openai-and-cropcircles/
#' @examples
#' \dontrun{
#' if(interactive()){
#'   ref_image <- "man/figures/DALL·E 2023-07-15 23.59.36 - Ukrainian cat wearing a beret and black turtleneck.png"
#'   file_name <- "man/figures/hex.png"
#'   pkg_name <- "psam"
#'   git_name <- "parmsam/psam"
#'   gen_hex_sticker(
#'      ref_image,
#'      file_name,
#'      pkg_name,
#'      git_name,
#'      txt_main = "white",
#'      txt_repo = "black"
#'   )
#'  }
#' }
#' @seealso
#'  \code{\link[sysfonts]{font_add_google}}
#'  \code{\link[showtext]{showtext_auto}}
#'  \code{\link[cropcircles]{crop}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{annotate}}, \code{\link[ggplot2]{lims}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{coord_fixed}}, \code{\link[ggplot2]{ggsave}}
#'  \code{\link[ggpath]{geom_from_path}}
#'  \code{\link[utils]{browseURL}}
#' @rdname gen_hex_sticker
#' @export
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom cropcircles hex_crop
#' @importFrom ggplot2 ggplot aes annotate xlim ylim theme_void coord_fixed ggsave
#' @importFrom ggpath geom_from_path
#' @importFrom utils browseURL
gen_hex_sticker <- function(
    ref_image,
    file_name = "hex.png",
    pkg_name,
    git_name = NULL,
    ft = "bangers",
    ft1 = "rob",
    txt_main = "black",
    txt_repo = "black"
  ) {
  sysfonts::font_add_google("Bangers", "bangers")
  sysfonts::font_add_google("Roboto", "rob")
  sysfonts::font_add_google("Barlow", "bar")
  showtext::showtext_auto()
  img_cropped <- cropcircles::hex_crop(
    images = ref_image,
    border_colour = txt,
    border_size = 24
  )

  g <- ggplot2::ggplot() +
    ggpath::geom_from_path(
      ggplot2::aes(0.5, 0.5, path = img_cropped)
      ) +
    ggplot2::annotate("text", x = 0.5, y = 0.25, label = pkg_name,
                     family = ft, size = 55, colour = txt_main,
                     lineheight = 0.25,
                     angle = 30, hjust = 0, fontface = "bold") +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void() +
    ggplot2::coord_fixed()

  if(!is.null(git_name)){
    g <- g +
      ggplot2::annotate("text", x=0.5, y = 0.017, family = ft1,
        size = 15, angle = 30,
        colour = txt_repo, hjust = 0,
        label = git_name
        )
  }
  ggplot2::ggsave(file_name, plot = g, height = 6, width = 6)
  utils::browseURL(file_name)
  return(file_name)
}
