#' Formula interface to ggforce sina plots
#'
#' @inherit ggforce::geom_sina description references
#' @inherit gf_point
#' @inheritParams ggforce::geom_sina
#' @inheritParams ggforce::stat_sina
#' @importFrom ggforce geom_sina stat_sina
#' @seealso [ggforce::geom_sina]
#' @export
#' @examples
#' gf_sina(age ~ substance, data = mosaicData::HELPrct)
gf_sina <-
  layer_factory(
    aes_form = list(y ~ x, ~x),
    geom = 'sina',
    stat = 'sina',
    position = 'dodge',
    extras = alist(
      alpha = , color = , fill = , group = , seed = ,
      size = , weight = , trim = TRUE,
      maxwidth = , adjust = ,
      scale = "area", bw = , adjust = 1, kernel = "gaussian",
      binwidth = , bin_limit = , bins = 50
    )
  )

