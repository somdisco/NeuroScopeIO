
.HSL_Lightness = function(R,G,B) {
  return((pmin(R,G,B) + pmax(R,G,B))/(255*2))
}



#' View a color table
#'
#' @description This is a helper function to provide a quick visualization of the labels and their associated colors that are defined
#' in an input color table.
#'
#' @param ctab the color table dataframe to view. Must have fields  \code{$class}, \code{$R}, \code{$G}, \code{$B}.
#' @param label.cex the size of labels, as plotted on the visual color grid
#' @param label.font the font size of the label, 1=regular, 2 = bold
#' @param nrows_in_display sets the number of rows in the visualized color table. Default = NULL invokes automatic generation of this value,
#' with an attempt to make the resulting color grid as square as possible.
#' @return nothing, only used for visualization
#' @export
vis_ctab = function(ctab, label.cex = 0.9, label.font = 2, nrows_in_display = NULL) {

  ### Dimensions of display table
  n = nrow(ctab)

  if(is.null(nrows_in_display)) {
    xdim = floor(sqrt(n))
    ydim = ceiling(n / xdim)
  } else {
    xdim = ceiling(n / nrows_in_display)
    ydim = ceiling(n / xdim)
  }

  ### x & y coordinates of center squares
  xcoord = 1:xdim - 0.5
  ycoord = ydim:1 - 0.5
  xy = as.matrix(expand.grid(xcoord, ycoord))
  xy = xy[1:n,]

  ### Colors of blocks from color table
  block.colors = grDevices::rgb(red = ctab$R, green = ctab$G, blue = ctab$B, maxColorValue = 255)

  ### The text color of the labels is dependent on the corresponding block color
  ### Default text color is black, but if the block is too dark (toward black)
  ### the text should be white to be readable.
  text.colors = rep("black", times = n)
  Lightness = .HSL_Lightness(R = ctab$R, G = ctab$G, B = ctab$B)
  text.colors[Lightness < 0.4] = "white" ## use default 0.4 as a cutoff if not given

  graphics::par(mar = rep(0,4), oma = rep(0,4), bty="n", xaxs = "i", yaxs = "i")
  plot(x = c(0, xdim), y = c(0, ydim), type="n", asp = 1, axes = F)
  graphics::rect(xleft = xy[,1]-0.5, ybottom = xy[,2] - 0.5, xright = xy[,1] + 0.5, ytop = xy[,2] + 0.5,
       col = block.colors, border = NA)
  graphics::text(x = xy[,1], y = xy[,2], labels = ctab$class, cex = label.cex , offset = 0, col = text.colors, font = label.font)

}



#' Visualize a class map (class labels) of image pixels
#'
#' Provides a visual of the class map with colors specified by the color table data frame.
#'
#' @param class_matrix a matrix (nrows = img_y, ncols = img_x) whose [i,j] entry is a character label for pixel[i,j]
#' @param ctab a color table data frame, such as the result of calling \code{read_ctab}
#' @param pixel_expansion_factor an integer multiple dictating how much to expand each pixel in the resulting image.  No expansion correspoinds to a value of 1.  A value of 2, for example, will double both the width and height of the resulting image.  This is most useful when visualizing information about prototypes since they SOM lattice is generally much smaller than the image data cube.
#' @return none, a visualization
#'
#' Example:
#' vis_classmap(read_incl('8class-caseI-all.incl', 15, 15)$class, ctab, 1)
#'
#' @export
vis_classmap = function(class_matrix, ctab, pixel_expansion_factor=1) {

  ### ****** If class_matrix contains values NOT in ctab, abort ******
  unq.class_matrix = unique(c(class_matrix))
  unq.ctab = unique(ctab[,1])
  if( any(!(na.omit(unq.class_matrix) %in% unq.ctab)) ) {
    cat(sprintf('Color table does not contain all instances of input matrix'))
    return(0)
  }

  ### ****** Construct the RGB array for imager ******
  R = matrix(NA, nrow = nrow(class_matrix), ncol = ncol(class_matrix))
  G = matrix(NA, nrow = nrow(class_matrix), ncol = ncol(class_matrix))
  B = matrix(NA, nrow = nrow(class_matrix), ncol = ncol(class_matrix))

  for(i in unq.class_matrix) {
    imgidx = which(class_matrix == i)
    ctabidx = which(ctab[,1] == i)
    R[imgidx] = ctab[ctabidx,2]
    G[imgidx] = ctab[ctabidx,3]
    B[imgidx] = ctab[ctabidx,4]
  }

  RGBarray = array(NA, dim = c(pixel_expansion_factor*nrow(class_matrix), pixel_expansion_factor*ncol(class_matrix), 3))

  for(i in 1:nrow(class_matrix)) {
    for(j in 1:ncol(class_matrix)) {
      idx = expand.grid((i-1)*pixel_expansion_factor + 1:pixel_expansion_factor, (j-1)*pixel_expansion_factor + 1:pixel_expansion_factor)
      idx = as.matrix(idx)
      RGBarray[cbind(idx,1)] = R[j,i] ### MUST TRANSPOSE FOR IMAGER
      RGBarray[cbind(idx,2)] = G[j,i]
      RGBarray[cbind(idx,3)] = B[j,i]
    }
  }

  RGBarray = RGBarray / 255

  RGBarray = imager::as.cimg(RGBarray)

  #par(mar=c(0,0,0,0))
  plot(RGBarray, xlab=NULL, ylab=NULL, axes=FALSE, rescale=F, col.na = grDevices::rgb(0,0,0))
}



#' Visualize discrete values in a matrix
#'
#' This is the equivalent of a heatmap for discrete values
#'
#' @param mat the matrix whose values will be visualized
#' @param colors an option vector containing the colors used during visualization.
#' The colors will be assigned in ascending order of the unique values in mat, so that
#' colors[1] corresponds to the value sort(unique(mat))[1],
#' colors[2] corresponds to the value sort(unique(mat))[2], and so on.
#' If not given, a color vector will be sampled from R's built in color palette
#' @param seed the sampling seed used for setting the colors vector internally.
#'
#' @return nothing, visualization is produced
#'
#' @export
vis_matrix_discrete = function(mat, colors = NULL, seed = 100) {
  ## Determine unique values in input matrix
  unq_values = sort(unique(c(mat)))

  ## If no vector of colors was given, build one by sampling Rs color palette (with given seed).
  ## If one was given, make sure it contains enough colors to represent each distinct value found in mat
  if(is.null(colors)) {
    colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    set.seed(seed)
    if(length(colors) > length(unq_values))
      colors = sample.int(n = length(unq_values), size = length(colors), replace = T)
    else
      colors = sample.int(n = length(unq_values), size = length(colors), replace = F)
  } else {
    if(length(colors) < length(unq_values)) {
      colors = rep(colors, length.out = length(unq_values))
      warning("num. unique values in mat < length(colors), colors were recycled")
    }
  }

  RGBarray = t(col2rgb(colors[match(c(mat), unq_values)]))
  RGBarray = array(RGBarray, dim = c(nrow(mat), ncol(mat), 3))
  RGBarray = base::aperm(RGBarray, perm = c(2,1,3))

  RGBarray = RGBarray / 255

  RGBarray = imager::as.cimg(RGBarray)

  graphics::par(mar=c(0,0,0,0))
  plot(RGBarray, xlab=NULL, ylab=NULL, axes=FALSE, rescale=F, col.na = grDevices::rgb(0,0,0))


}



#' Visualize continuous values in a matrix
#'
#' This is a heatmap of matrix values
#'
#' @param mat the matrix whose values will be visualized
#' @param scale_fxn the function used to map the values in mat to the range [0,1].
#' Can be either 'ecdf' for empirical distribution values or 'linear'
#' @param qclip two-element vector specifying a range in [0,1] of quantiles used to clamp values.
#' Values < lower quantile are reset to lower quantile, and values > high quantile are reset to the high quantile.
#' Optional, default is [0,1].
#' @param low_color Optional, specifies the color used for the low end of the color ramp.
#' Defaults to 'white'.
#' @param high_color Optional, specifies the color used for the high end of the color ramp.
#' Defaults to 'navy'
#'
#' @return nothing, visualization is produced
#'
#' @export
vis_matrix_gradient = function(mat, scale_fxn = "linear", qclip=c(0,1), low_color = 'white', high_color = 'navy') {

  if(!(scale_fxn %in% c('linear','ecdf'))) stop("scale_fxn must be either 'linear' or 'ecdf'")
  if(length(qclip)!=2 || any(qclip < 0) || any(qclip > 1)) stop("qclip must be a vector (length=2) in [0,1].")

  nr = nrow(mat)
  nc = ncol(mat)
  mat = c(mat)

  val_rng = stats::quantile(mat, probs = qclip, na.rm = T)
  val_low = val_rng[1]
  val_high = val_rng[2]
  mat[mat <= val_low] = val_low
  mat[mat >= val_high] = val_high

  if(scale_fxn == "ecdf") {
    mat = stats::ecdf(mat)(mat)
  } else if(scale_fxn == "linear") {
    mat = (mat - min(mat, na.rm = T)) / diff(range(mat,na.rm=T)) * (1 - 0) + 0
  }


  color = grDevices::colorRamp(c(low_color,high_color))(mat)
  if(any(is.na(mat))) {
    color[which(is.na(mat)),] = c(0,0,0)
  }

  RGBarray = array(color, dim = c(nr,nc,3))
  RGBarray = RGBarray / 255
  RGBarray = base::aperm(RGBarray, perm = c(2,1,3))

  RGBarray = imager::as.cimg(RGBarray)

  graphics::par(mar=c(0,0,0,0))
  plot(RGBarray, xlab=NULL, ylab=NULL, axes=FALSE, rescale=F, col.na = grDevices::rgb(0,0,0))


}


