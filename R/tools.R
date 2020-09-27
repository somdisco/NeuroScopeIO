#' Convert a data cube to a data matrix
#'
#' Converts two or three dimensional arrays in [row,col] layout (such as those returned from \code{read_khoros_viff})
#' to either a vector (if cube only has 1 band) or matrix (if cube has > 1).
#' The columns of the data matrix correspond to the bands of the data cube.
#'
#' The rows of the data matrix are populated by the pixels of the cube, starting from the (x=1,y=1) position of the cube, then
#' (x=2,y=1), ..., (x=img_x,y=1), (x=1, y=2), ..., (x=img_x, y=img_y)
#'
#' @param datcub either a matrix or 3d array
#'
#' @return one of:
#' \itemize{
#'    \item a vector (if datcub is a matrix) of length = nrow(datcub)*ncol(datcub)
#'    \item a matrix (if datcub is an array) of dimension (nrow(datcub)*ncol(datcub), nbands(datcub))
#' }
#'
#'
#' Example:
#' convert_datcub_to_datmat(read_khoros_viff('8class-caseI-n_m0v5.viff'))
#'
#' @export
convert_datcub_to_datmat = function(datcub) {
  # ### a is either a 3d array or a matrix
  # ### function will "vectorize" the array to band sequential layout
  # ### starting from the NW corner, ending at SE corner.  The first row/element of the output represents data
  # ### in the NW corner of the image, while the last represents data in the SE corner of the image
  # width = dim(X)[2]
  # height = dim(X)[1]
  # depth = dim(X)[3]
  #
  # ### For 3d arrays, depth will be an integer
  # if(!is.na(depth)) {
  #   out = matrix(NA, nrow = width*height, ncol = depth)
  #   for(i in 1:depth) {out[,i] = c(t(X[,,i]))}
  # } else {
  #   out = c(t(X))
  # }
  #
  # return(out)

  ## Force X to be an array if it was given as a matrix
  if(is.na(dim(datcub)[3])) {
    datcub = array(datcub, dim = c(nrow(datcub), ncol(datcub), 1))
  }

  datcub = base::aperm(datcub, perm = c(2,1,3))
  datamat = matrix(datcub, nrow = prod(dim(datcub)[1:2]), ncol = dim(datcub)[3])
  return(datamat)
}




#' Convert a data matrix to a data cube
#'
#' The rows of a data matrix contain the pixel spectra.
#' This function puts a data matrix back into a cube (normally 3d array, 2d matrix if there is only one band)
#'
#' @param datmat a data matrix of dimension (img_x*img_y, num bands)
#' @param img_x an integer specifying the width of the image in pixels
#' @param img_y an integer specifying the height of the image in pixels
#'
#' @return a 3d array of dimension (img_y, img_x, num bands).  The first row of X will be put in the [1,1,] entry of the result.
#'
#' Example:
#' convert_datmat_to_datcub(convert_datcub_to_datmat(read_khoros_viff('8class-caseI-n_m0v5.viff')))
#'
#' @export
convert_datmat_to_datcub = function(datmat, img_x, img_y) {
  # n_dim = dim(X)[2]
  # n_pixels = img_x * img_y
  # out = array(NA, dim = c(img_y, img_x, n_dim))
  # for(i in 1:n_dim) {
  #   out[,,i] = matrix(X[,i], nrow = img_height, ncol = img_width, byrow = T)
  # }

  # return(out)

  if(!is.matrix(datmat)) datmat = as.matrix(datmat)
  datacube = array(datmat, dim = c(img_x,img_y,ncol(datmat)))
  datacube = base::aperm(datacube, perm = c(2,1,3))
  return(datacube)
}



#' Convert pixel indices to linear indices
#'
#' Cube layout indices are of the form [row, col].  This function returns the linear index in band sequential layout of [row, col]
#'
#' @param row the row index of the pixel(s)
#' @param col the column index of the pixel(s)
#' @param img_x the width of the image in pixels
#' @param img_y the height of the image in pixels
#'
#' @details row and col can be vectors, in which case they must have the same length, and the output will be this length.
#' Both row and col are assumed to be 1-indexed (vs. 0), and the output will be returned 1-indexed.
#'
#' @return integer indices in 1:(img_x*img_y)
#'
#' Example:
#' convert_pxlidx_to_linidx(1,1,10,10) returns 1
#' convert_pxlidx_to_linidx(2,1,10,10) returns 11
#' convert_pxlidx_to_linidx(10,1,10,10) returns 91
#' convert_pxlidx_to_linidx(10,10,10,10) returns 100
#'
#' @export
convert_pxlidx_to_linidx = function(row, col, img_x, img_y) {
  return( (row-1)*img_x + col );
}


#' Convert linear indices to pixel indices
#'.
#'
#' @param linidx linear index(ex) to convert
#' @param img_x the width of the image in pixels
#' @param img_y the height of the image in pixels
#'
#' @return a two-column matrix of [row,col] where row in 1:img_y and col in 1:img_x
#'
#' Example:
#' convert_linidx_to_pxlidx(1,10,10) returns [1,1]
#' convert_linidx_to_pxlidx(10,10,10) returns [1,10]
#' convert_linidx_to_pxlidx(91,10,10) returns [10,1]
#' convert_linidx_to_pxlidx(100,10,10) returns [10,10]
#'
#' @export
convert_linidx_to_pxlidx = function(linidx, img_x, img_y) {
  rowidx = ceiling(linidx/img_x)
  colidx = linidx - (rowidx-1)*img_x
  out = cbind(rowidx,colidx)
  colnames(out) = c('row','col')
  return( out )
}



#' Flip a data cube, along either it's width, height, or bands
#'
#' @param datcub a data cube
#' @param flipX boolean, whether to reverse the column order
#' @param flipY boolean, whether to reverse the row order
#' @param flipZ boolean, whether to reverse the band order
#'
#' @return a cube of the same
#'
#' @export
flip_datcub = function(datcub, flipX = F, flipY = F, flipZ = F) {

  if(flipX) {
    datcub = datcub[,ncol(datcub):1,]
  }

  if(flipY) {
    datcub = datcub[nrow(datcub):1,,]
  }

  if(flipZ) {
    datcub = datcub[,,dim(datcub)[3]:1]
  }

  return(datcub)
}

#' Compute summary statistics over the bands of a data cube
#'
#' @param datcub a data cube of dimensions (nrows, ncols, nbands)
#' @param stat the summary statistic to compute, can be one of:
#' 'sum', 'mean', 'sd', 'min', 'max', 'median', 'q25' (0.25 quantile), 'q75', (0.75 quantile)
#'
#' @details NAs will be ommitted
#'
#' @return a matrix (dimension = nrows x ncols) containing the summary statistic per band
#'
#' @export
datcub_band_stats = function(datcub, stat = "mean") {
  ## Make sure datcub is an array
  stopifnot(!is.na(dim(datcub)[3]))

  ## Decode the summary stat
  if(stat=="sum") summary_fxn = function(z) {return(sum(z, na.rm = T))}
  else if(stat=="mean") summary_fxn = function(z) {return(mean(z, na.rm = T))}
  else if(stat=="sd") summary_fxn = function(z) {return(stats::sd(z, na.rm = T))}
  else if(stat=="min") summary_fxn = function(z) {return(min(z, na.rm = T))}
  else if(stat=="max") summary_fxn = function(z) {return(max(z, na.rm = T))}
  else if(stat=="median") summary_fxn = function(z) {return(stats::median(z, na.rm = T))}
  else if(stat=="q25") summary_fxn = function(z) {return(stats::quantile(z, probs = 0.25, na.rm = T))}
  else if(stat=="q75") summary_fxn = function(z) {return(stats::quantile(z, probs = 0.75, na.rm = T))}
  else {stop("unknown value of stat")}

  ## Apply it
  out = apply(datcub, MARGIN = c(1,2), FUN = summary_fxn)

  return(out)
}



