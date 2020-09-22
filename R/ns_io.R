#' Reading and processing an include file
#'
#' Include files contain information on which parts of the image cube are masked (i.e., pixels that should be ignored) as well as the class designation of each pixel, if available.
#'
#' @param filename string specifying the file path & name of the include file
#' @param img_x integer, width of image in pixels
#' @param img_y integer, height of image in pixels
#'
#' @return a list with components:
#' \itemize{
#'   \item masked:  an img_y x img_x boolean matrix whose [i,j] entry specifies whether the pixel in row i, column j of the image should be masked.
#'   \item class:  an img_y x img_x character matrix whose [i,j] entry defines the class label of the pixel in row i, column j of the image.  Due to Khoros limitations, the available classes are A-Z,a-z plus some other special characters.\cr
#'                 Note:  labeling of all pixels in the include file is not mandatory.  If no class information is found, NA will be returned.
#' }
#'
#' Example:
#' read_incl('8class-caseI-all.incl', 15, 15)
#'
#' @export
read_incl = function(filename, img_x, img_y) {

  ### filename = string of file path name
  ### img_x & img_y are integers denoting # of pixels in each dim of image
  ### output_type is either "matrix" or "vector", indicating whether the results should be returned
  ###    as a mat or a vec.  If vec, the order will be starting from the NW corner to the SE corner, reading
  ###    left to right

  num_pixels = img_x * img_y

  ### ****** Read include file into character vector ******
  ### Each entry in the vector represents one line in the include file
  incl = readLines(filename)

  ### ****** Remove comments ******
  ### If any element contains a # (comment character), remove everything in that line after it
  comment_start_pos = stringr::str_locate(incl,"#")[,1]
  idx_to_change = which(!is.na(comment_start_pos))
  incl[idx_to_change] = stringr::str_sub(incl[idx_to_change], start = rep(1,length(idx_to_change)), end = comment_start_pos[idx_to_change]-1)

  ### ****** Remove lines that do not contain either "include" or "exclude"
  incl = incl[stringr::str_detect(incl, "include") | stringr::str_detect(incl, "exclude")]

  #* Following no longer needed, since we filtered to include only lines containing 'include' or 'exclude'
  #* ### ****** Remove any empty lines ******
  #* line_lengths = stringr::str_length(incl)
  #* incl = incl[-which(line_lengths==0)]

  ### ****** Remove any duplicate whitespace ******
  incl = stringr::str_replace_all(incl, pattern = "\\s+", replacement = " ")

  ### ****** Trim any leading / trailing whitespace ******
  incl = stringr::str_trim(incl, side='both')

  ### ****** Split each line into components
  ### Since include files lines are something like "include area x1 y1 x2 y2 CLASS"
  ### we will split on spaces and ask for 7 columns in the resulting matrix
  ### If the CLASS field is missing, str_split_fixed will return a "" in the last column.
  ### In that case, overwrite the "" with "?"
  incl = stringr::str_split_fixed(incl, pattern = " ", 7)
  incl[incl==""] = "?"

  ### The columns of this matrix are now something like "include","area","x1","y1","x2","y2","CLASS"
  ### Drop the 2nd column since it's not informative.
  ### The columns will now be "include","x1","y1","x2","y2","CLASS"
  incl = incl[,-2]

  ### ****** Create the mask and class sets ******
  ### mask will be a img_y x img_x boolean matrix, where mask[i,j] = TRUE corresponds to "exclude" pixels
  ### and mask[i,j] = FALSE corresponds to "include" pixels
  mask = matrix(FALSE, nrow = img_y, ncol = img_x)
  classmat = matrix(NA, nrow = img_y, ncol = img_x)
  for(i in 1:nrow(incl)) {
    thisgroup = expand.grid(incl[i,3]:incl[i,5], incl[i,2]:incl[i,4])
    thisgroup = as.matrix(thisgroup)

    if(incl[i,1]=='exclude') mask[thisgroup] = TRUE

    classmat[thisgroup] = incl[i,6]
  }

  out = NULL
  out$masked = mask
  out$class = classmat

  return(out)
}


#' Writing an include file in ascii format
#'
#' Include files are ascii files.
#' Each row (entry) contains an include statement of the form "include area x0 y0 x1 y1 C"
#' where (x0,y0) and (x1,y1) are the x-y coordinates of the top left and bottom right (respectively) pixels in the defined region.
#' C = a class definition.
#'
#' @param  classmap character vector, length = number of pixels in image.  Each element contains a pixel label.
#' @param  inclmap boolean vector, length = number of pixels in image.  Each element defines whether the pixel is included (=TRUE) or masked (=FALSE) in the image.
#' @param  img_x integer, width of image cube in pixels
#' @param  img_y integer, height of image cube in pixels
#' @param  filename string, address of output file
#'
#' @return None
#'
#' Special note:  The vectors are assumed to be ordered such that the first element represents the NW pixel and last element represents the SE pixel
#'
#' @export
write_incl = function(classmap, inclmap, img_x = NULL, img_y = NULL, filename) {

  ### If both classmap & inclmap are vectors, they must be the same length, and the img dimensions must be given
  if(is.vector(classmap) && is.vector(inclmap)) {
    if(length(classmap) != length(inclmap)) stop('write_khoros_includefile:  classmap & inclmap must be the same dimension.')
    if(is.null(img_x) || is.null(img_y)) stop('write_khoros_includefile:  Image dimensions must be given if classmap & inclmap are vectors.')
  }


  ### If both classmap & inclmap are matricies, they must be the same dimension, and those dimensions must match the input ones
  if(is.matrix(classmap) && is.matrix(inclmap)) {
    if(any(dim(classmap) != dim(inclmap))) stop('write_khoros_includefile:  classmap & inclmap must be the same dimension.')

    if(is.null(img_x)) {
      img_x = ncol(classmap)
    } else {
      if(img_x != ncol(classmap)) {
        warning('write_khoros_includefile:  img_x does not match number of columns of classmap.  Using number of columns instead.')
        img_x = ncol(classmap)
      }
    }

    if(is.null(img_y)) {
      img_y = nrow(classmap)
    } else {
      if(img_y != nrow(classmap)) {
        warning('write_khoros_includefile:  img_y does not match number of rows of classmap.  Using number of rows instead.')
        img_y = nrow(classmap)
      }
    }

    ### For consistency, vectorize both classmap & inclmap
    classmap = c(t(classmap))
    inclmap = c(t(inclmap))
  }

  ### Total number of pixels in image
  npixels = img_x * img_y

  ### Determine unique classes given in classmap, and their pixel counts
  unq.class = sort(unique(classmap))
  k = length(unq.class)
  nk = sapply(1:k, FUN = function(z) {sum(classmap == unq.class[z])} )

  ### Open the connection to the text file for writing
  con = file(filename, open = 'wt')

  ### Write my header
  cat(sprintf('# img_x = %d   img_y = %d\n',img_x, img_y), file = con)
  cat(sprintf('# Total pixels = %d   Unmasked = %d   Masked = %d\n', npixels, sum(inclmap == TRUE), sum(inclmap == FALSE)), file = con)
  cat(sprintf('# Class Distribution:\n'), file = con)
  for(i in 1:k) {
    cat(sprintf('#    %s = %d\n', unq.class[i], nk[i]), file = con)
  }

  ### Write the include text
  inclmap_str = rep('exclude area', npixels)
  inclmap_str[inclmap] = 'include area'
  incl_row = 1
  for(i in 1:img_y) {
    for(j in 1:img_x) {
      ### Format is "include area fromx fromy tox toy"
      cat(sprintf('%s\t%d\t%d\t%d\t%d\t%s\n', inclmap_str[incl_row], j, i, j, i, classmap[incl_row]), file = con)
      incl_row = incl_row + 1
    }
  }

  ### Close the connection
  close(con)
}



#' Reading and processing a color table file
#'
#' Color tables specify the RGB codes for the classes defined in either the include file or remap.
#'
#' @param filename string specifying the file path & name of the color table file
#'
#' @return a data frame with columns:
#' \itemize{
#'   \item class:  a character vector defining the class label (see ?read_khoros_includefile for more information on possible class label characters)
#'   \item R:  an integer vector in [0-255] defining the red plane of the class color
#'   \item G:  an integer vector in [0-255] defining the green plane of the class color
#'   \item B:  an integer vector in [0-255] defining the blue plane of the class color
#' }
#'
#' Example:
#' read_ctab('6d-synthetic_30.ctab')
#'
#' @export
read_ctab = function(filename) {
  ctab = readLines(filename)

  ### ****** Remove comments ******
  ### If any element contains a # (comment character), remove everything in that line after it
  comment_start_pos = stringr::str_locate(ctab,"#")[,1]
  idx_to_change = which(!is.na(comment_start_pos))
  ctab[idx_to_change] = stringr::str_sub(ctab[idx_to_change], start = rep(1,length(idx_to_change)), end = comment_start_pos[idx_to_change]-1)

  ### ****** Remove the "color_table" and "end color_table" declarations ******
  ctab = stringr::str_replace(ctab, pattern = "end color_table", replacement = "")
  ctab = stringr::str_replace(ctab, pattern = "color_table", replacement = "")

  ### ****** Remove any empty lines ******
  line_lengths = stringr::str_length(ctab)
  ctab = ctab[-which(line_lengths==0)]

  ### ****** Remove any duplicate whitespace ******
  ctab = stringr::str_replace_all(ctab, pattern = "\\s+", replacement = " ")

  ### ****** Trim any leading / trailing whitespace ******
  ctab = stringr::str_trim(ctab, side='both')

  ### ****** Split each line into components
  ### Since color table lines are something like "A 255 0 0" for "CLASS R G B"
  ### we will split on spaces and ask for 4 columns in the resulting matrix
  ctab = stringr::str_split_fixed(ctab, pattern = " ", 4)

  ### ****** Convert to data frame and label everything ******
  ctab = as.data.frame(ctab, stringsAsFactors = FALSE)
  colnames(ctab) = c('class','R','G','B')
  ctab$R = as.numeric(ctab$R)
  ctab$G = as.numeric(ctab$G)
  ctab$B = as.numeric(ctab$B)

  return(ctab)
}


#' Write a color table to file
#'
#' @description Given an color table, write its contents as a text file, with NeuroScope formatting (i.e., "color_table" and "end color_table" tags will be appended).
#' @param ctab the color table dataframe to write Must have fields  \code{$class}, \code{$R}, \code{$G}, \code{$B}.
#' @param filename the file path name to be written to
#' @export
write_ctab = function(ctab, filename) {
  # if(stringr::str_sub(output_dir, -1, -1) != "/") {
  #   output_dir = paste(output_dir, "/", sep = "")
  # }
  #filepathname = paste(output_dir, filename, sep = "")
  filepathname = filename
  cn = file(filepathname, open = "wt")
  cat("color_table\n", file = cn)
  for(i in 1:nrow(ctab)) {
    cat(sprintf("\t%s\t%d\t%d\t%d\n", ctab[i,1], ctab[i,2], ctab[i,3], ctab[i,4]), file = cn)
  }
  cat("end color_table\n", file = cn)
  close(cn)
}




#' Reading and processing a nunr file
#'
#' NUNR files contain neuron-specific information about the mapping of the data through the SOM.
#'
#' @param filename string specifying the file path & name of the color table file
#' @param  som_x integer, width of SOM lattice in neurons
#' @param  som_y integer, height of SOM lattice in neurons
#' @param  img_x integer, width of image cube in pixels
#' @param  img_y integer, height of image cube in pixels
#'
#' @return a list with components:
#' \itemize{
#'   \item density, a (som_y x som_x) length integer vector whose i-th element contains the number of image pixels (observations) mapped to prototype i.
#'   \item class, a (som_y x som_y) length character vector whose i-th element contains the majority label of the pixels mapped to prototype i.
#'   \item mapping, an integer matrix of dimension (img_y, img_x) whose [i,j] entry is the (linear) index of the prototype which pixel [i,j] was mapped to.
#'   \item som_indexmap, an integer matrix of dimension (som_y, som_x) whose [i,j] entry is the linear index of the neuron at the [i,j] location in the lattice.
#' }
#'
#' Special note:
#' The indexing starts at the SW corner of the lattice, reading along the rows, and finishes at the NE corner of the lattice.
#' Thus, the first element of the density and class vectors corresponds to the prototype attached to the neuron at the SW corner of the lattice.
#' View som_indexmap for inspection of this.
#' The indexing is 1-based.
#'
#' Example:
#' read_nunr('8class-caseI.27x27.k3.r.5000000.nunr')
#'
#' @export
read_nunr = function(filename, som_x, som_y, img_x, img_y) {

  ### ****** Read data line by line ******
  ### Each line represents information about a PE, with the first line referencing
  ### the PE in the SW corner of the lattice.  The information is in the format
  ### "PEID HITCOUNT [CLASS IMGX IMGY]
  nunr = readLines(filename)

  ### The first line contains a header of sorts.  Ignore it
  nunr = nunr[-1]

  ### Verify that the vector is of the expected length = som_x * som_y
  if(length(nunr)!=(som_x*som_y)) {
    stop('The nunr file does not have som_x * som_y lines!  Exiting.')
  }

  ### ****** Begin Processing ******

  ### First remove the PEID from the list, as we don't need it for anything
  ### To do this, locate the position of the first whitespace, and only keep
  ### everything after that
  ws_start_pos = stringr::str_locate(nunr, pattern = "\\s")[,1]
  nunr = stringr::str_sub(nunr, start = ws_start_pos)

  ### Trim up any leading whitespace in each line
  nunr = stringr::str_trim(nunr, side = 'left')

  ### Now the first element in each vector is the PE hitcount.
  ### Extract it, and adjust the strings
  ws_start_pos = stringr::str_locate(nunr, pattern = "\\s")[,1]
  pe.hits = stringr::str_sub(nunr, start = 1, end = ws_start_pos-1)
  nunr = stringr::str_sub(nunr, start = ws_start_pos)
  nunr = stringr::str_trim(nunr, side = 'both')

  ### Make sure the hitcount is numeric, and determine which PEs are live (vs. dead)
  pe.hits = as.numeric(pe.hits)
  live_pe_idx = which(pe.hits > 0)

  ### Split each element of the vector on whitespace
  ### This yields a list (of length = nunr) of character vectors
  ### The length of the vector in nunr[[i]] = 3*number of hits.
  ### Each group of 3 represents "CLASS" "imgX" "imgY"
  nunr = stringr::str_split(nunr, pattern = "\\s+")

  ### Determine the mapping label of each PE
  ### This is the majority class label of the set of points mapped to each PE
  hitclasses = lapply(nunr, function(y) y[seq(1,length(y), by=3)])
  pe.class = rep(NA, som_x*som_y)
  pe.class[live_pe_idx] = unlist(lapply(hitclasses[live_pe_idx], function(y) names(sort(table(y), decreasing = TRUE))[1]))

  ### Build a "hitmap", which is a matrix of [img_y x img_x] pixels which
  ### gives the index of the PE that each pixel is mapped to
  ### The pe index starts from 1 at the SW corner (since the nunr list is ordered that way)
  hitmap = matrix(NA, nrow = img_y, ncol = img_x)

  xy_idx = function(z) {
    x = as.numeric(z[seq(2,length(z), by=3)])
    y = as.numeric(z[seq(3,length(z), by=3)])
    return(cbind(y,x))
  }

  for(i in live_pe_idx) {
    hitmap[xy_idx(nunr[[i]])] = i
  }

  idxmap = matrix(1:(som_y*som_x), nrow=som_y, ncol=som_x, byrow=T)
  #idxmap = reorder_cbl_along_height(idxmap)
  idxmap = idxmap[nrow(idxmap):1,]


  out = NULL
  out$density = pe.hits
  out$class = pe.class
  out$mapping = hitmap
  out$som_indexmap = idxmap
  return(out)
}



#' Read a the min/max mapping stored in a .nna file
#'
#' @param filename string specifying the file path & name of the nna file
#' @param img_z the number of bands in the image that this nna corresponds to
#'
#' @return a list with component:
#'  \itemize{
#'   \item input_min, the min of network inputs, by dimension
#'   \item input_max, the max of network inputs, by dimension
#'   \item output_min, the min of network outputs
#'   \item output_max, the max of network outputs
#' }
#'
#' @export
read_nna = function(filename, img_z) {
  infile = readr::read_lines(file = filename)

  ## Remove any leading or trailing whitespace from lines
  infile = stringr::str_trim(infile, side="both")

  ## Remove any lines with comments
  hash_pos = stringr::str_locate(infile, "#")[,"start"]
  for(i in 1:length(infile)) {
    if(is.na(hash_pos[i])) next
    if(hash_pos[i]==1) infile[i] = ""
    else infile[i] = stringr::str_sub(infile[i], start = 1, end = hash_pos[i]-1)
  }

  ## Replace any & characters with nothing
  infile = stringr::str_replace_all(infile, pattern = "&", replacement = "")

  ## Replace any duplicate repeatitive whitespace with single whitespace
  infile = stringr::str_replace_all(infile, pattern = "\\s+", replacement = " ")

  ## Remove any leading or trailing whitespace from lines
  infile = stringr::str_trim(infile, side="both")

  ## Remove any empty lines
  infile = infile[stringr::str_length(infile) > 0]

  ## Split on whitespace
  infile = unlist(stringr::str_split(infile, pattern = "\\s", simplify = F))

  ## Use length of line and network input dimension to determine network output dimension
  len = length(infile)
  output_dim = len/2 - img_z

  ## Read the 1st set of <input_dim> bounds as lower bounds
  lb = infile[1:img_z]
  ub = infile[(img_z+output_dim+1):(len - output_dim)]

  ## Get network range
  net_lb = infile[img_z+1]
  net_ub = infile[len-output_dim+1]

  out = NULL
  out$input_min = as.numeric(lb)
  out$input_max = as.numeric(ub)
  out$output_min = as.numeric(net_lb)
  out$output_max = as.numeric(net_ub)
  return(out)
}


#' Write a nna (minmax table)
#'
#' @param filename string specifying the file path & name of the nna file
#' @param input_min vector of min range of network inputs (by dim)
#' @param input_max vector of max range of network inputs (by dim)
#' @param output_min vector of min range of network outputs
#' @param output_max vector of max range of network outputs
#' @param group_size size of groups for each line of the nna file
#' @export
write_nna = function(filename, input_min, input_max, output_min, output_max, group_size = 5) {

  ## Ensure all the dimensions line up
  if(length(input_min) != length(input_max)) stop("length(input_min) != length(input_max)")
  if(length(output_min) != length(output_max)) stop("length(output_min) != length(output_max)")

  con = file(filename, open="wt")

  for(i in 1:length(input_min)) {
    cat(sprintf("%g",input_min[i]), file = con)
    if(i %% group_size == 0 && i < length(input_min)) {
      cat(sprintf("\n&"), file=con)
    } else {
      cat(sprintf(" "), file=con)
    }
  }

  cat(sprintf("\n&"), file=con)
  for(i in 1:length(output_min)) {
    cat(sprintf("%g",output_min[i]), file=con)
    if(i %% group_size == 0 && i < length(output_min)) {
      cat(sprintf("\n&"), file=con)
    } else {
      cat(sprintf(" "), file=con)
    }
  }
  cat(sprintf("\n"), file=con)

  for(i in 1:length(input_max)) {
    cat(sprintf("%g",input_max[i]), file=con)
    if(i %% group_size == 0 && i < length(input_max)) {
      cat(sprintf("\n&"), file=con)
    } else {
      cat(sprintf(" "), file=con)
    }
  }

  cat(sprintf("\n&"), file=con)
  for(i in 1:length(output_max)) {
    cat(sprintf("%g",output_max[i]), file=con)
    if(i %% group_size == 0 && i < length(output_max)) {
      cat(sprintf("\n&"), file=con)
    } else {
      cat(sprintf(" "), file=con)
    }
  }

  close(con)
}


#' Load all SOM-related NeuroScope products
#'
#' @param data_file filename of the data file
#' @param include_file filename of the include file
#' @param nna_file filename of the nna file
#' @param wgtcub_file filename of the wgtcub file
#' @param nunr_file filename of the nunr file
#' @param ctab_file filename of the ctab file
#' @param cadj_file filename of the cadj file
#'
#' @return a list with components
#' \itemize{
#' \item X the data cube, converted to a data matrix (bands in columns)
#'
#' }
#'
#' @export
load_SOM_products = function(data_file = NULL, include_file = NULL, nna_file = NULL, wgtcub_file = NULL, nunr_file = NULL, ctab_file = NULL, cadj_file = NULL) {

  cat("Loading NeuroScope products ...\n")
  out = NULL

  ## Load data file
  if(is.null(data_file)) {
    cat("++ data cube ... skipped (given as null)\n")
  } else if(!file.exists(data_file)) {
    cat("++ data cube ... skipped (data_file does not exist)\n")
  } else {
    out$X = NeuroScopeIO::read_khoros_viff(data_file, verbose = F)
    out$img_x = ncol(out$X); out$img_y = nrow(out$X); out$img_z = dim(out$X)[3]
    out$X = NeuroScopeIO::convert_datcub_to_datmat(out$X)
    cat("++ data cube ... done\n")
  }


  ## Load Include File
  if(is.null(include_file)) {
    cat("++ include file ... skipped (given as null)\n")
  } else if(!file.exists(include_file)) {
    cat("++ include file ... skipped (include_file does not exist)\n")
  } else {
    if(is.null(out$X)) {
      cat("++ include file ... skipped (must supply data_file to load .incl info)\n")
    } else {
      out$incl = NeuroScopeIO::read_incl(include_file, img_x = out$img_x, img_y = out$img_y)
      cat("++ include file ... done\n")
    }
  }


  ## Load nna information
  if(is.null(nna_file)) {
    cat("++ nna file ... skipped (given as null)\n")
  } else if(!file.exists(nna_file)) {
    cat("++ nna file ... skipped (nna_file does not exist)\n")
  } else {
    if(is.null(out$X)) {
      cat("++ nna file ... skipped (must supply data_file to load .nna info)\n")
    } else {
      out$nna = NeuroScopeIO::read_nna(nna_file, img_z = out$img_z)
      cat("++ nna file ... done\n")
    }
  }


  ## Load the weight cube file
  if(is.null(wgtcub_file)) {
    cat("++ weight cube ... skipped (given as null)\n")
  } else if(!file.exists(wgtcub_file)) {
    cat("++ weight cube ... skipped (nna_file does not exist)\n")
  } else {
    out$W = NeuroScopeIO::read_khoros_viff(wgtcub_file, verbose = F)
    out$som_x = ncol(out$W); out$som_y = nrow(out$W)
    out$W = NeuroScopeIO::flip_datcub(out$W, flipX = F, flipY = T, flipZ = F)
    out$W = NeuroScopeIO::convert_datcub_to_datmat(out$W)
    cat("++ weight cube ... done\n")
    if(is.null(out$nna)) {
      cat("++ weight cube scaling ... skipped (no nna info found)\n")
    } else {
      for(j in 1:ncol(out$W)) {
        out$W[,j] = (out$W[,j] - out$nna$output_min[j]) / (out$nna$output_max[j] - out$nna$output_min[j]) * (out$nna$input_max[j] - out$nna$input_min[j]) + out$nna$input_min[j]
      }
      cat("++ weight cube scaling ... done\n")
    }
  }


  ## Load nunr information
  if(is.null(nunr_file)) {
    cat("++ nunr file ... skipped (given as null)\n")
  } else if(!file.exists(nunr_file)) {
    cat("++ nunr file ... skipped (nunr_file does not exist)\n")
  } else {
    if(is.null(out$X) || is.null(out$W)) {
      cat("++ nunr file ... skipped (must supply data_file and wgtcub_file to load .nunr info)\n")
    } else {
      out$nunr = NeuroScopeIO::read_nunr(nunr_file, som_x = out$som_x, som_y = out$som_y, img_x = out$img_x, img_y = out$img_y)
      cat("++ nunr file ... done\n")
    }
  }


  ## Lattice coords
  if(is.null(out$W)) {
    cat("++ lattice coords ... skipped (must supply wgtcub_file to compute lattice coords)\n")
  } else {
    out$lattice_coords = as.matrix(expand.grid(1:out$som_x, 1:out$som_y))
    colnames(out$lattice_coords) = c('x','y')
    cat("++ lattice coords ... done\n")
  }


  ## ctab
  if(is.null(ctab_file)) {
    cat("++ ctab file ... skipped (given as null)\n")
  } else if(!file.exists(ctab_file)) {
    cat("++ ctab file ... skipped (ctab_file does not exist)\n")
  } else {
    out$ctab = NeuroScopeIO::read_ctab(ctab_file)
    cat("++ ctab file ... done\n")
  }


  ## CADJ file
  if(is.null(cadj_file)) {
    cat("++ cadj file ... skipped (given as null)\n")
  } else if(!file.exists(cadj_file)) {
    cat("++ cadj file ... skipped (cadj_file does not exist)\n")
  } else {
    out$CADJ = NeuroScopeIO::read_khoros_viff(cadj_file, verbose = F)
    out$CADJ = out$CADJ[,,1]
    out$CONN = out$CADJ + t(out$CADJ)
    cat("++ cadj file ... done\n")
  }


  return(out)
}
