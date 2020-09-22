#' Read a khoros viff header
#'
#' Information about the image cube is stored in the proprietary Khoros header format.  This function reads that header
#' and extracts the relevant characteristics needed to read the binary image cube.  Note that this can only parse viff
#' files (not kdf format; if you have kdf you must convert in Khoros with \code{kformats})
#'
#' @param filename string specifying the file path & name of the khoros viff cube
#'
#' @return list with components:
#' \itemize{
#'    \item imgEndian = string, either 'little' or 'big'
#'    \item imgW = integer, width of image in pixels
#'    \item imgH = integer, height of image in pixels
#'    \item imgB = integer, number of image bands
#'    \item imgT = string, image data type ('double', 'integer', etc) \cr
#'          Note that this data type is for R's \code{readBin} function.
#'    \item imgTsize = integer, size of data type in bytes
#'    \item imgTsigned = boolean, TRUE for signed data
#' }
#'
#' Example:
#' read_khoros_header('8class-caseI-n_m0v5.viff')
#'
#' @export
read_khoros_header = function(filename) {

  # initialize the vars
  viff = TRUE;
  imgE = -1;
  imgW = -1;
  imgH = -1;
  imgB = -1;
  imgT = -1;

  ### Open the file for reading
  fid = file(filename, "rb")

  ### Read header
  FileId   = readBin(con = fid, what = 'int', n = 1, size = 1, signed = F, endian = 'little')
  FileType = readBin(con = fid, what = 'int', n = 1, size = 1, signed = F, endian = 'little')
  if(FileId == 171 && FileType == 01) {
    viff = TRUE;
  } else {
    viff = TRUE;
    stop('readViffHead: file is not viff (xv) formatted');
    close(fid)
  }

  ### Skip 2 elements
  readBin(con = fid, what = 'raw', n = 2, size = 1)

  ### Set the endianness
  MachineDep = readBin(con = fid, what = 'int', n = 1, size = 1, signed = F, endian = 'little')
  if(MachineDep == 8) {
    imgE = 'little'
  } else if(MachineDep == 2) {
    imgE = 'big'
  } else {
    viff = FALSE
    stop('readViffHead: Cannot find endianness')
    close(fid)
  }

  ### Skip 515 elements
  readBin(con = fid, what = 'raw', n = 515, size = 1)

  ### Set the image width, height
  imgW = readBin(con = fid, what = 'integer', n = 1, size = 4, endian = imgE)
  imgH = readBin(con = fid, what = 'integer', n = 1, size = 4, endian = imgE)

  ### Skip 32 elements
  readBin(con = fid, what = 'raw', n = 32, size = 1)

  ### Set the number of bands
  imgB = readBin(con = fid, what = 'integer', n = 1, size = 4, endian = imgE)

  ### Set the data storage type
  DataStorageType = readBin(con = fid, what = 'integer', n = 1, size = 4, endian = imgE)
  if(DataStorageType == 0) {
    imgT = 'raw'; imgTsize = 1; imgTsigned = FALSE;
  } else if(DataStorageType == 1) {
    imgT = 'int'; imgTsize = 1; imgTsigned = FALSE;
  } else if(DataStorageType == 2) {
    imgT = 'int'; imgTsize = 2; imgTsigned = TRUE;
  } else if(DataStorageType == 4) {
    imgT = 'integer'; imgTsize = 4; imgTsigned = TRUE;
  } else if(DataStorageType == 5) {
    imgT = 'numeric'; imgTsize = 4; imgTsigned = TRUE;
  } else if(DataStorageType == 6) {
    stop('readViffHead: Cannot import complex type')
  } else if(DataStorageType == 9) {
    imgT = 'double'; imgTsize = 8; imgTsigned = TRUE;
  } else if(DataStorageType == 10) {
    stop('readViffHead: Cannot import complex type')
  } else {
    viff = FALSE
    stop('readViffHead: Cannot find data type')
    close(fid)
  }

  ### Skip to end
  readBin(con = fid, what = 'raw', n = 456, size = 1)

  ### Close file
  close(fid)

  ### Return
  out = NULL
  out$imgEndian = imgE
  out$imgW = imgW
  out$imgH = imgH
  out$imgB = imgB
  out$imgT = imgT
  out$imgTsize = imgTsize
  out$imgTsigned = imgTsigned

  return(out)
}



#' Writing a Khoros viff header
#'
#' Khoros data cubes contain a 1024 byte proprietary header.
#'
#' @param  fid connection to binary file for writing
#' @param  imgE string, the endian-ness to write in.  Deprecated -- all files are written in little endian
#' @param  imgW integer, width of image cube in pixels
#' @param  imgH integer, height of image cube in pixels
#' @param  imgB integer, number of image bands
#' @param  imgT string, type of data to write.  Can be 'double','numeric' (float),'integer' (16-bit signed integer)
#' @param  comment string, any comments about the data to include in the header
#'
#' @return None
#'
#' Special note:  The data matrix is assumed to be ordered such that the first row represents the NW pixel and last row represents the SE pixel
#'
#' @export
write_khoros_header = function(fid,imgE,imgW,imgH,imgB,imgT = 'double',comment = ' ') {
  # % AUTHOR:           Patrick O'Driscoll - po2@rice.edu
  # % LAST UPDATED:     2017/05/24
  # %
  # % FUNCTION:
  # %   Write the kohros viff file header. This header is 1024 bytes, and is
  # %   is used to natively interface with khoros, or visiquest.
  # %
  # % Syntax:
  # %   viffHead(fid,imgW,imgH,imgB,imgT)
  # %
  # % INPUTS:
  # %   fid : file id produced from fopen()
  # %   imgE: image data endianness
  # %       'b': big-endian byte ordering
  # %       'l': little-endian byte ordering
  # %   imgW: image width in pixels
  # %   imgH: image height in pixels
  # %   imgB: number of image bands
  # %   imgT: image data khoros type
  # %       'bit'   : bit
  # %       'byte'  : byte
  # %       'ubyte' : unsigned byte
  # %       'short' : short
  # %       'ushort': unsigned short
  # %       'int'   : int
  # %       'uint'  : unsigned int
  # %       'long'  : long
  # %       'ulong' : unsigned long
  # %       'float' : float
  # %       'double': double
  # %       'comp'  : complex
  # %     'dcomp' : double complex
  # % comment: Comment string. Must be less than 512 char
  # %
  # % OUTPUS:
  # %   Outputs are directly written to the fid file.
  # %
  # % NOTES:
  # %   We mimic the C structure found at:
  # %   http://www.fileformat.info/format/viff/egff.htm
  # % typedef struct _ViffHeader
  # % {
  # % CHAR  FileId;            /* Khoros file ID value (always ABh)*/
  # % CHAR  FileType;          /* VIFF file ID value (always 01h) */
  # % CHAR  Release;           /* Release number (1) */
  # % CHAR  Version;           /* Version number (0) */
  # % CHAR  MachineDep;        /* Machine dependencies indicator */
  # % CHAR  Padding[3];        /* Structure alignment padding (always 00h)*/
  # % CHAR  Comment[512];      /* Image comment text */
  # % DWORD NumberOfRows;      /* Length of image rows in pixels */
  # % DWORD NumberOfColumns;   /* Length of image columns in pixels */
  # % DWORD LengthOfSubrow;    /* Size of any sub-rows in the image */
  # % LONG  StartX;            /* Left-most display starting position */
  # % LONG  StartY;            /* Upper-most display starting position */
  # % FLOAT XPixelSize;        /* Width of pixels in meters */
  # % FLOAT YPixelSize;        /* Height of pixels in meters */
  # % DWORD LocationType;      /* Type of pixel addressing used */
  # % DWORD LocationDim;       /* Number of location dimensions */
  # % DWORD NumberOfImages;    /* Number of images in the file */
  # % DWORD NumberOfBands;     /* Number of bands (color channels) */
  # % DWORD DataStorageType;   /* Pixel data type */
  # % DWORD DataEncodingScheme;/* Type of data compression used */
  # % DWORD MapScheme;         /* How map is to be interpreted */
  # % DWORD MapStorageType;    /* Map element data type */
  # % DWORD MapRowSize;        /* Length of map rows in pixels */
  # % DWORD MapColumnSize;     /* Length of map columns in pixels */
  # % DWORD MapSubrowSize;     /* Size of any subrows in the map */
  # % DWORD MapEnable;         /* Map is optional or required */
  # % DWORD MapsPerCycle;      /* Number of different   maps present */
  # % DWORD ColorSpaceModel;   /* Color model used to represent image */
  # % DWORD ISpare1;           /* User-defined field */
  # % DWORD ISpare2;           /* User-defined field */
  # % FLOAT FSpare1;           /* User-defined field */
  # % FLOAT FSpare2;           /* User-defined field */
  # % CHAR  Reserve[404];      /* Padding */
  # % } VIFFHEADER;

  imgW = as.integer(imgW)
  imgH = as.integer(imgH)
  imgB = as.integer(imgB)

  # check comment size
  if(stringr::str_length(comment) > 512) {
    stop('Comment is too long.  Must be <= 512 characters.')
  } else {
    comment = stringr::str_pad(comment, width=512, side = 'right')
    #commentPadding = 512-stringr::str_length(comment);
  }



  # print header                                           #Val  #B  #tot reason       comment
  writeBin(171L, con = fid, size = 1, endian = 'little')    #ab   1   1    FileId
  writeBin(001L, con = fid, size = 1, endian = 'little')    #01   1   2    FileType
  writeBin(001L, con = fid, size = 1, endian = 'little')    #01   1   3    Release
  writeBin(003L, con = fid, size = 1, endian = 'little')    #03   1   4    Version

  # Machine endianness
  if(imgE == 'big') {
    writeBin(002L, con = fid, size = 1, endian = 'little')
  } else if(imgE == 'little') {
    writeBin(008L, con = fid, size = 1, endian = 'little')
  } else {
    stop('viffHead: unknown imgE argument')
  }

  for(ii in 1:3) {
    writeBin(000L, con = fid, size = 1, endian = 'little') #00   3   8    Padding[3]
  }


  #                                 %00   512 520  Comment[512]
  # print('hello!')
  # for(ii in 1:512) {
  #     writeBin(stringr::str_sub(comment,ii,ii), con = fid, size = 1, endian = 'little')
  # }
  # for(ii in 1:commentPadding) {
  #     writeBin(000L, con = fid, size = 1, endian = 'little')
  # }

  # for(ii in 1:512) {
  #     writeBin(0L, con = fid, size = 1, endian = 'little')
  # }

  writeBin(stringr::str_sub(comment,1,511), con = fid, size = 1, endian = 'little')

  writeBin(imgW, con = fid, size = 4, endian = 'little')      #??   4   524  NumberOfRows
  writeBin(imgH, con = fid, size = 4, endian = 'little')      #??   4   528  NumberOfColumns
  writeBin(0000L, con = fid, size = 4, endian = 'little')      #00   4   532  LengthOfSubrow
  for(ii in 1:4) {
    writeBin(255L, con = fid, size = 1, endian = 'little')     #ff   4   536  StartX       % ff ff ff ff
  }
  for(ii in 1:4) {
    writeBin(255L, con = fid, size = 1, endian = 'little')     #ff   4   540  StartY       % ff ff ff ff
  }

  writeBin(000L, con = fid, size = 2, endian = 'little')       #0000 4   544  XPixelSize   % 00 00 80 3f
  writeBin(128L, con = fid, size = 1, endian = 'little')       #80
  writeBin(063L, con = fid, size = 1, endian = 'little')       #3f
  writeBin(000L, con = fid, size = 2, endian = 'little')       #0000 4   548  YPixelSize   % 00 00 80 3f
  writeBin(128L, con = fid, size = 1, endian = 'little')       #80
  writeBin(063L, con = fid, size = 1, endian = 'little')       #3f
  writeBin(001L, con = fid, size = 4, endian = 'little')       #01   4   552  LocationType % implicit
  writeBin(000L, con = fid, size = 4, endian = 'little')       #00   4   556  LocationDim  % implicit
  writeBin(001L, con = fid, size = 4, endian = 'little')       #01   4   560  NumberOfImages
  writeBin(imgB, con = fid, size = 4, endian = 'little')      #??   4   564  NumberOfBands


  switch(imgT,                #??   4   568  DataStorageType
         'bit' = writeBin(00L, con = fid, size = 4, endian = 'little'),      # 00 -> bit
         'byte' = writeBin(01L, con = fid, size = 4, endian = 'little'),     # 01 -> byte
         'ubyte' = writeBin(01L, con = fid, size = 4, endian = 'little'),    # 01 -> unsigned byte
         'short' = writeBin(02L, con = fid, size = 4, endian = 'little'),    # 02 -> short
         'ushort' = writeBin(02L, con = fid, size = 4, endian = 'little'),   # 02 -> unsigned short
         'int' = writeBin(04L, con = fid, size = 4, endian = 'little'),      # 04 -> int
         'uint' = writeBin(04L, con = fid, size = 4, endian = 'little'),     # 04 -> unsigned int
         'long' = writeBin(04L, con = fid, size = 4, endian = 'little'),     # 04 -> long
         'ulong' = writeBin(04L, con = fid, size = 4, endian = 'little'),    # 04 -> unsigned long
         'float' = writeBin(05L, con = fid, size = 4, endian = 'little'),    # 05 -> float
         'double' = writeBin(09L, con = fid, size = 4, endian = 'little'),   # 09 -> double
         'comp' = writeBin(06L, con = fid, size = 4, endian = 'little'),     # 06 -> complex
         'dcomp' = writeBin(10L, con = fid, size = 4, endian = 'little'),    # 0a -> double complex
         stop('writeViffHead: unknown imgT argument')
  )

  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   572  DataEncodingScheme % none
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   576  MapScheme          % each band is img
  writeBin(001L, con = fid, size = 4, endian = 'little')    # 01   4   580  MapStorageType
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   584  MapRowSize
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   588  MapColumnSize
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   592  MapSubrowSize
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   596  MapEnable
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   600  MapsPerCycle
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   604  ColorSpaceModel    % none
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   608  ISpare1
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   612  ISpare2
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   616  FSpare1
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   620  FSpare2
  for(ii in 1:404) {
    writeBin(000L, con = fid, size = 1, endian = 'little')   # 00   404 1024 Padding
  }


}



#' Import a Khoros cube in viff format
#'
#' The header will be read to obtain the pertinent information required for reading the binary image cube:  image width & height, number of image bands, etc.  The actual reading is done with \code{readBin}.
#'
#' @param filename string specifying the file path & name of the khoros viff cube
#' @param verbose whether to print the (1,1) and (img_x,img_y) pixel values to the console
#'
#' @return a 3-dimensional array whose [i,j,k] entry represents the value in row i, column j and band k of the image cube.
#'
#'
#' @export
read_khoros_viff = function(filename, verbose = T) {
  ### filename is string (path + filename) to viff file
  ### img_width & img_height are number of pixels in image
  ### n_dim is integer specifying number of dimensions / bands of data cube
  ### data_type is one of "FLOAT","SHORT","DOUBLE" which tells what precision the data are stored in
  ### header_bytes gives number of bytes in header.  If NULL, we will determine this programatically
  ### endian = 'little' or 'big' indicates endian-ness of file
  ### output_type is one of "cube" or "matrix", indicating whether to return the data as a 3d array or a 2d data matrix

  ### ****** Attempt to read the Khoros header file to detrmine image characteristics ******
  ### If error, then we can't read the file
  hdr = NULL
  hdr = read_khoros_header(filename)
  if(is.null(hdr)) stop('Cannot read Khoros header file.  Exiting.')

  ### Extract the image characteristics
  endian = hdr$imgEndian
  img_width = hdr$imgW
  img_height = hdr$imgH
  n_dim = hdr$imgB
  datatype = hdr$imgT
  num_bytes_per_datum = hdr$imgTsize
  signed = hdr$imgTsigned

  num_pixels = img_width * img_height
  num_data_bytes = num_pixels * n_dim * num_bytes_per_datum

  ### ****** Initialize connection to file and read past header ******
  to.read = file(filename, "rb")
  readBin(con = to.read, what = 'raw', n = 1024, size = 1)

  ### ****** Initialize cube array and fill it up ******
  outcube = array(NA, dim = c(img_height, img_width, n_dim))
  for(i in 1:n_dim) {
    dimvec = readBin(con = to.read, what = datatype, n = img_width*img_height, size = num_bytes_per_datum, signed = signed, endian = endian)
    outcube[,,i] = matrix(dimvec, nrow = img_height, ncol = img_width, byrow = T)
  }
  close(to.read)

  ### ****** Write the NW,NE,SW,SE corners of the image cube to the console window
  if(verbose) {
    cat(sprintf('NW corner of cube: \n'))
    cat(sprintf('%6f',outcube[1,1,]))
    cat(sprintf('\nNE corner of cube:\n'))
    cat(sprintf('%6f',outcube[1,img_width,]))
    cat(sprintf('\nSW corner of cube:\n'))
    cat(sprintf('%6f',outcube[img_height,1,]))
    cat(sprintf('\nSE corner of cube:\n'))
    cat(sprintf('%6f',outcube[img_height,img_width,]))
  }


  return(outcube)
}



#' Writing a Khoros data cube in .viff format
#'
#' Khoros data cubes contain a 1024 byte header, followed by the data cube in band sequential layout
#'
#' @param  X either a data matrix (nrows = img_x * img_y, ncols = img_z), or a data cube (nrows = img_y, ncols = img_x, nbands = img_z)
#' @param  img_x integer, width of image cube in pixels
#' @param  img_y integer, height of image cube in pixels
#' @param  filename string, address of output file
#' @param  header_comment string, any comments about the data to include in the header
#'
#' @details If X is given as a data matrix then img_x and img_y must also be provided. Otherwise, if X is a data cube,
#' these are taken from cube dimensions.
#'
#' If X is a data matrix it is assumed to be ordered such that the first row represents the (1,1) pixel and last row represents the (img_x,img_y) pixel
#'
#' @return None
#'
#' Special note:
#'
#' @export
write_khoros_viff = function(X, img_x = NULL, img_y = NULL, filename, header_comment = ' ') {

  ### If X is a vector (unlikely, unless data has dimension 1) turn it into a matrix
  if(is.vector(X)) X = as.matrix(X)

  ### If X is a matrix, the dimensions MUST be given, and they must match
  if(is.matrix(X)) {
    if(is.null(img_x) || is.null(img_y)) {stop('write_khoros_viff:  Image dimensions must be given if input is a matrix.')}
    if(img_x * img_y != nrow(X)) {stop('write_khoros_viff:  Input dimensions must match the number of rows of the input matrix.')}
    img_depth = ncol(X)
  }

  ### If X is a cube (3d array), we can extract the dimensions directly if needed
  x_is_cube = !is.na(dim(X)[3])
  if(x_is_cube) {
    img_depth = dim(X)[3]
    if(is.null(img_x) || is.null(img_y)) {
      img_y = dim(X)[1]
      img_x = dim(X)[2]
    }
  }

  ### Open the binary connection for writing
  con = file(filename, open = 'wb')

  ### Write the header
  write_khoros_header(con, imgE = 'little', imgW = img_x, imgH = img_y, imgB = img_depth, imgT = 'double', comment = header_comment)

  ### Write the data
  for(i in 1:img_depth) {

    if(is.matrix(X)) {
      writeBin(X[,i], con = con, size = 8, endian = 'little')
    }

    if(x_is_cube) {
      writeBin(c(t(X[,,i])), con = con, size = 8, endian = 'little')
    }
  }

  ### Close the connection
  close(con)
}



#' Write a Khoros KRGB header
#'
#' @description If a connection to a binary writeable file is open, this function will write the header required
#' for Khoros to recognize the cube as KRGB formatted.
#' @param fid a connection to a binary file, see \code{?file} for more info.
#' @param imgE the file endian-ness, can be 'little' or 'big'
#' @param imgW the image width, in pixels
#' @param imgH the image height, in pixels
#' @param comment optional, any comment string the user wishes to add to the header information. Default = ' ' adds nothing. This must be < 512 characters.
#' @return nothing
#' @details This is translated from Khoros I/O manipulation originally written in Matlab by Patrick O'Driscoll.
#' @export
write_khoros_header_KRGB = function(fid,imgE,imgW,imgH,comment = ' ') {
  # % AUTHOR:           Patrick O'Driscoll - po2@rice.edu
  # % LAST UPDATED:     2017/05/24
  # %
  # % FUNCTION:
  # %   Write the kohros viff file header. This header is 1024 bytes, and is
  # %   is used to natively interface with khoros, or visiquest.
  # %
  # % Syntax:
  # %   viffHead(fid,imgW,imgH,imgB,imgT)
  # %
  # % INPUTS:
  # %   fid : file id produced from fopen()
  # %   imgE: image data endianness
  # %       'b': big-endian byte ordering
  # %       'l': little-endian byte ordering
  # %   imgW: image width in pixels
  # %   imgH: image height in pixels
  # %   imgB: number of image bands
  # %   imgT: image data khoros type
  # %       'bit'   : bit
  # %       'byte'  : byte
  # %       'ubyte' : unsigned byte
  # %       'short' : short
  # %       'ushort': unsigned short
  # %       'int'   : int
  # %       'uint'  : unsigned int
  # %       'long'  : long
  # %       'ulong' : unsigned long
  # %       'float' : float
  # %       'double': double
  # %       'comp'  : complex
  # %     'dcomp' : double complex
  # % comment: Comment string. Must be less than 512 char
  # %
  # % OUTPUS:
  # %   Outputs are directly written to the fid file.
  # %
  # % NOTES:
  # %   We mimic the C structure found at:
  # %   http://www.fileformat.info/format/viff/egff.htm
  # % typedef struct _ViffHeader
  # % {
  # % CHAR  FileId;            /* Khoros file ID value (always ABh)*/
  # % CHAR  FileType;          /* VIFF file ID value (always 01h) */
  # % CHAR  Release;           /* Release number (1) */
  # % CHAR  Version;           /* Version number (0) */
  # % CHAR  MachineDep;        /* Machine dependencies indicator */
  # % CHAR  Padding[3];        /* Structure alignment padding (always 00h)*/
  # % CHAR  Comment[512];      /* Image comment text */
  # % DWORD NumberOfRows;      /* Length of image rows in pixels */
  # % DWORD NumberOfColumns;   /* Length of image columns in pixels */
  # % DWORD LengthOfSubrow;    /* Size of any sub-rows in the image */
  # % LONG  StartX;            /* Left-most display starting position */
  # % LONG  StartY;            /* Upper-most display starting position */
  # % FLOAT XPixelSize;        /* Width of pixels in meters */
  # % FLOAT YPixelSize;        /* Height of pixels in meters */
  # % DWORD LocationType;      /* Type of pixel addressing used */
  # % DWORD LocationDim;       /* Number of location dimensions */
  # % DWORD NumberOfImages;    /* Number of images in the file */
  # % DWORD NumberOfBands;     /* Number of bands (color channels) */
  # % DWORD DataStorageType;   /* Pixel data type */
  # % DWORD DataEncodingScheme;/* Type of data compression used */
  # % DWORD MapScheme;         /* How map is to be interpreted */
  # % DWORD MapStorageType;    /* Map element data type */
  # % DWORD MapRowSize;        /* Length of map rows in pixels */
  # % DWORD MapColumnSize;     /* Length of map columns in pixels */
  # % DWORD MapSubrowSize;     /* Size of any subrows in the map */
  # % DWORD MapEnable;         /* Map is optional or required */
  # % DWORD MapsPerCycle;      /* Number of different   maps present */
  # % DWORD ColorSpaceModel;   /* Color model used to represent image */
  # % DWORD ISpare1;           /* User-defined field */
  # % DWORD ISpare2;           /* User-defined field */
  # % FLOAT FSpare1;           /* User-defined field */
  # % FLOAT FSpare2;           /* User-defined field */
  # % CHAR  Reserve[404];      /* Padding */
  # % } VIFFHEADER;

  imgW = as.integer(imgW)
  imgH = as.integer(imgH)
  #imgB = as.integer(imgB)

  # check comment size
  if(stringr::str_length(comment) > 512) {
    stop('Comment is too long.  Must be <= 512 characters.')
  } else {
    comment = stringr::str_pad(comment, width=512, side = 'right')
  }



  # print header                                           #Val  #B  #tot reason       comment
  writeBin(171L, con = fid, size = 1, endian = 'little')    #ab   1   1    FileId
  writeBin(001L, con = fid, size = 1, endian = 'little')    #01   1   2    FileType
  writeBin(001L, con = fid, size = 1, endian = 'little')    #01   1   3    Release
  writeBin(003L, con = fid, size = 1, endian = 'little')    #03   1   4    Version

  # Machine endianness
  if(imgE == 'big') {
    writeBin(002L, con = fid, size = 1, endian = 'little')
  } else if(imgE == 'little') {
    writeBin(008L, con = fid, size = 1, endian = 'little')
  } else {
    stop('viffHead: unknown imgE argument')
  }

  for(ii in 1:3) {
    writeBin(000L, con = fid, size = 1, endian = 'little') #00   3   8    Padding[3]
  }


  writeBin(stringr::str_sub(comment,1,511), con = fid, size = 1, endian = 'little')

  writeBin(imgW, con = fid, size = 4, endian = 'little')      #??   4   524  NumberOfRows
  writeBin(imgH, con = fid, size = 4, endian = 'little')      #??   4   528  NumberOfColumns
  writeBin(0000L, con = fid, size = 4, endian = 'little')      #00   4   532  LengthOfSubrow
  for(ii in 1:4) {
    writeBin(255L, con = fid, size = 1, endian = 'little')     #ff   4   536  StartX       % ff ff ff ff
  }
  for(ii in 1:4) {
    writeBin(255L, con = fid, size = 1, endian = 'little')     #ff   4   540  StartY       % ff ff ff ff
  }

  writeBin(000L, con = fid, size = 2, endian = 'little')       #0000 4   544  XPixelSize   % 00 00 80 3f
  writeBin(128L, con = fid, size = 1, endian = 'little')       #80
  writeBin(063L, con = fid, size = 1, endian = 'little')       #3f
  writeBin(000L, con = fid, size = 2, endian = 'little')       #0000 4   548  YPixelSize   % 00 00 80 3f
  writeBin(128L, con = fid, size = 1, endian = 'little')       #80
  writeBin(063L, con = fid, size = 1, endian = 'little')       #3f
  writeBin(001L, con = fid, size = 4, endian = 'little')       #01   4   552  LocationType % implicit
  writeBin(000L, con = fid, size = 4, endian = 'little')       #00   4   556  LocationDim  % implicit
  writeBin(001L, con = fid, size = 4, endian = 'little')       #01   4   560  NumberOfImages
  imgB = as.integer(3) ## always 3 bands for KRGB viffs
  writeBin(imgB, con = fid, size = 4, endian = 'little')      #??   4   564  NumberOfBands

  imgT = 'ubyte' ## KRGB viff cubes must be unsigned byte
  switch(imgT,                #??   4   568  DataStorageType
         'bit' = writeBin(00L, con = fid, size = 4, endian = 'little'),      # 00 -> bit
         'byte' = writeBin(01L, con = fid, size = 4, endian = 'little'),     # 01 -> byte
         'ubyte' = writeBin(01L, con = fid, size = 4, endian = 'little'),    # 01 -> unsigned byte
         'short' = writeBin(02L, con = fid, size = 4, endian = 'little'),    # 02 -> short
         'ushort' = writeBin(02L, con = fid, size = 4, endian = 'little'),   # 02 -> unsigned short
         'int' = writeBin(04L, con = fid, size = 4, endian = 'little'),      # 04 -> int
         'uint' = writeBin(04L, con = fid, size = 4, endian = 'little'),     # 04 -> unsigned int
         'long' = writeBin(04L, con = fid, size = 4, endian = 'little'),     # 04 -> long
         'ulong' = writeBin(04L, con = fid, size = 4, endian = 'little'),    # 04 -> unsigned long
         'float' = writeBin(05L, con = fid, size = 4, endian = 'little'),    # 05 -> float
         'double' = writeBin(09L, con = fid, size = 4, endian = 'little'),   # 09 -> double
         'comp' = writeBin(06L, con = fid, size = 4, endian = 'little'),     # 06 -> complex
         'dcomp' = writeBin(10L, con = fid, size = 4, endian = 'little'),    # 0a -> double complex
         stop('writeViffHead: unknown imgT argument')
  )

  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   572  DataEncodingScheme % none
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   576  MapScheme          % each band is img
  writeBin(001L, con = fid, size = 4, endian = 'little')    # 01   4   580  MapStorageType
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   584  MapRowSize
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   588  MapColumnSize
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   592  MapSubrowSize
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   596  MapEnable
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   600  MapsPerCycle
  writeBin(001L, con = fid, size = 4, endian = 'little')    # 00   4   604  ColorSpaceModel    % none
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   608  ISpare1
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   612  ISpare2
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   616  FSpare1
  writeBin(000L, con = fid, size = 4, endian = 'little')    # 00   4   620  FSpare2
  for(ii in 1:404) {
    writeBin(000L, con = fid, size = 1, endian = 'little')   # 00   404 1024 Padding
  }
}



#' Write a 3-band RGB array as a Khoros KRGB formatted viff cube
#'
#' @param rgb_img a 3-band RGB array, each element in [0-255]
#' @param filename full file path to save the resulting .viff
#' @return nothing
#' @details The image width and height will be inferred from the dimensions of \code{rgb_img}.
#' A call to \code{file()} is made to open a binary file connection, \code{write_khoros_KRGB_header} is called to write the
#' appropriate header, and then the integer RGB codes are written.
#' @export
write_khoros_KRGB = function(rgb_img, filename) {
  fid = file(filename, open = 'wb')
  write_khoros_header_KRGB(fid = fid, imgE = "little", imgW = ncol(rgb_img), imgH = nrow(rgb_img))
  writeBin(object = as.integer(t(rgb_img[,,1])), con = fid, size = 1, endian = 'little')
  writeBin(object = as.integer(t(rgb_img[,,2])), con = fid, size = 1, endian = 'little')
  writeBin(object = as.integer(t(rgb_img[,,3])), con = fid, size = 1, endian = 'little')
  close(fid)
}

