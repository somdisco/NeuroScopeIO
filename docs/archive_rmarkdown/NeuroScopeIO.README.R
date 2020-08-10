#' ---
#' title: "NeuroScopeIO Demo"
#' author: "Josh Taylor"
#' date: "7/17/2020"
#' output: 
#'   html_document:
#'     fig_caption: yes
#'     theme: sandstone
#'     highlight: tango
#'     number_sections: true
#'     toc: true
#'     toc_float: 
#'       collapsed: false
#'       smooth_scroll: false
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' The NeuroScopeIO package provides input/output functionality to read NeuroScope learning products into an R working environment, and partial functionality to write such products back to file.  We will use a 10-dimensional SHGR cube and its associated SOM products to demonstrate this functionality.  We consider "NeuroScope" products to comprise the following list: 
#' 
#' * a data cube, in Khoros viff format 
#' * an include file (.incl)
#' * an nna file (.nna)
#' * a weight cube (.wgtcub) file, in Khoros viff format 
#' * a nunr file (.nunr)
#' * a ctab file 
#' * a .cadj file, in Khoros viff format. Note: $\texttt{ann-SOMconsc}$ recall produces the .cadj file in Khoros KDF format. This must be converted to viff format manually in order to import with NeuroScopeIO.  
#' 
#' This document demonstrates common uses of functions in the NeuroScopeIO package, but does not exercise every function in the package. Complete documentation is available (both in stanard man pdf format, as well as interactive ```?<function_name>``` format), so any of the package functions can be explored further via these help options.  
#' 
#' # Collect Product Paths
#' To make this process ".parm-file-like", collect the paths of each product to start: 
#' 
## ---- eval=T, echo=T----------------------------------------------------------
data_file = "~/Google Drive/Datasets/SGRW/ds/100d/SGRW-100d_11class_cov100-500.viff"
include_file = "~/Google Drive/Datasets/SGRW/ds/SGRW_11class.incl"
nna_file = "~/Google Drive/Datasets/SGRW/ds/100d/SGRW-100d.nna"
wgtcub_file = "~/Google Drive/Datasets/SGRW/dp/100d/recall/SGRW-100d_11class_cov100-500_20x20.k3.r.5M.wgtcub"
nunr_file = "~/Google Drive/Datasets/SGRW/dp/100d/recall/SGRW-100d_11class_cov100-500_20x20.k3.r.5M.nunr"
ctab_file = "~/Google Drive/Datasets/SGRW/ds/SGRW.ctab"
cadj_file = "~/Google Drive/Datasets/SGRW/dp/100d/recall/SGRW-100d_11class_cov100-500_20x20.k3.r.5M.cadj"

#' 
#' # Importing Data Products
#' 
#' ## Data Cubes
#' 
#' The function ```NeuroScopeIO::read_khoros_viff``` function accepts two arguments: $\texttt{filename}$, which is a path to the viff cube to import, and $\texttt{verbose}$, which is boolean. If ```verbose=T``` then the pixel spectra of the four "corners" of the image cube are printed to the R console window, which can be helpful for spot checking your import.  The return value is a 3d data cube (array) in R, whose (1,1) element contains the spectral from pixel (1,1) in the image, and so on.  
#' 
## ---- eval=T, echo=T----------------------------------------------------------
X = NeuroScopeIO::read_khoros_viff(filename = data_file, verbose = T)
str(X)

#' 
#' Note that the function ```read_khoros_viff``` is internally calling another function of the NeuroScopeIO library to read and interpret the Khoros viff header: ```read_khoros_header```. It is rarely necessary to invoke this function by itself.  
#' 
#' We can strip out and save the cube dimensions for later use: 
## ---- eval=T, echo=T----------------------------------------------------------
img_x = ncol(X)
img_x
img_y = nrow(X)
img_y
img_z = dim(X)[3]
img_z

#' 
#' Most base R functionality is built around **data matrices**, not arrays, so we need to convert the 3d cube we just imported to a data matrix. Use the command:
## ---- eval=T, echo=T----------------------------------------------------------
X = NeuroScopeIO::convert_datcub_to_datmat(datcub = X)
str(X)

#' Data matrices have nrows = img_x*img_y and ncols = img_z.  The data matrix is populated in band-sequantial order (top to bottom, left to right), so that row 1 corresponds to the spectra in pixel (x=1,y=1), row 2 corresponds to pixel (x=2,y=1), and so on.  
#' 
#' If desired at some point, any data matrix can be converted back into a data cube via
## ---- eval=T, echo=T----------------------------------------------------------
Xcube = NeuroScopeIO::convert_datmat_to_datcub(datmat = X, img_x = img_x, img_y = img_y)
str(Xcube)

#' You must supply the image x-y dimensions for this conversion.  
#' 
#' ## Include Files 
#' 
#' The *.incl file associated with each data cube contains pertinent information about pixel labels and masking (that is, whether each pixel is "masked" from the training / recall process).  We import this information into R with the function $\texttt{read_incl}$, which takes a path to a .incl file, along with the image x-y dimensions as inputs: 
## ---- eval=T, echo=T----------------------------------------------------------
incl = NeuroScopeIO::read_incl(filename = include_file, img_x = img_x, img_y = img_y)
str(incl)

#' The return value of $\texttt{read_incl}$ is an R list with components: 
#' 
#' * **masked**, which is a matrix (nrows = img_y, ncols = img_x) whose (i,j) entry = TRUE if pixel (i,j) is maksed from processing, or FALSE otherwise 
#' * **class**, which is a matrix (nrows = img_y, ncols = img_x) whose (i,j) entry = the class label of pixel (i,j). If no class labels are found in the .incl file, all entries of this matrix are set = "?".  
#' If an application requires matching a label or mask designation to each row of the data matrix $X$, we can convert the incl matrices (which are just cubes which have a 3rd dimension size = 1) to $\texttt{convert_datcub_to_datmat}$: 
#' 
## ---- eval=T, echo=T----------------------------------------------------------
vec_mask = NeuroScopeIO::convert_datcub_to_datmat(datcub = incl$masked)
str(vec_mask)
vec_class = NeuroScopeIO::convert_datcub_to_datmat(datcub = incl$class)
str(vec_class)

#' $\color{red}{\text{Note}}$:  Since R follows column-major order, DO NOT just vectorize the matrices (i.e., with ```c(incl$class)```). This would result in a vector of labels in pixel order (x=1,y=1), (x=1,y=2) (i.e, along rows).  You could achieve correct behavior with ```c(t(incl$class))```, but I suggest using the built-in conversion functions for clarity.  
#' 
#' ## nna Files 
#' 
#' The scaling information (from network input range to network output range) utilized during training and recall is contained in .nna files, which we load into R with 
## ---- eval=T, echo=T----------------------------------------------------------
nna = NeuroScopeIO::read_nna(filename = nna_file, img_z = img_z)
str(nna)

#' The return value of ```read_nna``` is again a list with components: 
#' 
#' * **input_min** the min of the network input range, by dimension (a vector of length=img_z)
#' * **input_max** the max of the network input range, by dimension (a vector of length=img_z)
#' * **output_min** the min of the network output range (a single number, usually = 0)
#' * **output_max** the max of the network output range (a single number, usually = 1) 
#' 
#' Importing .nna information is required for replicating a recall (performing BMU selection, building a CADJ matrix, etc) in R.  
#' 
#' # ann-SOMconsc Products 
#' 
#' ## Weight Cubes 
#' 
#' Since weight cubes (*.wgtcub files) are just Khoros cubes in viff format, we can import them with the same function used to import the data cubes: 
## ---- eval=T, echo=T----------------------------------------------------------
W = NeuroScopeIO::read_khoros_viff(filename = wgtcub_file, verbose = F)
str(W)

#' Note that, as imported, W is in internal network range, so if any comparison to the data is desired it must be mapped to data range first.  The weight cube has dimensions width = som_x and height = som_y, which we can save for later use: 
## ---- eval=T, echo=T----------------------------------------------------------
som_x = ncol(W)
som_x
som_y = nrow(W)
som_y

#' 
#' Ultimately we need to convert the imported array W to a data matrix as well, but there is a crucial intermediate step.  The (1,1) entry of W corresponds to the prototype vector attached to the top-left neuron of the SOM lattice.  However, NeuroScope convention for other SOM products (nunr, CADJ) begins neuron/prototype ordering at the **bottom left** corner of the lattice.  Thus, we need to flip the rows of this cube before converting to a data matrix using the function: 
## ---- eval=T, echo=T----------------------------------------------------------
W = NeuroScopeIO::flip_datcub(datcub = W, flipX = F, flipY = T, flipZ = F)

#' Note the arguments flipX, flipY, and flipZ all default to F (to flip rows of a data cube, we set flipY = TRUE).  Now we can convert W to a data matrix as above: 
## ---- eval=T, echo=T----------------------------------------------------------
W = NeuroScopeIO::convert_datcub_to_datmat(datcub = W)
str(W)

#' 
#' ## NUNR Mapping Info 
#' 
#' The actual mapping formed during SOM learning is contained in the associated .nunr file, which we load into R via 
## ---- eval=T, echo=T----------------------------------------------------------
nunr = NeuroScopeIO::read_nunr(filename = nunr_file, som_x = som_x, som_y = som_y, img_x = img_x, img_y = img_y)
str(nunr)

#' 
#' Note that the function ```read_nunr``` requires not only the path of the nunr file itself, but also the dimensions of the image cube and SOM.  This information is contained in the header of .nunr files, but I have required it here as a check.  Again, the nunr information is returned to R as a list with components: 
#' 
#' * **density** a vector (length = # of PEs, or som_x*som_y) containing the size of each prototype's receptive field 
#' * **class** a vector (length = # of PEs) containing the plurality data label of pixels mapped to each prototype 
#' * **mapping** a matrix (nrows = img_y, ncols = img_x) whose (i,j) entry contains the index of the PEto which pixel (i,j) is mapped.  For this purpose, PE indices are treated as linear indices (not array indices), and range from 1 to som_x*som_y.  
#' * **som_indexmap** a matrix(nrows = som_y, ncols = som_x) whose (i,j) entry contains the linear index of the (i,j) neuron on the SOM lattice.  The information in this matrix can be merged with that in the mapping matrix to determing the (x,y) neuron location to which a data vector is mapped, if desired. 
#' 
#' ## Color Tables
#' 
#' Color table information is contained in .ctab files, which can be loaded with 
## ---- eval=T, echo=T----------------------------------------------------------
ctab = NeuroScopeIO::read_ctab(filename = ctab_file)
str(ctab)

#' 
#' The color table is returned to R as a data frame with columns: 
#' 
#' * **class** the set of distinct labels for pixels in the data cube
#' * **R**, **G**, **B** which are the RGB color components associated with each label. 
#' 
#' Note that no assumption is made to the completeness of the ctab (i.e., whether it is missing color specifications for labels found in incl$class). The above function merely reads the information as it exists in a .ctab file.  
#' 
#' ## CADJ 
#' 
#' CADJ matrices are stored in .cadj files which are Khoros KDF formatted.  **For use in R, .cadj files must be converted to viff format** with kformats.  Once this conversion is complete, we can read the CADJ with the viff reader: 
## ---- eval=T, echo=T----------------------------------------------------------
CADJ = NeuroScopeIO::read_khoros_viff(filename = cadj_file, verbose = F)
str(CADJ)

#' Note that the above imported CADJ as a 3d cube, whose 3rd dimension has size = 1.  It is generally easier to work with matrices in R, so we can drop the third dimension
## ---- eval=T, echo=T----------------------------------------------------------
CADJ = CADJ[,,1]

#' Since the CONN matrix is also of frequent use, I suggest building it from the CADJ import immediately: 
## ---- eval=T, echo=T----------------------------------------------------------
CONN = CADJ + t(CADJ)

#' 
#' # Importing a Complete Product Suite 
#' 
#' The above examples demonstrate import of NeuroScope products individually, but often we would like the entire product suite brought into an R environment. This can be accomplished with the helper function: 
## ---- eval=T, echo=T----------------------------------------------------------
SHGRSOM = NeuroScopeIO::load_SOM_products(data_file = data_file, include_file = include_file, nna_file = nna_file, wgtcub_file = wgtcub_file, nunr_file = nunr_file, ctab_file = ctab_file, cadj_file = cadj_file)
str(SHGRSOM)

#' The function ```load_SOM_products``` is just a wrapper which calls each step of the above under the hood.  Its return value is an R list with component names as above. The conversion of both the data and weight cubes to their respective matrices is done internally (along with the row reordering of W discussed above). List elements can be accessed via the ```$```, e.g., to return the data matrix
## ---- eval=T, echo=T----------------------------------------------------------
str(SHGRSOM$X)

#' One caveat here is that, if nna_file is given, the prototype matrix is mapped from network range to input range.  
#' 
#' Note that not all inputs to ```load_SOM_products```  are required. If, for example, you only wish to import the data cube, you can specify the data_file path and leave the others blank 
## ---- eval=T, echo=T----------------------------------------------------------
str(NeuroScopeIO::load_SOM_products(data_file = data_file))

#' 
#' 
#' # Visualizations
#' 
#' Minor visualization capabilities exist within the package, mostly for checking the integrity of the imported information.  For example, once imported, a color table can be viewed: 
## ---- eval=T, echo=T----------------------------------------------------------
NeuroScopeIO::vis_ctab(ctab = ctab, label.cex = 0.9, label.font = 2, nrows_in_display = NULL)

#' The label.cex argument controls the size of each label as plotted in its square, and the label.font controls whether the label is printed in bold (=2) or regular face (=1).  nrows_in_display allows manually setting the number of rows in the visualized table. If not given, this defaults to NULL, which is a flag for the function to pick its own visualized table dimensions.  
#' 
#' For labeled data, class maps can be visualized with 
## ---- eval=T, echo=T----------------------------------------------------------
NeuroScopeIO::vis_classmap(class_matrix = incl$class, ctab = ctab)

#' The argument class_matrix must contain class labels for each pixel (and be in "image" format, i.e., with dimensions = image dimension).  This obviously requires import of the include and ctab information.
#' 
#' PE plurality labels of the SOM can be visualized in the same manner by converting the nunr class information back to a cube: 
## ---- eval=T, echo=T----------------------------------------------------------
som_labels = NeuroScopeIO::convert_datmat_to_datcub(datmat=nunr$class, img_x = som_x, img_y = som_y)
NeuroScopeIO::vis_classmap(class_matrix = som_labels, ctab = ctab, pixel_expansion_factor = 5)

#' 
#' The additional argument pixel_expansion_factor enlarges each pixel defined in the class_matrix by its value. This is helpful for visualizing SOM labels, as SOM lattices are typically small (and the resulting plot hard to see).  
#' 
#' 
