###################################################################
#                                                                 #
#     MuseumScanBox R code for chopping batch scanned panels      #
#             of fluid-based natural history specimen             #
#                            2018-07-16                           #
#                                                                 #
#     P. K. Mendez, University of California, Berkeley            #
#     patina.mendez@berkeley.edu                                  #
#                                                                 #
#                                                                 #
###################################################################

# SOME NOTES ON THE CODE
# This code assumes that the scan is a JPG that is 2 boxes wide and 3 boxes tall and will need to be cropped and then rotated clockwise 90 degrees.  The code loads the whole image first, then crops out each of the boxes, and then rotates.  
# Working with these images is space and file intensive, so make sure that you have a few (~5) gig available, it should free up as the code releases the temp files between scans.  

#------  SETWD, CREATE OUTPUT DIRECTORY & GET FILE LIST
# before starting, set the working directory to the location of the batch scanned images that you would like to crop
setwd("") # add the correct directory in the quotes
dir.create("cropped") #make the output directory (within the current working dir)

# Create the list of files (batches) to be cut into 6 individual panels, note that the files must be JPGs
files <- list.files(pattern="*.jpg", full.names=T, recursive=FALSE)

#------  INSTALL AND LOAD PACKAGES 
# install.packages("magick", dependencies=TRUE)
# install.packages("tools", dependencies=TRUE)

library("magick")
library("tools")

#------  INCREASE MEMORY LIMIT
memory.limit(20000) # increase to load in the full panel images

#------  CROPPED IMAGE PANELS
# set up the panel coordinates
panel_code <-c("s.1.1", "s.1.2", "s.2.1", "s.2.2", "s.3.1", "s.3.2") 

#------  PANEL_DIMS - function to create the list of dimension to loop through in order, does this custom for each batch in case the marquee changed between batches or sessions.  The marquee must be set to only capture the 2x3 set of boxes, or the crops will be off.  This function is required to get the dimensions in the format for the image_crop command.  If you already rotated the images, you'll need to adjust the code here for it to become 3x2.

panel_dims <- function (x) { 
  info <- image_info(x) # get the dimensions of the image
  widinc <- info$width/2 # find the width increment of a 2x3 scan
  heiinc <- info$height/3 # find the height increment of a 2x3 scan
  s.1.1 <- paste(widinc, "x", heiinc, "+", "0", "+", "0", sep="") #1.1
  s.1.2 <- paste(widinc, "x", heiinc, "+", widinc, "+", "0", sep="") #`1.2
  s.2.1 <- paste(widinc, "x", heiinc, "+", "0", "+", heiinc, sep="") #2.1
  s.2.2 <- paste(widinc, "x", heiinc, "+", widinc, "+", heiinc, sep="") #2.2
  s.3.1 <- paste(widinc, "x", heiinc, "+", "0", "+", heiinc*2, sep="") #3.1
  s.3.2 <- paste(widinc, "x", heiinc, "+", widinc, "+", heiinc*2, sep="") #3.2
  panel_dims <-c(s.1.1, s.1.2, s.2.1, s.2.2, s.3.1, s.3.2) #return the dimensions in the form required by the magick image_crop command
  return(panel_dims)
}

#------ PANEL_CUT - function to cut the image into 6 panels using magick and write out the new files

panels_cut <- function (x,filename) {
for (i in 1:6) { #loop to make the panels, replaces single each round and removes it
  img.sz <- dimensions[i]
  print(paste(i, "-" , filename, "-", panel_code[i], "-", img.sz))
  single <- image_crop(x, img.sz) # crop out one frame
  single <- image_rotate(single, 90) # rotate to portrait, change the number if your scanner setup is different
  image_write(single, path = paste("./cropped",filename,panel_code[i],".jpg", sep=""), format = "jpg") # write out the large file
  rm(single) # necessary to have R release the temp file
}}


#################### CROP THE BATCH IMAGES ########################

# Do a small test set to batch them in case you don't have time to do them all at once.  Go back and change this number as you work through them, e.g., [1:20], [21:40], etc.
files1 <- files[1:20]
flist <- files1

#----- CROP THE BATCH IMAGES BY LOOPING THROUGH THE FILES IN THE DIRECTORY
# There's a problem that exists where magick doesn't delete temp files well, requires ample HD space -- this code removes img and single at each iteration, and also detatches and reloads the magick library.  Note that this step can take awhile, sometimes up to 2 mins for the first image.

for(i in 1:length(flist)) {
  library(magick) # re-load the magick library
  print(i) # print the file iteration that you're on
  img <- image_read(flist[i]) # read via magick
  dimensions <- panel_dims(img) # setup the panel dimensions for this batch file
  fname <- file_path_sans_ext(flist[i]) # remove the .jpg extension
  panels_cut(img, fname) # cut the panel for the current file
  rm(img) # remove the img file to free up the temp file
  detach("package:magick", unload=TRUE) #detatch the package
}
