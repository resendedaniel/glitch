package_list <- c('jpeg', 'ggplot2', 'gridExtra', 'reshape2')
for(p in package_list) {
    if(!(p %in% rownames(installed.packages()))) install.packages(p, repos='http://cran.rstudio.com') #, lib='/usr/local/lib/R/site-library/', dependencies=TRUE)
    library(p, character.only = TRUE)
}

source('api.R')
source('utils.R')
source('filters.R')
library(jpeg)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(beepr)

# path <- "~/Pictures/zzz"
path <- "~/Desktop"

files <- list.files(path)
files <- paste(path, files, sep="/")
files <- rev(files[!grepl("treated", files)])
sizes <- sapply(files, file.info)
sizes <- unlist(data.frame(t(sizes))$size)
names(sizes) <- NULL
files <- files[order(sizes)]
# files <- sample(files, 80)


abysm <- sapply(files, function(file) {
# Just stare to the abysm, it is usefull only to not receive warnings
# Das Sprach Zarathustra

    t <- proc.time()
    
    cat(paste0(round(which(file==files) * 100 / length(files)), "% ", file))
    
    parameters <- list()
    
    parameters["size"] <- file.info(file)$size
    cat(round2(file.info(file)$size / (2^10)^2), "Mb", "\n")
    raw_img <- read_img(file)
    file <- sub(".jpg", "", file)
    file <- sub(".jpeg", "", file)
    file <- sub(".JPG", "", file)
    file <- sub(".JPEG", "", file)

    f <- "pixel_sort_sd"
    img <- apply_filter(f, raw_img)
    destfile <- gsub(paste0(path,"/"), paste0(path, "/treated/"), file)
    destfile <- paste0(destfile, "-", f, "2.jpg")
    save_img(img, destfile)
#     rm(img)
    cat("\n")
    
#     f1 <- "pixel_sort_sd"
#     img <- apply_filter(f1, img)
#     destfile <- gsub(paste0(path,"/"), paste0(path, "/treated/"), file)
#     destfile <- paste0(destfile, "-sorted_sd-", f, ".jpg")
#     save_img(img, destfile)
#     rm(img)
#     cat("\n")
    print(proc.time() - t)
    cat("\n")
    beep()
})
# system(paste0("say 'images done.'"))

# img <- read_img(files[1])
# plot_channels(img)
# save_img(img, "~/Desktop/test.jpg")
