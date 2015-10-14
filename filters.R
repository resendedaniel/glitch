invert_color <- function(img, color="green") {
    img[[color]] <- apply(img[[color]], 2, rev)
    img
}

saturate <- function(img, color="blue") {
    # img[[color]] <- img[["red"]] ^ 2
    # img[[color]] <- img[["green"]] ^ 2
    img[[color]] <- img[["blue"]] ^ 0
    img
}

random_sidewalk <- function(img) {
    img <- lapply(img, function(x) {
        n <- sample(100, 1)
        x <- cbind(x, x[,1:n])
        x <- x[,(n+1):(ncol(x))]
        x
    })
}

extrude <- function(img, direction="horizontal") {
    block <- .1 + runif(1)*.8
    position <- runif(1)
    
    img <- lapply(img, function(x) {
        col <- floor(dim(x)[1] * position)
        n <- floor(dim(x)[1] * block)
        
        x <- cbind(x[,1:col], matrix(rep(x[,col], n), ncol=n), x[,col:dim(x)[1]])
        x
    })
}

single_col <- function(img, direction="horizontal") {
    position <- runif(1)
    img <- lapply(img, function(x) {
        col <- floor(dim(x)[1] * position)
        n <- dim(x)[1]
        
        matrix(rep(x[,col], n), ncol=n)
    })
    
    img
}

noise_on_standard_deviation <- function(img, threshold=.925) {
    p <- proc.time()
    sd <- numeric(length(img[[1]]))
    for(i in seq_along(img[[1]])) {
        sd[i] <- sd(c(img[[1]][i], img[[2]][i], img[[3]][i]))
    }
    mask <- sd > quantile(sd, threshold)
    img[[1]][mask] <- runif(sum(mask))
    img[[2]][mask] <- runif(sum(mask))
    img[[3]][mask] <- runif(sum(mask)) ^ 6
    
    print(proc.time() - p)
    
    list(img=img,
         sd=sd)
}

pixel_sort_color <- function(img, color="green") {
    base <- img[[color]]
    other <- img[!names(img) %in% color]
    n <- dim(base)[2]
    for(i in 1:n) {
        if(i %% round(n/100) == 0) { cat(paste0(round(i / n * 100), "% ")) }
        
        ind <- rev(order(base[,i] - max(c(other[[1]][,i], other[[2]][,i]))))
        img <- lapply(img, function(x) {
            curr <- x[,i]
            x[,i] <- curr[ind]
            x
        })
    }
    
    img
}

pixel_sort_sd <- function(img) {
    rows <- dim(img[[1]])[1]
    cols <- dim(img[[1]])[2]
    for(i in 1:cols) {
        if(i %% round(cols/100) == 0) {
            cat(paste0(round(i / cols * 100), "% "))
        }
        ind <- sapply(1:rows, function(j) {
            sd(unlist(lapply(img, function(x) {
                x[j, i]
            })))
        })
        ind <- rev(order(ind))
        img <- lapply(img, function(x) {
            curr <- x[,i]
            x[,i] <- curr[ind]
            x
        })
    }
    
    img
}

pixel_sort_var <- function(img) {
    rows <- dim(img[[1]])[1]
    cols <- dim(img[[1]])[2]
    for(i in 1:cols) {
        if(i %% round(cols/100) == 0) {
            cat(paste0(round(i / cols * 100), "% "))
        }
        ind <- sapply(1:rows, function(j) {
            var(unlist(lapply(img, function(x) {
                x[j, i]
            })))
        })
        ind <- rev(order(ind))
        img <- lapply(img, function(x) {
            curr <- x[,i]
            x[,i] <- curr[ind]
            x
        })
    }
    
    img
}

linearX_randomY <- function(img, add_x=10, add_y=100) {
    dim <- dim(img[[1]])
    print(dim)
    
    x <- 1  
    points <- list()
    
    print("partitioning")
    while(x < dim[2]) {    
#         add_x <- round(runif(1) * dim[2]/2^4)
        add_x <- round(dim[2]/12)
        if(x + add_x > dim[2]) {
            add_x <- dim[2] - x
        }
        
        i_x <- seq(x, x + add_x)
        
        y <- 1
        while(y < dim[1]) {
#             add_y <- sample(200, 1)
#             add_y <- round(abs(rnorm(1, 100, 5)))
            add_y <- abs(round(rnorm(1, dim[1]/2^3, dim[1]/2^9)))
            if(y + add_y > dim[1]) {
                add_y <- dim[1] - y
            }
            
            i_y <- seq(y, y + add_y)
            
            points <- c(points, list(cbind(rep(i_y, length(i_x)), rep(i_x, each=length(i_y)))))
            
            y <- y + add_y + 1
        }
        x <- x + add_x + 1
        
#         cat(paste0(round(x * 100 / dim[2], 1), "% "))
    }
    
    cat("\n")
    print("averaging")
    for(i in seq(points)) {
        if(i %in% seq(1, length(points), by=100)) {
            cat(paste0(round(i * 100 / length(points)), "% "))
        }
        coord <- points[[i]]
        img$red[coord]   <- mean(img$red[coord])
        img$green[coord] <- mean(img$green[coord])
        img$blue[coord]  <- mean(img$blue[coord])
    }
    
    img
}

# img <- read_img(files[1])
# x <- linearX_randomY(img)
