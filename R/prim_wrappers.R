#' Generate a grid output of prim_boxes
#'
#' \code{prim_grid()} returns a data table of prim_boxes with given features of
#' mass, density, coverage, and weighted coverage
#' @param input a matrix of input parameter values
#' @param y is a vector of response values
#' @param probs placeholder
#' @param vdim placeholder
#' @param ... placeholder
#' @export
prim_grid <- function(input, y, probs, threshold = 0, vdim = 2, ...) {
    
    require(prim)
    require(dplyr)
    
    # All combinations of input variables
    var_com <- t(combn(x = 1:ncol(input), vdim))
    num_com <- nrow(var_com)
    
    points_all_y <- y * probs
    
    # Store results in a list
    out <- as.list(rep(NA, num_com))
    
    # Loop through each combination
    for (i in seq_len(num_com)) {
        
        var_names <- paste(colnames(input)[var_com[i, ]], collapse = "-")
        x <- input[, var_com[i, ]]
        
        model_prim <- prim.box(x = x, y = points_all_y, ...)
        # model_prim <- prim.box(x=x, y=points_all_y, threshold.type = 1)
        
        # Template to store data
        num <- model_prim$num.hdr.class
        df <- data_frame(ID = i, vars = 0, Boxes = seq_len(num), fun = 0, mass = 0, 
            wdens = 0, wcov = 0)
        
        # Points in the entire domain
        points_all_x <- x %>% as_tibble() %>% mutate(Index = 1:n())
        
        # Points inside the box & indices & probs
        points_box_y <- lapply(1:num, function(s) model_prim$y[[s]])
        
        points_box_x <- lapply(1:num, function(s) model_prim$x[[s]] %>% as_tibble())
        
        points_box_index <- lapply(1:num, function(s) suppressMessages(semi_join(points_all_x, 
            points_box_x[[s]])) %$% Index)
        
        points_box_probs <- lapply(points_box_index, function(s) probs[s])
        
        # Points of interest in the box & domain
        interest_box <- sapply(1:num, function(s) points_box_y[[s]][points_box_y[[s]] > 
            threshold])
        interest_all <- points_all_y[points_all_y > threshold]
        
        # Scenario features
        df$vars <- var_names
        df$fun <- model_prim$y.fun[1:num]
        
        # Scenario mass
        df$mass <- sapply(1:num, function(s) round(length(points_box_y[[s]])/length(points_all_y), 
            3))
        
        # Scenario weighted density
        df$wdens <- sapply(1:num, function(s) round(sum(interest_box[[s]])/sum(points_box_probs[[s]]), 
            3))
        
        # scenario weighted coverage
        df$wcov <- sapply(1:num, function(s) round(sum(interest_box[[s]])/sum(interest_all), 
            3))
        
        out[[i]] <- df
    }
    
    # return output
    return(bind_rows(out))
}


#' prim_select
#'
#' \code{prim_grid()} returns a data table of prim_boxes with given features of
#' mass, density, coverage, and weighted coverage
#' @param input a matrix of input parameter values
#' @param y is a vector of response values
#' @param probs placeholder
#' @param var placeholder
#' @param box placeholder
#' @export
prim_select <- function(input, y, probs, var, box, ...) {
    
    require(lazyeval)
    
    # List to store the results
    out <- list()
    
    # Prepare inputs
    var_x <- select(as.data.frame(input), one_of(var)) %>% data.matrix()
    
    # Prim model model_prim <- prim.box(x=var_x, y=y*probs, threshold.type = 1,
    # threshold = 0)
    model_prim <- prim.box(x = var_x, y = y * probs, ...)
    
    # Points in the entire domain
    points_all_x <- var_x %>% as_tibble() %>% mutate(Index = 1:n())
    points_all_y <- y * probs
    
    # Points inside the box & indices & probs
    points_box_y <- model_prim$y[[box]]
    points_box_x <- model_prim$x[[box]] %>% as_tibble()
    points_box_index <- suppressMessages(semi_join(points_all_x, points_box_x) %$% 
        Index)
    points_box_probs <- probs[points_box_index]
    
    # Points of interest in the box & domain
    interest_box <- points_box_y[points_box_y > threshold]
    interest_all <- points_all_y[points_all_y > threshold]
    
    # Scenario features
    df <- data_frame(feature = c("fun", "mass", "wdens", "wcov"), value = 0)
    df[1, 2] <- model_prim$y.fun[box]
    df[2, 2] <- length(points_box_y)/length(points_all_y)
    df[3, 2] <- sum(interest_box)/sum(points_box_probs)
    df[4, 2] <- sum(interest_box)/sum(interest_all)
    
    out$quality <- df
    out$points_box <- model_prim$x[[box]]
    out$points_all <- points_all_x
    out$dims <- model_prim$box[[box]]
    
    return(out)
    
}








