
#' Generate a grid output of prim_boxes
#'
#' \code{primGrid()} returns results for many prim_boxes with given features of
#' mass, density, coverage, and weighted coverage
#' @param input matrix of input values
#' @param y vector of response values
#' @param probs occurrance proability of each state
#' @param vdim restrictions in number of input parameters
#' @param ... placeholder
#' @export
primGrid <- function(x, y, probs, threshold = 0, vdim = 2, ...) {

  # All combinations of input variables at desired vdim size
  var_com <- t(combn(x = 1:ncol(x), vdim))
  num_com <- nrow(var_com)

  #Weighted score (magnitude * probability)
  y_wgt <- y * probs

  # Store results in a list
  out <- as.list(rep(NA, num_com))

  # Loop through each combination
  for (i in seq_len(num_com)) {

    #Selected input variables
    select_x <- x[, var_com[i, ]]

    #selected input variable names (concanated)
    select_x_names <- paste(colnames(x)[var_com[i, ]], collapse = "-")

    #Run prim code (with weighted response)
    model_prim <- prim::prim.box(x = select_x, y = y_wgt, ...)

    # Number of high definition boxes
    num <- model_prim$num.hdr.class

    #Data frame to store the results
    df  <- data_frame(ID = i, vars = 0, Boxes = seq_len(num),
      fun = 0, mass = 0, prob = 0, wdens = 0, wcov = 0)

    # Points within the box & entire domain (input values)
    x_full <- x %>% as_tibble() %>% mutate(Index = 1:n())
    x_box  <- lapply(1:num, function(s) model_prim$x[[s]] %>% as_tibble())

    #Indices of points within the box
    y_box_index <- lapply(1:num, function(s)
      suppressMessages(semi_join(x_full, x_box[[s]])) %$% Index)

    # Points within the box & entire domain (response values)
    y_full <- y_wgt
    y_box  <- lapply(y_box_index, function(s) y_full[s])

    # Points of interest in the box & domain
    interest_full <- y_full[y_full > threshold]
    interest_box  <- sapply(1:num, function(s) y_box[[s]][y_box[[s]] > threshold])

    # Probabilities of the points inside the box & domain
    full_probs <- probs
    box_probs  <- lapply(y_box_index, function(s) probs[s])


    ###### Scenario features

    # Variable names
    df$vars <- select_x_names

    # Weighted response within the box
    df$fun <- model_prim$y.fun[1:num]

    # BOX mass
    df$mass <- sapply(1:num,
      function(s) round(length(y_box[[s]])/length(y_full), 3))

    # BOX probability
    df$prob <- sapply(1:num,
      function(s) round(sum(box_probs[[s]])/sum(full_probs), 3))

    # BOX weighted density
    df$wdens <- sapply(1:num,
      function(s) round(sum(y_box[[s]])/sum(box_probs[[s]]), 3))

    # BOX weighted coverage
    df$wcov <- sapply(1:num,
      function(s) round(sum(interest_box[[s]])/sum(interest_full), 3))

    out[[i]] <- df
  }

    # return output
    return(bind_rows(out))
}


#-------------------------------------------------------------------------------

#' prim_select
#'
#' \code{primSelect()} returns PRIM results with weighted density and coverage
#' @param input a matrix of input parameter values
#' @param y is a vector of response values
#' @param probs occurrance proability of each state
#' @param var parameters to define the prim box
#' @param box rank of the box being evaluated
#' @export
primSelect <- function(input, y, probs, var, box, ...) {

  # List to store the results
  out <- list()

  # Prepare inputs
  var_x <- select(as.data.frame(input), one_of(var)) %>% data.matrix()

  # Prim model model_prim <- prim.box(x=var_x, y=y*probs, threshold.type = 1,
  # threshold = 0)
  model_prim <- prim.box(x = var_x, y = y * probs, ...)

  # Points in the entire domain
  x_full <- var_x %>% as_tibble() %>% mutate(Index = 1:n())
  y_full <- y * probs

  # Points inside the box & indices & probs
  x_box <- model_prim$x[[box]] %>% as_tibble()
  y_box <- model_prim$y[[box]]

  # Indices of points inside the box
  y_box_index <- suppressMessages(semi_join(x_full, x_box) %$% Index)

  # Probabilities of points inside the box & entire domain
  box_probs  <- probs[y_box_index]
  full_probs <- probs

  # Points of interest in the box & domain
  interest_box  <- y_box[y_box > threshold]
  interest_full <- y_full[y_full > threshold]

  # Scenario features
  df <- data_frame(feature = c("fun", "mass", "prob", "wdens", "wcov"), value = 0)
  df[1,2] <- model_prim$y.fun[box]
  df[2,2] <- length(y_box)/length(y_full)
  df[3,2] <- sum(box_probs) / sum(full_probs)
  df[4,2] <- sum(interest_box)/sum(box_probs)
  df[5,2] <- sum(interest_box)/sum(interest_full)

  out$quality <- df
  out$points_box <- model_prim$x[[box]]
  out$points_all <- x_full
  out$dims <- model_prim$box[[box]]

  return(out)
}


#-------------------------------------------------------------------------------





