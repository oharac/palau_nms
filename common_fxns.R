### Support functions for this project

### setup common directories
dir_git   <- here()
dir_setup <- file.path(dir_git, '_setup')
dir_data  <- file.path(dir_git, '_data')
dir_spatial  <- file.path(dir_git, '_spatial')
dir_output  <- file.path(dir_git, '_output')
dir_o_anx <- file.path(dir_O, 'git-annex/spp_risk_dists')

### * IUCN API functions
### * Simple Features and Raster common functions
### * Other random helper functions

### common plot functions/variables
### This color scheme and label scheme matches the mean risk maps
risk_cols <- c('green4', viridis::inferno(6, direction = -1))
risk_vals <- c(0, .2, .3, .4, .5, .7, 1.0)
risk_lbls <- c('LC', 'NT', 'VU', 'EN', 'CR', 'EX')
risk_brks <- c( 0.0,  0.2,  0.4,  0.6,  0.8,  1.0)

### This color scheme and label scheme matches the pct threatened maps
thr_cols <- viridis::viridis(6, direction = +1)
thr_vals <- c(0, .25, .33, .42, .50, 1)
### much of the density between .25 and .50
thr_brks <- c(0, .25, .50, .75, 1)
thr_lbls <- c('0%', '25%', '50%', '75%', '100%')


### cat if not knitting; message if knitting

cat_msg <- function(x, ...) {
  if(is.null(knitr:::.knitEnv$input.dir)) {
    ### not in knitr environment, so use cat()
    cat(x, ..., '\n')
  } else {
    ### in knitr env, so use message()
    message(x, ...)
  }
  return(invisible(NULL))
}

### Simple Features functions
gp_proj4 <- '+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'


### Functions for accessing IUCN API

### `get_from_api()` is a function to access the IUCN API, given a url 
### (specific to data sought), parameter, and key (stored in ohi/git-annex), 
### as well as a delay if desired (to ease the load on the IUCN API server). 
### `mc_get_from_api()` runs the `get_from_api()` function across multiple 
### cores for speed...

library(parallel)
library(jsonlite)

### api_key stored on git-annex so outside users can use their own key
api_file <- file.path(dir_M, 'git-annex/globalprep/spp_ico',
                      'api_key.csv')
api_key <- scan(api_file, what = 'character')

api_version <- fromJSON('http://apiv3.iucnredlist.org/api/v3/version') %>%
  .$version

# api_version <- '2019-2'


get_from_api <- function(url, param, api_key, delay) {
  
  i <- 1; tries <- 5; success <- FALSE
  
  while(i <= tries & success == FALSE) {
    message('try #', i)
    Sys.sleep(delay * i) ### be nice to the API server? later attempts wait longer
    api_info <- fromJSON(sprintf(url, param, api_key)) 
    if (class(api_info) != 'try-error') {
      success <- TRUE
    } else {
      warning(sprintf('try #%s: class(api_info) = %s\n', i, class(api_info)))
    }
    message('... successful? ', success)
    i <- i + 1
  }
  
  if (class(api_info) == 'try-error') { ### multi tries and still try-error
    api_return <- data.frame(param_id  = param,
                             api_error = 'try-error after multiple attempts')
  } else if (class(api_info$result) != 'data.frame') { ### result isn't data frame for some reason
    api_return <- data.frame(param_id  = param,
                             api_error = paste('non data.frame output: ', class(api_info$result), ' length = ', length(api_info$result)))
  } else if (length(api_info$result) == 0) { ### result is empty
    api_return <- data.frame(param_id  = param,
                             api_error = 'zero length data.frame')
  } else {
    api_return <- api_info %>%
      data.frame(stringsAsFactors = FALSE)
  }
  
  return(api_return)
}

mc_get_from_api <- function(url, param_vec, api_key, cores = NULL, delay = 0.5) {
  
  if(is.null(cores)) 
    numcores <- ifelse(Sys.info()[['nodename']] == 'mazu', 12, 1)
  else 
    numcores <- cores
  
  out_list <- parallel::mclapply(param_vec, 
                                 function(x) get_from_api(url, x, api_key, delay),
                                 mc.cores   = numcores,
                                 mc.cleanup = TRUE) 
  
  if(any(sapply(out_list, class) != 'data.frame')) {
    error_list <- out_list[sapply(out_list, class) != 'data.frame']
    message('List items are not data frame: ', paste(sapply(error_list, class), collapse = '; '))
    message('might be causing the bind_rows() error; returning the raw list instead')
    return(out_list)
  }
  
  out_df <- out_list %>%
    bind_rows()
  out_df <- out_df %>%
    setNames(names(.) %>%
               str_replace('result.', ''))
  return(out_df)
}

drop_geom <- function(sf) {
  sf %>% as.data.frame() %>% select(-geometry)
}
