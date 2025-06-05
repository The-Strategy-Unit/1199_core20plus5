#' -----------------------------------------------------------------------------
#' QUALITATIVE COMPARATIVE ANALYSIS (QCA) DATA GENERATOR
#' 
#' This script contains functions to generate data sets containing dummy data
#' for practising QCA analysis.
#' -----------------------------------------------------------------------------

# udf --------------------------------------------------------------------------

#' Get QCA variable labels
#' 
#' Returns a named list of variable_name:description for use in labelling a df
#'
#' @returns list
qca_get_labels <- function() {
  
  var_labels <- list(
    # descriptors
    ics_case = "Unique ID for each case ICS",
    # conditions
    c_readiness = "Local readiness to tackle healthcare inequalities",
    c_funding = "Dedicated funding allocated to addressing healthcare inequalities",
    c_analysis = "Data, analysis and evaluation is used to tackle healthcare inequalities",
    c_interventions = "Locally-designed interventions are implemented to tackle healthcare inequalities",
    c_partnership = "Partership working to deliver on healthcare inequalities",
    c_governance = "Effective governance mechanisms that allow for assurance and accountability",
    c_roles = "Healthcare staff have clear roles, remit and capacity to tackle inequalities",
    # outcomes
    o_credible = "Is the Core20PLUS5 approach credible for the system?",
    o_adopted = "Has the Core20PLUS5 approach been adopted by the system?",
    o_embedded = "Are Core20PLUS5 approaches being locally embedded?",
    o_equity = "Is the system narrowing the gap on hypertension inequalities?"
  )
  
  return(var_labels)
}

#' Generate a crisp QCA dataset
#' 
#' Internal function
#' Returns a tibble containing a crisp QCA dataset (i.e. values are either 
#' 0 or 1)
#'
#' @param rows Integer - the number of rows of data to return 
#' @param seed Integer - the seed number if you wish to generate replicable values
#'
#' @returns Tibble
qca_crisp <- function(rows, seed) {
  
  # generate the tibble
  set.seed(seed)
  df <- tibble::tibble(
    
    # descriptors
    ics_case = ids::adjective_animal(n = rows),
    # conditions
    c_readiness = sample(x = 0:1, size = rows, replace = TRUE),
    c_funding = sample(x = 0:1, size = rows, replace = TRUE),
    c_analysis = sample(x = 0:1, size = rows, replace = TRUE),
    c_interventions = sample(x = 0:1, size = rows, replace = TRUE),
    c_partnership = sample(x = 0:1, size = rows, replace = TRUE),
    c_governance = sample(x = 0:1, size = rows, replace = TRUE),
    c_roles = sample(x = 0:1, size = rows, replace = TRUE),
    # outcomes
    o_credible = sample(x = 0:1, size = rows, replace = TRUE),
    o_adopted = sample(x = 0:1, size = rows, replace = TRUE),
    o_embedded = sample(x = 0:1, size = rows, replace = TRUE),
    o_equity = sample(x = 0:1, size = rows, replace = TRUE)
    
  )
  
  return(df)
  
}

#' Generate a fuzzy QCA dataset
#' 
#' Internal function
#' Returns a tibble containing a fuzzy QCA dataset
#'
#' @param rows Integer - the number of rows of data to return 
#' @param seed Integer - the seed number if you wish to generate replicable values
#'
#' @returns Tibble
qca_fuzzy <- function(rows, seed) {
  
  # generate the number of steps in each variable
  var_steps <- sample(x = 3:7, size = 11, replace = TRUE)
  
  # generate the tibble
  set.seed(seed)
  df <- tibble::tibble(
    
    # descriptors
    ics_case = ids::adjective_animal(n = rows),
    # conditions
    c_readiness = sample(x = 1:var_steps[1], size = rows, replace = TRUE),
    c_funding = sample(x = 1:var_steps[2], size = rows, replace = TRUE),
    c_analysis = sample(x = 1:var_steps[3], size = rows, replace = TRUE),
    c_interventions = sample(x = 1:var_steps[4], size = rows, replace = TRUE),
    c_partnership = sample(x = 1:var_steps[5], size = rows, replace = TRUE),
    c_governance = sample(x = 1:var_steps[6], size = rows, replace = TRUE),
    c_roles = sample(x = 1:var_steps[7], size = rows, replace = TRUE),
    # outcomes
    o_credible = sample(x = 1:var_steps[8], size = rows, replace = TRUE),
    o_adopted = sample(x = 1:var_steps[9], size = rows, replace = TRUE),
    o_embedded = sample(x = 1:var_steps[10], size = rows, replace = TRUE),
    o_equity = sample(x = 1:var_steps[11], size = rows, replace = TRUE)
    
  )
  
  return(df)
  
}

#' Generate a QCA data frame
#' 
#' Generates a random tibble of data ready for use in QCA analyses.
#'
#' @param type String - name of the type of data to return (crisp|fuzzy)
#' @param rows Integer - the number of rows of data to return (default = 50)
#' @param seed Integer - set this if you wish to return reproducible values (default = random)
#'
#' @returns Tibble
#' @examples
#' # generate a set of data using default (50 rows, crisp set, random seed)
#' crQCA_1 <- generate_qca_df()
#' 
#' # generate a crisp set of data consisting of 100 rows
#' crQCA_2 <- generate_qca_df(rows = 100)
#' 
#' # generate a fuzzy set of data
#' fsQCA_1 <- generate_qca_df(type = "fuzzy")
#' 
#' # generate a fuzzy set of data that can be re-generated using the seed 123
#' fsQCA_2 <- generate_qca_df(type = "fuzzy", seed = 123)
#' 
generate_qca_df <- function(type = "crisp", rows = 50, seed = NA) {
  
  # get variable labels
  var_labels <- qca_get_labels()
  
  # generate a seed if not supplied
  if (is.na(seed)) {
    seed <- sample(100:999, 1)
  }
  
  # decide what type of data to return
  df <- 
    if (type == "crisp") {
      qca_crisp(rows = rows, seed = seed)
    } else if (type == "fuzzy") {
      qca_fuzzy(rows = rows, seed = seed)
    }
  
  # add the variable labels
  labelled::var_label(df) <- var_labels
  
  return(df)
  
}
