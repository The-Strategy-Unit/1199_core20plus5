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
#' @param sets Boolean - whether there should be built-in associations between sets of conditions (TRUE) or if there should be random assignment (FALSE)
#'
#' @returns Tibble
qca_crisp <- function(rows, seed, sets) {
  
  # generate the tibble of conditions
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
    
  )
  
  # add in outcomes
  if (sets) {
    
    # build-in some relationships between sets of conditions and outcomes
    set.seed(seed)
    df <-
      df |>
      dplyr::mutate(
        o_credible = dplyr::case_when(
          c_readiness == 1 &
            c_analysis == 1 ~ sample(
              x = 0:1,
              size = rows,
              replace = TRUE,
              prob = c(0.1, 0.9)
            ),
          c_governance == 1 ~ sample(
            x = 0:1,
            size = rows,
            replace = TRUE,
            prob = c(0.3, 0.7)
          ),
          c_partnership == 1 &
            c_governance == 1 ~ sample(
              x = 0:1,
              size = rows,
              replace = TRUE,
              prob = c(0.1, 0.9)
            ),
          .default = sample(
            x = 0:1,
            size = rows,
            replace = TRUE
          )
        ),
        o_adopted = dplyr::case_when(
          c_funding == 1 &
            c_analysis == 0 ~ sample(
              x = 0:1,
              size = rows,
              replace = TRUE,
              prob = c(0.1, 0.9)
            ),
          c_partnership == 1 ~ sample(
            x = 0:1,
            size = rows,
            replace = TRUE,
            prob = c(0.3, 0.7)
          ),
          c_governance == 1 &
            c_roles == 0 ~ sample(
              x = 0:1,
              size = rows,
              replace = TRUE,
              prob = c(0.1, 0.9)
            ),
          .default = sample(
            x = 0:1,
            size = rows,
            replace = TRUE
          )
        ),
        o_embedded = dplyr::case_when(
          c_governance == 1 &
            c_funding == 1 ~ sample(
              x = 0:1,
              size = rows,
              replace = TRUE,
              prob = c(0.1, 0.9)
            ),
          .default = sample(
            x = 0:1,
            size = rows,
            replace = TRUE
          )
        ),
        o_equity = dplyr::case_when(
          c_funding == 1 &
            c_partnership == 1 &
            c_roles == 1 ~ sample(
              x = 0:1,
              size = rows,
              replace = TRUE,
              prob = c(0.1, 0.9)
            ),
          .default = sample(
            x = 0:1,
            size = rows,
            replace = TRUE
          )
        )
      )
    
  } else {
    
    # use random assignment
    set.seed(seed)
    df <-
      df |> 
      dplyr::mutate(
        o_credible = sample(x = 0:1, size = rows, replace = TRUE),
        o_adopted = sample(x = 0:1, size = rows, replace = TRUE),
        o_embedded = sample(x = 0:1, size = rows, replace = TRUE),
        o_equity = sample(x = 0:1, size = rows, replace = TRUE)
      )
  }
  
  return(df)
  
}

# test this function
#qca_crisp(rows = 50, seed = 123, sets = TRUE)

#' Generate a fuzzy QCA dataset
#' 
#' Internal function
#' Returns a tibble containing a fuzzy QCA dataset
#'
#' @param rows Integer - the number of rows of data to return 
#' @param seed Integer - the seed number if you wish to generate replicable values
#' @param sets Boolean - whether there should be built-in associations between sets of conditions (TRUE) or if there should be random assignment (FALSE)
#'
#' @returns Tibble
qca_fuzzy <- function(rows, seed, sets) {
  
  # generate the number of steps in each variable
  set.seed(seed)
  var_steps <- sample(x = 3:7, size = 11, replace = TRUE)
  
  # generate the tibble of conditions
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
  
  # add in outcomes
  if (sets) {
    
    # find mid-point in var_steps
    var_steps_mid <- (var_steps + 1) / 2
    
    # set probabilities for where we want a relationship
    list_prob <- list()
    
    # create a set of probabilities for each of the likert-scale lengths
    for (i in 3:7) {
      probs <- (1:i)^2 # give weight to higher numbers
      probs <- probs / sum(probs) # normalise to sum to 1
      list_prob <- c(list_prob, list(probs)) # add this prob distribution to the list
    }
    # name each of these sets
    names(list_prob) <- 3:7
    
    # build-in some relationships between sets of conditions and outcomes
    set.seed(seed)
    df <-
      df |>
      dplyr::mutate(
        # credible outcome - has var_steps[8] likert scale steps
        # credible = readiness ∩ analysis, governance, partnership ∩ governance 
        o_credible = dplyr::case_when(
          (c_readiness >= var_steps_mid[1]) & (c_analysis >= var_steps_mid[3]) ~
            sample(
              x = 1:var_steps[8],
              size = rows,
              replace = TRUE,
              prob = list_prob[var_steps[8] |> as.character()] |> unlist()
            ),
          c_governance >= var_steps_mid[6] ~
            sample(
              x = 1:var_steps[8],
              size = rows,
              replace = TRUE,
              prob = list_prob[var_steps[8] |> as.character()] |> unlist()
            ), 
          (c_partnership >= var_steps_mid[5]) & (c_governance >= var_steps_mid[6]) ~ 
            sample(
              x = 1:var_steps[8],
              size = rows,
              replace = TRUE,
              prob = list_prob[var_steps[8] |> as.character()] |> unlist()
            ), 
          .default = 
            sample(
              x = 1:var_steps[8], 
              size = rows, 
              replace = TRUE
            )
        ),
        # adopted outcome - has var_steps[9] likert scale steps
        # adopted = funding ∩ analysis, partnership, governance ∩ roles
        o_adopted = dplyr::case_when(
          (c_funding >= var_steps_mid[2]) & (c_analysis >= var_steps_mid[3]) ~ 
            sample(
              x = 1:var_steps[9],
              size = rows,
              replace = TRUE,
              prob = list_prob[var_steps[9] |> as.character()] |> unlist()
            ), 
          c_partnership >= var_steps_mid[5] ~ 
            sample(
              x = 1:var_steps[9],
              size = rows,
              replace = TRUE,
              prob = list_prob[var_steps[9] |> as.character()] |> unlist()
            ), 
          (c_governance >= var_steps_mid[6]) & (c_roles >= var_steps_mid[7]) ~ 
            sample(
              x = 1:var_steps[9],
              size = rows,
              replace = TRUE,
              prob = list_prob[var_steps[9] |> as.character()] |> unlist()
            ), 
          .default = 
            sample(
              x = 1:var_steps[9],
              size = rows,
              replace = TRUE
            )
        ),
        # embedded outcome - has var_steps[10] likert scale steps
        # embedded = governance ∩ funding
        o_embedded = dplyr::case_when(
          (c_governance >= var_steps_mid[6]) & (c_funding >= var_steps_mid[2]) ~ 
            sample(
              x = 1:var_steps[10],
              size = rows,
              replace = TRUE,
              prob = list_prob[var_steps[10] |> as.character()] |> unlist()
            ),
          .default = 
            sample(
              x = 1:var_steps[10], 
              size = rows, 
              replace = TRUE
            )
        ),
        # equity outcome - has var_steps[11] likert scale steps
        # equity = funding ∩ partnership ∩ roles
        o_equity = dplyr::case_when(
          (c_funding >= var_steps_mid[2]) &
            (c_partnership >= var_steps_mid[5]) &
            (c_roles >= var_steps_mid[7]) ~ 
            sample(
              x = 1:var_steps[11],
              size = rows,
              replace = TRUE,
              prob = list_prob[var_steps[11] |> as.character()] |> unlist()
            ), 
          .default = sample(x = 0:1, size = rows, replace = TRUE)
        )
      )
    
  } else {
    
    # use random assignment
    set.seed(seed)
    df <-
      df |> 
      dplyr::mutate(
        o_credible = sample(x = 0:1, size = rows, replace = TRUE),
        o_adopted = sample(x = 0:1, size = rows, replace = TRUE),
        o_embedded = sample(x = 0:1, size = rows, replace = TRUE),
        o_equity = sample(x = 0:1, size = rows, replace = TRUE)
      )
  }
  
  # return the data
  return(df)
  
}


# test this function
#qca_fuzzy(rows = 50, seed = 123, sets = TRUE)

#' Generate a QCA data frame
#' 
#' Generates a random tibble of data ready for use in QCA analyses.
#'
#' @param type String - name of the type of data to return (crisp|fuzzy)
#' @param rows Integer - the number of rows of data to return (default = 50)
#' @param seed Integer - set this if you wish to return reproducible values (default = random)
#' @param sets Boolean - whether there should be built-in associations between sets of conditions (TRUE) or if there should be random assignment (FALSE)
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
generate_qca_df <- function(type = "crisp", rows = 50, seed = NA, sets = TRUE) {
  
  # get variable labels
  var_labels <- qca_get_labels()
  
  # generate a seed if not supplied
  if (is.na(seed)) {
    seed <- sample(100:999, 1)
  }
  
  # decide what type of data to return
  df <- 
    if (type == "crisp") {
      qca_crisp(rows = rows, seed = seed, sets = sets)
    } else if (type == "fuzzy") {
      qca_fuzzy(rows = rows, seed = seed, sets = sets)
    }
  
  # add the variable labels
  labelled::var_label(df) <- var_labels
  
  return(df)
  
}
