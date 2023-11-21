
str_rep_squish = function(x, ...) {
  x = stringr::str_replace_all(x, ...)
  x = stringr::str_squish(x)
  x
}

course_sub = function(x, find, replace) {
  x[x %in% find] = replace
  x
}

remove_roman_ending = function(x) {
  x = str_rep_squish(x, " (a|b|c|ab|bc)$", " ")
  x = str_rep_squish(x, " (i|ii|iii|iv|v|vi)$", " ")
  x = str_rep_squish(x, " ([1-9])$", " ")
  x
}

remove_endings = function(x) {
  # x = str_rep_squish(x, " advanced$", " ")
  x = str_rep_squish(x, " introduction to$", " ")
  x = str_rep_squish(x, " (h|d)$", " ")
  x = str_rep_squish(x, " w ", " with ")
  x = str_rep_squish(x, " (e|c)$", " ")
  x
}

remove_other_roman_ending = function(x) {
  x = str_rep_squish(x, " (ai|bi|ci|aii|bii|cii)$", " ")
  x = str_rep_squish(x, " (vi|vii)$", " ")
  x
}

remove_punct = function(x) {
  x = str_rep_squish(x, ":", " ")
  x = str_rep_squish(x, "-", " ")
  x = str_rep_squish(x, "[.]", " ")
  x = str_rep_squish(x, "[*]", " ")
  x = str_rep_squish(x, ",", " ")
  x = str_rep_squish(x, "/", " ")
  x = str_rep_squish(x, "&", " and ")
  x = str_rep_squish(x, '"', " ")
  x = str_rep_squish(x, "'s( |$)", " ")
  x = str_rep_squish(x, "\\(", " (")
  x = str_rep_squish(x, "\\)", ") ")
  x
}

harmonize_linear_algebra = function(x) {
  x = str_rep_squish(x, "(^| )(algba|algbra|algeb|algrebra)( |$)", " algebra ")

  x = str_rep_squish(x, "(^| )boolean alg( |$)", " boolean algebra ")
  x = str_rep_squish(x, "(^| )matrix alg( |$)", " matrix algebra ")

  x = str_rep_squish(x, "(^| )lin alg", " linear alg")
  x = str_rep_squish(x, "(^| )linear alg( |$)", " linear algebra ")
  x = str_rep_squish(x, "matrc and linr trnsf", " matrix and linear transformations ")
  x = str_rep_squish(x, "(^| )linr ", " linear ")
  x = str_rep_squish(x, "(^| )lin reg( |$)", " linear regression ")
  x = str_rep_squish(x, "(^| )lin model", " linear model")

  x = str_rep_squish(x, "( |^)abs algebra( |$)", " abstract algebra ")

  x = str_rep_squish(x, "( |^)gen lin( |$)", " generalized linear ")
  x = str_rep_squish(x, "( |^)gen linear", " generalized linear ")
  x = str_rep_squish(x, "( |^)app linear( |$)", " applied linear ")
  x = str_rep_squish(x, "( |^)comp(ut|) linear( |$)", " computational linear ")
  # x = str_rep_squish(x, "( |^)int linear( |$)", " intermediate linear ")

  x = str_rep_squish(x, " w apps", " with applications ")
  x = str_rep_squish(x, " for appl$", " with applications ")
  x
}

harmonize_course = function(x) {
  x = str_rep_squish(x, "(1st|2nd|3rd|4th) yr ", "\\1 year ")
  x = str_rep_squish(x, "(^| )first (yr|year) ", " 1st year ")
  x = str_rep_squish(x, "(^| )freshman ", " 1st year ")
  x = str_rep_squish(x, "(^| )freshmen ", " 1st year ")

  x = str_rep_squish(x, "(^| )second (yr|year) ", " 2nd year ")
  x = str_rep_squish(x, "(^| )third (yr|year) ", " 3rd year ")
  x = str_rep_squish(x, "(^| )fourth (yr|year) ", " 4th year ")

  x = str_rep_squish(x, "(^| )advance ", " advanced ")

  x = str_rep_squish(x, "(^| )diff equation", " differential equation")
  x = str_rep_squish(x, "(^| )diff eq( |$)", " differential equation")
  x = str_rep_squish(x, "(^| )differential eq( |$)", " differential equation")
  x = str_rep_squish(x, "differential equation( |$)", " differential equations ")
  x = str_rep_squish(x, "diff equat( |$)", " differential equations ")
  x = str_rep_squish(x, " eqn(s)( |$)", " equations ")

  x = str_rep_squish(x, " lab$", " ")
  x = str_rep_squish(x, " laboratory$", " ")



  x = str_rep_squish(x, "(^| )intro ", " introduction ")
  x = str_rep_squish(x, "(^| )adv ", " advanced ")
  x = str_rep_squish(x, "(^| )accel ", " accelerated ")
  x = str_rep_squish(x, "(^| )accelerate ", " accelerated ")
  x = str_rep_squish(x, "(^| )an intro", " intro")



  x = harmonize_linear_algebra(x)

  x = str_rep_squish(x, "(^| )ap ", " advanced placement ")


  x = str_rep_squish(x, "(^| )princ of ", " principles of ")
  x = str_rep_squish(x, " m(a|i)croecon( |$)", " m\\1croeconomics ")
  x = str_rep_squish(x, " (micro|macro) economic", " \\1economic")
  x = str_rep_squish(x, "(^| )micro economic", " microeconomic")
  x = str_rep_squish(x, "(^| )macro economic", " macroeconomic")
  x = str_rep_squish(x, "^biost ", " biostatistical ")
  x = str_rep_squish(x, "^calc ", " calculus ")
  x = str_rep_squish(x, " calc( |$)", " calculus ")
  x = str_rep_squish(x, "(^| )cal func", " calculus func")

  x = str_rep_squish(x, " analyt( |$)", " analytic ")
  x = str_rep_squish(x, " anlys( |$)", " analysis ")
  x = str_rep_squish(x, " anly( |$)", " analysis ")
  x = str_rep_squish(x, " analys( |$)", " analysis ")
  x = str_rep_squish(x, "(^| )analys ", " analysis ")
  x = str_rep_squish(x, "(^| )num analys", " numerical analys")

  x = str_rep_squish(x, " real anal( |$)", " real analysis ")
  x = str_rep_squish(x, " real analysi(i|)( |$)", " real analysis ")



  x = str_rep_squish(x, " geom( |$)", " geometry ")
  x = str_rep_squish(x, "( |^)(anal|anyl|anyl|analyt) geom", " analytic geom")
  x = str_rep_squish(x, "( |^)(appl|appld) ", " applied ")
  x = str_rep_squish(x, "( |^)(numricl) ", " numerical ")
  x = str_rep_squish(x, "( |^)(mathematcl) ", " mathematical ")

  x = str_rep_squish(x, "^math ", " mathematical ")


  x = str_rep_squish(x, "( |^)thry ", " theory ")
  x = str_rep_squish(x, "( |^)lngtdl ", " longitudinal ")

  x = str_rep_squish(x, " stat( |$)", " statistics ")
  x = str_rep_squish(x, " stats( |$)", " statistics ")
  x = str_rep_squish(x, " prob( |$)", " probablility ")

  x = str_rep_squish(x, "^introductory ", " introduction to ")

  x = str_rep_squish(x, "(^| )equationslinear( |$)", " equations linear ")

  x = str_rep_squish(x, " to to ", " to ")
  x = str_rep_squish(x, " and$", " ")
  x = str_rep_squish(x, " anly( |$)", " analysis ")

  x = str_rep_squish(x, "(^| )mthds( |$)", " methods ")
  x = str_rep_squish(x, "(^| )systemicanatomy( |$)", " systemic anatomy ")

  x = str_rep_squish(x, "(^| )prgrmng( |$)", " programming ")
  x = str_rep_squish(x, "(^| )hlth( |$)", " health ")



  x
}

harmonize_real = function(x) {
  x = str_rep_squish(x, " algebra and real$", " algebra and real analysis")
  x = str_rep_squish(x, "(^| )theor func real variabl( |$)",
                     " theory of real analysis and functional analysis ")
  x = str_rep_squish(x, "(^| )real and functional analysis( |$)",
                     " real analysis and functional analysis ")
  x = str_rep_squish(x, "(^| )real and functional analysis( |$)",
                     " real analysis and functional analysis ")

  x = str_rep_squish(x, "(^| )trans higher math( |$)",
                     " transition to higher mathematics ")

  x = str_rep_squish(x, " (real|complex) variable( |$)", " \\1 variables ")

  # may want to call this real numbers
  x = course_sub(x, "analysis in rn",
                 "real analysis")

  x = str_rep_squish(x, "complex variable functions",
                     "complex variables functions")


  x
}

harmonize_calc = function(x) {
  x = str_rep_squish(x, "(^| )multivar calc", " multivariable calc")
  x = str_rep_squish(x, "(^| )single var calc", " single variable calc")
  x = str_rep_squish(x, "(^| )sgl var calc", " single variable calc")
  x = str_rep_squish(x, "(^| )multi vari calc", " multivariable calc")

  x = str_rep_squish(x, "(^| )multivariate( |$)", " multivariable ")
  x = str_rep_squish(x, "^calculus for .*", " calculus ")
  x = str_rep_squish(x, "(^| )sevrl var( |$)", " several variables")
  x = str_rep_squish(x, "(^| )of a single( |$)", " of single ")
  x = str_rep_squish(x, "(^| )functns( |$)", " functions ")
  x = str_rep_squish(x, "(^| )calculus (of |)single variable(s|)( |$)",
                     " calculus of one variable ")
  x = str_rep_squish(x, "(^| )integ calc",
                     " integral calc")
  x = str_rep_squish(x, "(^| )pre calc", " precalc")
  x = str_rep_squish(x, "(^| )precalc($| )", " precalculus ")
  x = str_rep_squish(x, "(^| )calcul($| )", " calculus")

  x = str_rep_squish(x, "(^| )meth multiva ", " methods of multivariable ")

  x = str_rep_squish(x, " part$", " ")
  x = str_rep_squish(x, " calc(\\d)$", " calculus \\1")
  x = str_rep_squish(x, "(^| )multivariable calci($| )",
                     " multivariable calculus ")
  x
}

harmonize_data_struct = function(x) {
  x = str_rep_squish(x, "(^| )data structure( |$)", " data structures ")
  x
}

harmonize_stat = function(x) {
  x = str_rep_squish(x, "(^| )bio stat", " biostat")
  x = str_rep_squish(x, "(^| )biostat ", " biostatistics ")
  x = str_rep_squish(x, " biostat$", " biostatistics ")
  x = str_rep_squish(x, " biostats( |$)", " biostatistics ")
  x = str_rep_squish(x, " biostatistical ", " biostatistics ")
  x = str_rep_squish(x, "(^| )regr analysis", " regression analysis ")
  x = str_rep_squish(x, "(^| )multi variable ", " multivariable ")
  x = str_rep_squish(x, "(^| )sev variable ", " several variable ")
  x = str_rep_squish(x, "(^| )metods( |$)", " methods ")
  x = str_rep_squish(x, "(^| )mehtod", " method")
  x = str_rep_squish(x, "^curr topic", " current topic")
  x = str_rep_squish(x, " rsrch ", " research ")

  x = str_rep_squish(x, "^prob and ", " probability and ")
  x = str_rep_squish(x, " thry$", " theory ")
  x = str_rep_squish(x, " math$", " mathematics ")
  x = str_rep_squish(x, "^stats", " statistics ")
  x = str_rep_squish(x, "^stat (machine|model|data|computing)",
                     " statistical \\1")
  x = str_rep_squish(x, "^anly ", " analysis ")

  x = str_rep_squish(x, "(^| )stochastic process($| )",
                     " stochastic processes ")

  x = str_rep_squish(x, " math statistics",
                     " mathematical statistics")

  x = str_rep_squish(x, "^bayes ", " bayesian ")
  x = str_rep_squish(x, "^data sci ", " data science ")



  x = str_rep_squish(x, "infernce$", " inference")
  x = str_rep_squish(x, " inferenc( |$)", " inference ")



  x = course_sub(x, "stochastic process", "stochastic processes")

  x = str_rep_squish(x, "(^| )non parametric( |$)",
                     " nonparametric ")
  x = str_rep_squish(x, "^stat ",
                     " statistical ")




  x
}

harmonize_cs = function(x) {
  x = str_rep_squish(x, "(^| )comp sci", " computer sci")
  x = str_rep_squish(x, "(^| )com sci", " computer sci")
  x
}

harmonize_elem = function(x) {
  x = str_rep_squish(x, "^elem ", "elementary ")
  x = str_rep_squish(x, "^elements of ", "elementary ")
  x
}

harmonize_exper = function(x) {
  x = str_rep_squish(x, "^experiment(s|) (of|in) ", " experimental ")
  x = str_rep_squish(x, "^exper ", " experimental ")
  x = str_rep_squish(x, " chem$", " chemistry ")
  x
}

harmonize_intro = function(x) {
  x = str_rep_squish(x, "^a general intro ", " introductory ")
  x = str_rep_squish(x, "^an introduction ", " introduction ")
  x = str_rep_squish(x, "^introduction to ", " introductory ")
  x = str_rep_squish(x, "^introduction (in|for) ", " introductory ")

  x = str_rep_squish(x, "^fundamentals (of|in|for) ", " fundamental ")

  x = str_rep_squish(x, "^principle (in|for|of) ", " principles of ")
  x = str_rep_squish(x, "^prin ", " principles ")

  x = str_rep_squish(x, "^gen ", " general ")
  x = str_rep_squish(x, "^genrl ", " general ")

  x = str_rep_squish(x, "^undergrad ", " undergraduate ")
  x = str_rep_squish(x, "^grad ", " graduate ")

  x = str_rep_squish(x, " stdy$", " study ")
  x = str_rep_squish(x, "^hon ", " honors ")
  x = str_rep_squish(x, "^honor ", " honors ")
  x = str_rep_squish(x, "(^| )honours( |$)", " honors ")

  x = str_rep_squish(x, "(^| )foundatns( |$)", " foundations ")



  x = str_rep_squish(x, "^basics of ", " basic ")
  x = str_rep_squish(x, "^basis (of|for) ", " basic ")
  x = str_rep_squish(x, "^beginner(s|) ", " beginner ")
  x = str_rep_squish(x, "^beginning(s|) ", " beginner ")

  x = str_rep_squish(x, "^introduction to$", " ")

  x = str_rep_squish(x, "^biol  ", " biological ")
  x = str_rep_squish(x, "^biomed  ", " biomedical ")

  x = str_rep_squish(x, "^introduction ", " introductory ")

  x
}




remove_parens_ending = function(x) {
  x = str_rep_squish(x, stringr::fixed("(english)"), " ")
  x = str_rep_squish(x, stringr::fixed("(in english)"), " ")
  x = str_rep_squish(x, stringr::fixed("(french)"), " ")
  x = str_rep_squish(x, stringr::fixed("(in french)"), " ")
  x = str_rep_squish(x, stringr::fixed("(accelerated)"), " ")
  x = str_rep_squish(x, stringr::fixed("(dissertation)"), " ")
  x = str_rep_squish(x, stringr::fixed("(practical)"), " ")
  x = str_rep_squish(x, stringr::fixed("(advanced)"), " ")
  x = str_rep_squish(x, stringr::fixed("(intermediate)"), " ")
  x = str_rep_squish(x, stringr::fixed("(general mathematics)"), " ")
  x = str_rep_squish(x, stringr::fixed("(beginner)"), " ")
  x = str_rep_squish(x, stringr::fixed("(honors)"), " ")
  x = str_rep_squish(x, stringr::fixed("(elementary)"), " ")
  x = str_rep_squish(x, stringr::fixed("(lab i waived)"), " ")
  x = str_rep_squish(x, stringr::fixed("(lab ii waived)"), " ")
  x = str_rep_squish(x, stringr::fixed("(seminar)"), " ")
  x = str_rep_squish(x, stringr::fixed("?"), " ")

  x = str_rep_squish(x, stringr::fixed("(science)"), " ")

  x = str_rep_squish(x, stringr::fixed("(basic)"), " ")
  x = str_rep_squish(x, stringr::fixed("(basic level)"), " ")

  x = str_rep_squish(x, stringr::fixed("(1st half)"), " ")
  x = str_rep_squish(x, stringr::fixed("(2nd half)"), " ")

  x = str_rep_squish(x, stringr::fixed("(lab)"), " ")
  x = str_rep_squish(x, stringr::fixed("(a)"), " ")
  x = str_rep_squish(x, stringr::fixed("(b)"), " ")
  x = str_rep_squish(x, stringr::fixed("(c)"), " ")
  x = str_rep_squish(x, stringr::fixed("(w)"), " ")
  x = str_rep_squish(x, stringr::fixed("(i)"), " ")
  x = str_rep_squish(x, stringr::fixed("(ii)"), " ")
  x = str_rep_squish(x, " of$", " ")
  x = str_rep_squish(x, " in$", " ")
  x = str_rep_squish(x, " with$", " ")
  x = str_rep_squish(x, " (aii|bii)$", " ")

  x
}

harmonize_sit = function(x) {
  x = str_rep_squish(x, "^situation and", "situations and")
  x = str_rep_squish(x, " and policy$", " and policies")
  x = str_rep_squish(x, "^u s history", " us history")
  x
}

process_course = function(x) {
  course = tolower(x)
  course = remove_punct(course)
  course = remove_roman_ending(course)
  course = remove_endings(course)
  course = remove_punct(course)
  course = harmonize_course(course)
  course = harmonize_real(course)
  course = harmonize_calc(course)
  course = harmonize_stat(course)
  course = harmonize_calc(course)
  course = harmonize_data_struct(course)
  course = harmonize_elem(course)
  course = harmonize_exper(course)
  course = harmonize_intro(course)
  course = remove_roman_ending(course)
  course = remove_parens_ending(course)
  course = harmonize_cs(course)
  course = harmonize_sit(course)
  course = remove_other_roman_ending(course)
  course = harmonize_intro(course)
  course = remove_endings(course)
  course = harmonize_calc(course)
}

flag_courses = function(df) {
  course = NULL
  rm(list = c("course"))
  is_algebra = is_calculus = is_linear_algebra = is_adv_math = NULL
  is_analysis = NULL
  rm(list = c("is_algebra", "is_analysis",
              "is_calculus", "is_linear_algebra",
              "is_adv_math"))

  summary = attr(df, "summary")
  df = df %>%
    dplyr::mutate(
      is_calculus = grepl("calculus", course) & !grepl("precalc", course),
      is_algebra = grepl("(^| )algeb", course),
      is_linear_algebra = grepl("(^| )lin", course) & is_algebra,
      is_matrix_algebra = grepl("(^| )matrix", course) & is_algebra,
      is_analysis = grepl("analysis", course),
      is_analysis_math = is_analysis & grepl("math", course),
      is_adv_math = grepl("advanced", course) & grepl("math", course)
    )
  attr(df, "summary") = summary
  df
}



subset_summary = function(summary) {
  gpa = gpa_type = NULL
  rm(list = c("gpa_type", "gpa"))

  if (is.null(summary)) {
    return(NULL)
  }
  summary = summary %>%
    dplyr::select(gpa_type = dplyr::any_of("title"),
                  gpa = dplyr::any_of("gpa_score"),
                  hours = dplyr::any_of("graded_hours"),
                  dplyr::everything())
  if (all(c("gpa_type", "gpa") %in% colnames(summary))) {
    if ("hours" %in% colnames(summary)) {
      summary = summary %>%
        dplyr::group_by(gpa_type) %>%
        dplyr::summarise(
          gpa = max(as.numeric(gpa))
        )
    }
    summary = summary %>%
      dplyr::select(dplyr::all_of(c("gpa_type", "gpa")))
  }
  if ("gpa_type" %in% colnames(summary)) {
    summary = summary %>%
      dplyr::mutate(
        gpa_type = dplyr::case_when(
          gpa_type %in% "cumulative undergraduate" ~ "undergraduate",
          TRUE ~ gpa_type
        )
      )
    if (all(c("undergraduate", "graduate") %in% summary$gpa_type)) {
      summary = summary %>%
        dplyr::filter(gpa_type %in% c("undergraduate", "graduate"))
    }
  }
  summary
}

#' Parse the academic history output
#'
#' @param df A `data.frame` from `read_academic_history`
#'
#' @return A list of checks and more subsetted data
#' @export
parse_academic_history = function(df) {
  name_course = course = NULL
  is_calculus = is_linear_algebra = is_adv_math = NULL
  is_algebra = is_matrix_algebra = is_analysis = is_analysis_math = NULL
  rm(list = c(
    "course", "name_course",
    "is_calculus", "is_linear_algebra", "is_adv_math",
    "is_algebra", "is_matrix_algebra", "is_analysis",
    "is_analysis_math"))
  if (is.null(df)) {
    warning("df is NULL, returning NULL")
    return(NULL)
  }
  summary = attr(df, "summary")
  if (is.null(summary) || nrow(summary) == 0) {
    summary = NULL
  }
  summary = subset_summary(summary)
  if (!"course" %in% colnames(df) && "name_course" %in% colnames(df)) {
    df = df %>%
      dplyr::mutate(course = tolower(name_course))
  }
  df = df %>%
    dplyr::mutate(
      course = process_course(course),
    )
  df = flag_courses(df)
  df = df %>%
    dplyr::filter(is_calculus | is_linear_algebra | is_adv_math |
                    is_algebra | is_matrix_algebra | is_analysis |
                    is_analysis_math) %>%
    dplyr::arrange(dplyr::desc(is_calculus),
                   dplyr::desc(is_linear_algebra),
                   dplyr::desc(is_adv_math)) %>%
    dplyr::select(course,
                  dplyr::any_of("cat_grade"),
                  dplyr::starts_with("is_"),
                  dplyr::any_of("type"),
                  dplyr::any_of("name_course")
    )
  attr(df, "summary") = summary
  out = list(
    prereq = df,
    gpa = summary
  )
  out$data_parsed = nrow(df) > 0
  out = prereq_check(out)
  out = gpa_check(out, run_gpa_type = "undergraduate")
  out = gpa_check(out, run_gpa_type = "graduate")
  out = transform_parsed_gpa(out)
  out
}


gpa_check = function(out, run_gpa_type = "undergraduate") {
  gpa_type = NULL
  rm(list = c("gpa_type"))
  df = out$gpa
  run_gpa_type = run_gpa_type[1]
  cn = paste0("passes_gpa_", run_gpa_type)
  if (is.null(df)) {
    out[[cn]] = FALSE
    return(out)
  }
  if (run_gpa_type %in% df$gpa_type) {
    gpa = df %>%
      dplyr::filter(gpa_type %in% run_gpa_type) %>%
      dplyr::pull(gpa)
    out[[cn]] = gpa >= 2.75
    return(out)
  } else {
    out[[cn]] = NA
    return(out)
  }
  out
}

prereq_check = function(out) {
  df = out$prereq
  if (is.null(df)) {
    out$passes_prereq = FALSE
    out$prereq_message = "NULL data"
    return(out)
  }
  if (sum(df$is_calculus) > 1 & sum(df$is_linear_algebra) > 0) {
    out$passes_prereq = TRUE
    out$prereq_message = "2 calc + linear algebra"
    return(out)
  }
  if (sum(df$is_calculus) > 1 & sum(df$is_matrix_algebra) > 0) {
    out$passes_prereq = TRUE
    out$prereq_message = "2 calc + matrix algebra"
    return(out)
  }
  if (sum(df$is_analysis) > 1 & sum(df$is_algebra) > 0) {
    out$passes_prereq = TRUE
    out$prereq_message = "2 Analysis + algebra"
    return(out)
  }

  if (sum(df$is_adv_math) > 1) {
    out$passes_prereq = TRUE
    out$prereq_message = "2 Advanced Math + algebra"
    return(out)
  }

  out$passes_prereq = FALSE
  out$prereq_message = "Manual Flag"
  return(out)
}

transform_parsed_gpa = function(out) {
  gpa_type = NULL
  rm(list = c("gpa_type"))
  df = out$gpa
  if (is.null(df)) {
    return(out)
  }
  df = df %>%
    dplyr::select(gpa_type = dplyr::any_of("title"),
                  gpa = dplyr::any_of("gpa_score"),
                  # hours = dplyr::any_of("graded_hours"),
                  dplyr::everything())
  if (is.null(df)) {
    return(NULL)
  }
  out$transformed_gpa = df %>%
    dplyr::mutate(gpa_type = paste0(gpa_type, "_gpa")) %>%
    tidyr::spread(key = "gpa_type", value = "gpa")
  out
}

#' Transform the parsed the academic history output into a `data.frame`
#'
#' @param out A `list` from `parse_academic_history`
#'
#' @return A single-row `data.frame` of the checks and passes.
#' @export
#'
transform_parsed_history = function(out) {
  data = dplyr::as_tibble(
    out[c("data_parsed", "passes_prereq", "prereq_message",
          "passes_gpa_undergraduate", "passes_gpa_graduate")]
  )
  dplyr::bind_cols(data, out$transformed_gpa)
}
