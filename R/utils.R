
remove_empty = function(x) {
  x[!trimws(x) %in% ""]
}
split_trim = function(x, ...) {
  lapply(strsplit(x, ...), trimws)
}


#### US Transcripts #####
subset_academic_history = function(file) {
  text = pdftools::pdf_text(file)
  text = tolower(text)
  names(text) = 1:length(text)
  ind = grep("academic\\s+his", text)
  acc_history = text[ind]
  return(acc_history)
}
academic_history_table = function(x) {
  xx = x
  x = strsplit(x, "\\n")[[1]]
  start_of_grades = grep("completed.*verified", x)
  if (length(start_of_grades) == 0) {
    # unverified grades
    start_of_grades = grep("completed.*r\\s*un", x)
  }
  if (length(start_of_grades) == 0) {

    return(NULL)
  }
  x = x[seq(min(start_of_grades), length(x))]
  # not applicable on 2 lines
  x[trimws(x) == "applicable"] = ""
  x = trimws(x)
  # remove footer
  x = x[!grepl("^\\d.*generated:.*", x)]
  x = x[!grepl("^(freshman|sophomore|junior|senior)", x)]
  x = remove_empty(x)
  y = split_trim(x, split = "  ")
  y = lapply(y, remove_empty)
  not_enough = sapply(y, length) <= 5
  x = x[!not_enough]
  x = split_trim(x, "   ")
  x = lapply(x, remove_empty)
  x = sapply(x, function(r) r[1:8])
  stopifnot(is.matrix(x))
  x = t(x)
  x = as.data.frame(x, stringsAsFactors = FALSE)
  colnames(x) = c("id_course", "name_course", "cat_subject",
                  "cat_special_class", "cat_course_type",
                  "num_credits", "cat_grade_app", "cat_grade_cas")
  return(x)
}

create_academic_history_table = function(acc_history) {
  result = lapply(acc_history, academic_history_table)
  dplyr::bind_rows(result, .id = "page_number")
}

get_ocr_text = function(file, pages, ...) {
  tfile = tempfile(fileext = ".pdf")
  file.copy(file, tfile, overwrite = TRUE)
  full_text = pdftools::pdf_ocr_text(pdf = tfile, pages = pages, ...)
  return(full_text)
}

get_gpa_wes = function(file, pages, ...) {
  full_text = get_ocr_text(file = file, pages = pages, ...)
  text = full_text
  text = tolower(text)
  names(text) = pages
  text = strsplit(text, "\n")

  search_string = "official\\s*wes\\s*gpa"
  text = lapply(text, function(x) {
    ind = grep(search_string, x)
    if (length(ind) == 0) {
      return(NULL)
    }
    if (any(grepl("generated:", x))) {
      end = grep("generated:", x) - 1
    } else {
      end = length(x)
    }
    if (length(ind) > 1) {
      ind = max(ind)
    }
    x[ind:end]
  })

  result = lapply(text, function(x) {
    r = x[grepl("(under|)graduate.*\\d", x)]
    if (length(r) == 0) {
      return(NULL)
    }
    out = do.call(rbind, strsplit(r, " "))
    colnames(out) = c("gpa_type", "gpa", "hours")
    out = as.data.frame(out)
  })
  result = dplyr::bind_rows(result)
  result
}

create_academic_summary_table = function(file, pages, ...) {
  full_text = get_ocr_text(file = file, pages = pages, ...)
  text = full_text
  text = tolower(text)
  names(text) = pages
  text = strsplit(text, "\n")

  search_string = "gpa\\s*by\\s*school"
  text = lapply(text, function(x) {
    ind = grep(search_string, x)
    if (length(ind) == 0) {
      return(NULL)
    }
    if (any(grepl("generated:", x))) {
      end = grep("generated:", x) - 1
    } else {
      end = length(x)
    }
    if (length(ind) > 1) {
      ind = max(ind)
    }
    x[ind:end]
  })
  result = lapply(text, function(x) {
    if (length(x) == 0) {
      return(NULL)
    }
    x = x[!x %in% ""]
    out = sapply(strsplit(x, " "), function(r) {
      lr = length(r)
      if (length(r) < 4 && !grepl("\\d", r[length(r)])) {
        return(rep("", 5))
      }
      out = c(
        paste(r[1:(lr-4)], collapse = " "),
        paste(r[lr-3], collapse = " "),
        r[(lr-2):lr]
      )
      out
    })
    out = t(out)
    colnames(out) = c("title", "type",
                      "quality_points", "graded_hours",
                      "gpa_score")
    out = as.data.frame(out, stringsAsFactors = FALSE)
    out = out[!(out$title %in% "" & out$gpa_score %in% ""), ]
    out = out %>%
      dplyr::select(gpa_type = dplyr::any_of("title"),
                    gpa = dplyr::any_of("gpa_score"),
                    hours = dplyr::any_of("graded_hours"),
                    dplyr::everything())
    out
  })
  dplyr::bind_rows(result, .id = "page_number")
}

get_academic_history_sophas = function(file) {
  acc_history = subset_academic_history(file)
  pages = as.numeric(names(acc_history))
  out = create_academic_history_table(acc_history = acc_history)
  if (nrow(out) == 0) {
    return(NULL)
  }
  if (!"page_number" %in% colnames(out)) {
    browser()
  }
  no_out = setdiff(pages, out$page_number)
  summary = NULL
  if (length(no_out) > 0) {
    summary = try({
      create_academic_summary_table(
        file = file,
        pages = no_out)
    })
    if (nrow(summary) == 0 || inherits(summary, "try-error")) {
      summary = NULL
    }
  }
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$file = file
  out = dplyr::as_tibble(out)
  attr(out, "summary") = summary
  return(out)
}

read_sophas = get_academic_history_sophas

read_summary_sophas = function(x) {
  acc_history = subset_academic_history(file)
  pages = as.numeric(names(acc_history))
  out = create_academic_history_table(acc_history = acc_history)
  no_out = setdiff(pages, out$page_number)
  summary = NULL
  if (length(no_out) > 0) {
    summary = create_academic_summary_table(
      file = file,
      pages = no_out)
  }
  return(summary)
}


#### WES/Foreign Transcripts ####
reshape_grade_table = function(df) {
  name_course = num_credits = NULL
  rm(list = c("name_course", "num_credits"))

  cat_grade = text = x = y = space = group = colno = NULL
  rm(list = c("cat_grade", "text", "x", "y",
              "space", "group",
              "colno"))

  have_grade = "COURSE-BY-COURSE" %in% df$text
  if (!have_grade) {
    return(NULL)
  }
  df = df %>%
    dplyr::filter(!text %in% c("(L)", "(U)", "(Continued on next page)"))
  # taken from
  # https://stackoverflow.com/questions/60127375/using-the-pdf-data-function-from-the-pdftools-package-efficiently
  df <- df %>%
    dplyr::mutate(
      x = round(x/3),        #reduce resolution to minimise inconsistent coordinates
      y = round(y/3)) %>%
    dplyr::arrange(y, x) %>%                        #sort in reading order
    dplyr::mutate(group = cumsum(!dplyr::lag(space, default = 0))) %>%  #identify text with spaces and paste
    dplyr::group_by(group) %>%
    dplyr::summarise(x = dplyr::first(x),
                     y = dplyr::first(y),
                     text = paste(text, collapse = " "))
  df = df %>%
    dplyr::filter(!text %in% c("(L)", "(U)", "(Continued on next page)"))
  df = df %>%
    dplyr::group_by(y) %>%
    dplyr::mutate(colno = dplyr::row_number()) %>%         #add column numbers for table data
    dplyr::ungroup() %>%
    dplyr::select(text, colno, y) %>%
    tidyr::pivot_wider(names_from = colno, values_from = text)
  df = df %>% #pivot into table format
    dplyr::select(-y)
  colnames(df) = c("name_course", "num_credits", "cat_grade")

  df = df %>%
    dplyr::filter(!is.na(num_credits) & !is.na(cat_grade))
  df = df %>%
    dplyr::filter(!name_course %in% c("Name:", "Date of Birth:")) %>%
    dplyr::filter(!num_credits %in% c("Semester")) %>%
    dplyr::filter(!cat_grade %in% c("U.S."))
  return(df)
}

get_summary_table_wes = function(file) {
  acc_history = subset_academic_history(file)
  pages = as.numeric(names(acc_history))
  out = create_academic_history_table(acc_history = acc_history)
  if (nrow(out) == 0) {
    out = NULL
  }
  no_out = setdiff(pages, out$page_number)
  summary = NULL
  if (length(no_out) > 0) {
    summary = get_gpa_wes(
      file = file,
      pages = no_out)
  }

  text = pdftools::pdf_text(file)
  search_string = "total\\s(under|)graduate.*credits"
  text_ind = grepl(search_string, text, ignore.case = TRUE)
  if (any(text_ind)) {
    res = text[text_ind]
    res = strsplit(res, "\n")
    res = lapply(res, function(x) {
      x[grep(search_string, x, ignore.case = TRUE)]
    })
    res = c(unlist(res))
    res = unique(res)
    res = stringr::str_squish(res)
    res = sub("total\\s((under|)graduate.*)semester\\s*credits", "\\1",
              res, ignore.case = TRUE)
    res = split_trim(res, split = ":")
    res = lapply(res, sub, pattern = "GPA", replacement = "")
    res = lapply(res, tolower)
    res = lapply(res, remove_empty)
    res = lapply(res, trimws)
    out = do.call(rbind, res)
    colnames(out) = c("gpa_type", "hours", "gpa")
    out = as.data.frame(out)
    out = out[, c("gpa_type", "gpa", "hours")]
    summary = dplyr::bind_rows(summary, out)
    summary$gpa = as.numeric(summary$gpa)
    summary$hours = as.numeric(summary$hours)
    summary = dplyr::distinct(summary)
  }
  return(summary)
}


get_academic_history_wes = function(file) {
  name_course = num_credits = NULL
  rm(list = c("name_course", "num_credits"))
  data = pdftools::pdf_data(file)
  names(data) = 1:length(data)
  data = data[sapply(data, nrow) > 0]
  res = lapply(data, reshape_grade_table)
  df = dplyr::bind_rows(res, .id = "page_number")
  if (nrow(df) == 0) {
    return(NULL)
  }
  df = df %>%
    dplyr::mutate(
      num_credits = sub("\\(", "", num_credits),
      num_credits = sub("\\)", "", num_credits)
    )
  df = dplyr::as_tibble(df)
  df = df %>%
    dplyr::mutate(course = tolower(name_course))
  df$file = file
  summary = get_summary_table_wes(file)
  attr(df, "summary") = summary
  df
}

read_wes = get_academic_history_wes


#' Read Academic History
#'
#' @param file PDF file of the transcripts/application
#'
#' @return A `tbl` with information of the courses that were taken.
#' A GPA summary is given by `attr(output, "summary")`
#' @export
read_academic_history = function(file) {
  if (length(file) > 1) {
    result = lapply(file, read_academic_history)
    names(result) = file
    return(result)
  }
  result_wes = try({read_wes(file) %>%
      dplyr::mutate(type = "wes")}, silent = TRUE)
  # if (is.null(result) || inherits(result, "try-error")) {
  result_sophas = try({read_sophas(file) %>%
      dplyr::mutate(type = "sophas")}, silent = TRUE)
  if (is.null(result_wes) || inherits(result_wes, "try-error")) {
    result_wes = NULL
  }
  if (is.null(result_sophas) || inherits(result_sophas, "try-error")) {
    result_sophas = NULL
  }


  if (is.null(result_wes) && is.null(result_sophas)) {
    result = NULL
  } else if (!is.null(result_wes) &&  is.null(result_sophas)) {
    result = result_wes
  } else if ( is.null(result_wes) && !is.null(result_sophas)) {
    result = result_sophas
  } else if (!is.null(result_wes) && !is.null(result_sophas)) {
    if (nrow(result_sophas) > nrow(result_wes)) {
      result = result_sophas
    } else {
      result = result_wes
    }
  }
  # }
  return(result)
}

