get_visitation <- function(park_code, chrome) {
  
  url <- paste0(
    "https://irma.nps.gov/Stats/SSRSReports/",
    "Park%20Specific%20Reports/Recreation%20Visitors%20By%20Month%20",
    "(1979%20-%20Last%20Calendar%20Year)?Park=",
    park_code
  )
  
  # load url
  chrome$go_to(url)
  Sys.sleep(5) # pray
  
  location <- chrome$Runtime$evaluate(
    "window.location.href",
    returnByValue = TRUE
  )$result$value
  
  title <- chrome$Runtime$evaluate(
    "document.title",
    returnByValue = TRUE
  )$result$value
  
  iframe_urls <- chrome$Runtime$evaluate(
    "Array.from(document.querySelectorAll('iframe')).map(x => x.src)",
    returnByValue = TRUE
  )$result$value
  
  iframe_url <- iframe_urls[[1]]
  
  chrome$go_to(iframe_url)
  Sys.sleep(5)
  
  body_text <- chrome$Runtime$evaluate(
    "document.documentElement.innerText || ''",
    returnByValue = TRUE
  )$result$value
  
  controls <- chrome$Runtime$evaluate(
    "Array.from(document.querySelectorAll('select, input, button')).map(x => ({
      tag: x.tagName,
      id: x.id,
      name: x.name,
      value: x.value,
      text: x.innerText
    }))",
    returnByValue = TRUE
  )$result$value
  
  # Select the park using park_code
  js_select <- sprintf(
    "(() => {
      const code = '%s';

      const select = Array.from(document.querySelectorAll('select'))
        .find(s => Array.from(s.options)
        .some(o => o.value.toUpperCase() === code.toUpperCase()));

      const option = Array.from(select.options)
        .find(o => o.value.toUpperCase() === code.toUpperCase());

      select.value = option.value;
      select.dispatchEvent(new Event('change', {bubbles: true}));

      const button = Array.from(
        document.querySelectorAll('input[type=\"submit\"], button')
      ).find(x => /view report/i.test(x.value || x.innerText));

      if (button) button.click();

      return {
        selected: option.text,
        button_found: !!button
      };
    })()",
    park_code
  )
  
  chrome$Runtime$evaluate(
    js_select,
    returnByValue = TRUE
  )$result$value
  
  Sys.sleep(5)
  
  body_text <- chrome$Runtime$evaluate(
    "document.documentElement.innerText || ''",
    returnByValue = TRUE
  )$result$value
  
  # Find the report data automatically
  body_lines <- strsplit(body_text, "\n", fixed = TRUE)[[1]]
  body_lines <- trimws(body_lines)
  body_lines <- body_lines[nzchar(body_lines)]
  
  report_start <- which(
    grepl("^(19|20)[0-9]{2}$", body_lines)
  )[1]
  
  report_end <- which(
    grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", body_lines)
  )[1] - 1
  
  if (is.na(report_end) || report_end < report_start) {
    report_end <- length(body_lines)
  }
  
  rest_of_data <- paste(
    body_lines[report_start:report_end],
    collapse = " "
  )
  
  tokens <- unlist(strsplit(rest_of_data, "\\s+"))
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  
  tokens <- tokens[
    grepl("^[0-9,]+$", tokens) |
      grepl("^(19|20)[0-9]{2}$", tokens)
  ]
  
  year_positions <- which(
    grepl("^(19|20)[0-9]{2}$", tokens)
  )
  
  months <- toupper(month.abb)
  
  annual_rows <- lapply(seq_along(year_positions), function(i) {
    
    start <- year_positions[i]
    
    end <- if (i < length(year_positions)) {
      year_positions[i + 1] - 1
    } else {
      length(tokens)
    }
    
    block <- tokens[start:end]
    
    year <- block[1]
    total <- block[length(block)]
    monthly_values <- block[2:(length(block) - 1)]
    
    monthly <- rep(NA_character_, 12)
    monthly[seq_along(monthly_values)] <- monthly_values
    
    row <- c(
      Year = year,
      setNames(monthly, months),
      Year_vis = total
    )
    
    as.data.frame(
      as.list(row),
      stringsAsFactors = FALSE
    )
  })
  
  visitation <- bind_rows(annual_rows)
  
  write.csv(
    visitation,
    paste0("../data/visitation/", park_code, ".csv"),
    row.names = FALSE,
    na = ""
  )
  
  return(visitation)
}
