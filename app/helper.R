## Scrape UW  Time schedule data to see how many students per quarter

prepare_to_scrape_for_a_given_prefix_fn <- function(){
  
  library(tidyverse)
  library(rvest) 
  
  # for a given course prefix, find the URL to its time schedule pages, since
  # they are not obvious
  
  current_time_schedule_url_fragment <-     
    "https://www.washington.edu/students/timeschd/" %>% 
    read_html() %>% 
    html_nodes("h1~ ul li") %>% 
    grep(pattern = "href", 
         value = TRUE) %>% 
    regmatches(., regexpr("[A-Z]{3}\\d{4}/", .))
  
  current_time_schedule_url <- 
    paste0("https://www.washington.edu/students/timeschd/",
           current_time_schedule_url_fragment[1])
  
  current_time_schedule_index <- 
    current_time_schedule_url %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    # get only text with the pattern "Program name (PREFIX)"
    grep(pattern = "[A-Za-z ]+ \\([A-Z]+\\)", 
         value = TRUE) 
  
  # this is our lookup table for course prefixes to get URLs
  current_time_schedule_tbl_urls <- 
    tibble(txt = current_time_schedule_index %>% 
             regmatches(., regexpr("[A-Za-z,' ]+ \\([A-Z]+\\)", .)) %>% 
             str_squish(),
           urls = current_time_schedule_index %>% 
             grep(pattern = "href", 
                  value = TRUE) %>% 
             regmatches(., regexpr("[a-z]*.html", .))
    ) %>% 
    mutate(prefixes = gsub("[()]", "", 
                           regmatches(txt, 
                                      regexpr("\\([A-Z]+\\)",
                                              txt)))) %>% 
    mutate(full_urls = paste0(current_time_schedule_url,
                              urls)) %>% 
    arrange(txt)
  
  return(current_time_schedule_tbl_urls)
  
}

scrape_time_schedule_fn <- 
  function(current_time_schedule_tbl_urls, 
           prefix = "ARCHY",
           start_year = 2008,
           end_year = substr(Sys.Date(), 1, 4) # current year
           ){

library(tidyverse)
library(rvest)

# start computing on function parameters ---- 

# look up the URL for a given prefix 
url_suffix_for_the_prefix <- 
current_time_schedule_tbl_urls %>% 
  filter(prefixes == prefix) %>% 
  pull(full_urls) %>% 
  sub(".*/", "", .)


# get quarters and years to scrape
qtr <- c("WIN", "SPR", "AUT")
year <- start_year:end_year
# make vector of qtr-year combinations
qtryear <- apply(expand.grid(qtr,year), 1, paste, collapse ="")

# objects to store loop output, for diagnostics... 
total_students <- vector("numeric", length = length(qtryear))
theurl <- vector("character", length = length(qtryear))
sitedata <- NULL

# loop over each qtr-year combination and scrape the webpage

for(i in seq_along(qtryear)){
  
  theurl[[i]] <- paste0("https://www.washington.edu/students/timeschd/", 
                        qtryear[i], 
                        "/",
                        url_suffix_for_the_prefix) # 
  # slurp data from the URL
  url_contents <- read_html(theurl[[i]])
  
  sitedata_tbls <- bind_rows(html_table(url_contents))
  
  # exclude QZ
  sitedata_tbls_no_qz <- 
    sitedata_tbls %>% 
    filter(!str_detect(X1, "QZ"))
  
  # if the row begins with prefix, nothing
  # if the row begins with a number, paste the previous row at the front
  
  out_df  <- sitedata_tbls_no_qz %>%
    mutate(last_PREFIX = if_else(str_detect(X1, 
                                            paste0("^", prefix)), 
                                            X1, 
                                            NA_character_)) %>%
    fill(last_PREFIX) %>%  # Fill down the "PREFIX" values
    mutate(X1 = if_else(!str_detect(X1, paste0("^", prefix)),  # If the row starts with anything but prefix
                        paste(last_PREFIX, X1),  # Paste the last "prefix" value with the current row
                        X1)) %>%
    select(X1)  %>% 
    # drop rows that are course name only with no data
    filter(nchar(X1) > 50) %>% 
    # drop rows that don't start with prefix
    filter(str_detect(X1, prefix))
  
  sitedata[[i]]  <-    out_df %>% 
    separate_wider_position(X1,
                            c(prefix = 5, 
                              course_number = 8),
                            too_many = "debug") %>% 
    mutate(student_count = str_extract(X1, "\\d{1,3}/\\s*\\d{1,3}")) %>% 
    mutate(actual_number_of_students = str_extract(student_count, "^\\d{1,3}")) %>% 
    mutate(max_number_of_students   = str_extract(student_count, "\\d{1,3}$"))
  
}

names(sitedata) <- qtryear

sitedata_df <- 
  bind_rows(sitedata, .id = "qtryear") %>% 
  separate_wider_position(qtryear, 
                          c(qtr = 3, year = 4),
                          too_many = "drop") %>% 
  mutate(qtr_num = case_when(
    qtr == "AUT" ~ 3,
    qtr == "WIN" ~ 6,
    qtr == "SPR" ~ 9
  )) %>% 
  mutate(year_qtr = paste0(year, ".", qtr_num)) %>% 
  mutate(year_qtr = parse_number(year_qtr),
         course_number = parse_number(course_number),
         actual_number_of_students = parse_number(actual_number_of_students),
         max_number_of_students = parse_number(max_number_of_students)) %>% 
  # undergrad only
  filter(course_number <= 499) %>% 
  # drop independent study
  filter(!course_number %in% c(299, 499)) %>% 
  # course level
  mutate(level = case_when(
    between(course_number, 100, 199) ~ "100-level classes",
    between(course_number, 200, 299) ~ "200-level classes",
    between(course_number, 300, 399) ~ "300-level classes",
    between(course_number, 400, 499) ~ "400-level classes"
  ))


# plot counts of students --------------------------------------
require(shadowtext)
require(ggplot2)
require(patchwork)
# split
dfs1 = split(sitedata_df, f = sitedata_df$level)
# apply ggplot function and write to list
gg_1 = lapply(dfs1, function(x) {
  ggplot(x, aes(x = year_qtr,
                y = actual_number_of_students, 
                group = factor(course_number), 
                label = course_number,
                colour = factor(course_number))) + 
    geom_line() +
    geom_shadowtext(size = 3, bg.colour='white') + 
    facet_wrap(~ level, ncol = 1) +
    guides(colour= "none") +
    theme_minimal() +
    ylab("Students") +
    xlab("") +
    scale_x_continuous(breaks = unique(as.numeric(sitedata_df$year)),
                       guide = guide_axis(angle = 90))
}) 
# patchwork
plot_counts_over_time <- 
wrap_plots(gg_1, ncol = 1) +
  plot_annotation(paste0(prefix, " classes taught AU-WI-SP ", 
                         min(year), "-", max(year)))



# plot ratio of students --------------------------------------

sitedata_df_ratio <- 
  sitedata_df %>% 
  mutate(ratio_of_cap = actual_number_of_students / max_number_of_students)

# split
dfs2 = split(sitedata_df_ratio, f = sitedata_df_ratio$level)
# apply ggplot function and write to list
gg_2 = lapply(dfs2, function(x) {
  ggplot(x, aes(x = year_qtr,
                y = ratio_of_cap, 
                group = factor(course_number), 
                label = course_number,
                colour = factor(course_number))) + 
    geom_line() +
    geom_shadowtext(size = 3, bg.colour='white') + 
    facet_wrap(~ level, ncol = 1) +
    guides(colour= "none") +
    theme_minimal() +
    ylab("") +
    xlab("") +
    scale_x_continuous(breaks = unique(as.numeric(sitedata_df$year)),
                       guide = guide_axis(angle = 90))
}) 
# patchwork

plot_ratios_over_time <- 
wrap_plots(gg_2, ncol = 1) +
  ylab("Ratio of enrollment to capacity") +
  theme(axis.title.y = 
          element_text(margin = 
                         margin(0,0.5,0,0,'cm'),
                       hjust = -1))  +
  plot_annotation(paste0(prefix, " classes taught AU-WI-SP ", 
                         min(year), "-", max(year)))

# boxplots of courses with cap ratio

library(ggbeeswarm)

# split
dfs3 = split(sitedata_df_ratio, f = sitedata_df_ratio$level)
# apply ggplot function and write to list
gg_3 = lapply(dfs3, function(x) {
  median_cap_ratio <- median(x$ratio_of_cap, na.rm = TRUE)
  ggplot(x) +
    aes(
      x = reorder(factor(course_number), 
                  ratio_of_cap),
      y = ratio_of_cap,
      group = factor(course_number)) + 
    geom_boxplot() +
    geom_quasirandom(alpha = 0.3) +
    geom_hline(aes(yintercept = median_cap_ratio),
               colour = "red") +
    facet_wrap(~ level, ncol = 1) +
    guides(colour= "none") +
    theme_minimal() +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    ylab("Ratio of enrollment to capacity") +
    xlab("") 
}) 
# patchwork
plot_boxplots_by_level <- 
wrap_plots(gg_3, 
           ncol = 1,
           axis_titles = "collect") +
  plot_annotation(paste0(prefix, " classes taught AU-WI-SP ", 
                         min(year), "-", max(year)),
                  subtitle = "red line is overall median") 

return(list(plot_counts_over_time = plot_counts_over_time,
            plot_ratios_over_time = plot_ratios_over_time,
            plot_boxplots_by_level = plot_boxplots_by_level ))

}





