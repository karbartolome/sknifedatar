automagic_tabs <- function(input_data , panel_name, .output, .layout = NULL, ...){

  #Capture extra arguments
  list_arguments <- list(...)
  .arguments <- names(list_arguments)
  .inputs <- list_arguments %>% unlist() %>% unname()
  parse_extra_argumnets <- NULL
  if(!is.null(.arguments)) parse_extra_argumnets <- purrr::map2(.arguments,.inputs, ~paste(.x,.y, sep = " = ")
  ) %>% unlist() %>% paste(collapse=" , ")

  #Capture name of data
  data_name <- match.call()
  data_name <- as.list(data_name[-1])$input_data %>% as.character()

  #Layaout page
  if(is.null(.layout)) .layout <- "l-body"
  if(!.layout %in% c("l-body","l-body-outset","l-page","l-screen")) stop('the specified layout does not match those available. c("l-body","l-body-outset","l-page","l-screen")')
  layaout_code <- paste0("::: {.",.layout,"}\n::: {.panelset}\n")

  #knit code
  knit_code <- NULL
  for (i in 1:nrow(input_data)) {
    #Capture time to diference same chunks
    time_acual <- paste0(Sys.time() %>% lubridate::hour() %>% as.character(),
                         Sys.time() %>% lubridate::minute() %>% as.character(),
                         Sys.time() %>% lubridate::second() %>% as.character())

    knit_code_individual <- paste0(":::{.panel}\n### `r ", data_name,"$",panel_name,"[[",i,
                                   "]]` {.panel-name}\n```{r   `r ", i,
                                   time_acual,
                                   "`, echo=FALSE, layout='",.layout,"', ",
                                   parse_extra_argumnets,
                                   "}\n\n ",data_name,"$",.output,"[[",i,
                                   "]] \n\n```\n:::")

    knit_code <- c(knit_code, knit_code_individual)

  }

  #layout code + knit code + close ::: :::
  knit_code <- c(layaout_code,knit_code,"\n:::\n:::")

  #knirt code
  paste(knitr::knit(text = knit_code), collapse = '\n')

}
