
#+ fn_Getpkgs, eval=FALSE
fn_Getpkgs <- function(codeFile) {
  #def fn_Getpkgs: Gets all packages used in this project (wip)
  #@ codeFile: path to file(s) containing code
  srcx <- srcfile('abcdefxxx')
  f <- codeFile
  pkgs <- parse(text = readLines(f), srcfile = srcx) %>%  getParseData() %>% select(token, text) %>%  
    filter(token == "SYMBOL_PACKAGE") %>% select(text) %>%  unlist() %>% unname() %>% 
    append(c("forcats", "stringr", "dplyr", "purrr", "readr", "tidyr", "tibble",
             "ggplot2", "tidyverse")) %>% unique() %>% # Tidyverse packages added manually
    map(function(x){
      x <- packageDescription(x)
      x <- list(Package= x$Package, Description = x$Title, Version = x$Version)
      return(x)
    }) %>%  map_df(function(x){
      url <- paste0("https://CRAN.R-project.org/package=", x$Package)
      x <- x %>% unlist %>% t() %>% data.frame(stringsAsFactors = F) %>% 
        mutate(Package = 
                 if(RCurl::url.exists(url)) {
                   htmltools::tags$a(href = url,target="_blank", Package) %>% as.character()
                 } else {Package}
        )
      return(x)
    }) %>%  select(1,3,2) %>% arrange(Package)
  return(pkgs) # Returns a dataframe
  
  #' Use knitr::purl to get everything from everywhere and vectorize.
  #' Reminder: Notebook wd != project wd
  #' read_lines("./Notebook/GoalOneCover.Rmd")
  #' getParseData(parse(text = knitr::purl(text = l)))
}
#-

#+ fn_GetHelpers, eval=FALSE
fn_GetHelpers <- function(fnfile, lheight=.2, bkgclr = "#f9fafb21", hieght = 250) {
  #def fn_GetHelpers: A diffrent way to display helper functions with syntax highlighting.
  #@ fnfile path to file containing defined functions
  #@ lheight: output line hight, for some reason in notebook output the line have weird spacing.
  #@ bkgclr: background color, set it a bit different from usual code chunks
  #@ hieght: hight of code div
  f <- tempfile(tmpdir = getwd(),fileext = ".html")
  #' First render this script using rmarkdown to populate syntax highlighting
  rndr <- rmarkdown::render(input = fnfile,
                            output_format = rmarkdown::html_document(
                              theme= "flatly",
                              highlight= "haddock", 
                              toc= T),
                            output_file = f,quiet = T, intermediates_dir = getwd())
  #' steal the formatted syntax from the markdown document
  x <- xml2::read_html(rndr)
  x <- xml2::xml_find_all(x = x, xpath = ".//pre")
  xml2::xml_attr(x = x, "class") <- ""
  xml2::xml_attr(x = x, "style") <- paste0("line-height: ",lheight,";background-color: ",bkgclr,";max-height:", hieght,"px;")
  y <- map(x, function(t){
    def <- xml2::xml_text(xml2::xml_find_all(t, ".//*[starts-with(text(),'#def')]"))
    def <- def %>% str_replace("^#def","") %>% str_split(":") %>%
      purrr::map(trimws) %>% unlist()
    tdef <- xml2::xml_parent(xml2::xml_find_all(t, ".//*[starts-with(text(),'#def')]"))
    xml2::xml_remove(tdef)
    def <-  list(
      a = htmltools::tags$strong(def[1],style="font-weight: bold;font-size:medium;"),
      b = htmltools::tags$span(def[2],style="font-style: italic;"))
    t <- htmltools::tags$details(htmltools::tags$summary(def),
                                 htmltools::HTML(as.character(t)))
    unlink(rndr)
    return(t)
  })
  y <- htmltools::tags$div(y, id="helperFuncs2")
  return(y)
}
#-

#+ fn_findBookmarks, eval=FALSE
fn_findBookmarks <- function(fleloc, bkmrkfldr) {
  #def fn_findBookmarks: Extracts bookmarks from google chrome bookmarks folder (JSON file).
  #@ fleloc: path to JSON like file containing chrome book marks
  #@ bkmkfldr: name of bookmarks folder
  x <- jsonlite::read_json(fleloc)
  y <- bkmrkfldr
  find_inner <- function(z, v) {
    if(length(which(z==v))>0) {
      assign("tx", z, envir = parent.env(environment()))
    }  else { 
      lz <- length(z)
      x <- if(lz>0) {
        for(i in 1:lz) {
          if(is.list(z[[i]])) {
            find_inner(z[[i]], v)
          } 
        }
      } 
    }
  }
  find_inner(x, y)
  if(exists("tx")) { 
    return(tx) #r Returns a list containing the requested bookmarks folder
  } else {
    stop(paste0("Folder ", bkmrkfldr, " not found"))
  }
}
#-

#+ fn_CodeChunkOut, eval=FALSE
fn_CodeChunkOut <- function(lang = "R", File = NULL, txt=NULL,
                           lheight=.2, bkgclr = "#f9fafb21", hieght = 200 ) {
  #def fn_CodeChunkOut: Displays code string as code chunk with syntax highlighting.
  #@ lang: code language
  #@ File: xml file to display
  #@ txt: code as string
  #@ lheight: output line hight, for some reason in notebook output the line have weird spacing.
  #@ bkgclr: background color, set it a bit different from usual code chunks
  #@ hieght: hight of code div
  f_i <- tempfile(tmpdir = getwd(),fileext = ".R")
  f_o <- tempfile(tmpdir = getwd(),fileext = ".html")
  if(!is.null(txt)) {
    linez <- unlist( str_split(as.character(txt), "\n"))
  } else if(!is.null(File)) {
    linez <- read_lines(File)
  } else {
    stop("a File OR txt must be provided")
  }
  x <-c(paste0("#'```{", lang, " eval=FALSE, results='hide'}"),paste("#'",linez),"#'```")
  writeLines(x,f_i)
  rndr <- suppressWarnings(
    rmarkdown::render(
      input =  f_i,
      output_format = rmarkdown::html_document(
        theme= "flatly",
        highlight= "haddock", 
        toc= T),
      output_file = f_o,quiet = T, 
      intermediates_dir = getwd()
    )
  ) 
  
  x <- xml2::read_html(rndr)
  x <- xml2::xml_find_all(x = x, xpath = ".//pre")
  xml2::xml_attr(x = x, "class") <- NULL
  xml2::xml_attr(x = x, "style") <- paste0("line-height: ",lheight,";background-color: ",bkgclr,";max-height:", hieght,"px;")
  x <- htmltools::HTML(as.character(x))
  unlink(x = c(f_i, f_o))
  return(x)
}
#-

#+ fn_xmlToList, eval=FALSE
fn_xmlToList <- function(doc) {
  #def fn_xmlToList: Convert XML doc to list in a specific format
  #@ doc: xml doc result of xml2::read_xml()
  doc_len <- xml2::xml_length(doc)
  len_doc_len <- length(doc_len)
  if (doc_len==0 & len_doc_len==1) return(
    # Return leaf level, if it progress then doc have subs
    list(
      type = xml2::xml_name(doc),
      text = doc %>% xml2::xml_find_first('./text()') %>% 
        as.character() %>% trimws(),
      attributes = xml2::xml_attrs(doc) %>% unlist() %>% as.list()
    )
  )
  # Store text and attributes for current level element
  l <- list()
  nm <- xml2::xml_name(doc)
  tx <- doc %>% xml2::xml_find_first('./text()') %>% 
    as.character() %>% trimws()
  attrs <- xml2::xml_attrs(doc) %>% unlist() %>% as.list()
  
  # Naming root vs children level
  if(xml2::xml_length(xml2::xml_parents(doc))==0) {
    l[[nm]] <- list(type = nm, text = tx, attributes = attrs)
  } else {
    l$type <- nm
    l$text <- tx 
    l$attributes <- attrs
  }
  
  # Deal with multiple children
  ## extract children names
  zz <- unique(xml2::xml_name(xml2::xml_children(doc))) 
  ## group elements by name
  for (j in  zz) {
    doc_j <- xml2::xml_find_all(doc, paste0('.//*[local-name()="', j ,'"]'))
    ## deal with sub children if any depending on if it is root or not
    ## the last else is the return above
    for (i in 1:length(doc_j)){
      if(xml2::xml_length(xml2::xml_parents(doc))==0){
        l[[nm]][[j]][[i]] <- Recall(
          xml2::xml_find_all(doc,paste0('.//*[local-name()="', j ,'"][',i,']'))
        )
        
      } else {
        l[[j]][[i]] <- Recall(
          xml2::xml_find_all(doc,paste0('.//*[local-name()="', j ,'"][',i,']'))
          
        )
      }
      
    } 
  }
  
  l
}
#-

#+ fn_cmmnt_call_res, eval=FALSE
fn_cmmnt_call_res <- function(cmmnt_txt, fn_txt, invisible_ = T) {
  #def fn_cmmnt_call_res: Returns call together with result and comment.
  #@ cmmnt_txt: comment to show as first line before call
  #@ fn_txt: online call as text (quoted)
  #@ invisible_: Show result of the call or not
  func <- parse(text = fn_txt)
  if(invisible_) {
    eval(func, envir = globalenv())
    cat(
      paste(cmmnt_txt, fn_txt, 
            sep = "\n"
      ))
  } else {
    cat(
      paste(cmmnt_txt, fn_txt,
            eval(func, envir = globalenv()),
            sep = "\n"
      ))
  }
}
#-

#+ fn_testTwo, eval=FALSE
fn_testTwo <- function(x,y){
  #def fn_testTwo: Test function Two
  fs::dir_tree()
  here::here()
  jsonlite::fromJSON()
  return(x*y)
}
#-


