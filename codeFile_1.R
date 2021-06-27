library(tidyverse)
# Generate a table, same as previous test but 50 rows
set.seed(42)
# Number of rows in the table
table_rows <- 50 
# Customer names
customer_names <- c("abc", "mno","xyz")
# Data frame
tbl_1 <- data.frame(
  CustomerName = sample(customer_names, table_rows, replace = T),
  InvoiceNum = sort(sample(100:999, table_rows)),
  InvoiceDate = sort(sample(seq(as.Date('2000-01-01'), 
                                as.Date('2000-12-31'), 
                                by="day"), table_rows)
  ),
  InvoiceCurrency = rep("CU",table_rows),
  InvoiceAmt = round(runif(table_rows, min = 100, max = 1000),2), stringsAsFactors = F)

# Display first few rows of the data.frame
head(tbl_1)


# Convert dataframe to xml file
xml_root <- xml2::xml_new_root('table')

# store the dataframe information in the XML
for(r in asplit(tbl_1,1)) {
  nd <- xml2::xml_add_child(xml_root, 'invoice')
  for(r_n in names(r)){
    xml2::xml_add_child(.x=nd, .value = r_n, r[[r_n]] )
  }
}

xml_out_tbl_1 <- here::here('xml_files','xml_out.xml')
invisible(write_xml(xml_root, xml_out_tbl_1))


# Re-write the xml file with attributes

# create root element for the new XML
xml_root_2 <- xml2::xml_new_root('table')

# define children with attributes
for(r in asplit(tbl_1,1)) {
  nd <- xml2::xml_add_child(xml_root_2, 'invoice', r[[length(r)]])
  for(r_n in names(r)){
    xml2::xml_attrs(nd) <- r[-length(r)]
  }
}


# Re-consturct table from XML

# Read xml file
xml_tbl <- xml2::read_xml(xml_out_tbl_2)

# find all invoice elements
invoices <- xml2::xml_find_all(xml_tbl, './/invoice')
values <- xml2::xml_find_all(xml_tbl, './/invoice/text()') %>% xml2::as_list() %>% unlist()

# extract invoice attributes and values from all elements and convert to a dataframe
xml_to_tbl <- xml_attrs(invoices) %>% bind_rows() %>% mutate(InvoiceAmt= as.double(values))
# Correct data types
xml_to_tbl$InvoiceNum <- as.integer(xml_to_tbl$InvoiceNum)
xml_to_tbl$InvoiceDate <- as.Date(xml_to_tbl$InvoiceDate)

# Compare result of conversion to original table
all_equal(xml_to_tbl, tbl_1) # Should return TRUE
  

#specs
recs <- rvest::read_html('https://specifications.xbrl.org/specifications.html') %>% 
  rvest::html_element(xpath = ('.//*[text()="Recommendations"]/following-sibling::ul'))


links <- rvest::html_elements(x = recs, xpath = ('.//a')) %>% 
  map(function(x) {
    ref <- rvest::html_attr(x, 'href')
    span <- rvest::html_text(x, trim = TRUE)
    res <- paste0('[', span, '](', 'https://specifications.xbrl.org/', ref,'){target=_blank}: ' )
    return(res)
  }) %>% unlist 

cat(paste0(paste0('* ',links), collapse = '\n'))
