
list_sites <- function(domain = getOption('googlesites.site_domain')){
  
  url <- sprintf('https://sites.google.com/feeds/site/', domain)
  res <- sites_GET(target_url)
  return(res)
}

# This feature is only available to Google Apps domains.
# return metadata as list
new_site <- function(url, 
                     title = NULL,
                     summary = NULL,
                     theme = NULL,
                     categories = NULL){
  
  # next create the document
  entry <- xml_new_document() %>%
    xml_add_child("entry", 
                  xmlns = "http://www.w3.org/2005/Atom", 
                  `xmlns:sites` = "http://schemas.google.com/sites/2008")
  
  xml_add_child(entry, "title", title)
  
  if(is.null(summary)){
    summary <- title
  }
  xml_add_child(entry, "summary", summary)
  
  xml_add_child(entry, "sites:siteName", url)

  if(!is.null(theme)){
    valid_themes <- c('Ski','Iceberg','Open Sky','Rounders','Slate','Simple',
                      'Ember','Branches','Notebook','Legal Pad','Blank Slate',
                      'Micro Blueprint','Micro Lite','Micro Sport','Madison','Beige and Blue',
                      'Desert Panel','Vinyl Panel','Leather Panel','Lavender Panel','Crystal Panel',
                      'Terra: Water','Terra: Ice','Terra: Ruby','Terra: Rock','Terra: Sand',
                      'Solitude: Cherry','Solitude: Navy','Solitude: Olive','Solitude: Spice',
                      'Solitude: Violet','Sea Foam','Cherry Pie','Charcoal','Sunset','Glitter',
                      'Garden','Treehouse','Retropaint','Horizon','Shipshape','Schoolhouse',
                      'Wintermint','Parchment','Tron','Shortcake','Smoke','Homemade','Toothpaste',
                      'Patchwork','Tiles','Mint Chip','Launch default')
    xml_add_child(entry, "sites:theme", theme)
  }
  
  if(!is.null(categories)){
    add_site_categories(categories)
    xml_add_child(entry, "category", 
                  scheme = "http://schemas.google.com/g/2005#kind", 
                  term = sprintf("http://schemas.google.com/sites/2008#%s", category),
                  label = category)
  }  
  
  res <- sites_POST(target_url = 'https://sites.google.com/feeds/site/site/', 
                    body = entry)
}  

# This feature is only available to Google Apps domains.
copy_site  <- function(copy_site_url,
                       new_site_url,
                       title = NULL,
                       summary = NULL,
                       category = NULL){
  
  # next create the document
  entry <- xml_new_document() %>%
    xml_add_child("entry", 
                  xmlns = "http://www.w3.org/2005/Atom", 
                  `xmlns:sites` = "http://schemas.google.com/sites/2008")
  
  xml_add_child(entry, "link", 
                rel = "http://schemas.google.com/sites/2008#source",
                type = "application/atom+xml",
                href = copy_site_url)
  
  xml_add_child(entry, "title", title)
  
  if(is.null(summary)){
    summary <- title
  }
  xml_add_child(entry, "summary", summary)
  
  xml_add_child(entry, "sites:siteName", new_site_url)

  return(entry)
}


update_site_metadata <- function(site_name,
                                 title = NULL,
                                 summary = NULL){
  
  # next create the document
  entry <- xml_new_document() %>%
    xml_add_child("entry", 
                  xmlns = "http://www.w3.org/2005/Atom", 
                  `xmlns:sites` = "http://schemas.google.com/sites/2008", 
                  `gd:etag` = "&quot;YD0peyY.&quot;")
  
  
  url <- sprintf('https://sites.google.com/feeds/site/site/%s', 'testrgooglsites')
  xml_add_child(entry, "id", url)
  
  
  xml_add_child(entry, "title", title)
  
  if(is.null(summary)){
    summary <- title
  }
  xml_add_child(entry, "summary", summary)
  res <- sites_PUT(target_url = url, body = entry)
}

# # This feature is only available to Google Apps domains.
# add_site_categories()
# 
# 
# get_site_address_map()
# 
# 
# modify_site_address_map()
