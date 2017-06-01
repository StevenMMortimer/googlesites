#' Find an Entry on a Google Site
#' 
#' This function takes a value and field to search, then 
#' runs GET functions against the site to return 
#' metadata on any matched entries
#'
#' @keywords googlesites documentation wiki api find entry webpage announcement attachment
#' @source utils.R
#' @param value_to_match A string that should match an entry on the target google site
#' @param field_to_match A string indicating which field of the entry the match should be 
#' compared against.
#' @param use_regex_matching A logical indicating whether to take the \code{value_to_match} and 
#' use it to matched as-is similar to the \code{fixed} argument in \code{grep}. 
#' @param parent_page_id A string representing the entry id of a webpage
#' where the content should be located. This value is typically formatted like: 
#' https://sites.google.com/feeds/content/domainName/siteName/PARENT_ENTRY_ID. 
#' Leave the value NULL to not restrict results at all or leave blank to specify 
#' entries at the top-level of the site.
#' @param content_category A string or a collection of strings that specify which types of entries to search against. If 
#' none are provided then all entries are scanned. This argument acts as a filter to speed up the search process. Accepted 
#' values are announcement, announcementspage, attachment, comment, filecabinet, listitem, listpage, webpage, webattachment, template
#' @param site_domain A string representing 'site' or the domain of your Google Apps hosted domain (e.g. example.com)
#' @param site_name A string representing the webspace name of your site; found in the Site's URL (e.g. myCoolSite)
#' @param verbose A logical indicating whether to print messages
#' @return A list of length two consisting of a URL for the created attachment and the Google Sites Entry ID
#' @examples
#' \dontrun{
#' # find a page entitled My Report
#' page_id <- find_content(value_to_match='My Report', 
#'                         field_to_match='title', 
#'                         content_category='webpage')
#' 
#' # find all templates                     
#' page_id <- find_content(value_to_match='*', 
#'                         field_to_match='title', 
#'                         use_regex_matching=TRUE,
#'                         content_category='template')
#' }
#' @export
find_content <- function(value_to_match, 
                         field_to_match=c('id', 'url', 'title'),
                         use_regex_matching=FALSE,
                         parent_page_id=NULL,
                         content_category='', 
                         site_domain=getOption("googlesites.site_domain"),
                         site_name=getOption("googlesites.site_name"),
                         verbose=FALSE){
  
  stopifnot(!is.null(site_name))
  
  not_found_df <- data.frame(id='', 
                             url='', 
                             title='',
                             category='',
                             parent_page_id='', 
                             content_type='',
                             stringsAsFactors = FALSE)
  
  if(value_to_match == '') return(not_found_df)
  
  # pull down all existing entries and compare based on file title
  # because this will determine whether we overwrite the file or not
  all_entries <- get_entries(content_category = content_category,
                             site_domain = site_domain,
                             site_name = site_name,
                             verbose = verbose)
  
  if(length(all_entries) > 0){
    all_entries_metadata <- plyr::ldply(all_entries, 
                                        .fun=function(x){
                                          id <- x %>% 
                                            xml_find_all("id") %>% 
                                            xml_text() 
                                          url <- x %>% 
                                            xml_find_all('link[@rel="alternate"]') %>% 
                                            xml_attr('href')
                                          category <- x %>% 
                                            xml_find_all('category') %>% 
                                            xml_attr('label') %>%
                                            paste0(.,collapse="|")
                                          title <- x %>% 
                                            xml_find_all("title") %>% 
                                            xml_text()
                                          parent_page_id <- x %>% 
                                            xml_find_all('link[@rel="http://schemas.google.com/sites/2008#parent"]') %>% 
                                            xml_attr('href')
                                          content_type <- x %>% 
                                            xml_find_all("content") %>% 
                                            xml_attr('type')
                                          return(data.frame(id = if(length(id) != 1) '' else id, 
                                                            url = if(length(url) != 1) '' else url, 
                                                            category = if(length(category) != 1) '' else category, 
                                                            title = if(length(title) != 1) '' else title, 
                                                            parent_page_id = if(length(parent_page_id) != 1) '' else parent_page_id, 
                                                            content_type = if(length(content_type) != 1) '' else content_type,
                                                            stringsAsFactors = FALSE))
                                        })
    
    # currently there is a bug with google where 
    # they provide X number of entries and then set the 
    # start-index at that same number as the last entry
    # so we'll end up repeating that entry if we don't 
    # push the start-index up by 1. For example they are 
    # providing entries 1 thru 26, then setting the start-index on 26.
    # to workaround, just remove any duplicated ids since that will be unique
    all_entries_metadata <- all_entries_metadata[!duplicated(all_entries_metadata$id), ]
    
    
    # determine how the matching should be performed
    pattern <- value_to_match
    if(!use_regex_matching){
      pattern <- paste0('^', value_to_match, '$')
    } else {
      pattern <- value_to_match
    }
    
    # find the matching entries
    if(is.null(parent_page_id)){
      entry <- all_entries_metadata[grepl(pattern, all_entries_metadata[,field_to_match]), ]
    } else {
      entry <- all_entries_metadata[grepl(pattern, all_entries_metadata[,field_to_match]) & 
                                      all_entries_metadata$parent_page_id == parent_page_id, ]
    }
    
    if(nrow(entry) > 1){
      if(verbose) message(sprintf('%i entries were found with %s "%s".', nrow(entry), field_to_match, value_to_match))
    } else if(nrow(entry) == 0){
      if(verbose) message(sprintf('An entry with %s "%s" was not found', field_to_match, value_to_match))
      entry <- not_found_df
    } else {
      # we found one id so this will be updated
      if(verbose) message(sprintf('Exactly one entry with %s "%s" was found', field_to_match, value_to_match))
    }
  } else {
    entry <- not_found_df
  }
  return(entry)
}


get_entries <- function(content_category='',
                        site_domain,
                        site_name, 
                        verbose=FALSE){

  stopifnot(!is.null(site_name))
  
  # supported category items
  # https://developers.google.com/google-apps/sites/docs/1.0/reference#feed_Content
  accepted_entry_categories <- c('announcement', 'announcementspage', 
                                 'attachment', 'comment', 'filecabinet', 
                                 'listitem', 'listpage', 'webpage', 'webattachment', 
                                 'template')
  
  if(any(content_category != '')){
    content_category <- tolower(content_category)
    content_category <- content_category[content_category != '']
    new_content_category <- content_category[content_category %in% accepted_entry_categories]
    if(length(content_category) < 1){
      message(sprintf('content_category: "%s" is not in the list of accepted categories: ', 
                      content_category, 
                      paste0('"', paste0(accepted_entry_categories, collapse='","'), '"')))
      message('The process will proceed by pulling entries from all categories')
      new_content_category <- ''
    } else if(length(new_content_category) < length(content_category)){
      message(sprintf('Some categories could be found in the accepted list. 
                      The process will proceed by pulling entries from the following categories: %s', 
                      paste0('"', paste0(new_content_category, collapse='","'), '"')))
      new_content_category <- paste0(new_content_category, collapse='|')
    } else {
      # The process will proceed by pulling entries from all categories specified
      new_content_category <- paste0(new_content_category, collapse='|')
    }
  } else {
    new_content_category <- ''
  }
  
  if(new_content_category != ''){
    new_content_category <- paste0('/-/', new_content_category)
  }
  
  target_url <- sprintf('https://sites.google.com/feeds/content/%s/%s%s',
                        site_domain, 
                        site_name, 
                        new_content_category)

  req_xml <- sites_GET(url = target_url)
  req_xml_cleaned <- xml_ns_strip(req_xml)
  
  # use c() to conver to a collection immediately
  # since that is what will happen when we scoop up
  # additional nodesets
  all_entries <- req_xml_cleaned %>% 
    xml_find_all('.//entry') %>%
    c()
  
  this_next_url <- req_xml_cleaned %>% 
    xml_find_all("link[@rel='next']") %>% 
    xml_attr('href')
  
  if(length(this_next_url) == 1){
    if(verbose) message('grabbing more entries')
    all_entries <- get_more_entries(get_url=this_next_url, 
                                    entries=all_entries)
  }
  
  return(all_entries)
}

get_more_entries <- function(get_url, 
                             entries, 
                             verbose=FALSE){
  
  req_xml <- sites_GET(get_url)
  req_xml_cleaned <- xml_ns_strip(req_xml)
  
  more_entries <- req_xml_cleaned %>% 
    xml_find_all('.//entry') %>%
    c()
  
  entries <- c(entries, more_entries)
  
  this_next_url <- req_xml_cleaned %>% 
    xml_find_all("link[@rel='next']") %>% 
    xml_attr('href')
  
  if(length(this_next_url) == 1){
    if(verbose) message('grabbing more entries')
    entries <- get_more_entries(get_url=this_next_url, 
                                entries=entries)
  }
  
  return(entries)
}

# this function makes sure that everybody posts their article with a common set of 
# tags so that we don't run over each other with a variety of tags
# we want to stay within the same bounds and reuse tags so they have more meaning
validate_page_tags <- function(tags_to_check, 
                               tag_dictionary, 
                               parent_page_title=''){
  
  # here we assume that the parent page is in the first column and
  # the valid tags are in the second column, just in case someone screws 
  # with the names in the google sheet
  valid_tags <- unique(as.data.frame(tag_dictionary[tag_dictionary[,1] == parent_page_title,])[,2])
  if(length(valid_tags) < 1){
    message('no valid tags found for this post_parent_title, all tags are invalid')
  }
  
  res <- tags_to_check %in% valid_tags
  names(res) <- tags_to_check
  
  return(res)
  
}

replace_local_paths <- function(body, 
                                targets,
                                base_dir,
                                parent_page_id, 
                                overwrite, 
                                site_domain, 
                                site_name){
  
  stopifnot(!is.null(site_name))
  
  tags_of_interest <- lapply(seq.int(length(targets)), 
                             FUN=function(i, targets){
                               target_element <- targets[[c(i)]]$target_element
                               target_attribute <- targets[[c(i)]]$target_attribute
                               tags_of_interest <- body %>%
                                 xml_find_all(xpath=paste0('.//', 
                                                           target_element, 
                                                           '[@', target_attribute, ']')) %>% 
                                 xml_attr(target_attribute)
                               return(tags_of_interest)
                             }, targets=targets)
  
  tags_of_interest <- unlist(tags_of_interest)
  
  # find all local path references
  # replace with the absolute path
  tags_of_interest <- unique(tags_of_interest[!grepl('^http://|^https://', tags_of_interest)])
  original_wd <- getwd()
  setwd(base_dir)
  tags_w_local_paths <- normalizePath(tags_of_interest, winslash = '/', mustWork=FALSE)
  setwd(original_wd)
  names(tags_w_local_paths) <- tags_of_interest
  
  uploaded_tags <- list()
  if(length(tags_w_local_paths) >= 1){
    message('Uploading Local Files to Sites Page')
    uploaded_tags <- plyr::llply(tags_w_local_paths, 
                                 .fun=function(x, 
                                               parent_page_id, 
                                               overwrite, 
                                               site_domain, 
                                               site_name){
                                   res <- NULL
                                   if(file.exists(x)){
                                     res <- upload_file_to_site(local_file_path=x,
                                                                parent_page_id=parent_page_id,
                                                                overwrite=overwrite,
                                                                site_domain=site_domain,
                                                                site_name=site_name)
                                   }
                                   return(res)
                                 },
                                 parent_page_id=parent_page_id, 
                                 overwrite=overwrite, 
                                 site_domain=site_domain, 
                                 site_name=site_name, 
                                 .progress="text")
  }
  
  # only include the non-null tags
  # tags could be null if the file path
  # could not be confirmed locally
  uploaded_tags <- uploaded_tags[!sapply(uploaded_tags, is.null)]
  
  # convert the body to character just so that we can
  # gsub the needed src and href elements
  body <- as.character(body)
  
  for(n in names(uploaded_tags)){
    for(i in seq.int(length(targets))){
      animator_addon <- if(targets[[c(i)]]$target_element %in% c('a', 'img')) '?attredirects=0' else ''
      body <- gsub(paste0('(\\<', 
                          targets[[c(i)]]$target_element, 
                          ' )(.*)(', 
                          targets[[c(i)]]$target_attribute, 
                          '=")(', n, ')(".*\\>)'), 
                   paste0('\\1\\2\\3', 
                          uploaded_tags[[n]]$file_url, animator_addon, 
                          '\\5'), 
                   body, 
                   perl=TRUE)
    }
  }
  
  # convert body back to xml
  body <- read_xml(body)
  
  return(body)
}

construct_entry <- function(page_title, 
                            category,
                            body,
                            parent_page_id='', 
                            page_template_id='',
                            page_custom_url=''){
  
  # next create the document
  entry <- xml_new_document() %>%
    xml_add_child("entry", 
                  xmlns = "http://www.w3.org/2005/Atom", 
                  `xmlns:sites` = "http://schemas.google.com/sites/2008")
  
  xml_add_child(entry, "category", 
                scheme = "http://schemas.google.com/g/2005#kind", 
                term = sprintf("http://schemas.google.com/sites/2008#%s", category),
                label = category)
  
  if(parent_page_id != ''){
    xml_add_child(entry, "link", 
                  rel = "http://schemas.google.com/sites/2008#parent",
                  type = "application/atom+xml",
                  href = parent_page_id)
  }
  
  xml_add_child(entry, "title", page_title)
  
  if(page_template_id != ''){
    xml_add_child(entry, "link", 
                  rel = "http://schemas.google.com/sites/2008#template",
                  type = "application/atom+xml",
                  href = page_template_id)
  } else {
    xml_add_child(entry, "content", type="xhtml") %>%
      xml_add_child("div", xmlns = "http://www.w3.org/1999/xhtml") %>% 
      xml_add_child(body)
  }
  
  if(page_custom_url != ''){
    xml_add_child(entry, "sites:pageName", page_custom_url)
  }
  
  return(entry)
  
}
