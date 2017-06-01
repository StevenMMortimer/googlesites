#' Upload a local file to a Google Sites
#' 
#' This function uploads a local file (png, pdf, or other media) via API
#'
#' @keywords googlesites documentation wiki api login upload file media jpg png pdf
#' @importFrom httr upload_file stop_for_status
#' @importFrom xml2 xml_new_document xml_add_child write_xml read_xml xml_find_all xml_text xml_attr xml_ns_strip
#' @importFrom mime guess_type
#' @source utils.R
#' @param local_file_path A string representing a local file path where the target file exists to be uploaded
#' @param file_summary A string an (optional) description of the attachment when created
#' @param parent_page_id A string representing the absolute URL that identifies the entry id of a webpage
#' where the uploaded file will be attached. This should be an absolute URL looking like:
#' https://sites.google.com/feeds/content/mydomain/mysite/6477233125232797597. You can find this URL 
#' by using the \code{\link{find_content}} function.
#' @param overwrite A logical value. TRUE means that the function will identify and overwrite any
#' Google Site attachments that exists with the same name (case-insensitive). FALSE means 
#' fail whenever a conflicting attachment name exists on the site
#' @param site_domain A string representing 'site' or the domain of your Google Apps hosted domain (e.g. example.com)
#' @param site_name A string representing the webspace name of your site; found in the Site's URL (e.g. myCoolSite)
#' @return A list of length two consisting of a URL for the created attachment and the Google Sites Entry ID
#' @examples
#' \dontrun{
#' file <- upload_file_to_site(local_file_path='./files/test.png')
#' }
#' @export
upload_file_to_site <- function(local_file_path,
                                file_summary='',
                                parent_page_id='',
                                overwrite=TRUE,
                                site_domain=getOption('googlesites.site_domain'),
                                site_name=getOption('googlesites.site_name')){
  
  stopifnot(!is.null(site_name))
  
  # normalize the file name per Google rules
  # https://developers.google.com/google-apps/sites/docs/1.0/developers_guide_protocol#ContentFeedPOST
  # however, leave in the period since it helps denote file type. This is the one deviation
  # from Google's normalization routine but it helps your browser know how to treat it
  # during the download
  file_title <- gsub('[^a-zA-Z0-9-_.]+', '', gsub('[[:space:]]+', '-', tolower(basename(local_file_path))), perl=T)
  
  file_id <- find_content(value_to_match=file_title, 
                          field_to_match='title',
                          parent_page_id=parent_page_id,
                          content_category='attachment', 
                          site_domain=site_domain, 
                          site_name=site_name)$id
  
  if(overwrite == FALSE & file_id != ''){
    stop(message('file already exists and overwrite == FALSE'))
  }
  
  # create the metadata
  entry <- xml_new_document() %>%
    xml_add_child("entry", 
                  xmlns = "http://www.w3.org/2005/Atom", 
                  `xmlns:sites` = "http://schemas.google.com/sites/2008")
  
  xml_add_child(entry, "category", 
                scheme = "http://schemas.google.com/g/2005#kind", 
                term = "http://schemas.google.com/sites/2008#attachment", 
                label = "attachment")
  
  if(parent_page_id != ''){
    xml_add_child(entry, "link", 
                  rel = "http://schemas.google.com/sites/2008#parent",
                  type = "application/atom+xml",
                  href = parent_page_id)
  }
  
  xml_add_child(entry, "title", file_title)
  
  if(file_summary != ''){
    xml_add_child(entry, "summary", file_summary)
  }
  
  metadata <- tempfile()
  write_xml(entry, file=metadata)
  
  if(file_id != ''){
    
    # delete and recreate
    # it's easier than modifying because 
    # there is a problem if the file type
    # of the new modification is different
    # than what created it
    message(sprintf('Overwriting the existing file with title: "%s"', file_title))
    
    target_url <- sprintf('https://sites.google.com/feeds/content/%s/%s/%s', 
                          site_domain, 
                          site_name, 
                          basename(file_id))
    this_body = list(
      metadata = upload_file(metadata, type = "application/atom+xml"),
      media = upload_file(local_file_path, type = guess_type(file=local_file_path))
    )
    
    req_xml <- sites_PUT(url = target_url, 
                         body = this_body,
                         content_type = 'multipart/related')
    req_xml_cleaned <- xml_ns_strip(req_xml)
    
    file_id <- req_xml_cleaned %>% 
      xml_find_all("id") %>% 
      xml_text()
    file_url <- req_xml_cleaned %>% 
      xml_find_all("link[@rel='alternate']") %>% 
      xml_attr('href')
    
  } else {
    
    target_url <- sprintf('https://sites.google.com/feeds/content/%s/%s', 
                          site_domain, 
                          site_name)
    this_body = list(
      metadata = upload_file(metadata, type = "application/atom+xml"),
      media = upload_file(local_file_path, type = guess_type(file=local_file_path))
    )
    
    req_xml <- sites_POST(url = target_url, 
                          body = this_body,
                          content_type = 'multipart/related')
    req_xml_cleaned <- xml_ns_strip(req_xml)
    
    file_id <- req_xml_cleaned %>% 
      xml_find_all("id") %>% 
      xml_text()
    file_url <- req_xml_cleaned %>% 
      xml_find_all("link[@rel='alternate']") %>% 
      xml_attr('href')
  }
  
  return(list(file_id = file_id, 
              file_url = file_url))
}


#' Create a page on Google Sites
#' 
#' This function takes metadata and a file path to an HTML
#' document and pushes that HTML up to a Google Site
#'
#' @keywords googlesites documentation wiki api create page webpage
#' @importFrom xml2 read_xml xml_add_child xml_new_document xml_add_sibling xml_ns_strip
#' @importFrom stringi stri_split_boundaries stri_trim_both stri_trans_totitle
#' @source utils.R
#' @param page_xhtml_source A string representing the path to a local HTML file that can be successfully 
#' read into R as XML. Google SItes data must be valid XML so this check is performed by \code{xml2::read_xml()}
#' @param page_parent_page_title A string (optional) with the title of a webpage on the existing Google 
#' Site where this particular post will be created as a subpage
#' @param page_template_id A string (optional) with id of a template on the existing Google 
#' Site that this particular post will inherit
#' @param page_title A string (optional) that will be used as the page Title on the created post. If not
#' specified, then the title will be the same as the HTML file name, excluding .html and santitized to 
#' only include letters, numbers, hypens, or underscores.
#' @param page_custom_url A string (optional) that will be used as the page URL on the created post. If not
#' specified, then the url will be the same as the HTML file name, excluding .html and santitized to 
#' only include letters, numbers, hypens, or underscores.
#' @param overwrite A logical indicating whether to overwrite any existing pages on the Google Site with the
#' same title or url. Matches on url are prioritized above matches with same title, meaning that if a page with 
#' the same title and another page with the same url are found, then we will overwrite the one with the same url
#' since, technically, Google Sites pages with the same title are allowed, even though we would prefer to keep those
#' unique under each parent page
#' @param site_domain A string representing 'site' or the domain of your Google Apps hosted domain (e.g. example.com)
#' @param site_name A string representing the webspace name of your site; found in the Site's URL (e.g. myCoolSite)
#' @examples
#' \dontrun{
#' # add a page
#' test_html <- system.file("inst", "test.html", package="googlesites")
#' add_html_page(page_xhtml_source = test_html)
#' 
#' # add a page and overwrite the existing one in event it already exists
#' sample_html <- system.file("extdata", "sample-doc.html", package="googlesites")
#' add_html_page(page_xhtml_source = sample_html,
#'               page_title = 'Post with GIF',
#'               page_custom_url = 'post-with-gif', 
#'               overwrite=TRUE)
#' }
#' @export
add_html_page <- function(page_xhtml_source,
                          page_parent_page_title='',
                          page_template_id='',
                          page_title='',
                          page_custom_url='',
                          overwrite=TRUE,
                          site_domain=getOption('googlesites.site_domain'),
                          site_name=getOption('googlesites.site_name')){
  
  stopifnot(!is.null(site_name))
  
  # check if the page source exists, otherwise
  # we can't proceed in posting it, so stop immediately
  stopifnot(file.exists(page_xhtml_source))
  
  # find the parent_page_id
  # run a call to determine the id for pages with a certain name
  parent_page_entry_metadata <- find_content(value_to_match=page_parent_page_title,
                                             field_to_match='title',
                                             content_category=c('webpage', 'announcementspage'), 
                                             site_domain=site_domain, 
                                             site_name=site_name)
                              
  parent_page_id <- parent_page_entry_metadata$id
  parent_page_category <- parent_page_entry_metadata$category
  
  # determine the title of this post if it is not specified
  if(page_title == ''){
    page_title <- gsub('\\.html$', '', basename(page_xhtml_source))
    # normalize the post url per google rules
    # https://developers.google.com/google-apps/sites/docs/1.0/developers_guide_protocol#ContentFeedPOST
    page_title <- gsub('[^a-zA-Z0-9-_]+', '', 
                       gsub('[[:space:]]+', '-', tolower(page_title)), perl=TRUE)
  }
  
  # check if a post exists with the same title
  # if we're not supposed to overwrite then quit immediately
  # there is no need to proceed
  page_id_by_title <- find_content(value_to_match=page_title,
                                   field_to_match='title',
                                   parent_page_id=parent_page_id,
                                   content_category=if(grepl('announcementspage', parent_page_category)) 'announcement' else 'webpage',
                                   site_domain=site_domain, 
                                   site_name=site_name)$id
  
  if(page_custom_url != ''){
    if(parent_page_id != ''){
      parent_page_url <- find_content(value_to_match=parent_page_id,
                                      field_to_match='id',
                                      content_category=c('webpage', 'announcementspage'),
                                      site_domain=site_domain, 
                                      site_name=site_name)$url
      full_page_custom_url <- sprintf('%s/%s', 
                                      parent_page_url, 
                                      page_custom_url)
    } else {
      full_page_custom_url <- sprintf('https://sites.google.com/%s/%s/%s', 
                                      site_domain, 
                                      site_name, 
                                      page_custom_url)
    }
    
    page_id_by_url <- find_content(value_to_match=full_page_custom_url,
                                   field_to_match='url',
                                   content_category=if(grepl('announcementspage', parent_page_category)) 'announcement' else 'webpage',
                                   site_domain=site_domain, 
                                   site_name=site_name)$id
    
  } else {
    page_id_by_url <- ''
  }
  
  # prioritize using the url to match
  if(page_id_by_url != ''){
    page_id <- page_id_by_url
    overwrite_name <- sprintf('with URL %s', full_page_custom_url)
  } else {
    if(length(page_id_by_title) > 1){
      stop(sprintf('%i pages were matched on title. Cannot determine which one to overwrite. Please fix and rerun.', length(page_id_by_title)))  
    }
    page_id <- page_id_by_title
    overwrite_name <- sprintf('with title %s', page_title)
  }
  
  if(overwrite == FALSE & page_id != ''){
    stop(paste0('A Post with title: "',
                page_title, '" already exists ', 
                if(page_parent_page_title != '') paste0('under a Parent Page with title: "', page_parent_page_title, '" ') else '',
                'and overwrite == FALSE'))
  }
  
  # determine the url of this post if it is not specified
  if(page_custom_url == ''){
    page_custom_url <- gsub('\\.html$', '', basename(page_xhtml_source))
  }
  # normalize the post url per google rules
  # https://developers.google.com/google-apps/sites/docs/1.0/developers_guide_protocol#ContentFeedPOST
  page_custom_url <- gsub('[^a-zA-Z0-9-_]+', '', gsub('[[:space:]]+', '-', tolower(page_custom_url)), perl=T)
  
  # build up a starter entry and create the page first, so later 
  # we can push attachments to it
  entry <- construct_entry(page_title=page_title, 
                           category=if(grepl('announcementspage', parent_page_category)) 'announcement' else 'webpage',
                           body=xml_new_document() %>% xml_add_child('div', 'Hang Tight! This page is being created'),
                           parent_page_id=parent_page_id, 
                           page_template_id=if(page_id == '') page_template_id else '',
                           page_custom_url=page_custom_url)
  
  if(page_id != ''){
    message(paste('Overwriting Existing Page', overwrite_name))
    req_xml <- sites_PUT(url = page_id, 
                         body = as.character(entry),
                         content_type = 'application/atom+xml')
  } else {
    message('Creating New Page')
    target_url <- sprintf('https://sites.google.com/feeds/content/%s/%s', site_domain, site_name)
    req_xml <- sites_POST(url = target_url, 
                          body = as.character(entry),
                          content_type = 'application/atom+xml')
  }
  message('Successfully Created Page Skeleton')
  req_xml_cleaned <- xml_ns_strip(req_xml)
  
  page_id <- req_xml_cleaned %>% 
    xml_find_all("id") %>% 
    xml_text()
  page_url <- req_xml_cleaned %>% 
    xml_find_all("link[@rel='alternate']") %>% 
    xml_attr('href')
  
  # attempt to read in the page source
  # if this fails, then we wont be able to send to Google Sites API
  # so checking upfront makes sense
  # NOTE: read the file as a connection
  # so that we can close it and not force
  # the user to restart RStudio to close
  # the hanging connection
  tmp <- file(page_xhtml_source)
  open(tmp, "rb")
  body <- tryCatch({
    read_xml(tmp)
  }, error=function(e){
    message('The page_xhtml_source is not valid XML. Please check that the HTML is also XML compatible.')
    stop(e)
  }, finally = {
    try(close(tmp), silent=T)
  })
  
  # determine the path of the HTML, any relative links in the body
  # will be expanded relative to the HTML's directory
  source_dir <- dirname(normalizePath(page_xhtml_source, winslash = '/', mustWork=FALSE))
  # search for all img and <a> tags specifically, the "src" attribute in <img> tags
  # and the "href" attribute in the <a> tags. If the format is a relative path, then
  # try to upload the file and change the reference in the body
  message('Replacing Links to Local Files')
  body <- replace_local_paths(body=body, 
                              targets=list(list(target_element='a', 
                                                target_attribute='href'), 
                                           list(target_element='img', 
                                                target_attribute='src')),
                              base_dir=source_dir,
                              parent_page_id=page_id, 
                              overwrite=overwrite, 
                              site_domain=site_domain, 
                              site_name=site_name)
  
  entry <- construct_entry(page_title=page_title,
                           category=if(grepl('announcementspage', parent_page_category)) 'announcement' else 'webpage',
                           body=body,
                           parent_page_id=parent_page_id, 
                           page_template_id='',
                           page_custom_url=page_custom_url)
  req_xml <- sites_PUT(url = page_id, 
                       body = as.character(entry),
                       content_type = 'application/atom+xml')

  message('Successfully Added All Page Content')
  
  return(list(page_id = page_id, 
              page_url = page_url))
}


#' Delete a page on Google Sites
#' 
#' This function takes a title, url, or Id of a page, then deletes it from 
#' a Google Sites
#'
#' @keywords googlesites documentation wiki api delete webpage announcement attachment
#' @param id A string representing the URL that identifies the entry id of content 
#' to delete. This should be an absolute URL looking like:
#' https://sites.google.com/feeds/content/mydomain/mysite/6477233125232797597. You can find this URL 
#' by using the \code{\link{find_content}} function.
#' @param verbose A logical indicating whether to print messages
#' @examples
#' \dontrun{
#' # delete a webpage with title My Report
#' delete_content(id = find_content(value_to_match='My Report',
#'                                  field_to_match='title')$id)
#'             
#' # delete an attachment 
#' delete_content(id = find_content(value_to_match='rmarkdown-cheatsheet-2.0',
#'                                  field_to_match='title', 
#'                                  content_category='attachment')$id)
#' }
#' @export
delete_content <- function(id, 
                           verbose=TRUE){
  
  res <- sites_DELETE(url = id)
  
  if(verbose) message('Content Succesfully Deleted.')
  
  return(invisible(res))
}
