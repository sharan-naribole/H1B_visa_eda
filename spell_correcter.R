# Author: Sharan Naribole
# Filename: helpers.R
# Spell CorrectionR implementation
# Inspired by the probabilistic model described in http://norvig.com/spell-correct.html

require(hashmap)

get_deletes <- function(split_left,split_right, i) {
  # Generate deletion of one letter from word
  return(paste0(split_left[i], substr(split_right[i],2,nchar(split_right[i]))))
}

get_replaces <- function(split_left,split_right, i,letters) {
  # Generate replacement of a letter by a-z or space
  if(!is.null(split_right[i]) &  nchar(split_right[i]) > 0) {
    return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = substr(split_right[i],2,nchar(split_right[i])))))
  }
  return(NULL)
}

get_transposes <- function(split_left, split_right,i) {
  # Generate interchanging of the positions of adjacent letters
  if(!is.null(split_right[i]) & nchar(split_right[i]) > 1) {
    return(paste0(split_left[i],substr(split_right[i],2,2),substr(split_right[i],1,1),substr(split_right[i],3,nchar(split_right[i]))))
  }
  return(NULL)
}

edits1site <- function(site) {
  # All edits that are one edit away from site
  letters = toupper(strsplit("abcdefghijklmnopqrstuvwxyz ",split='')[[1]])
  site_len <- nchar(site)
  #print(site_len)
  if(site_len < 4) {
    return(site)
  }
  split_left <- sapply(seq(0,site_len), substr,x = site,start = 1)
  split_right <- sapply(seq(1,site_len+1), substr,x = site,stop = site_len) 
  deletes <- sapply(seq(1,site_len+1),get_deletes, split_left = split_left, split_right = split_right)
  transposes <- unlist(sapply(seq(1,site_len+1),get_transposes, split_left = split_left, split_right = split_right))
  replaces <- unlist(sapply(seq(1,site_len+1),get_replaces, split_left = split_left, split_right = split_right, letters=letters))
  inserts <- unlist(sapply(seq(1,site_len+1),get_inserts, split_left = split_left, split_right = split_right,letters = letters))
  
  return(unique(c(deletes,transposes,replaces,inserts)))
}

edits2site <- function(site) { 
  # All edits that are two edits away from `word`
  edits1_sites = edits1site(site)
  return (unlist(sapply(edits1_sites, edits1site)))
}

get_prob <- function(site, site_hash) {
  # probability of site in our dataset
  return(site_hash[[site]])
}

known <- function(sites,site_hash = site_hash) {
  # The subset of candidate sites that appear in the dictionary of sites
  return(sites[site_hash$has_keys(sites)])
}

find_candidates <- function(site,...) {
  # Generate possible spelling corrections for word
  return(c(known(site,...), known(edits1site(site),...), c(site)))
}

site_spell_correcter <- function(site,...) {
  # best possible correction to the site
  candidates = find_candidates(site,...)
  best_candi = candidates[which.max(sapply(candidates,get_prob, ...))]
  
  #if(get_prob(best_candi,...) > get_prob(site,...) ) {
  #  return(best_candi)
  #}
  return(best_candi)
}

site_count <- function(site, site_hash) {
  
  if(site_hash$has_key(site)) {
    return(site_hash[[site]])
  }
  return(site)
}