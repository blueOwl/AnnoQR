host = 'http://annoq.org:3404'
#' regionQuery
#' @usage regionQuery(contig, start, end)
#' @description This function use a genome coordinate to query variants within that region
#' @param contig contig string
#' @param start  sgtart position in int
#' @param end end position in int
#' @param configFile  optional parameter that provide fields to return, FALSE by default
#' @return A list contain variants
#' @import jsonlite httr
#' @export
regionQuery <- function(contig, start, end, configFile = FALSE) {
  body = '{
  "query": {
  "bool": {
  "filter": [
  {"term": {"chr":"2"}},
  {"range" : { "pos" : { "gte" : 10, "lte" : 20000 } }}]
  }}}'
  body = parse_json(body)
  body[['query']][['bool']][['filter']][[1]][['term']][['chr']] = contig
  body[['query']][['bool']][['filter']][[2]][['range']][['pos']][['gte']] = start
  body[['query']][['bool']][['filter']][[2]][['range']][['pos']][['lte']] = end
  if (!isFALSE(configFile)) {
    s = read_json(configFile)
    body[['_source']] = unlist(s[['_source']])
  }
  r <- POST(paste0(host, "/vs-index/_search"), body = toJSON(body))
  stop_for_status(r)
  content(r, "parsed", "application/json")
}

#' rsidQuery
#' @usage rsidQuery(rsid)
#' @description This function use a rsid to query a variant
#' @param rsid rsid a string
#' @param configFile  optional parameter that provide fields to return, FALSE by default
#' @return One variant if found
#' @export
rsidQuery <- function(rsid){
  body = '{
  "query": {
  "bool": {
  "filter": [
  {"term": {"rs_dbSNP151":"rs559687999"}}]
  }}}'
  body = parse_json(body)
  body[['query']][['bool']][['filter']][[1]][['term']][['rs_dbSNP151']] = rsid
  r <- POST(paste0(host, "/vs-index/_search"), body = toJSON(body))
  stop_for_status(r)
  content(r, "parsed", "application/json")
}


#' keywordsQuery
#' @usage keywordsQuery(keywords)
#' @description Perform full text search in our variant annotation dataset
#' @param keywords keywords a string
#' @param configFile  optional parameter that provide fields to return, FALSE by default
#' @return A list contain variants
#' @export
keywordsQuery <- function(keywords){
  body = '{
  "query": {
  "multi_match": {"query":"Signaling by GPCR"}
  }}'
  body = parse_json(body)
  body[['query']][['multi_match']][['query']] = keywords
  r <- POST(paste0(host, "/vs-index/_search"), body = toJSON(body))
  stop_for_status(r)
  content(r, "parsed", "application/json")
  }
