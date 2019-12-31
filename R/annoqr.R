#' regionQuery
#' @usage regionQuery(contig, start, end)
#' @description This function use a genome coordinate to query variants within that region
#' @param contig contig string
#' @param start  sgtart position in int
#' @param end end position in int
#' @param configFile  optional parameter that provide fields to return, FALSE by default
#' @return A list contain variants
#' @export
regionQuery <- function(contig, start, end, configFile = FALSE) {
  host = 'http://annoq.org:3404'
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
