
desc_indi = function(x, xrefs){
  nms = character()
  for(xref in xrefs){
    nms = c(nms, pull_record(x,xref)@primary_name)
  }
  nms
}