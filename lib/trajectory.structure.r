
# setGeneric
setGeneric(
  name = 'as.ordinal',
  def = function(object, data, exclusion)
  {
    standardGeneric('as.ordinal')
  }
)

setGeneric(
  name = 'extract',
  def = function(object, data)
  {
    standardGeneric('extract')
  }
)

setGeneric(
  name = 'filter.prefix',
  def = function(object)
  {
    standardGeneric('filter.prefix')
  }
)



# helper function
.as.ordinal.Series = function(object, data, exclusion=TRUE)
{
  nodes = get.items(object)
  nodes = c('no', nodes)
  
  ordinal = rep(nodes[2], nrow(data))
  ordinal = factor(ordinal, levels=nodes, ordered=TRUE)
  if ( exclusion==TRUE ) {
    sigma = colnames(data)
    compl = data[, !(sigma%in%nodes), drop=FALSE]
    sel = rowSums(compl)!=0
    ordinal[sel] = 'no'
  }
  
  if ( count(object)==0 )
    return( ordinal )
  
  for ( i in 1:count(object) ) {
    if ( i==1 & object[i]=='int' ) {
      sel1 = ordinal!= 'no'
      sel2 = ordinal!= 'no'
      ordinal[ sel1 & sel2] = nodes[i+1]
      ordinal[!sel1 & sel2] = 'no'
    } else if ( i==1 & object[i]!='int' ) {
      sel1 = ordinal!= 'no'
      sel2 = extract(object[i], data)==TRUE
      ordinal[ sel1 & sel2] = nodes[i+1]
      ordinal[!sel1 & sel2] = 'no'
    } else if ( i!=1 & object[i]=='int' ) {
      sigma = colnames(data)
      sel2 = rowSums(data[, sigma%in%nodes, drop=FALSE])==0
      ordinal[sel2] = 'no'
    } else if ( i!=1 & object[i]!='int' ) {
      sel1 = ordinal==nodes[i]
      sel2 = extract(object[i], data)==TRUE
      ordinal[ sel1 & sel2] = nodes[i+1]
      ordinal[!sel1 & sel2] = 'no'
    }
  }
  
  return( ordinal )
}

.extract.Node = function(object, data)
{
  data = data[, object@item]
  return( data )
}

.filter.prefix = function(object)
{
  sel = sapply(object, function(prefix, references) {
    sapply(references, is.prefix, prefix=prefix)
  }, references=object)
  sel = colSums(sel)
  tids = which( sel>1 )
}


# setMethod
setMethod(
  f = 'as.ordinal',
  signature = signature(object='Series', data='data.frame', exclusion='logical'),
  definition = .as.ordinal.Series
)

setMethod(
  f = 'as.ordinal',
  signature = signature(object='Series', data='matrix', exclusion='logical'),
  definition = .as.ordinal.Series
)

setMethod(
  f = 'extract',
  signature = signature(object='Node', data='data.frame'),
  definition = .extract.Node
)

setMethod(
  f = 'extract',
  signature = signature(object='Node', data='matrix'),
  definition = .extract.Node
)

setMethod(
  f = 'filter.prefix',
  signature = signature(object='list'),
  definition = .filter.prefix
)









