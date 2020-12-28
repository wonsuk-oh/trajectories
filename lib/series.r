
Series = setClass(
  Class = 'Series',
  representation = representation(nodes='list'),
  prototype = NULL
)



# setGeneric
setGeneric(
  name = 'append',
  def = function(object, node)
  {
    standardGeneric('append')
  }
)

setGeneric(
  name = 'count',
  def = function(object)
  {
    standardGeneric('count')
  }
)

setGeneric(
  name = 'get.items',
  def = function(object)
  {
    standardGeneric('get.items')
  }
)

setGeneric(
  name = 'index',
  def = function(object, node)
  {
    standardGeneric('index')
  }
)

setGeneric(
  name = 'is.prefix',
  def = function(prefix, reference)
  {
    standardGeneric('is.prefix')
  }
)

setGeneric(
  name = 'last',
  def = function(object)
  {
    standardGeneric('last')
  }
)

setGeneric(
  name = 'remove',
  def = function(object, node)
  {
    standardGeneric('remove')
  }
)

setGeneric(
  name = 'sub',
  def = function(object, from, to)
  {
    standardGeneric('sub')
  }
)



# helper function
.index.Series = function(object, node)
{
  for ( i in 1:count(object) )
    if ( object[i]==node )
      return( i )
  return( NA )
}

.get.items.Series = function(object)
{
  items = sapply(object@nodes, get.item)
  return( items )
}

.remove.Series = function(object, node)
{
  i = index(object, node)
  object@nodes[i] = NULL
  return( object )
}

.sub.node.Series = function(object, from, to)
{
  sub = new('Series')
  from = index(object, from)
  to   = index(object, to  )
  for (i in from:to)
    sub = sub + object[i]
  return( sub )
}

.sub.numeric.Series = function(object, from, to)
{
  sub = new('Series')
  for (i in from:to)
    sub = sub + object[i]
  return( sub )
}



# setMethod
setMethod(
  f = 'append',
  signature = signature(object='Series', node='Node'),
  definition = function(object, node)
  {
    object@nodes = c(object@nodes, node)
    return( object )
  }
)

setMethod(
  f = 'append',
  signature = signature(object='Series', node='character'),
  definition = function(object, node)
  {
    node=new(Class = 'Node', item = node)
    object@nodes = c(object@nodes, node)
    return( object )
  }
)

setMethod(
  f = 'as.character',
  signature = signature(x='Series'),
  definition = function(x)
  {
    series = sapply(x@nodes, as.character)
    series = paste(series, collapse=' -> ')
    return( series )
  }
)

setMethod(
  f = 'count',
  signature = signature(object='Series'),
  definition = function(object)
  {
    len = length( object@nodes )
    return( len )
  }
)

setMethod(
  f = 'get.items',
  signature = signature(object='Series'),
  definition = .get.items.Series
)

setMethod(
  f = 'index',
  signature = signature(object='Series', node='Node'),
  definition = .index.Series
)

setMethod(
  f = 'index',
  signature = signature(object='Series', node='character'),
  definition = .index.Series
)

setMethod(
  f = 'is.prefix',
  signature = signature(prefix='character', reference='Series'),
  definition = function(prefix, reference)
  {
    if ( length(prefix)>count(reference) )
      return( FALSE )
    
    bool = sapply( 1:length(prefix), function(i, prefix, reference) {
      return( reference[i]==prefix[i] )
    }, prefix=prefix, reference=reference)
    bool = all(bool)
    
    return( bool )
  }
)

setMethod(
  f = 'is.prefix',
  signature = signature(prefix='Series', reference='Series'),
  definition = function(prefix, reference)
  {
    if ( count(prefix)>count(reference) )
      return( FALSE )
    
    bool = sapply( 1:count(prefix), function(i, prefix, reference) {
      return( reference[i]==prefix[i] )
    }, prefix=prefix, reference=reference)
    bool = all(bool)
    
    return( bool )
  }
)

setMethod(
  f = 'last',
  signature = signature(object='Series'),
  definition = function(object)
  {
    if ( count(object)==0 )
      return( NULL )
    return( object[count(object)] )
  }
)

setMethod(
  f = 'remove',
  signature = signature(object='Series', node='Node'),
  definition = .remove.Series
)

setMethod(
  f = 'remove',
  signature = signature(object='Series', node='character'),
  definition = .remove.Series
)

setMethod(
  f = 'show',
  signature = signature(object='Series'),
  definition = function(object)
  {
    series = as.character(object)
    print( series )
  }
)

setMethod(
  f = 'sub',
  signature = signature(object='Series', from='character', to='character'),
  definition = .sub.node.Series
)

setMethod(
  f = 'sub',
  signature = signature(object='Series', from='Node', to='Node'),
  definition = .sub.node.Series
)

setMethod(
  f = 'sub',
  signature = signature(object='Series', from='numeric', to='numeric'),
  definition = .sub.numeric.Series
)

setMethod(
  f = '+',
  signature = signature(e1='Series', e2='Node'),
  definition = function(e1, e2)
  {
    series = append(e1, e2)
    return( series )
  }
)

setMethod(
  f = '+',
  signature = signature(e1='Series', e2='character'),
  definition = function(e1, e2)
  {
    series = append(e1, e2)
    return( series )
  }
)

setMethod(
  f = '-',
  signature = signature(e1='Series', e2='Node'),
  definition = function(e1, e2)
  {
    series = remove(e1, e2)
    return( series )
  }
)

setMethod(
  f = '-',
  signature = signature(e1='Series', e2='character'),
  definition = function(e1, e2)
  {
    series = remove(e1, e2)
    return( series )
  }
)

setMethod(
  f = '[',
  signature = signature(x='Series', i='numeric'),
  definition = function(x, i)
  {
    return( x@nodes[[i]] )
  }
)

## test_series = new('Series')
## test_series
## ## [1] ""
## test_series = new('Series') + 'int' + 'ob' + 'htn'
## as.character(test_series)
## ## [1] "int -> ob -> htn"
## test_series
## ## [1] "int -> ob -> htn"
## count(test_series)
## ## [1] 3
## index(test_series, Node(item='ob'))
## ## [1] 2
## index(test_series, 'ob')
## ## [1] 2
## test_series - 'ob'
## ## [1] "int -> htn"
## test_series[1]
## ## [1] "int"
## test_series[2]
## ## [1] "ob"
## test_series1 = new('Series') + 'int' + 'ob'
## test_series2 = new('Series') + 'int' + 'ob' + 'htn'
## test_series3 = new('Series') + 'int' + 'ob' + 'hld'
## is.prefix(test_series1, test_series2)
## ## [1] TRUE
## is.prefix(test_series2, test_series1)
## ## [1] FALSE
## is.prefix(test_series2, test_series3)
## ## [1] FALSE









