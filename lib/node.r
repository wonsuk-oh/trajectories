
Node = setClass(
  Class = 'Node',
  representation = representation(item='character'),
  prototype = list(item = NULL)
)



# setGeneric
setGeneric(
  name = 'get.item',
  def = function(object)
  {
    standardGeneric('get.item')
  }
)



# setMethod
setMethod(
  f = 'as.character',
  signature = signature(x='Node'),
  definition = function(x)
  {
    return( x@item )
  }
)

setMethod(
  f = 'get.item',
  signature = signature(object='Node'),
  definition = function(object)
  {
    return( object@item )
  }
)

setMethod(
  f = 'show',
  signature = signature(object='Node'),
  definition = function(object)
  {
    print( as.character(object) )
  }
)

setMethod(
  f = '==',
  signature = signature(e1='Node', e2='Node'),
  definition = function(e1, e2){
    return( e1@item==e2@item )
  }
)

setMethod(
  f = '==',
  signature = signature(e1='Node', e2='character'),
  definition = function(e1, e2){
    return( e1@item==e2 )
  }
)

setMethod(
  f = '!=',
  signature = signature(e1='Node', e2='Node'),
  definition = function(e1, e2){
    return( e1@item!=e2@item )
  }
)

setMethod(
  f = '!=',
  signature = signature(e1='Node', e2='character'),
  definition = function(e1, e2){
    return( e1@item!=e2 )
  }
)

## test_ob  = new(Class='Node', item='ob' )
## test_htn = new(Class='Node', item='htn')
## as.character(test_ob)
## ## [1] "ob"
## test_ob
## ## [1] "ob"
## test_ob==test_ob
## ## [1] TRUE
## test_ob=='ob'
## ## [1] TRUE
## test_ob==test_htn
## ## [1] FALSE
## test_ob=='htn'
## ## [1] FALSE









