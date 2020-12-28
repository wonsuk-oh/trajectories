# 

.filter.association = function(series, node, data0, data1, exclusion0=TRUE, exclusion1=FALSE)
{
  if ( count(series)<=1 )
    return( -Inf )
  
  prefix = series - last(series)
  px_pxy = series + node
  p_py   = prefix + node
  
  w = get.item( last(prefix) )
  x = get.item( last(series) )
  y = get.item( last(px_pxy) )
  
  px_pxy0 = as.ordinal(object=px_pxy, data=data0, exclusion=exclusion0)
  px_pxy1 = as.ordinal(object=px_pxy, data=data1, exclusion=exclusion1)
  p_py0   = as.ordinal(object=p_py  , data=data0, exclusion=exclusion0)
  p_py1   = as.ordinal(object=p_py  , data=data1, exclusion=exclusion1)
  px_pxy1[ px_pxy0=='no'    ] = 'no'
  p_py0  [ data0[, x]==TRUE ] = 'no'
  p_py1  [ data1[, x]==TRUE ] = 'no'
  p_py1  [ p_py0=='no'      ] = 'no'
  
  p_px_pxy = sum( px_pxy1>=y & px_pxy0==x ) / sum( px_pxy0==x )
  p_p_py   = sum( p_py1  >=y & p_py0  ==w ) / sum( p_py0  ==w )
  
  association = p_px_pxy / p_p_py
  association = ifelse( !is.nan(p_p_py  )                , association, Inf )
  association = ifelse( !is.nan(p_px_pxy)                , association,   0 )
  association = ifelse( sum( p_py1  >=y & p_py0  ==w )!=0, association, Inf )
  association = ifelse( sum( px_pxy1>=y & px_pxy0==x )!=0, association,   0 )
  
  return( association )
}

filter.association = function(series, node, data0, data1, exclusion0=TRUE, exclusion1=FALSE, B=1, mc.cores=1)
{
  if ( B==1 ) {
    association = .filter.association(series=series, node=node, data0=data0, data1=data1, exclusion0=exclusion0, exclusion1=exclusion1)
  } else {
    association = mclapply(1:B, function(b, series, node, data0, data1, exclusion0, exclusion1) {
      set.seed(b)
      tr = sample(x=1:nrow(data0), size=nrow(data0), replace=TRUE)
      association = filter.association(series=series, node=node, data0=data0[tr, , drop=FALSE], data1=data1[tr, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1)
    }, series=series, node=node, data0=data0, data1=data1, exclusion0=exclusion0, exclusion1=exclusion1, mc.cores=mc.cores)
    association = unlist(association)
  }
  
  return( association )
}

.filter.confidence = function(series, node, data0, data1, exclusion0=TRUE, exclusion1=FALSE)
{
  x_xy = series + node
  x = get.item( last(series) )
  y = get.item( last(x_xy  ) )
  
  x_xy0 = as.ordinal(object=x_xy, data=data0, exclusion=exclusion0)
  x_xy1 = as.ordinal(object=x_xy, data=data1, exclusion=exclusion1)
  x_xy1[ x_xy0=='no' ] = 'no'
  
  confidence = sum( x_xy1>=y & x_xy0==x ) / sum( x_xy0==x )
  confidence = ifelse( !is.nan(confidence), confidence, -Inf )
  
  return( confidence )
}

filter.confidence = function(series, node, data0, data1, exclusion0=TRUE, exclusion1=FALSE, B=1, mc.cores=1)
{
  if ( B==1 ) {
    confidence = .filter.confidence(series=series, node=node, data0=data0, data1=data1, exclusion0=exclusion0, exclusion1=exclusion1)
  } else {
    confidence = mclapply(1:B, function(b, series, node, data0, data1, exclusion0, exclusion1) {
      set.seed(b)
      tr = sample(x=1:nrow(data0), size=nrow(data0), replace=TRUE)
      confidence = filter.confidence(series=series, node=node, data0=data0[tr, , drop=FALSE], data1=data1[tr, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1)
    }, series=series, node=node, data0=data0, data1=data1, exclusion0=exclusion0, exclusion1=exclusion1, mc.cores=mc.cores)
    confidence = unlist(confidence)
  }
  
  return( confidence )
}

.filter.precedence = function(series, node, data0, data1, exclusion0=TRUE, exclusion1=FALSE)
{
  prefix = series - last(series)
  x_xy = prefix + last(series) + node
  y_yx = prefix + node + last(series)
  
  x = get.item( last(y_yx) )
  y = get.item( last(x_xy) )
  
  x_xy0 = as.ordinal(object=x_xy, data=data0, exclusion=exclusion0)
  x_xy1 = as.ordinal(object=x_xy, data=data1, exclusion=exclusion1)
  y_yx0 = as.ordinal(object=y_yx, data=data0, exclusion=exclusion0)
  y_yx1 = as.ordinal(object=y_yx, data=data1, exclusion=exclusion1)
  x_xy1[ x_xy0=='no' ] = 'no'
  y_yx1[ y_yx0=='no' ] = 'no'
  
  p_x_xy = sum( x_xy0==x & x_xy1>=y ) / sum( x_xy1>=y )
  p_y_yx = sum( y_yx0==y & y_yx1>=x ) / sum( y_yx1>=x )
  
  precedence = p_x_xy / p_y_yx
  precedence = ifelse( !is.nan(p_y_yx)              , precedence, Inf )
  precedence = ifelse( !is.nan(p_x_xy)              , precedence,   0 )
  precedence = ifelse( sum( y_yx0==y & y_yx1>=x )!=0, precedence, Inf )
  precedence = ifelse( sum( x_xy0==x & x_xy1>=y )!=0, precedence,   0 )
  
  return( precedence )
}

filter.precedence = function(series, node, data0, data1, exclusion0=TRUE, exclusion1=FALSE, B=1, mc.cores=1)
{
  if ( B==1 ) {
    precedence = .filter.precedence(series=series, node=node, data0=data0, data1=data1, exclusion0=exclusion0, exclusion1=exclusion1)
  } else {
    precedence = mclapply(1:B, function(b, series, node, data0, data1, exclusion0, exclusion1) {
      set.seed(b)
      tr = sample(x=1:nrow(data0), size=nrow(data0), replace=TRUE)
      precedence = filter.precedence(series=series, node=node, data0=data0[tr, , drop=FALSE], data1=data1[tr, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1)
    }, series=series, node=node, data0=data0, data1=data1, exclusion0=exclusion0, exclusion1=exclusion1, mc.cores=mc.cores)
    precedence = unlist(precedence)
  }
  
  return( precedence )
}

.filter.support = function(series, node, data0, data1, exclusion0=TRUE, exclusion1=FALSE)
{
  x_xy = series + node
  x = get.item( last(series) )
  x_xy0 = as.ordinal(object=x_xy, data=data0, exclusion=exclusion0)
  
  support = mean( x_xy0==x )
  
  return( support )
}

filter.support = function(series, node, data0, data1, exclusion0=TRUE, exclusion1=FALSE, B=1, mc.cores=1)
{
  if ( B==1 ) {
    support = .filter.support(series=series, node=node, data0=data0, data1=data1, exclusion0=exclusion0, exclusion1=exclusion1)
  } else {
    support = mclapply(1:B, function(b, series, node, data0, data1, exclusion0, exclusion1) {
      set.seed(b)
      tr = sample(x=1:nrow(data0), size=nrow(data0), replace=TRUE)
      support = filter.support(series=series, node=node, data0=data0[tr, , drop=FALSE], data1=data1[tr, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1)
    }, series=series, node=node, data0=data0, data1=data1, exclusion0=exclusion0, exclusion1=exclusion1, mc.cores=mc.cores)
    support = unlist(support)
  }
  
  return( support )
}

