
# -------------------------------------------------------------------------------------
# 
# 

# Construct a single row data frame that represents presents of events.
as.dummy = function(items, events=NULL)
{
  # dummy data
  dummy = sapply(items, function(item) { FALSE }, simplify=FALSE)
  dummy = cbind.data.frame(dummy)
  
  # set 1 on events
  for ( event in events )
    dummy[, event] = TRUE
  
  return( dummy )
}
## as.dummy(items, c('htn', 'pvd'))
##      ob   hld  htn  pred    dm renal  pvd   cad    mi brain   chf
## 1 FALSE FALSE TRUE FALSE FALSE FALSE TRUE FALSE FALSE FALSE FALSE

# Make flags on each combination of possible output, x1, that indicate documentation
# errors on a patient. For now, we are not going to care about documentation error. 
as.error = function(path, items, combn)
{
  error = apply(combn, 1, function(x, path, items) {
    path = path[-1]
    x1   = items[ which(x) ]
    
    # insertion
    insertion = setdiff(x1, path)
    
    # omission
    omission = NULL
    sm = tail( x=intersect(path, x1), n=1 )
    if ( length(sm)!=0 ) {
      m  = which( path==sm ) 
      omission = setdiff(path[1:m], x1)
    }
    
    c( insertion=length(insertion), omission=length(omission) )
  }, path=path, items=items)
}

# Find the last node on the prefix assuming that 
# 1) combinations of disease follow the trajectory
# 2) we do not care about insertion or omission. 
as.last_prefix = function(path, combn)
{
  lprefix = cbind(int=TRUE, combn)
  lprefix = lprefix[, path, drop=FALSE]
  
  ## covered = FALSE
  ## for ( v in rev(path) ) {
  ##   pattern[covered, v] = FALSE
  ##   covered = covered | pattern[, v]
  ## }
  covered = FALSE
  for ( v in rev(path) ) {
    lprefix[, v] = lprefix[, v] & !covered
    covered = covered | lprefix[, v]
  }
  lprefix = t(lprefix)
  
  return( lprefix )
}

count.following = function(trajs, f0, f1, mc.cores=1)
{
  following = mclapply(1:length(trajs), 
    function(k, trajs, f0, f1) {
      traj = trajs[[k]]
      lapply(1:count(traj), 
        function(i, k, traj, f0, f1) {
          prefix = get.items(traj)[1:i]
          v = as.character(traj[i])
          n = sum( f0[, k]==v & f1[, k]>=v )
          
          return( list(prefix=prefix, v=v, n=n) )
      }, k=k, traj=traj, f0=f0, f1=f1)
  }, trajs=trajs, f0=f0, f1=f1, mc.cores=mc.cores)
  
  return( following )
}

get.portions = function(following, pool, k)
{
  portions = sapply(1:length(following[[k]]), 
    function(i, k, pool, following) {
      prefix = following[[k]][[i]]$prefix
      n      = following[[k]][[i]]$n
      
      ks = NULL
      ns = 0
      for ( j in pool ) {
        if ( length(following[[j]])>=i ) {
          if ( all(prefix %in% following[[j]][[i]]$prefix) ) {
            ks = c(ks, j)
            ns = ns + following[[k]][[i]]$n
          }
        }
      }
      p = n/ns
      if ( is.nan(p) ) p = 1
      
      return( p )
    
  }, k=k, pool=pool, following=following)
  
  return( portions )
}

# S(ingle) T(rajectory) M(odel)... likes L(inear) M(odel)... 
.itm = function(s0, s1, r, items, logits)
{
  # Initialization
  prob = 1
  dummy = as.dummy(items, s0)
  
  # s1: a set of events that are developed on the second time slice. 
  for (v in s1) {
    t = predict( object=logits[[v]], newdata=dummy, type='response' )
    prob = prob * t
    dummy[, v] = TRUE
  }
  
  # r: a set of events that are not developed on the second time slice. 
  for (v in r) {
    t = predict( object=logits[[v]], newdata=dummy, type='response' )
    prob = prob * (1-t)
    dummy[, v] = TRUE
    # Stop here.
    break
  }
  
  return( prob)
}

# S(ingle) T(rajectory) M(odel)... likes L(inear) M(odel)... 
itm = function(k, trajs, items, logits, combn)
{
  traj = trajs[[ k ]]
  path = get.items( traj )
  lprefix = as.last_prefix(path=path, combn=combn)
  
  # Translation matrix.
  itm1 = matrix(data=0, nrow=length(path), ncol=length(path), dimnames=list(path, path))
  for (i in 1:length(path)) {
    for (j in i:length(path)) {
      s0 = setdiff(path[1:i], 'int')
      s1 = setdiff(path[1:j], path[1:i])
      r  = setdiff(path, path[1:j])
      
      itm1[i, j] = .itm(s0=s0, s1=s1, r=r, items=items, logits=logits)
    }
  }
  
  # Joint probability
  itm2 = sapply(1:nrow(itm1), 
    function(i, itm1, lprefix) {
      itm1[i, ] %*% lprefix
  }, itm1=itm1, lprefix=lprefix)
  itm2 = t(itm2)
  rownames(itm2) = path
  
  error      = as.error(path=path, items=items, combn=combn)
  nerror     = colSums(error)
  ninsertion = error[1, ]
  nomission  = error[2, ]
  
  model = list(
    k          = k,
    path       = path,
    itm1       = itm1,
    itm2       = itm2,
    lprefix    = lprefix,
    nerror     = nerror,
    ninsertion = ninsertion,
    nomission  = nomission
  )
  
  return( model )
}

predict.itm = function(object, newdata, errors=NULL)
{
  coef       = object$itm2
  lprefix    = object$lprefix
  nerror     = object$nerror
  ninsertion = object$ninsertion
  nomission  = object$nomission
  
  # We do not allow insertion.
  for (i in 1:nrow(coef) )
    coef[i, ] = coef[i, ] * (ninsertion==0 * 1)
  ## if ( !is.null(errors) ) {
  ##   error = errors[ ninsertion+1 ]
  ##   for (i in 1:nrow(coef) )
  ##     coef[i, ] = coef[i, ] * error
  ## }
  # We do not allow omission.
  for (i in 1:nrow(coef) )
    coef[i, ] = coef[i, ] * (nomission==0 * 1)
  
  ## prob = sapply(1:ncol(coef), function(i, data, coef) {
  ##   data %*% coef[, i]
  ## }, data=newdata, coef=coef)
  prob = newdata %*% coef
  
  return(prob)
}

predict.tm = function(object, data, ks, following, baseline, errors)
{
  yhats = 0
  
  for (k in ks) {
    portions = get.portions(following=following, pool=ks, k=k)
    newdata  = model.matrix(~ ., data[, k, drop=FALSE])[, -1, drop=FALSE]
    model    = object[[k]]
    yhat     = predict.itm(object=model, newdata=newdata, errors=errors)
    yhat     = ifelse(yhat!=0, yhat, baseline)
    yhat     = yhat * rowSums(newdata %*% diag(portions))
    yhats    = yhats + yhat
  }
  yhat = yhats
  
  return( yhat )
}

search.trajectories = function(f0, i1, prefix, pool, fit, following, baseline, errors, mc.cores=1)
{
  cands = setdiff(pool, prefix)
  if (length(cands)==0) { return(NULL) }
  
  fits = mclapply(cands, function(cand, f0, i1, prefix, fit, following, baseline, errors) {
    cat('.')
    ord = c(prefix, cand)
    
    yhat = predict.tm(object=fit, data=f0, ks=ord, follow=following, baseline=baseline, errors=errors)
    yhat = yhat[cbind(seq_along(i1), i1)]
    
    yhat[is.na (yhat)] = baseline[is.na (yhat)]
    yhat[is.nan(yhat)] = baseline[is.nan(yhat)]
    yhat[yhat==0     ] = baseline[yhat==0     ]
    
    nll = -sum(log(yhat))
    #nll = nll / length(yhat)
    
    return(list(order=ord, nll=nll))
  }, f0=f0, i1=i1, prefix=prefix, fit=fit, following=following, baseline=baseline, errors=errors, mc.cores=mc.cores)
  cat('\n')
  df = lapply(fits, function(fit) { data.frame(order=paste(fit$order, collapse='.'), nll=fit$nll) })
  df = do.call(rbind, df)
  
  i = which.min(df$nll)
  print(df[i, ])
  
  rank = search.trajectories(f0=f0, i1=i1, prefix=fits[[i]]$order, pool=pool, fit=fit, following=following, baseline=baseline, errors=errors, mc.cores=mc.cores)
  rank = rbind(df[i, , drop=FALSE], rank)
  
  return(rank)
}









