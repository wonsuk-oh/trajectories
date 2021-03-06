
# =====================================================================================
# Objective
# - 
# 

# -------------------------------------------------------------------------------------
# Load libraries and parameters
# 

options(stringsAsFactors = FALSE)
options(contrasts=c(unordered="contr.treatment", ordered="contr.treatment"))

library(parallel)
mc.cores = detectCores()

source('./lib/node.r')
source('./lib/series.r')
source('./lib/trajectory.structure.r')
source('./lib/trajectory.likelihood.r')



# -------------------------------------------------------------------------------------
# 
# 

load('./data/trajectory.proposed.a_ps.mc.rdata')



# -------------------------------------------------------------------------------------
# 
# 

load('./data/rank.proposed.a_ps.mc.rdata')
rank.proposed.a_ps.mc = rank.proposed.a_ps.mc$order[1:20]

trajs.proposed.a_ps.mc = trajs.proposed.a_ps.mc[ rank.proposed.a_ps.mc ]
rank.proposed.a_ps.mc = 1:20



# -------------------------------------------------------------------------------------
# Load data, transform data, and initialize parameters.
# 

# Mayo data
load('./data/preprocess.progression_pairs.3_year.mc.rdata')

# Rename.
d.t0.mc = d_prev_2005_2007
d.t1.mc = d_prev_2012_2014
rm(d_prev_2005_2007, d_prev_2012_2014)

# Convert a data to a set of prefixes.
f.t0.mc = mclapply(trajs.proposed.a_ps.mc, as.ordinal, data=d.t0.mc, exclusion=TRUE, mc.cores=mc.cores)
f.t1.mc = mclapply(trajs.proposed.a_ps.mc, as.ordinal, data=d.t1.mc, exclusion=TRUE, mc.cores=mc.cores)
f.t0.mc = cbind.data.frame(f.t0.mc)
f.t1.mc = cbind.data.frame(f.t1.mc)
colnames(f.t0.mc) = paste('t', 1:length(trajs.proposed.a_ps.mc), sep='')
colnames(f.t1.mc) = paste('t', 1:length(trajs.proposed.a_ps.mc), sep='')

# Construct 
bool = c(FALSE, TRUE)
combn.vs = expand.grid(
  ob    = bool, 
  hld   = bool, 
  htn   = bool, 
  pred  = bool, 
  dm    = bool, 
  renal = bool, 
  pvd   = bool, 
  cad   = bool, 
  mi    = bool, 
  brain = bool, 
  chf   = bool
)
combn.vs.index = cbind(combn.vs, index=1:nrow(combn.vs))

i.t0.mc = merge(x=cbind(i=1:nrow(d.t0.mc), d.t0.mc), y=combn.vs.index, all.x=TRUE)
i.t0.mc = i.t0.mc[order(i.t0.mc$i), ]$index
i.t1.mc = merge(x=cbind(i=1:nrow(d.t1.mc), d.t1.mc), y=combn.vs.index, all.x=TRUE)
i.t1.mc = i.t1.mc[order(i.t1.mc$i), ]$index

# Fairview data
load('./data/preprocess.progression_pairs.3_year.fv.rdata')

# Rename.
d.t0.fv = d_prev_2005_2007
d.t1.fv = d_prev_2012_2014
rm(d_prev_2005_2007, d_prev_2012_2014)

# Convert a data to a set of prefixes.
f.t0.fv = mclapply(trajs.proposed.a_ps.mc, as.ordinal, data=d.t0.fv, exclusion=TRUE, mc.cores=mc.cores)
f.t1.fv = mclapply(trajs.proposed.a_ps.mc, as.ordinal, data=d.t1.fv, exclusion=TRUE, mc.cores=mc.cores)
f.t0.fv = cbind.data.frame(f.t0.fv)
f.t1.fv = cbind.data.frame(f.t1.fv)
colnames(f.t0.fv) = paste('t', 1:length(trajs.proposed.a_ps.mc), sep='')
colnames(f.t1.fv) = paste('t', 1:length(trajs.proposed.a_ps.mc), sep='')

# Construct 
bool = c(FALSE, TRUE)
combn.vs = expand.grid(
  ob    = bool, 
  hld   = bool, 
  htn   = bool, 
  pred  = bool, 
  dm    = bool, 
  renal = bool, 
  pvd   = bool, 
  cad   = bool, 
  mi    = bool, 
  brain = bool, 
  chf   = bool
)
combn.vs.index = cbind(combn.vs, index=1:nrow(combn.vs))

i.t0.fv = merge(x=cbind(i=1:nrow(d.t0.fv), d.t0.fv), y=combn.vs.index, all.x=TRUE)
i.t0.fv = i.t0.fv[order(i.t0.fv$i), ]$index
i.t1.fv = merge(x=cbind(i=1:nrow(d.t1.fv), d.t1.fv), y=combn.vs.index, all.x=TRUE)
i.t1.fv = i.t1.fv[order(i.t1.fv$i), ]$index

items = colnames(d.t0.mc)

# Documentation error. 
# - For now, we are not going to allow documentation error. 
errors.proposed.a_ps = .000
errors.proposed.a_ps = c(errors.proposed.a_ps, rep(0, length(items)-length(errors.proposed.a_ps)))
errors.proposed.a_ps = c(1-sum(errors.proposed.a_ps), errors.proposed.a_ps)



# -------------------------------------------------------------------------------------
# 
# 

B = 250
nlls.rank.proposed.a_ps.mc = mclapply(1:B, function(b, trajs, rank, d0.mc, d1.mc, f0.mc, f1.mc, i1.mc, items, combn, errors) {
  cat('.')
  
  # Train
  set.seed(b)
  tr = sample(x=1:nrow(d0.mc), size=nrow(d0.mc), replace=TRUE)
  ts = setdiff(x=1:nrow(d0.mc), y=tr)
  
  logits = lapply(items, 
    function(dx, d0, d1) {
      data = data.frame(y=d1[, dx], d0)
      fit = glm(y ~ ., data=data, family=binomial, subset=(d0[, dx]==0))
      return(fit)
  }, d0=d0.mc[tr, , drop=FALSE], d1=d1.mc[tr, , drop=FALSE])
  names(logits) = items
  
  following = count.following(trajs=trajs, f0=f0.mc[tr, , drop=FALSE], f1=f1.mc[tr, , drop=FALSE])
  
  fit = lapply(1:length(trajs), 
    function(k, trajs, items, logits, combn) {
      itm(k=k, trajs=trajs, items=items, logits=logits, combn=combn)
  }, trajs=trajs, items=items, logits=logits, combn=combn)
  
  # Test
  nlls = lapply(1:20, function(i, rank, f0, i1, fit, following, errors) {
    yhat = predict.tm(object=fit, data=f0, ks=rank[1:i], following=following, errors=errors)
    yhat = yhat[cbind(seq_along(i1), i1)]
    
    yhat[is.na (yhat)] = .Machine$double.eps
    yhat[is.nan(yhat)] = .Machine$double.eps
    yhat[yhat==0     ] = .Machine$double.eps
    
    nll = -sum(log(yhat))
    
    nll = nll / nrow(f0)
    
    return( nll )
  }, rank=rank, f0=f0.mc[ts, , drop=FALSE], i1=i1.mc[ts], fit=fit, following=following, errors=errors)
  
  nlls = unlist(nlls)
  
  return( nlls )
}, trajs=trajs.proposed.a_ps.mc, rank=rank.proposed.a_ps.mc, d0.mc=d.t0.mc, d1.mc=d.t1.mc, f0.mc=f.t0.mc, f1.mc=f.t1.mc, i1.mc=i.t1.mc, items=items, combn=combn.vs, errors=errors.proposed.a_ps, mc.cores=mc.cores)

B = 250
nlls.rank.proposed.a_ps.fv = mclapply(1:B, function(b, trajs, rank, d0.mc, d1.mc, f0.mc, f1.mc, f0.fv, i1.fv, items, combn, errors) {
  cat('.')
  
  # Train
  set.seed(b)
  tr = sample(x=1:nrow(d0.mc), size=nrow(d0.mc), replace=TRUE)
  ts = setdiff(x=1:nrow(d0.mc), y=tr)
  
  logits = lapply(items, 
    function(dx, d0, d1) {
      data = data.frame(y=d1[, dx], d0)
      fit = glm(y ~ ., data=data, family=binomial, subset=(d0.mc[, dx]==0))
      return(fit)
  }, d0=d0.mc[tr, , drop=FALSE], d1=d1.mc[tr, , drop=FALSE])
  names(logits) = items
  
  following = count.following(trajs=trajs, f0=f0.mc[tr, , drop=FALSE], f1=f1.mc[tr, , drop=FALSE])
  
  fit = lapply(1:length(trajs), 
    function(k, trajs, items, logits, combn) {
      itm(k=k, trajs=trajs, items=items, logits=logits, combn=combn)
  }, trajs=trajs, items=items, logits=logits, combn=combn)
  
  # Test
  set.seed(b)
  tr = sample(x=1:nrow(f0.fv), size=nrow(f0.fv), replace=TRUE)
  ts = setdiff(x=1:nrow(f0.fv), y=tr)
  
  nlls = lapply(1:20, function(i, rank, f0, i1, fit, following, errors) {
    yhat = predict.tm(object=fit, data=f0, ks=rank[1:i], following=following, errors=errors)
    yhat = yhat[cbind(seq_along(i1), i1)]
    
    yhat[is.na (yhat)] = .Machine$double.eps
    yhat[is.nan(yhat)] = .Machine$double.eps
    yhat[yhat==0     ] = .Machine$double.eps
    
    nll = -sum(log(yhat))
    
    nll = nll / nrow(f0)
    
    return( nll )
  }, rank=rank, f0=f0.fv[ts, , drop=FALSE], i1=i1.fv[ts], fit=fit, following=following, errors=errors)
  
  nlls = unlist(nlls)
  
  return( nlls )
}, trajs=trajs.proposed.a_ps.mc, rank=rank.proposed.a_ps.mc, d0.mc=d.t0.mc, d1.mc=d.t1.mc, f0.mc=f.t0.mc, f1.mc=f.t1.mc, f0.fv=f.t0.fv, i1.fv=i.t1.fv, items=items, combn=combn.vs, errors=errors.proposed.a_ps, mc.cores=mc.cores)



# -------------------------------------------------------------------------------------
# Save the likelihood.
# 

save(nlls.rank.proposed.a_ps.mc, nlls.rank.proposed.a_ps.fv, file='./data/likelihood.rank.proposed.a_ps.mc.rdata')









