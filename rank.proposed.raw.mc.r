
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

load('./data/trajectory.proposed.raw.mc.rdata')



# -------------------------------------------------------------------------------------
# Load data, transform data, and initialize parameters.
# 

# Mayo data
load('./data/preprocess.progression_pairs.3_year.mc.rdata')

# Rename.
d.t0.mc = d_prev_2005_2007
d.t1.mc = d_prev_2012_2014
rm(d_prev_2005_2007, d_prev_2012_2014)

# Convert raw data to a set of prefixes.
f.t0.mc = mclapply(trajs.proposed.raw.mc, as.ordinal, data=d.t0.mc, exclusion=TRUE, mc.cores=mc.cores)
f.t1.mc = mclapply(trajs.proposed.raw.mc, as.ordinal, data=d.t1.mc, exclusion=TRUE, mc.cores=mc.cores)
f.t0.mc = cbind.data.frame(f.t0.mc)
f.t1.mc = cbind.data.frame(f.t1.mc)
colnames(f.t0.mc) = paste('t', 1:length(trajs.proposed.raw.mc), sep='')
colnames(f.t1.mc) = paste('t', 1:length(trajs.proposed.raw.mc), sep='')

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

items = colnames(d.t0.mc)

# Documentation error. 
# - For now, we are not going to allow documentation error. 
errors.proposed.raw = .000
errors.proposed.raw = c(errors.proposed.raw, rep(0, length(items)-length(errors.proposed.raw)))
errors.proposed.raw = c(1-sum(errors.proposed.raw), errors.proposed.raw)

pool.proposed.raw = 1:length(trajs.proposed.raw.mc)



# -------------------------------------------------------------------------------------
# 
# 

logits.mc = lapply(items, 
  function(dx, d0, d1) {
    data = data.frame(y=d1[, dx], d0)
    fit = glm(y ~ ., data=data, family=binomial, subset=(d0[, dx]==0))
    return(fit)
}, d0=d.t0.mc, d1=d.t1.mc)
names(logits.mc) = items

yhat.baseline = sapply(items, 
  function(dx, d0, d1, models) {
    yhat = predict(object=logits.mc[[ dx ]], newdata=d0, type='response')
    yhat = ifelse(test=d0[, dx]==TRUE, yes=1   , no=yhat  )
    yhat = ifelse(test=d1[, dx]==TRUE, yes=yhat, no=1-yhat)
    yhat[yhat==0] = .Machine$double.eps
    names(yhat) = NULL
    
    return(yhat)
}, d0=d.t0.mc, d1=d.t1.mc, models=logits.mc)
yhat.baseline = apply(yhat.baseline, 1, prod)

count.following = function(trajs, f0, f1)
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
following.proposed.raw.mc = count.following(trajs=trajs.proposed.raw.mc, f0=f.t0.mc, f1=f.t1.mc)

fit.itms.proposed.raw.mc = mclapply(1:length(trajs.proposed.raw.mc), 
  function(k, trajs, items, logits, combn) {
    itm(k=k, trajs, items=items, logits=logits, combn=combn)
}, trajs=trajs.proposed.raw.mc, items=items, logits=logits.mc, combn=combn.vs, mc.cores=mc.cores)



# -------------------------------------------------------------------------------------
# 
# 

rank.proposed.raw.mc = search.trajectories(
  f0        = f.t0.mc, 
  i1        = i.t1.mc, 
  prefix    = NULL, 
  pool      = pool.proposed.raw, 
  fit       = fit.itms.proposed.raw.mc, 
  following = following.proposed.raw.mc, 
  baseline  = yhat.baseline,
  errors    = errors.proposed.raw, 
  mc.cores  = mc.cores
)
rank.proposed.raw.mc$order = as.integer(strsplit(x=rank.proposed.raw.mc$order[nrow(rank.proposed.raw.mc)], split='[.]')[[1]])



# -------------------------------------------------------------------------------------
# Save the rank.
# 

save(rank.proposed.raw.mc, file='./data/rank.proposed.raw.mc.rdata')









