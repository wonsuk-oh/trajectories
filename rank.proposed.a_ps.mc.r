
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
# Load data, transform data, and initialize parameters.
# 

# Mayo data
load('./data/preprocess.progression_pairs.3_year.mc.rdata')

# Rename.
d.t0.mc = d_prev_2005_2007
d.t1.mc = d_prev_2012_2014
rm(d_prev_2005_2007, d_prev_2012_2014)

# Convert raw data to a set of prefixes.
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

items = colnames(d.t0.mc)

# Documentation error. 
# - For now, we are not going to allow documentation error. 
errors.proposed.a_ps = .000
errors.proposed.a_ps = c(errors.proposed.a_ps, rep(0, length(items)-length(errors.proposed.a_ps)))
errors.proposed.a_ps = c(1-sum(errors.proposed.a_ps), errors.proposed.a_ps)

pool.proposed.a_ps = 1:length(trajs.proposed.a_ps.mc)



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
following.proposed.a_ps.mc = count.following(trajs=trajs.proposed.a_ps.mc, f0=f.t0.mc, f1=f.t1.mc)

fit.itms.proposed.a_ps.mc = mclapply(1:length(trajs.proposed.a_ps.mc), 
  function(k, trajs, items, logits, combn) {
    itm(k=k, trajs, items=items, logits=logits, combn=combn)
}, trajs=trajs.proposed.a_ps.mc, items=items, logits=logits.mc, combn=combn.vs, mc.cores=mc.cores)



# -------------------------------------------------------------------------------------
# 
# 

rank.proposed.a_ps.mc = search.trajectories(
  f0        = f.t0.mc, 
  i1        = i.t1.mc, 
  prefix    = NULL, 
  pool      = pool.proposed.a_ps, 
  fit       = fit.itms.proposed.a_ps.mc, 
  following = following.proposed.a_ps.mc, 
  errors    = errors.proposed.a_ps, 
  mc.cores  = mc.cores
)
rank.proposed.a_ps.mc$order = as.integer(strsplit(x=rank.proposed.a_ps.mc$order[nrow(rank.proposed.a_ps.mc)], split='[.]')[[1]])



# -------------------------------------------------------------------------------------
# Save the rank.
# 

save(rank.proposed.a_ps.mc, file='./data/rank.proposed.a_ps.mc.rdata')









