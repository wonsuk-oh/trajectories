
# =====================================================================================
# Objective
# - The objective of this code is to extract a set of trajectories 
#   from the Bayesian network. 
# 

# -------------------------------------------------------------------------------------
# Load libraries and parameters
# 

options(stringsAsFactors = FALSE)

library(parallel)
mc.cores = detectCores()

source('./lib/node.r')
source('./lib/series.r')
source('./lib/trajectory.structure.r')
source('./lib/trajectory.filter.r')



# -------------------------------------------------------------------------------------
# Extract a set of trajectories.
# 

.extract.trajectories = function(path, ppairs, items)
{
  cat('.')
  
  vs = sapply(ppairs, 
    function(ppair, path) {
    (length(ppair$s0)==length(path)) & all(ppair$s0%in%path)
  }, path=path)
  vs = lapply(ppairs[vs], 
    function(ppair, path) { 
      setdiff(ppair$s1, path)
  }, path=path)
  vs = intersect(items, vs)
  
  if ( length(vs)==0 )
    return( path )
  
  paths = sapply(vs, function(v, path, ppairs, items) {
    path = c(path, v)
    
    paths = .extract.trajectories(path=path, ppairs=ppairs, items=items)
    return( paths )
  }, path=path, ppairs=ppairs, items=items, simplify=FALSE)
  
  return(paths)
}

extract.trajectories = function(paths, items) {
  trajs = lapply(paths, function(path, items) {
    cat('.')
    traj = new('Series') + 'int'
    for(k in 1:length(path)) {
      traj = traj + path[k]
    }
    return( traj )
  }, items=items)
  
  return(trajs)
}

# Mayo data
load('./data/preprocess.progression_pairs.3_year.mc.rdata')

# Rename.
d.t0 = d_prev_2005_2007
d.t1 = d_prev_2012_2014
rm(d_prev_2005_2007, d_prev_2012_2014, d_prev, d_prev2, d_prev2.1, d_prev2.2, d_prev2.3, d_prev2.4, d_prev2.5, d_prev2.6, d_prev2.7, d_prev2.8)

items = colnames(d.t0)

load('./data/progression.pairs.raw.mc.rdata')

trajs.proposed.raw.mc = .extract.trajectories(path=character(), ppairs=ppairs.raw.mc, items=items)
trajs.proposed.raw.mc = rlang::squash(trajs.proposed.raw.mc)
trajs.proposed.raw.mc = extract.trajectories(trajs.proposed.raw.mc, items=items)
trajs.proposed.raw.mc = trajs.proposed.raw.mc[ sapply(trajs.proposed.raw.mc, count)>2 ]



# -------------------------------------------------------------------------------------
# Save the trajectories.
# 

save(trajs.proposed.raw.mc, file='./data/trajectory.proposed.raw.mc.rdata')









