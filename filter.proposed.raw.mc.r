
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
# Load the trajectories.
# 

load('./data/trajectory.proposed.raw.mc.rdata')



# -------------------------------------------------------------------------------------
# 
# 

trajs.proposed.raw.mc2 = lapply(trajs.proposed.raw.mc, 
  function(traj) {
    cat('.')
    trajs = lapply(3:count(traj), sub, object=traj, from=1)
    return(trajs)
})
trajs.proposed.raw.mc2 = rlang::squash(trajs.proposed.raw.mc2)

for (i in 1:length(trajs.proposed.raw.mc2)) {
  cat(i, ':', length(trajs.proposed.raw.mc2), '\n')
  if ( i>=length(trajs.proposed.raw.mc2) )
    break
  
  sel = mclapply( (i+1):length(trajs.proposed.raw.mc2),
    function(trajs, i, j) {
    cat('.')
      is.prefix(trajs[[i]], trajs[[j]]) & is.prefix(trajs[[j]], trajs[[i]])
  }, trajs=trajs.proposed.raw.mc2, i=i, mc.cores=mc.cores)
  sel = unlist(sel)
  sel = c(rep(FALSE, i), sel)
  trajs.proposed.raw.mc2 = trajs.proposed.raw.mc2[ !sel ]
  
  cat('\n')
}

save(trajs.proposed.raw.mc2, file='./data/filter.proposed.raw.mc.rdata')



# -------------------------------------------------------------------------------------
# 
# 

# Mayo data
load('./data/preprocess.progression_pairs.3_year.mc.rdata')

# Rename.
d.t0 = d_prev_2005_2007
d.t1 = d_prev_2012_2014
rm(d_prev_2005_2007, d_prev_2012_2014, d_prev, d_prev2, d_prev2.1, d_prev2.2, d_prev2.3, d_prev2.4, d_prev2.5, d_prev2.6, d_prev2.7, d_prev2.8)

items = colnames(d.t0)

sel.asso.mc = lapply(trajs.proposed.raw.mc2, 
  function(traj, data0, data1, items, exclusion0, exclusion1, criteria, B, mc.cores) {
    cat(as.character(traj), '\n')
    
    path  = get.items(traj)
    errs  = setdiff(items, path)
    subs  = rowSums(data1[, errs, drop=FALSE])==0
    
    sel = sapply(3:count(traj), function(j, traj, data0, data1, subset, exclusion0, exclusion1, criteria, B, mc.cores) {
      eval = filter.association(series=sub(object=traj, from=1, to=(j-1)), node=traj[j], data0=data0[subset, , drop=FALSE], data1=data1[subset, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1, B=B, mc.cores=mc.cores)
      cat(sprintf('%-70s eval: %1.4f (%1.4f, %1.4f)\n', as.character(sub(object=traj, from=1, to=j)), median(eval), quantile(eval, .025), quantile(eval, .975)))
      return( !(quantile(eval, .025) <= criteria) )
    }, traj=traj, data0=data0, data1=data1, subset=subs, exclusion0=exclusion0, exclusion1=exclusion1, criteria=criteria, B=B, mc.cores=mc.cores)
    return( all(sel) )
}, data0=d.t0, data1=d.t1, items=items, exclusion0=TRUE, exclusion1=FALSE, criteria=1.0, B=1000, mc.cores=mc.cores)
sel.asso.mc = unlist(sel.asso.mc)

sel.precs.mc = lapply(trajs.proposed.raw.mc2, 
  function(traj, data0, data1, items, exclusion0, exclusion1, criteria, B, mc.cores) {
    cat(as.character(traj), '\n')
    
    path  = get.items(traj)
    errs  = setdiff(items, path)
    subs  = rowSums(data1[, errs, drop=FALSE])==0
    
    sel = sapply(3:count(traj), function(j, traj, data0, data1, subset, exclusion0, exclusion1, criteria, B, mc.cores) {
      eval = filter.precedence(series=sub(object=traj, from=1, to=(j-1)), node=traj[j], data0=data0[subset, , drop=FALSE], data1=data1[subset, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1, B=B, mc.cores=mc.cores)
      cat(sprintf('%-70s eval: %1.4f (%1.4f, %1.4f)\n', as.character(sub(object=traj, from=1, to=j)), median(eval), quantile(eval, .025), quantile(eval, .975)))
      return( !(quantile(eval, .025) <= criteria) )
    }, traj=traj, data0=data0, data1=data1, subset=subs, exclusion0=exclusion0, exclusion1=exclusion1, criteria=criteria, B=B, mc.cores=mc.cores)
    return( all(sel) )
}, data0=d.t0, data1=d.t1, items=items, exclusion0=TRUE, exclusion1=FALSE, criteria=1.0, B=1000, mc.cores=mc.cores)
sel.precs.mc = unlist(sel.precs.mc)

sel.precp.mc = lapply(trajs.proposed.raw.mc2, 
  function(traj, data0, data1, items, exclusion0, exclusion1, criteria, B, mc.cores) {
    cat(as.character(traj), '\n')
    
    path  = get.items(traj)
    errs  = setdiff(items, path)
    subs  = rowSums(data1[, errs, drop=FALSE])==0
    
    sel = sapply(3:count(traj), function(j, traj, data0, data1, subset, exclusion0, exclusion1, criteria, B, mc.cores) {
      eval = filter.precedence(series=sub(object=traj, from=1, to=(j-1)), node=traj[j], data0=data0[subset, , drop=FALSE], data1=data1[subset, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1, B=B, mc.cores=mc.cores)
      cat(sprintf('%-70s eval: %1.4f (%1.4f, %1.4f)\n', as.character(sub(object=traj, from=1, to=j)), median(eval), quantile(eval, .025), quantile(eval, .975)))
      return( !(quantile(eval, .975) <= criteria) )
    }, traj=traj, data0=data0, data1=data1, subset=subs, exclusion0=exclusion0, exclusion1=exclusion1, criteria=criteria, B=B, mc.cores=mc.cores)
    return( all(sel) )
}, data0=d.t0, data1=d.t1, items=items, exclusion0=TRUE, exclusion1=FALSE, criteria=1.0, B=1000, mc.cores=mc.cores)
sel.precp.mc = unlist(sel.precp.mc)

save(trajs.proposed.raw.mc2, sel.asso.mc, sel.precs.mc, sel.precp.mc, file='./data/filter.proposed.raw.mc.rdata')



# -------------------------------------------------------------------------------------
# 
# 

trajs.proposed.a.mc    = trajs.proposed.raw.mc2[ sel.asso.mc                ]
trajs.proposed.ps.mc   = trajs.proposed.raw.mc2[ sel.precs.mc               ]
trajs.proposed.pp.mc   = trajs.proposed.raw.mc2[ sel.precp.mc               ]
trajs.proposed.a_ps.mc = trajs.proposed.raw.mc2[ sel.asso.mc & sel.precs.mc ]
trajs.proposed.a_pp.mc = trajs.proposed.raw.mc2[ sel.asso.mc & sel.precp.mc ]



# -------------------------------------------------------------------------------------
# 
# 

trajs.proposed.a.mc = trajs.proposed.a.mc[ length(trajs.proposed.a.mc):1 ]
for ( i in 1:(length(trajs.proposed.a.mc)-1) ) {
  cat(i, '\t', length(trajs.proposed.a.mc), '\n')
  if ( i>=length(trajs.proposed.a.mc) )
    break
  
  sel = mclapply((i+1):length(trajs.proposed.a.mc),
    function(j, trajs) {
      ref    = trajs[[ i ]]
      prefix = trajs[[ j ]]
      
      count(ref)>=count(prefix) & is.prefix(prefix=prefix, reference=ref)
  }, trajs=trajs.proposed.a.mc, mc.cores=mc.cores)
  sel = unlist(sel)
  sel = c( rep(FALSE, i), sel )
  trajs.proposed.a.mc[ sel ] = NULL
}
trajs.proposed.a.mc = trajs.proposed.a.mc[ length(trajs.proposed.a.mc):1 ]
length(trajs.proposed.a.mc) # [1] 386

trajs.proposed.ps.mc = trajs.proposed.ps.mc[ length(trajs.proposed.ps.mc):1 ]
for ( i in 1:(length(trajs.proposed.ps.mc)-1) ) {
  cat(i, '\t', length(trajs.proposed.ps.mc), '\n')
  if ( i>=length(trajs.proposed.ps.mc) )
    break
  
  sel = mclapply((i+1):length(trajs.proposed.ps.mc),
    function(j, trajs) {
      ref    = trajs[[ i ]]
      prefix = trajs[[ j ]]
      
      count(ref)>=count(prefix) & is.prefix(prefix=prefix, reference=ref)
  }, trajs=trajs.proposed.ps.mc, mc.cores=mc.cores)
  sel = unlist(sel)
  sel = c( rep(FALSE, i), sel )
  trajs.proposed.ps.mc[ sel ] = NULL
}
trajs.proposed.ps.mc = trajs.proposed.ps.mc[ length(trajs.proposed.ps.mc):1 ]
length(trajs.proposed.ps.mc) # [1] 47

trajs.proposed.pp.mc = trajs.proposed.pp.mc[ length(trajs.proposed.pp.mc):1 ]
for ( i in 1:(length(trajs.proposed.pp.mc)-1) ) {
  cat(i, '\t', length(trajs.proposed.pp.mc), '\n')
  if ( i>=length(trajs.proposed.pp.mc) )
    break
  
  sel = mclapply((i+1):length(trajs.proposed.pp.mc),
    function(j, trajs) {
      ref    = trajs[[ i ]]
      prefix = trajs[[ j ]]
      
      count(ref)>=count(prefix) & is.prefix(prefix=prefix, reference=ref)
  }, trajs=trajs.proposed.pp.mc, mc.cores=mc.cores)
  sel = unlist(sel)
  sel = c( rep(FALSE, i), sel )
  trajs.proposed.pp.mc[ sel ] = NULL
}
trajs.proposed.pp.mc = trajs.proposed.pp.mc[ length(trajs.proposed.pp.mc):1 ]
length(trajs.proposed.pp.mc) # [1] 158

trajs.proposed.a_ps.mc = trajs.proposed.a_ps.mc[ length(trajs.proposed.a_ps.mc):1 ]
for ( i in 1:(length(trajs.proposed.a_ps.mc)-1) ) {
  cat(i, '\t', length(trajs.proposed.a_ps.mc), '\n')
  if ( i>=length(trajs.proposed.a_ps.mc) )
    break
  
  sel = mclapply((i+1):length(trajs.proposed.a_ps.mc),
    function(j, trajs) {
      ref    = trajs[[ i ]]
      prefix = trajs[[ j ]]
      
      count(ref)>=count(prefix) & is.prefix(prefix=prefix, reference=ref)
  }, trajs=trajs.proposed.a_ps.mc, mc.cores=mc.cores)
  sel = unlist(sel)
  sel = c( rep(FALSE, i), sel )
  trajs.proposed.a_ps.mc[ sel ] = NULL
}
trajs.proposed.a_ps.mc = trajs.proposed.a_ps.mc[ length(trajs.proposed.a_ps.mc):1 ]
length(trajs.proposed.a_ps.mc) # [1] 38

trajs.proposed.a_pp.mc = trajs.proposed.a_pp.mc[ length(trajs.proposed.a_pp.mc):1 ]
for ( i in 1:(length(trajs.proposed.a_pp.mc)-1) ) {
  cat(i, '\t', length(trajs.proposed.a_pp.mc), '\n')
  if ( i>=length(trajs.proposed.a_pp.mc) )
    break
  
  sel = mclapply((i+1):length(trajs.proposed.a_pp.mc),
    function(j, trajs) {
      ref    = trajs[[ i ]]
      prefix = trajs[[ j ]]
      
      count(ref)>=count(prefix) & is.prefix(prefix=prefix, reference=ref)
  }, trajs=trajs.proposed.a_pp.mc, mc.cores=mc.cores)
  sel = unlist(sel)
  sel = c( rep(FALSE, i), sel )
  trajs.proposed.a_pp.mc[ sel ] = NULL
}
trajs.proposed.a_pp.mc = trajs.proposed.a_pp.mc[ length(trajs.proposed.a_pp.mc):1 ]
length(trajs.proposed.a_pp.mc) # [1] 58

save(trajs.proposed.a.mc   , file='./data/trajectory.proposed.a.mc.rdata'   )
save(trajs.proposed.ps.mc  , file='./data/trajectory.proposed.ps.mc.rdata'  )
save(trajs.proposed.pp.mc  , file='./data/trajectory.proposed.pp.mc.rdata'  )
save(trajs.proposed.a_ps.mc, file='./data/trajectory.proposed.a_ps.mc.rdata')
save(trajs.proposed.a_pp.mc, file='./data/trajectory.proposed.a_pp.mc.rdata')

## options(stringsAsFactors = FALSE)
## index = lapply(1:281, 
##   function(i, limit){
##     from = (i-1)*100 + 1
##     to   = ifelse(i*100<=28039, i*100, 28039)
##     data.frame(i=i, from=from, to=to)
## }, limit=28039)
## index = do.call(rbind, index)
## for (i in 1:281) {
##   sink(sprintf('sel.asso.%03d.mc.r', i))
##   str01 = 'source(\'./batch/preprocess.mc.r\')\n'
##   str02 = sprintf('sink(\'./log/asso/sel.asso.%03d.mc.txt\')\n', index$i[i])
##   str03 = 'items = colnames(d.t0)\n'
##   str04 = sprintf('sel.asso.%03d = lapply(trajs.proposed.raw.mc2[%05d:%05d], \n', index$i[i], index$from[i], index$to[i])
##   str05 = '  function(traj, data0, data1, items, exclusion0, exclusion1, criteria, B, mc.cores) {\n'
##   str06 = '    cat(as.character(traj), \'\\n\')\n'
##   str07 = '    path  = get.items(traj)\n'
##   str08 = '    errs  = setdiff(items, path)\n'
##   str09 = '    subs  = rowSums(data1[, errs, drop=FALSE])==0\n'
##   str10 = '    sel = sapply(3:count(traj), function(j, traj, data0, data1, subset, exclusion0, exclusion1, criteria, B, mc.cores) {\n'
##   str11 = '      eval = filter.association(series=sub(object=traj, from=1, to=(j-1)), node=traj[j], data0=data0[subset, , drop=FALSE], data1=data1[subset, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1, B=B, mc.cores=mc.cores)\n'
##   str12 = '      cat(sprintf(\'%-70s eval: %1.4f (%1.4f, %1.4f)\\n\', as.character(sub(object=traj, from=1, to=j)), median(eval), quantile(eval, .025), quantile(eval, .975)))\n'
##   str13 = '      return( !(quantile(eval, .025) <= criteria) )\n'
##   str14 = '    }, traj=traj, data0=data0, data1=data1, subset=subs, exclusion0=exclusion0, exclusion1=exclusion1, criteria=criteria, B=B, mc.cores=mc.cores)\n'
##   str15 = '    return( all(sel) )\n'
##   str16 = '}, data0=d.t0, data1=d.t1, items=items, exclusion0=TRUE, exclusion1=FALSE, criteria=1.0, B=250, mc.cores=mc.cores)\n'
##   str17 = sprintf('save(sel.asso.%03d, file=\'./tmp/asso/sel.asso.%03d.mc.rdata\')\n', index$i[i], index$i[i])
##   
##   cat(paste(str01, str02, str03, str04, str05, str06, str07, str08, str09, str10, str11, str12, str13, str14, str15, str16, str17, sep=''))
##   sink(NULL)
## }
## for (i in 1:281) {
##   sink(sprintf('sel.asso.%03d.mc.pbs', i))
##   str01 = '#!/bin/bash -l\n'
##   str02 = '#PBS -l walltime=8:00:00,nodes=1:ppn=8\n'
##   str03 = '#PBS -m abe\n'
##   str04 = '#PBS -M ohxxx215@umn.edu\n'
##   str05 = 'module load R/3.4.4\n'
##   str06 = 'cd ~/Xinpeng/trajs/\n'
##   str07 = sprintf('R CMD BATCH --no-save --no-restore ./batch/asso/sel.asso.%03d.mc.r\n', index$i[i])
##   
##   cat(paste(str01, str02, str03, str04, str05, str06, str07, sep=''))
##   sink(NULL)
## }
## 
## options(stringsAsFactors = FALSE)
## index = lapply(1:281, 
##   function(i, limit){
##     from = (i-1)*100 + 1
##     to   = ifelse(i*100<=28039, i*100, 28039)
##     data.frame(i=i, from=from, to=to)
## }, limit=28039)
## index = do.call(rbind, index)
## for (i in 1:281) {
##   sink(sprintf('sel.precp.%03d.mc.r', i))
##   str01 = 'source(\'./batch/preprocess.mc.r\')\n'
##   str02 = sprintf('sink(\'./log/precp/sel.precp.%03d.mc.txt\')\n', index$i[i])
##   str03 = 'items = colnames(d.t0)\n'
##   str04 = sprintf('sel.precp.%03d = lapply(trajs.proposed.raw.mc2[%05d:%05d], \n', index$i[i], index$from[i], index$to[i])
##   str05 = '  function(traj, data0, data1, items, exclusion0, exclusion1, criteria, B, mc.cores) {\n'
##   str06 = '    cat(as.character(traj), \'\\n\')\n'
##   str07 = '    path  = get.items(traj)\n'
##   str08 = '    errs  = setdiff(items, path)\n'
##   str09 = '    subs  = rowSums(data1[, errs, drop=FALSE])==0\n'
##   str10 = '    sel = sapply(3:count(traj), function(j, traj, data0, data1, subset, exclusion0, exclusion1, criteria, B, mc.cores) {\n'
##   str11 = '      eval = filter.precedence(series=sub(object=traj, from=1, to=(j-1)), node=traj[j], data0=data0[subset, , drop=FALSE], data1=data1[subset, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1, B=B, mc.cores=mc.cores)\n'
##   str12 = '      cat(sprintf(\'%-70s eval: %1.4f (%1.4f, %1.4f)\\n\', as.character(sub(object=traj, from=1, to=j)), median(eval), quantile(eval, .025), quantile(eval, .975)))\n'
##   str13 = '      return( !(quantile(eval, .975) <= criteria) )\n'
##   str14 = '    }, traj=traj, data0=data0, data1=data1, subset=subs, exclusion0=exclusion0, exclusion1=exclusion1, criteria=criteria, B=B, mc.cores=mc.cores)\n'
##   str15 = '    return( all(sel) )\n'
##   str16 = '}, data0=d.t0, data1=d.t1, items=items, exclusion0=TRUE, exclusion1=FALSE, criteria=1.0, B=250, mc.cores=mc.cores)\n'
##   str17 = sprintf('save(sel.precp.%03d, file=\'./tmp/precp/sel.precp.%03d.mc.rdata\')\n', index$i[i], index$i[i])
##   
##   cat(paste(str01, str02, str03, str04, str05, str06, str07, str08, str09, str10, str11, str12, str13, str14, str15, str16, str17, sep=''))
##   sink(NULL)
## }
## for (i in 1:281) {
##   sink(sprintf('sel.precp.%03d.mc.pbs', i))
##   str01 = '#!/bin/bash -l\n'
##   str02 = '#PBS -l walltime=12:00:00,nodes=1:ppn=8\n'
##   str03 = '#PBS -m abe\n'
##   str04 = '#PBS -M ohxxx215@umn.edu\n'
##   str05 = 'module load R/3.4.4\n'
##   str06 = 'cd ~/Xinpeng/trajs/\n'
##   str07 = sprintf('R CMD BATCH --no-save --no-restore ./batch/precp/sel.precp.%03d.mc.r\n', index$i[i])
##   
##   cat(paste(str01, str02, str03, str04, str05, str06, str07, sep=''))
##   sink(NULL)
## }
## 
## options(stringsAsFactors = FALSE)
## index = lapply(1:281, 
##   function(i, limit){
##     from = (i-1)*100 + 1
##     to   = ifelse(i*100<=28039, i*100, 28039)
##     data.frame(i=i, from=from, to=to)
## }, limit=28039)
## index = do.call(rbind, index)
## for (i in 1:281) {
##   sink(sprintf('sel.precs.%03d.mc.r', i))
##   str01 = 'source(\'./batch/preprocess.mc.r\')\n'
##   str02 = sprintf('sink(\'./log/precs/sel.precs.%03d.mc.txt\')\n', index$i[i])
##   str03 = 'items = colnames(d.t0)\n'
##   str04 = sprintf('sel.precs.%03d = lapply(trajs.proposed.raw.mc2[%05d:%05d], \n', index$i[i], index$from[i], index$to[i])
##   str05 = '  function(traj, data0, data1, items, exclusion0, exclusion1, criteria, B, mc.cores) {\n'
##   str06 = '    cat(as.character(traj), \'\\n\')\n'
##   str07 = '    path  = get.items(traj)\n'
##   str08 = '    errs  = setdiff(items, path)\n'
##   str09 = '    subs  = rowSums(data1[, errs, drop=FALSE])==0\n'
##   str10 = '    sel = sapply(3:count(traj), function(j, traj, data0, data1, subset, exclusion0, exclusion1, criteria, B, mc.cores) {\n'
##   str11 = '      eval = filter.precedence(series=sub(object=traj, from=1, to=(j-1)), node=traj[j], data0=data0[subset, , drop=FALSE], data1=data1[subset, , drop=FALSE], exclusion0=exclusion0, exclusion1=exclusion1, B=B, mc.cores=mc.cores)\n'
##   str12 = '      cat(sprintf(\'%-70s eval: %1.4f (%1.4f, %1.4f)\\n\', as.character(sub(object=traj, from=1, to=j)), median(eval), quantile(eval, .025), quantile(eval, .975)))\n'
##   str13 = '      return( !(quantile(eval, .025) <= criteria) )\n'
##   str14 = '    }, traj=traj, data0=data0, data1=data1, subset=subs, exclusion0=exclusion0, exclusion1=exclusion1, criteria=criteria, B=B, mc.cores=mc.cores)\n'
##   str15 = '    return( all(sel) )\n'
##   str16 = '}, data0=d.t0, data1=d.t1, items=items, exclusion0=TRUE, exclusion1=FALSE, criteria=1.0, B=250, mc.cores=mc.cores)\n'
##   str17 = sprintf('save(sel.precs.%03d, file=\'./tmp/precs/sel.precs.%03d.mc.rdata\')\n', index$i[i], index$i[i])
##   
##   cat(paste(str01, str02, str03, str04, str05, str06, str07, str08, str09, str10, str11, str12, str13, str14, str15, str16, str17, sep=''))
##   sink(NULL)
## }
## for (i in 1:281) {
##   sink(sprintf('sel.precs.%03d.mc.pbs', i))
##   str01 = '#!/bin/bash -l\n'
##   str02 = '#PBS -l walltime=12:00:00,nodes=1:ppn=8\n'
##   str03 = '#PBS -m abe\n'
##   str04 = '#PBS -M ohxxx215@umn.edu\n'
##   str05 = 'module load R/3.4.4\n'
##   str06 = 'cd ~/Xinpeng/trajs/\n'
##   str07 = sprintf('R CMD BATCH --no-save --no-restore ./batch/precs/sel.precs.%03d.mc.r\n', index$i[i])
##   
##   cat(paste(str01, str02, str03, str04, str05, str06, str07, sep=''))
##   sink(NULL)
## }









