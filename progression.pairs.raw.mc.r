
# =====================================================================================
# Objective
# - The objective of this code is to extract frequent progression pairs.
# 

# -------------------------------------------------------------------------------------
# Load libraries and parameters
# 

options(stringsAsFactors = FALSE)

library(parallel)
mc.cores = detectCores()



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

# Frequent itemsets at time 0.
s0 = dplyr::group_by_all(d.t0)
s0 = dplyr::summarize(s0, n=n())
s0 = as.data.frame(s0)
s0 = s0[s0$n>0.0001*nrow(d.t0), ] # <- To ensure bootstrap resampling (below) works smoothly.
s0 = s0[, -ncol(s0)]
rownames(s0) = NULL
s0 = apply(s0, 1, 
  function(x, items) { 
    return( items[which(x)] )
}, items=items)

# Frequent itemsets at time 1.
s1 = dplyr::group_by_all(d.t1)
s1 = dplyr::summarize(s1, n=n())
s1 = as.data.frame(s1)
s1 = s1[s1$n>0, ]
s1 = s1[, -ncol(s1)]
rownames(s1) = NULL
s1 = apply(s1, 1, 
  function(x, items) { 
    return( items[which(x)] )
}, items=items)

#
ppairs.raw.mc = lapply(s1, 
  function(x, s1) {
    lapply(s1, 
      function(x, y) {
        if ((length(x)+1)==length(y) & all(x%in%y) )
          return( list(s0=x, s1=y) )
        return( NULL )
    }, x=x)
}, s1=s1)
ppairs.raw.mc = do.call(c, ppairs.raw.mc)
ppairs.raw.mc = ppairs.raw.mc[!sapply(ppairs.raw.mc, is.null)]


sel = mclapply(ppairs.raw.mc, 
  function(ppair, d0, d1, items, min.supp, min.conf, B) {
    cat('.')
    s0 = ppair$s0
    s1 = ppair$s1
    v  = setdiff(s1, s0)
    
    eval = lapply(1:B, function(b, s0, s1, v, d0, d1) {
      set.seed(b)
      tr = sample(1:nrow(d0), nrow(d0), replace=TRUE)
      d0 = d0[tr, ]
      d1 = d1[tr, ]
      sel0 = (rowSums(d0[, s0, drop=FALSE])==length(s0)) & (rowSums(d0[, setdiff(items, s0), drop=FALSE])==0)
      sel1 = (rowSums(d1[, s1, drop=FALSE])==length(s1)) & (rowSums(d1[, setdiff(items, s1), drop=FALSE])==0)
      
      supp = mean(sel0)
      conf = mean(d1[sel0, v]) * (sum(sel1[sel0]>0))
      supp = ifelse(is.na(supp), 0, supp)
      conf = ifelse(is.na(conf), 0, conf)
      
      return( data.frame(supp=supp, conf=conf) )
    }, s0=s0, s1=s1, v=v, d0=d0, d1=d1)
    eval = do.call(rbind, eval)
    eval = quantile(eval$supp,.025)>=min.supp & quantile(eval$conf,.025)>=min.conf
    names(eval) = NULL
    
    return( eval )
}, d0=d.t0, d1=d.t1, items=items, min.supp=0.0001, min.conf=0.000, B=1000, mc.cores=mc.cores)
sel = unlist(sel)

ppairs.raw.mc = ppairs.raw.mc[ sel ]



# -------------------------------------------------------------------------------------
# 
# 

save(ppairs.raw.mc, file='./data/progression.pairs.raw.mc.rdata')









