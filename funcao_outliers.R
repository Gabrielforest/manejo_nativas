bpoutlier
function (varx) 
{
  stats <- boxplot.stats(varx)$stats
  limit.top = stats[5]
  limit.bot = stats[1]
  return(varx > limit.top | varx < limit.bot)
}