tbi <- function(g.start,
                r.start,
                g.end,
                r.end,
                incubation.time,
                Hg=0.842,
                Hr=0.552,
                Wbag=0.2494625,
                FcorrGreen=1,
                FcorrRed=1){

g.tea.start <- g.start*FcorrGreen-Wbag #Mass green tea at the start
r.tea.start <- r.start*FcorrRed-Wbag #Mass rooibos tea at the start

#Fraction of green tea that was decomposed
fraction.decomposed.g.tea <- 1-(g.end/g.tea.start)

#Fraction of red tea that was not decomposed
fraction.remaining.r.tea <- r.end/r.tea.start

#Litter stabilisation factor
S <- 1-(fraction.decomposed.g.tea/Hg)

#Decomposable fraction of rooibos tea
predicted.labile.fraction.r.tea <- Hr*(1-S)

#Decomposition rate
k <- log(predicted.labile.fraction.r.tea/(fraction.remaining.r.tea-(1-predicted.labile.fraction.r.tea)))/incubation.time

return(data.frame(S=S, k=k))

}
