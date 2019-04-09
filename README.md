# Random forrest (Grupparbete 1 inom "Big data"-kurs)

Bara ett litet utkast"! Se `rf.R` för lite kod och `.png`-filen för en liten illustration. 

Hjälpfunktoinen `rdata` kan användas för olika storlekar av `n` och `p`. Har nu bara testat med fixt `n` och variabelt `p`. 
Endast okorrelerade X-variabler än så länge!
Jämförelser av standardmåtten enligt `yardstick::metrics` (bör kanske tänka till vad som är relevant). Endast regression (men bör vara lätt ändra till klassifikatoin också ev). 

## Frågan

> Use decision tree/random forests methods and compare their performance for high-dimensional data 
> (p increases vs n, for increasing p: additional unrelated vs related predictor variables). 
> Focus on variable selection through random forests.
