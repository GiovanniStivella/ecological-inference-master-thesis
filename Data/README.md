In Texas_experiment.R ho ripreso il codice usato l'anno scorso per la Pennsylvania per cercare di recuperare i valori demografici (in questo caso quelli di educational attainment ma si può poi estendere) a livello di VTD (sigla che vuol dire electoral precinct).

In realtà il primo passaggio è recuperare i dati da ALARM, che fornisce i dati a livello di singolo electoral precinct dandoci sia il numero di voti di ciascun candidato (in più cicli elettorali) sia la composizione etnica di ciascun seggio.
Alcuni esempi della formazione di questi dataset si possono vedere in Possible_States.R.

Alla fine quello di cui abbiamo bisogno per applicare il metodo di McCartan è un file analogo a quello prodotto da ALARM ma in cui aggiungiamo alcune altri valori demografici.

Il metodo di McCartan infatti ha bisogno di outcome (risultati elettorali), total (numero di elettori o di voti espressi per electoral precinct), predittori (una divisione in categorie che si escludono reciprocamente, tipo le etnie, ma probabilmente useremo i titoli di studio) e covariate (altre indicazioni demografiche).

ALARM fornisce già gli outcome, i total e le variabili etniche che possono essere usate come covariate o come predittori.

Con un codice come Texas_experiment.R possiamo aggiungere queste variabili demografiche a livello di electoral precinct.

Devo aggiustare alcune cose in Texas_experiment.R perché sembra non funzionare per recuperare i valori come vogliamo.

Poi in realtà la cartella Data potrebbe essere spostata in Code, perché non stiamo usando dataset scaricati ma direttamente script R che fanno richieste API per ottenere i dati.