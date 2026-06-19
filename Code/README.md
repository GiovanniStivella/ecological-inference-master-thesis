In Example.R ho semplicemente copiato l'esempio che facevano loro nella documentazione.

In Synthetic_experiments.R invece ho costruito un dataset sintetico che risponde a quello che ho scritto nella bozza di tesi ed è vagamente ispirato al referendum costituzionale del 2026.

Per ora il dataset sintetico prende in considerazione seggi che hanno tutti la stessa popolazione, divisi in Nord, Centro e Sud.

Ciascun seggio ha una sua percentuale di high_income e una percentuale complementare di low_income, con gli high_income maggiormente concentrati al Nord e meno concentrati al Sud.

Gli elettori poi hanno una propensione a votare Sì che dipende dall'income ma anche dalla collocazione geografica: in particolare, i low_income al Nord sono più propensi a votare sì rispetto ai low_income del Sud, mentre al Centro sia high_income che low_income hanno una bassa propensione a votare Sì.
Le propensioni sono divise a livello di Nord, Centro e Sud (che sono covariate) ma poi hanno ulteriori noise a livello di singolo collegio elettorale.

In questo modo si genera il dataset sintetico.

Con il dataset sintetico si possono mostrare i risultati di una regressione naif e dell'applicazione del metodo di McCartan, che ottiene risultati estremamente vicini al data-generating process.