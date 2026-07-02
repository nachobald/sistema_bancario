/* Programma Prolog per la gestione di un sistema bancario semplice. */

/* La funzione main avvia il programma:
   - stampa il messaggio di benvenuto;
   - legge il numero di conti da creare;
   - crea i conti;
   - stampa i conti creati;
   - avvia il menu interattivo. */

main :- nl,
        write('--- BENVENUTO NEL SISTEMA BANCARIO ---'), nl,
        leggi_numero_conti(N),
        crea_conti(N, 1, [], Conti), nl,
        write('Conti creati:'), nl,
        stampa_conti_dettaglio(Conti),
        menu(Conti).

/* Il predicato leggi_numero_conti legge il numero di conti da creare:
   - il suo unico argomento (nel risultato) è il numero letto.
   Richiede un numero intero positivo (> 0). */

leggi_numero_conti(N) :- write('Quanti conti vuoi creare? '),
                         read(N),
                         integer(N),
                         N > 0, !.
leggi_numero_conti(N) :- write('ERRORE: inserire un numero intero positivo.'), nl,
                         leggi_numero_conti(N).

/* Il predicato stampa_conti_dettaglio stampa l'elenco dei conti con tutti i dettagli:
   - il suo unico argomento è la lista dei conti da stampare.
   Per ogni conto mostra: numero, intestatario e saldo. */

stampa_conti_dettaglio([]).
stampa_conti_dettaglio([conto(Num, Int, Saldo, _) | Rest]) :- format('  - Conto ~w (~w): saldo = ~2f~n', [Num, Int, Saldo]),
                                                             stampa_conti_dettaglio(Rest).

/* Il predicato stampa_conti_rapidi stampa un riepilogo essenziale dei conti utili per le operazioni:
   - il suo unico argomento è la lista dei conti da stampare.
   La stampa avviene mantenendo un contatore interno; dopo 5 conti va a capo,
   altrimenti separa i conti con quattro spazi. */

stampa_conti_rapidi(Conti) :- stampa_conti_rapidi(Conti, 1).
stampa_conti_rapidi([], _) :- nl.
stampa_conti_rapidi([conto(Num, Int, _, _) | Rest], Contatore) :- Contatore mod 5 =:= 0,
                                                                  format('[~w|~w]', [Num, Int]), nl,
                                                                  NuovoContatore is Contatore + 1,
                                                                  stampa_conti_rapidi(Rest, NuovoContatore).
stampa_conti_rapidi([conto(Num, Int, _, _) | Rest], Contatore) :- format('[~w|~w]', [Num, Int]),
                                                                  write('    '),
                                                                  NuovoContatore is Contatore + 1,
                                                                  stampa_conti_rapidi(Rest, NuovoContatore).

/* Il predicato stampa_storico stampa l'elenco delle transazioni di un conto:
   - il suo unico argomento è la lista delle transazioni da stampare.
   Per ogni transazione mostra: tipo e importo. */

stampa_storico([]).
stampa_storico([trans(Imp, Tipo) | Rest]) :- format('  - ~w: ~2f euro~n', [Tipo, Imp]),
                                             stampa_storico(Rest).

/* Il predicato stampa_risultato_filtro stampa l'elenco dei conti con saldo superiore alla soglia:
   - il suo unico argomento è la lista dei conti da stampare. */

stampa_risultato_filtro([]) :- write('  Nessun conto soddisfa il criterio di ricerca'), nl.
stampa_risultato_filtro(Conti) :- Conti = [_ | _],
                                  stampa_conti_dettaglio(Conti).

/* Il predicato menu gestisce le operazioni richieste dall'utente:
   - il suo unico argomento è la lista dei conti corrente. */

menu(Conti) :- leggi_scelta(Conti, Scelta),
               gestisci_scelta(Scelta, Conti).

/* Il predicato gestisci_scelta gestisce la scelta dell'utente:
   - il suo primo argomento è la scelta effettuata;
   - il suo secondo argomento è la lista dei conti corrente.
   La scelta 7 termina il programma, le altre eseguono l'operazione corrispondente. */

gestisci_scelta(7, _) :- nl, write('Arrivederci!'), nl.
gestisci_scelta(Scelta, Conti) :- Scelta \= 7,
                                  esegui(Scelta, Conti, ContiNuovi),
                                  menu(ContiNuovi).

/* Il predicato esegui esegue l'operazione corrispondente alla scelta dell'utente:
   - il suo primo argomento è il numero dell'operazione (1-6);
   - il suo secondo argomento è la lista dei conti corrente;
   - il suo terzo argomento (nel risultato) è la lista dei conti aggiornata.
   Le operazioni 4, 5 e 6 sono di sola lettura e lasciano invariata la lista. */

/* Operazione 1: Deposito */
esegui(1, Conti, ContiNuovi) :- nl, 
                                write('--- DEPOSITO ---'), nl,
                                leggi_id_conto('Numero conto: ', Conti, Num),
                                leggi_importo_positivo('Importo da depositare: ', Importo),
                                deposita(Num, Importo, Conti, ContiNuovi),
                                format('Deposito di ~2f effettuato sul conto numero ~w.~n', [Importo, Num]). 
/* Operazione 2: Prelievo */
esegui(2, Conti, ContiNuovi) :- nl, 
                                write('--- PRELIEVO ---'), nl,
                                leggi_id_conto('Numero conto: ', Conti, Num),
                                saldo(Num, Conti, Saldo),
                                gestisci_prelievo(Saldo, Num, Conti, ContiNuovi).
/* Operazione 3: Bonifico */
esegui(3, Conti, Conti) :- length(Conti, N),
                                N =:= 1,
                                write('ERRORE: impossibile eseguire bonifico con un solo conto.'), nl.                                
esegui(3, Conti, ContiNuovi) :- nl, 
                                write('--- BONIFICO ---'), nl,
                                leggi_id_conto('Conto ordinante: ', Conti, NumS),
                                saldo(NumS, Conti, SaldoS),
                                gestisci_bonifico(SaldoS, NumS, Conti, ContiNuovi).
/* Operazione 4: Movimenti del conto */
esegui(4, Conti, Conti) :- nl, 
                           write('--- MOVIMENTI DEL CONTO ---'), nl,
                           leggi_id_conto('Numero conto: ', Conti, Num),
                           storico(Num, Conti, Transazioni),
                           stampa_storico(Transazioni).
/* Operazione 5: Ricerca per saldo */
esegui(5, Conti, Conti) :- nl, 
                           write('--- RICERCA PER SALDO ---'), nl,
                           leggi_soglia_saldo('Mostra i conti con saldo maggiore di euro: ', Soglia),
                           filtra_per_saldo(Soglia, Conti, ContiFiltrati),
                           stampa_risultato_filtro(ContiFiltrati).
/* Operazione 6: Saldi completi */
esegui(6, Conti, Conti) :- nl, 
                           write('--- SALDI COMPLETI ---'), nl,
                           stampa_conti_dettaglio(Conti).

/* Il predicato gestisci_prelievo gestisce il prelievo controllando il saldo:
   - il suo primo argomento è il saldo disponibile;
   - il suo secondo argomento è il numero del conto;
   - il suo terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata. */

gestisci_prelievo(Saldo, _, Conti, Conti) :- Saldo =:= 0,
                                             write('Saldo 0: impossibile eseguire prelievo.'), nl.
gestisci_prelievo(Saldo, Num, Conti, ContiNuovi) :- Saldo > 0,
                                                    leggi_importo_valido('Importo da prelevare: ', Saldo, Importo),
                                                    preleva(Num, Importo, Conti, ContiNuovi),
                                                    format('Prelievo di ~2f effettuato dal conto numero ~w.~n', [Importo, Num]).

/* Il predicato gestisci_bonifico gestisce il bonifico controllando il saldo:
   - il suo primo argomento è il saldo del conto sorgente;
   - il suo secondo argomento è il numero del conto sorgente;
   - il suo terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata. */

gestisci_bonifico(SaldoS, _, Conti, Conti) :- SaldoS =:= 0,
                                              write('Saldo 0: impossibile eseguire bonifico.'), nl.                                       
gestisci_bonifico(SaldoS, NumS, Conti, ContiNuovi) :- SaldoS > 0,
                                                      leggi_conto_destinatario('Conto beneficiario: ', NumS, Conti, NumD),
                                                      leggi_importo_valido('Importo del bonifico: ', SaldoS, Importo),
                                                      bonifico(NumS, NumD, Importo, Conti, ContiNuovi),
                                                      format('Bonifico di euro ~2f dal conto ~w al conto ~w effettuato.~n', [Importo, NumS, NumD]).

/* Il predicato leggi_scelta mostra il menu e i conti, poi avvia la lettura:
   - il suo primo argomento è la lista dei conti corrente;
   - il suo secondo argomento (nel risultato) è la scelta dell'utente. */

leggi_scelta(Conti, Scelta) :- stampa_menu(Conti),
                               leggi_scelta_operazione(Conti, Scelta).

/* Il predicato stampa_menu stampa il menu delle operazioni e i conti disponibili con cui operare:
   - il suo unico argomento è la lista dei conti corrente. */

stampa_menu(Conti) :- nl,
                      write('--- MENU ---'), nl,
                      write('1. Deposita'), nl,
                      write('2. Preleva'), nl,
                      write('3. Bonifico'), nl,
                      write('4. Movimenti del conto'), nl,
                      write('5. Ricerca per saldo'), nl,
                      write('6. Visualizza tutti i saldi'), nl,
                      write('7. Esci'), nl, nl,
                      write('Conti disponibili: '), nl,
                      stampa_conti_rapidi(Conti), nl.

/* Il predicato leggi_scelta_operazione legge la scelta dell'utente:
   - il suo primo argomento è la lista dei conti corrente;
   - il suo secondo argomento (nel risultato) è il numero della scelta valida effettuata. */

leggi_scelta_operazione(Conti, Scelta) :- write('Scegli operazione (digita 1-7): '),
                                          read(Scelta),
                                          integer(Scelta),
                                          Scelta >= 1, Scelta =< 7, !.
leggi_scelta_operazione(Conti, Scelta) :- write('ERRORE: inserire un numero tra 1 e 7.'), nl,
                                          leggi_scelta_operazione(Conti, Scelta).

/* Il predicato leggi_importo_positivo legge un importo positivo:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento (nel risultato) è l'importo letto. */

leggi_importo_positivo(Messaggio, Importo) :- write(Messaggio),
                                              read(Importo),
                                              number(Importo),
                                              Importo > 0, !.
leggi_importo_positivo(Messaggio, Importo) :- write('ERRORE: inserire un importo positivo.'), nl,
                                              leggi_importo_positivo(Messaggio, Importo).

/* Il predicato leggi_importo_valido legge un importo compreso tra 0 e un massimo:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento è il valore massimo consentito;
   - il suo terzo argomento (nel risultato) è l'importo letto. */

leggi_importo_valido(Messaggio, Massimo, Importo) :- write(Messaggio),
                                                     read(Importo),
                                                     number(Importo),
                                                     Importo > 0,
                                                     Importo < Massimo, !.
leggi_importo_valido(Messaggio, Massimo, Importo) :- format('ERRORE: inserire un valore maggiore di 0 e minore di ~2f.~n', [Massimo]),
                                                     leggi_importo_valido(Messaggio, Massimo, Importo).

/* Il predicato leggi_soglia_saldo legge un numero non negativo per la ricerca per saldo:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento (nel risultato) è il numero letto. */

leggi_soglia_saldo(Messaggio, Importo) :- write(Messaggio),
                                          read(Importo),
                                          number(Importo),
                                          Importo >= 0, !.
leggi_soglia_saldo(Messaggio, Importo) :- write('ERRORE: inserire un numero non negativo'), nl,
                                          leggi_soglia_saldo(Messaggio, Importo).

/* Il predicato leggi_intestatario legge un intestatario valido:
   - il suo unico argomento (nel risultato) è l'intestatario letto. */

leggi_intestatario(Int) :- read(Int),
                           valido_intestatario(Int), !.
leggi_intestatario(Int) :- write('ERRORE: intestatario non valido (deve contenere almeno una lettera).'),nl,
                           write('Inserisci nuovamente l''intestatario: '),
                           leggi_intestatario(Int).

/* Il predicato leggi_id_conto legge un numero di conto esistente:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento è la lista dei conti;
   - il suo terzo argomento (nel risultato) è il numero del conto letto. */

leggi_id_conto(Messaggio, Conti, Num) :- write(Messaggio),
                                         read(Num),
                                         integer(Num),
                                         esiste_conto(Num, Conti), !.
leggi_id_conto(Messaggio, Conti, Num) :- write('ERRORE: conto inesistente.'), nl,
                                         leggi_id_conto(Messaggio, Conti, Num).

/* Il predicato leggi_conto_destinatario legge un conto destinatario:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento è il numero del conto sorgente;
   - il suo terzo argomento è la lista dei conti;
   - il suo quarto argomento (nel risultato) è il numero del conto destinatario letto.
   Il conto destinatario deve essere esistente e diverso dal conto sorgente. */

leggi_conto_destinatario(Messaggio, NumS, Conti, NumD) :- write(Messaggio),
                                                          read(NumD),
                                                          integer(NumD),
                                                          esiste_conto(NumD, Conti),
                                                          NumD \= NumS, !.
leggi_conto_destinatario(Messaggio, NumS, Conti, NumD) :- write('ERRORE: conto inesistente o coincidente con il conto ordinante.'), nl,
                                                          leggi_conto_destinatario(Messaggio, NumS, Conti, NumD).

/* Il predicato esiste_conto verifica se un conto è presente nella lista:
   - il suo primo argomento è il numero del conto da cercare;
   - il suo secondo argomento è la lista dei conti. */

esiste_conto(Num, Conti) :- member(conto(Num, _, _, _), Conti).

/* Il predicato crea_conti crea N conti con numeri casuali:
   - il suo primo argomento è il numero di conti ancora da creare;
   - il suo secondo argomento è il contatore per la numerazione progressiva;
   - il suo terzo argomento è la lista dei conti accumulata;
   - il suo quarto argomento (nel risultato) è la lista dei conti completa. 
   L'uso di reverse consente di stampare in ordine i contri creati. */

crea_conti(0, _, ContiAcc, ContiFinali) :- reverse(ContiAcc, ContiFinali).
crea_conti(N, Contatore, ContiAcc, ContiFinali) :- N > 0,
                                                   format('Inserisci l''intestatario del conto numero ~w: ', [Contatore]),
                                                   leggi_intestatario(Int),
                                                   genera_numero_casuale(ContiAcc, Num),
                                                   NuovoConto = conto(Num, Int, 0, []),
                                                   N1 is N - 1,
                                                   Contatore1 is Contatore + 1,
                                                   crea_conti(N1, Contatore1, [NuovoConto | ContiAcc], ContiFinali).

/* Il predicato ultimo_numero restituisce il numero più alto tra i conti esistenti:
   - il suo primo argomento è la lista dei conti;
   - il suo secondo argomento (nel risultato) è il numero massimo.
   Se non ci sono conti, restituisce 999 (così il primo conto parte da 1000). */

ultimo_numero([], 999).
ultimo_numero([conto(Num, _, _, _)], Num).
ultimo_numero([conto(Num1, _, _, _) | Rest], Max) :- ultimo_numero(Rest, MaxRest),
                                                     Max is max(Num1, MaxRest).

/* Il predicato genera_numero_casuale genera un numero di conto:
   - il suo primo argomento è la lista dei conti esistenti;
   - il suo secondo argomento (nel risultato) è il numero generato.
   Se non ci sono conti, genera un numero casuale tra 1000 e 8999.
   Se ci sono già conti, genera il numero successivo (ultimo + 1). */

genera_numero_casuale(Conti, Num) :- Conti = [],
                                     random(1000, 9001, Num), !.

genera_numero_casuale(Conti, Num) :- Conti \= [],
                                     ultimo_numero(Conti, Ultimo),
                                     Num is Ultimo + 1, !.

/* Il predicato filtra_per_saldo restituisce la lista dei conti con saldo maggiore della soglia:
   - il suo primo argomento è la soglia;
   - il suo secondo argomento è la lista dei conti;
   - il suo terzo argomento (nel risultato) è la lista dei conti filtrati. */

filtra_per_saldo(S, [], []) :- S >= 0.
filtra_per_saldo(S, [C | Rest], [C | FiltratiRest]) :- S >= 0,
                                                       C = conto(_, _, Saldo, _),
                                                       Saldo > S,
                                                       filtra_per_saldo(S, Rest, FiltratiRest).
filtra_per_saldo(S, [C | Rest], Filtrati) :- S >= 0,
                                             C = conto(_, _, Saldo, _),
                                             Saldo =< S,
                                             filtra_per_saldo(S, Rest, Filtrati).

/* Il predicato valido_intestatario verifica che un atomo contenga almeno una lettera:
   - il suo unico argomento è l'atomo da verificare.
   Il predicato fallisce se l'atomo è vuoto o contiene solo caratteri non alfabetici. */

valido_intestatario(Atom) :- atom(Atom),
                             atom_codes(Atom, Codici),
                             Codici \= [],
                             member(C, Codici),
                             is_lettera(C).

/* Il predicato is_lettera verifica che un codice ASCII corrisponda a una lettera A-Z o a-z. */

is_lettera(C) :- C >= 65, C =< 90.
is_lettera(C) :- C >= 97, C =< 122.

/* Il predicato cerca_conto cerca un conto per numero all'interno della lista:
   - il suo primo argomento è il numero del conto da cercare;
   - il suo secondo argomento è la lista dei conti in cui cercare;
   - il suo terzo argomento (nel risultato) è il conto trovato;
   - il suo quarto argomento (nel risultato) è la lista dei conti rimanente.
   Il predicato fallisce se il conto non esiste. */

cerca_conto(Num, [conto(Num, Int, Saldo, Trans) | Rest], conto(Num, Int, Saldo, Trans), Rest).
cerca_conto(Num, [C | Rest], Conto, [C | Resto]) :- C = conto(N, _, _, _),
                                                    N \= Num,
                                                    cerca_conto(Num, Rest, Conto, Resto).
                                                   
/* Il predicato sostituisci_conto sostituisce un conto nella lista mantenendo la posizione:
   - il suo primo argomento è il conto aggiornato;
   - il suo secondo argomento è la lista dei conti corrente;
   - il suo terzo argomento (nel risultato) è la lista dei conti aggiornata. */

sostituisci_conto(_, [], []).
sostituisci_conto(ContoAgg, [ContoVecchio | Resto], [ContoAgg | Resto]) :- ContoVecchio = conto(Num, _, _, _),
                                                                           ContoAgg = conto(Num, _, _, _).
sostituisci_conto(ContoAgg, [C | Resto], [C | NuovoResto]) :- C = conto(NumC, _, _, _),
                                                              ContoAgg = conto(NumAgg, _, _, _),
                                                              NumC \= NumAgg,
                                                              sostituisci_conto(ContoAgg, Resto, NuovoResto).

/* Il predicato deposita aggiunge un importo positivo al saldo del conto specificato:
   - il suo primo argomento è il numero del conto su cui depositare;
   - il suo secondo argomento è l'importo da depositare;
   - il suo terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata.
   Il conto viene sostituito nella sua posizione originale, mantenendo l'ordine della lista.
   Il predicato fallisce se il conto non esiste o se l'importo non è positivo. */

deposita(Num, Importo, ContiVecchi, ContiNuovi) :- Importo > 0,
                                                   cerca_conto(Num, ContiVecchi, conto(Num, Int, Saldo, Trans), _),
                                                   NuovoSaldo is Saldo + Importo,
                                                   NuovaTrans = trans(Importo, deposito),
                                                   ContoAgg = conto(Num, Int, NuovoSaldo, [NuovaTrans | Trans]),
                                                   sostituisci_conto(ContoAgg, ContiVecchi, ContiNuovi).

/* Il predicato preleva sottrae un importo positivo dal saldo del conto specificato:
   - il suo primo argomento è il numero del conto da cui prelevare;
   - il suo secondo argomento è l'importo da prelevare;
   - il suo terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata.
   Il conto viene sostituito nella sua posizione originale, mantenendo l'ordine della lista.
   Il predicato fallisce se il conto non esiste, se l'importo non è positivo o se il saldo è insufficiente. */

preleva(Num, Importo, ContiVecchi, ContiNuovi) :- Importo > 0,
                                                  cerca_conto(Num, ContiVecchi, conto(Num, Int, Saldo, Trans), _),
                                                  Saldo > Importo,
                                                  NuovoSaldo is Saldo - Importo,
                                                  NuovaTrans = trans(Importo, prelievo),
                                                  ContoAgg = conto(Num, Int, NuovoSaldo, [NuovaTrans | Trans]),
                                                  sostituisci_conto(ContoAgg, ContiVecchi, ContiNuovi).

/* Il predicato bonifico trasferisce un importo positivo dal conto sorgente al conto destinatario:
   - il suo primo argomento è il numero del conto sorgente;
   - il suo secondo argomento è il numero del conto destinatario;
   - il suo terzo argomento è l'importo da trasferire;
   - il suo quarto argomento è la lista dei conti corrente;
   - il suo quinto argomento (nel risultato) è la lista dei conti aggiornata.
   I conti vengono sostituiti nella loro posizione originale, mantenendo l'ordine della lista.
   Il predicato fallisce se i due conti coincidono, se uno dei due non esiste, se l'importo non 
   è positivo o se il saldo del conto sorgente è insufficiente. */

bonifico(NumS, NumD, Importo, ContiVecchi, ContiNuovi) :- NumS \= NumD,
                                                          Importo > 0,
                                                          cerca_conto(NumS, ContiVecchi, conto(NumS, IntS, SaldoS, TransS), _),
                                                          SaldoS > Importo,
                                                          cerca_conto(NumD, ContiVecchi, conto(NumD, IntD, SaldoD, TransD), _),
                                                          NuovoSaldoS is SaldoS - Importo,
                                                          NuovoSaldoD is SaldoD + Importo,
                                                          NuovaTransS = trans(Importo, bonifico_uscita),
                                                          NuovaTransD = trans(Importo, bonifico_entrata),
                                                          ContoSorgAgg = conto(NumS, IntS, NuovoSaldoS, [NuovaTransS | TransS]),
                                                          ContoDestAgg = conto(NumD, IntD, NuovoSaldoD, [NuovaTransD | TransD]),
                                                          sostituisci_conto(ContoSorgAgg, ContiVecchi, ContiTemp),
                                                          sostituisci_conto(ContoDestAgg, ContiTemp, ContiNuovi).

/* Il predicato saldo restituisce il saldo attuale del conto specificato:
   - il suo primo argomento è il numero del conto;
   - il suo secondo argomento è la lista dei conti;
   - il suo terzo argomento (nel risultato) è il saldo del conto.
   Il predicato fallisce se il conto non esiste. */

saldo(Num, Conti, Saldo) :- cerca_conto(Num, Conti, conto(Num, _, Saldo, _), _).

/* Il predicato storico restituisce la lista delle transazioni del conto specificato:
   - il suo primo argomento è il numero del conto;
   - il suo secondo argomento è la lista dei conti;
   - il suo terzo argomento (nel risultato) è la lista delle transazioni.
   Il predicato fallisce se il conto non esiste. */

storico(Num, Conti, Trans) :- cerca_conto(Num, Conti, conto(Num, _, _, Trans), _).
    