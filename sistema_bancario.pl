/* Programma Prolog per la gestione di un sistema bancario semplice. */

main :- nl,
        write('--- BENVENUTO NEL SISTEMA BANCARIO ---'), nl,
        leggi_numero_conti(N),
        crea_conti(N, 1, [], Conti), nl,
        write('Conti creati:'), nl,
        stampa_conti(Conti),
        menu(Conti).

/* Il predicato leggi_numero_conti legge il numero di conti da creare:
   - il suo unico argomento (nel risultato) è il numero letto. */

leggi_numero_conti(N) :- write('Quanti conti vuoi creare? '),
                         read(N),
                         integer(N),
                         N > 0, !.
leggi_numero_conti(N) :- write('ERRORE: inserire un numero intero positivo.'), nl,
                         leggi_numero_conti(N).

/* Il predicato stampa_conti stampa in modo leggibile l'elenco dei conti:
   - il suo unico argomento è la lista dei conti da stampare. */

stampa_conti([]).
stampa_conti([conto(Num, Int, Saldo, _) | Rest]) :- format('  - Conto ~w (~w): saldo = ~2f~n', [Num, Int, Saldo]),
                                                    stampa_conti(Rest).

/* Il predicato stampa_storico stampa in modo leggibile l'elenco delle transazioni:
   - il suo unico argomento è la lista delle transazioni da stampare. */

stampa_storico([]).
stampa_storico([trans(Imp, Tipo) | Rest]) :- format('  - ~w: ~2f euro~n', [Tipo, Imp]),
                                             stampa_storico(Rest).

/* Il predicato stampa_risultato_filtro stampa l'elenco dei conti con saldo superiore alla soglia:
   - il suo unico argomento è la lista dei conti da stampare. */

stampa_risultato_filtro([]) :- write('  Nessun conto trovato.'), nl.
stampa_risultato_filtro(Conti) :- Conti = [_ | _],
                                  stampa_conti(Conti).

/* Il predicato menu gestisce il loop principale delle operazioni:
   - il suo unico argomento è la lista dei conti corrente. */

menu(Conti) :- leggi_scelta(Scelta),
               gestisci_scelta(Scelta, Conti).

/* Il predicato gestisci_scelta gestisce la scelta dell'utente:
   - il suo primo argomento è la scelta effettuata;
   - il suo secondo argomento è la lista dei conti corrente.
   La scelta 7 termina il programma; le altre eseguono l'operazione corrispondente. */

gestisci_scelta(7, _) :- nl, write('Arrivederci!'), nl.
gestisci_scelta(Scelta, Conti) :- Scelta \= 7,
                                  esegui(Scelta, Conti, ContiNuovi),
                                  menu(ContiNuovi).

/* Il predicato esegui esegue l'operazione corrispondente alla scelta dell'utente:
   - il suo primo argomento è il numero dell'operazione (1-7);
   - il suo secondo argomento è la lista dei conti corrente;
   - il suo terzo argomento (nel risultato) è la lista dei conti aggiornata.
   Le operazioni 4, 5 e 6 non modificano la lista dei conti. */

esegui(1, Conti, ContiNuovi) :- nl, write('--- DEPOSITO ---'), nl,
                                leggi_numero_conto('Numero conto: ', Num, Conti),
                                leggi_importo_positivo('Importo da depositare: ', Importo),
                                deposita(Num, Importo, Conti, ContiNuovi),
                                format('Deposito di ~2f effettuato sul conto ~w.~n', [Importo, Num]).
    
esegui(2, Conti, ContiNuovi) :- nl, write('--- PRELIEVO ---'), nl,
                                leggi_numero_conto('Numero conto: ', Num, Conti),
                                saldo(Num, Conti, Saldo),
                                gestisci_prelievo(Saldo, Num, Conti, ContiNuovi).

esegui(3, Conti, ContiNuovi) :- nl, write('--- BONIFICO ---'), nl,
                                leggi_numero_conto('Conto sorgente: ', NumS, Conti),
                                saldo(NumS, Conti, SaldoS),
                                gestisci_bonifico(SaldoS, NumS, Conti, ContiNuovi).

esegui(4, Conti, Conti) :- nl, write('--- SALDI ---'), nl,
                           stampa_conti(Conti).

esegui(5, Conti, Conti) :- nl, write('--- MOVIMENTI DEL CONTO ---'), nl,
                           leggi_numero_conto('Numero conto: ', Num, Conti),
                           storico(Num, Conti, Transazioni),
                           stampa_storico(Transazioni).

esegui(6, Conti, Conti) :- nl, write('--- RICERCA PER SALDO ---'), nl,
                           leggi_numero('Mostra i conti con saldo maggiore di: ', Soglia),
                           filtra_per_saldo(Soglia, Conti, ContiFiltrati),
                           stampa_risultato_filtro(ContiFiltrati).

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
                                                    format('Prelievo di ~2f effettuato sul conto ~w.~n', [Importo, Num]).

/* Il predicato gestisci_bonifico gestisce il bonifico controllando il saldo:
   - il suo primo argomento è il saldo del conto sorgente;
   - il suo secondo argomento è il numero del conto sorgente;
   - il suo terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata. */

gestisci_bonifico(SaldoS, _, Conti, Conti) :- SaldoS =:= 0,
                                              write('Saldo 0: impossibile eseguire bonifico.'), nl.
gestisci_bonifico(SaldoS, NumS, Conti, ContiNuovi) :- SaldoS > 0,
                                                      leggi_numero_conto_destinatario('Conto destinatario: ', NumS, NumD, Conti),
                                                      leggi_importo_valido('Importo da bonificare: ', SaldoS, Importo),
                                                      bonifico(NumS, NumD, Importo, Conti, ContiNuovi),
                                                      format('Bonifico di ~2f da ~w a ~w effettuato.~n', [Importo, NumS, NumD]).

/* Il predicato leggi_scelta legge l'operazione scelta dall'utente dal menu:
   - il suo unico argomento (nel risultato) è il numero della scelta. */

leggi_scelta(Scelta) :- nl,
                        write('--- MENU ---'), nl,
                        write('1. Deposita'), nl,
                        write('2. Preleva'), nl,
                        write('3. Bonifico'), nl,
                        write('4. Mostra saldi'), nl,
                        write('5. Mostra movimenti conto'), nl,
                        write('6. Cerca conti per saldo'), nl,
                        write('7. Esci'), nl, nl,
                        write('Scegli operazione (digita 1-7): '),
                        read(Scelta),
                        integer(Scelta),
                        Scelta >= 1, Scelta =< 7, !.
leggi_scelta(Scelta) :- write('ERRORE: scegliere un numero tra 1 e 7.'), nl,
                        leggi_scelta(Scelta).

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
                                                     Importo =< Massimo, !.
leggi_importo_valido(Messaggio, Massimo, Importo) :- format('ERRORE: inserire un valore maggiore di 0 e minore o uguale a ~2f.~n', [Massimo]),
                                                     leggi_importo_valido(Messaggio, Massimo, Importo).

/* Il predicato leggi_numero legge un numero qualsiasi:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento (nel risultato) è il numero letto. */

leggi_numero(Messaggio, Importo) :- write(Messaggio),
                                    read(Importo),
                                    number(Importo), !.
leggi_numero(Messaggio, Importo) :- write('ERRORE: inserire un numero valido.'), nl,
                                               leggi_numero(Messaggio, Importo).

/* Il predicato leggi_intestatario legge un intestatario valido:
   - il suo unico argomento (nel risultato) è l'intestatario letto. */

leggi_intestatario(Int) :- read(Int),
                           valido_intestatario(Int), !.
leggi_intestatario(Int) :- write('ERRORE: intestatario non valido (deve contenere almeno una lettera).'), nl,
                           leggi_intestatario(Int).

/* Il predicato leggi_numero_conto legge un numero di conto esistente:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento (nel risultato) è il numero letto;
   - il suo terzo argomento è la lista dei conti. */

leggi_numero_conto(Messaggio, Num, Conti) :- write(Messaggio),
                                             read(Num),
                                             integer(Num),
                                             esiste_conto(Num, Conti), !.
leggi_numero_conto(Messaggio, Num, Conti) :- write('ERRORE: conto inesistente.'), nl,
                                             leggi_numero_conto(Messaggio, Num, Conti).

/* Il predicato leggi_numero_conto_destinatario legge un conto destinatario:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento è il numero del conto sorgente;
   - il suo terzo argomento (nel risultato) è il numero del conto destinatario letto;
   - il suo quarto argomento è la lista dei conti.
   Il conto destinatario deve essere esistente e diverso dal conto sorgente. */

leggi_numero_conto_destinatario(Messaggio, NumS, NumD, Conti) :- write(Messaggio),
                                                                 read(NumD),
                                                                 integer(NumD),
                                                                 esiste_conto(NumD, Conti),
                                                                 NumD \= NumS, !.
leggi_numero_conto_destinatario(Messaggio, NumS, NumD, Conti) :- write('ERRORE: conto inesistente o coincidente con il sorgente.'), nl,
                                                                 leggi_numero_conto_destinatario(Messaggio, NumS, NumD, Conti).

/* Il predicato esiste_conto verifica se un conto è presente nella lista:
   - il suo primo argomento è il numero del conto da cercare;
   - il suo secondo argomento è la lista dei conti. */

esiste_conto(Num, Conti) :- member(conto(Num, _, _, _), Conti).

/* Il predicato crea_conti crea N conti con numeri casuali:
   - il suo primo argomento è il numero di conti ancora da creare;
   - il suo secondo argomento è il contatore per la numerazione progressiva;
   - il suo terzo argomento è la lista dei conti accumulata;
   - il suo quarto argomento (nel risultato) è la lista dei conti completa. */

crea_conti(0, _, ContiAcc, ContiFinali) :- ContiFinali = ContiAcc.
crea_conti(N, Contatore, ContiAcc, ContiFinali) :- N > 0,
                                                   format('Inserisci l''intestatario del conto numero ~w: ', [Contatore]),
                                                   leggi_intestatario(Int),
                                                   genera_numero_casuale(ContiAcc, Num),
                                                   NuovoConto = conto(Num, Int, 0, []),
                                                   N1 is N - 1,
                                                   Contatore1 is Contatore + 1,
                                                   crea_conti(N1, Contatore1, [NuovoConto | ContiAcc], ContiFinali).

/* Il predicato genera_numero_casuale genera un numero di conto casuale non ancora usato:
   - il suo primo argomento è la lista dei conti esistenti;
   - il suo secondo argomento (nel risultato) è il numero generato. */

genera_numero_casuale(Conti, Num) :- random(1000, 9999, Num),
                                     \+ esiste_conto(Num, Conti), !.
genera_numero_casuale(Conti, Num) :- get_time(T), 
                                     RandomNum is integer(1000 + (T * 1000000) mod 8999),
                                     Num is RandomNum,
                                     \+ esiste_conto(Num, Conti), !.
genera_numero_casuale(Conti, Num) :- genera_numero_sequenziale(Conti, 1000, Num).

/* Il predicato genera_numero_sequenziale genera un numero di conto
   sequenziale non ancora usato (fallback):
   - il suo primo argomento è la lista dei conti esistenti;
   - il suo secondo argomento è il tentativo corrente;
   - il suo terzo argomento (nel risultato) è il numero generato. */

genera_numero_sequenziale(Conti, Tentativo, Num) :- Tentativo < 10000,
                                                    \+ esiste_conto(Tentativo, Conti),
                                                    Num = Tentativo, !.
genera_numero_sequenziale(Conti, Tentativo, Num) :- Tentativo < 10000,
                                                    esiste_conto(Tentativo, Conti),
                                                    Prossimo is Tentativo + 1,
                                                    genera_numero_sequenziale(Conti, Prossimo, Num).

/* Il predicato filtra_per_saldo restituisce la lista dei conti con saldo maggiore della soglia:
   - il suo primo argomento è la soglia;
   - il suo secondo argomento è la lista dei conti;
   - il suo terzo argomento è la lista dei conti filtrati. */

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

/* Il predicato cerca_conto cerca un conto per numero all'interno della lista Conti:
   - il suo primo argomento è il numero del conto da cercare;
   - il suo secondo argomento è la lista dei conti in cui cercare;
   - il suo terzo argomento (nel risultato) è il conto trovato;
   - il suo quarto argomento (nel risultato) è la lista dei conti rimanente.
   Il predicato fallisce se il conto non esiste. */

cerca_conto(Num, [conto(Num, Int, Saldo, Trans) | Rest], conto(Num, Int, Saldo, Trans), Rest).
cerca_conto(Num, [C | Rest], Conto, [C | Resto]) :- C = conto(N, _, _, _),
                                                    N \= Num,
                                                    cerca_conto(Num, Rest, Conto, Resto).

/* Il predicato crea_conto aggiunge un nuovo conto con saldo zero e transazioni vuote:
   - il suo primo argomento è il numero del nuovo conto;
   - il suo secondo argomento è l'intestatario del nuovo conto;
   - il suo terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata.
   Il predicato fallisce se il numero di conto esiste già o se gli argomenti non sono validi. */

crea_conto(Num, Int, [], [conto(Num, Int, 0, [])]) :- integer(Num), 
                                                      Num > 0, 
                                                      atom(Int).
crea_conto(Num, _, [conto(Num, _, _, _) | _], _) :- !, fail.
crea_conto(Num, Int, [C | Rest], [C | NuovoRest]) :- crea_conto(Num, Int, Rest, NuovoRest).

/* Il predicato deposita aggiunge un importo positivo al saldo del conto specificato:
   - il suo primo argomento è il numero del conto su cui depositare;
   - il suo secondo argomento è l'importo da depositare;
   - il suo terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata.
   Il predicato fallisce se il conto non esiste o se l'importo non è positivo. */

deposita(Num, Importo, ContiVecchi, ContiNuovi) :- Importo > 0,
                                                   cerca_conto(Num, ContiVecchi, conto(Num, Int, Saldo, Trans), Resto),
                                                   NuovoSaldo is Saldo + Importo,
                                                   NuovaTrans = trans(Importo, deposito),
                                                   ContiNuovi = [conto(Num, Int, NuovoSaldo, [NuovaTrans | Trans]) | Resto].

/* Il predicato preleva sottrae un importo positivo dal saldo del conto specificato:
   - il suo primo argomento è il numero del conto da cui prelevare;
   - il suo secondo argomento è l'importo da prelevare;
   - il suo terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata.
   Il predicato fallisce se il conto non esiste, se l'importo non è positivo
   o se il saldo è insufficiente. */

preleva(Num, Importo, ContiVecchi, ContiNuovi) :- Importo > 0,
                                                  cerca_conto(Num, ContiVecchi, conto(Num, Int, Saldo, Trans), Resto),
                                                  Saldo >= Importo,
                                                  NuovoSaldo is Saldo - Importo,
                                                  NuovaTrans = trans(Importo, prelievo),
                                                  ContiNuovi = [conto(Num, Int, NuovoSaldo, [NuovaTrans | Trans]) | Resto].

/* Il predicato bonifico trasferisce un importo positivo dal conto sorgente al conto destinatario:
   - il suo primo argomento è il numero del conto sorgente;
   - il suo secondo argomento è il numero del conto destinatario;
   - il suo terzo argomento è l'importo da trasferire;
   - il suo quarto argomento è la lista dei conti corrente;
   - il suo quinto argomento (nel risultato) è la lista dei conti aggiornata.
   Il predicato fallisce se i due conti coincidono, se uno dei due non esiste,
   se l'importo non è positivo o se il saldo del conto sorgente è insufficiente. */

bonifico(NumS, NumD, Importo, ContiVecchi, ContiNuovi) :- NumS \= NumD,
                                                          Importo > 0,
                                                          cerca_conto(NumS, ContiVecchi, conto(NumS, IntS, SaldoS, TransS), RestoSenzaS),
                                                          SaldoS >= Importo,
                                                          cerca_conto(NumD, RestoSenzaS, conto(NumD, IntD, SaldoD, TransD), RestoFinale),
                                                          NuovoSaldoS is SaldoS - Importo,
                                                          NuovoSaldoD is SaldoD + Importo,
                                                          NuovaTransS = trans(Importo, bonifico_uscita),
                                                          NuovaTransD = trans(Importo, bonifico_entrata),
                                                          ContiNuovi = [
                                                              conto(NumS, IntS, NuovoSaldoS, [NuovaTransS | TransS]),
                                                              conto(NumD, IntD, NuovoSaldoD, [NuovaTransD | TransD])
                                                          | RestoFinale].

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
    