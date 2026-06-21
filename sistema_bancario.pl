/* Programma Prolog per la gestione di un sistema bancario semplice. */

/* -------------------- MAIN -------------------- */

main :- nl,
        write('--- BENVENUTO NEL SISTEMA BANCARIO ---'), nl,
        leggi_numero_conti(N),
        crea_conti(N, 1, [], Conti), nl,
        write('Conti creati:'), nl,
        stampa_conti(Conti), nl,
        menu(Conti).

/* -------------------- LETTURA INPUT -------------------- */

leggi_numero_conti(N) :- write('Quanti conti vuoi creare? '),
                         read(N),
                         integer(N),
                         N > 0, !.
leggi_numero_conti(N) :- write('ERRORE: inserire un numero intero positivo.'), nl,
                         leggi_numero_conti(N).


leggi_importo_positivo(Messaggio, Importo) :- write(Messaggio),
                                              read(Importo),
                                              number(Importo),
                                              Importo > 0, !.
leggi_importo_positivo(Messaggio, Importo) :- write('ERRORE: inserire un importo positivo.'), nl,
                                              leggi_importo_positivo(Messaggio, Importo).


leggi_importo_valido(Messaggio, Massimo, Importo) :- write(Messaggio),
                                                     read(Importo),
                                                     number(Importo),
                                                     Importo > 0,
                                                     Importo =< Massimo, !.
leggi_importo_valido(Messaggio, Massimo, Importo) :- format('ERRORE: inserire un valore maggiore di 0 e minore o uguale a ~2f.~n', [Massimo]),
                                                     leggi_importo_valido(Messaggio, Massimo, Importo).


leggi_importo_qualsiasi(Messaggio, Importo) :- write(Messaggio),
                                               read(Importo),
                                               number(Importo), !.
leggi_importo_qualsiasi(Messaggio, Importo) :- write('ERRORE: inserire un numero valido.'), nl,
                                               leggi_importo_qualsiasi(Messaggio, Importo).


leggi_intestatario(Int) :- write('Intestatario del conto: '),
                           read(Int),
                           valido_intestatario(Int), !.
leggi_intestatario(Int) :- write('ERRORE: intestatario non valido (deve contenere almeno una lettera).'), nl,
                           leggi_intestatario(Int).


leggi_scelta(Scelta) :- nl,
                        write('--- MENU ---'), nl,
                        write('1. Deposita'), nl,
                        write('2. Preleva'), nl,
                        write('3. Bonifico'), nl,
                        write('4. Visualizza tutti i saldi'), nl,
                        write('5. Visualizza storico di un conto'), nl,
                        write('6. Filtra conti per saldo (maggiore di)'), nl,
                        write('7. Esci'), nl,
                        write('Scegli operazione: '),
                        read(Scelta),
                        integer(Scelta),
                        Scelta >= 1, Scelta =< 7, !.
leggi_scelta(Scelta) :- write('ERRORE: scegliere un numero tra 1 e 7.'), nl,
                        leggi_scelta(Scelta).


leggi_numero_conto(Messaggio, Num, Conti) :- write(Messaggio),
                                             read(Num),
                                             integer(Num),
                                             esiste_conto(Num, Conti), !.
leggi_numero_conto(Messaggio, Num, Conti) :- write('ERRORE: conto inesistente.'), nl,
                                             leggi_numero_conto(Messaggio, Num, Conti).

leggi_numero_conto_destinatario(Messaggio, NumS, NumD, Conti) :- write(Messaggio),
                                                                 read(NumD),
                                                                 integer(NumD),
                                                                 esiste_conto(NumD, Conti),
                                                                 NumD \= NumS, !.
leggi_numero_conto_destinatario(Messaggio, NumS, NumD, Conti) :- write('ERRORE: conto inesistente o coincidente con il sorgente.'), nl,
                                                                 leggi_numero_conto_destinatario(Messaggio, NumS, NumD, Conti).

/* -------------------- UTILITY -------------------- */

esiste_conto(Num, Conti) :- member(conto(Num, _, _, _), Conti).

stampa_conti([]).
stampa_conti([conto(Num, Int, Saldo, _) | Rest]) :- format('  - Conto ~w (~w): saldo = ~2f~n', [Num, Int, Saldo]),
                                                    stampa_conti(Rest).

/* -------------------- CREAZIONE CONTI -------------------- */

crea_conti(0, _, ContiAcc, ContiFinali) :- ContiFinali = ContiAcc.
crea_conti(N, Contatore, ContiAcc, ContiFinali) :- N > 0,
                                                   Totale is Contatore + N - 1,
                                                   format('Creazione conto ~w di ~w~n', [Contatore, Totale]),
                                                   genera_numero_casuale(ContiAcc, Num),
                                                   leggi_intestatario(Int),
                                                   NuovoConto = conto(Num, Int, 0, []),
                                                   N1 is N - 1,
                                                   Contatore1 is Contatore + 1,
                                                   crea_conti(N1, Contatore1, [NuovoConto | ContiAcc], ContiFinali).

/* Genera un numero di conto casuale non ancora usato */
genera_numero_casuale(Conti, Num) :- random(1000, 9999, Num),
                                     \+ esiste_conto(Num, Conti), !.
genera_numero_casuale(Conti, Num) :- get_time(T), 
                                     RandomNum is integer(1000 + (T * 1000000) mod 8999),
                                     Num is RandomNum,
                                     \+ esiste_conto(Num, Conti), !.
genera_numero_casuale(Conti, Num) :- genera_numero_sequenziale(Conti, 1000, Num).


genera_numero_sequenziale(Conti, Tentativo, Num) :- Tentativo < 10000,
                                                    \+ esiste_conto(Tentativo, Conti),
                                                    Num = Tentativo, !.
genera_numero_sequenziale(Conti, Tentativo, Num) :- Tentativo < 10000,
                                                    esiste_conto(Tentativo, Conti),
                                                    Prossimo is Tentativo + 1,
                                                    genera_numero_sequenziale(Conti, Prossimo, Num).

/* -------------------- MENU -------------------- */

menu(Conti) :- leggi_scelta(Scelta),
               esegui_menu(Scelta, Conti).

/* Gestisce l'uscita */
esegui_menu(7, _) :- nl, write('Arrivederci!'), nl.
esegui_menu(Scelta, Conti) :- Scelta \= 7,
                              esegui(Scelta, Conti, ContiNuovi),
                              menu(ContiNuovi).

/* -------------------- OPERAZIONI -------------------- */

/* TUTTI I PREDICATI esegui/3 SONO RAGGRUPPATI INSIEME */
esegui(1, Conti, ContiNuovi) :- nl, write('--- DEPOSITO ---'), nl,
                                leggi_numero_conto('Numero conto: ', Num, Conti),
                                leggi_importo_positivo('Importo da depositare: ', Importo),
                                deposita(Num, Importo, Conti, ContiNuovi),
                                format('Deposito di ~2f effettuato sul conto ~w.~n', [Importo, Num]).
    
esegui(2, Conti, ContiNuovi) :- nl, write('--- PRELIEVO ---'), nl,
                                leggi_numero_conto('Numero conto: ', Num, Conti),
                                saldo(Num, Conti, Saldo),
                                esegui_prelievo(Saldo, Num, Conti, ContiNuovi).

esegui(3, Conti, ContiNuovi) :- nl, write('--- BONIFICO ---'), nl,
                                leggi_numero_conto('Conto sorgente: ', NumS, Conti),
                                saldo(NumS, Conti, SaldoS),
                                esegui_bonifico(SaldoS, NumS, Conti, ContiNuovi).

esegui(4, Conti, Conti) :- nl, write('--- SALDI DI TUTTI I CONTI ---'), nl,
                           stampa_conti(Conti).

esegui(5, Conti, Conti) :- nl, write('--- STORICO DI UN CONTO ---'), nl,
                           leggi_numero_conto('Numero conto: ', Num, Conti),
                           storico(Num, Conti, Transazioni),
                           stampa_storico(Transazioni).

esegui(6, Conti, Conti) :- nl, write('--- FILTRA CONTI PER SALDO (maggiore di) ---'), nl,
                           leggi_importo_qualsiasi('Inserisci il valore soglia: ', Soglia),
                           filtra_per_saldo(Soglia, Conti, ContiFiltrati), nl,
                           stampa_risultato_filtro(ContiFiltrati).

esegui(7, Conti, Conti) :- nl, write('Arrivederci!'), nl.

/* Predicati di supporto per le operazioni (NON sono esegui/3) */
esegui_prelievo(Saldo, _, Conti, Conti) :- Saldo =:= 0,
                                           write('Saldo 0: impossibile eseguire prelievo.'), nl.
esegui_prelievo(Saldo, Num, Conti, ContiNuovi) :- Saldo > 0,
                                                  leggi_importo_valido('Importo da prelevare: ', Saldo, Importo),
                                                  preleva(Num, Importo, Conti, ContiNuovi),
                                                  format('Prelievo di ~2f effettuato sul conto ~w.~n', [Importo, Num]).

esegui_bonifico(SaldoS, _, Conti, Conti) :- SaldoS =:= 0,
                                            write('Saldo 0: impossibile eseguire bonifico.'), nl.
esegui_bonifico(SaldoS, NumS, Conti, ContiNuovi) :- SaldoS > 0,
                                                    leggi_numero_conto_destinatario('Conto destinatario: ', NumS, NumD, Conti),
                                                    leggi_importo_valido('Importo da bonificare: ', SaldoS, Importo),
                                                    bonifico(NumS, NumD, Importo, Conti, ContiNuovi),
                                                    format('Bonifico di ~2f da ~w a ~w effettuato.~n', [Importo, NumS, NumD]).

/* -------------------- FILTRO PER SALDO -------------------- */

/* Il predicato stampa_risultato_filtro stampa l'elenco dei conti con saldo superiore alla soglia:
   - il suo unico argomento è la lista dei conti da stampare. */
stampa_risultato_filtro([]) :- write('  Nessun conto trovato.'), nl.
stampa_risultato_filtro(Conti) :- Conti = [_ | _],
                                  stampa_conti(Conti).

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

/* -------------------- VALIDAZIONE -------------------- */

valido_intestatario(Atom) :- atom(Atom),
                             atom_codes(Atom, Codici),
                             Codici \= [],
                             member(C, Codici),
                             is_lettera(C).

is_lettera(C) :- C >= 65, C =< 90.
is_lettera(C) :- C >= 97, C =< 122.

/* -------------------- STAMPA -------------------- */

stampa_storico([]).
stampa_storico([trans(Imp, Tipo) | Rest]) :- format('  - ~w: ~2f euro~n', [Tipo, Imp]),
                                             stampa_storico(Rest).

/* -------------------- PREDICATI CORE -------------------- */

cerca_conto(Num, [conto(Num, Int, Saldo, Trans) | Rest], conto(Num, Int, Saldo, Trans), Rest).
cerca_conto(Num, [C | Rest], Conto, [C | Resto]) :- C = conto(N, _, _, _),
                                                    N \= Num,
                                                    cerca_conto(Num, Rest, Conto, Resto).


crea_conto(Num, Int, [], [conto(Num, Int, 0, [])]) :- integer(Num), 
                                                      Num > 0, 
                                                      atom(Int).
crea_conto(Num, _, [conto(Num, _, _, _) | _], _) :- !, fail.
crea_conto(Num, Int, [C | Rest], [C | NuovoRest]) :- crea_conto(Num, Int, Rest, NuovoRest).


deposita(Num, Importo, ContiVecchi, ContiNuovi) :- Importo > 0,
                                                   cerca_conto(Num, ContiVecchi, conto(Num, Int, Saldo, Trans), Resto),
                                                   NuovoSaldo is Saldo + Importo,
                                                   NuovaTrans = trans(Importo, deposito),
                                                   ContiNuovi = [conto(Num, Int, NuovoSaldo, [NuovaTrans | Trans]) | Resto].


preleva(Num, Importo, ContiVecchi, ContiNuovi) :- Importo > 0,
                                                  cerca_conto(Num, ContiVecchi, conto(Num, Int, Saldo, Trans), Resto),
                                                  Saldo >= Importo,
                                                  NuovoSaldo is Saldo - Importo,
                                                  NuovaTrans = trans(Importo, prelievo),
                                                  ContiNuovi = [conto(Num, Int, NuovoSaldo, [NuovaTrans | Trans]) | Resto].

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


saldo(Num, Conti, Saldo) :- cerca_conto(Num, Conti, conto(Num, _, Saldo, _), _).

storico(Num, Conti, Trans) :- cerca_conto(Num, Conti, conto(Num, _, _, Trans), _).
    