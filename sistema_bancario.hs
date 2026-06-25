{- Programma Haskell per la gestione di un sistema bancario semplice. -}

import Data.Char (isAlpha)                   -- necessario per usare isAlpha, che verifica se un carattere è una lettera
import Data.Maybe()                          -- necessario per usare fromJust, che estrae il valore da Maybe
import Text.Printf                           -- necessario per usare printf, che formatta i numeri con due decimali
import Text.Read (readMaybe)                 -- necessario per usare readMaybe, che legge un valore in modo sicuro
import Data.Time.Clock.POSIX (getPOSIXTime)  -- necessario per usare getPOSIXTime, che genera numeri casuali basati sul tempo

-- Tipi di dato

data Tipo = Deposito | Prelievo | BonificoUscita | BonificoEntrata
    deriving (Show, Eq)

data Transazione = Trans Double Tipo
    deriving (Show, Eq)

data Conto = Conto Int String Double [Transazione]
    deriving (Show, Eq)

{- La funzione main avvia il programma:
   - stampa il messaggio di benvenuto
   - legge il numero di conti da creare
   - crea i conti
   - stampa i conti creati
   - avvia il menu interattivo -}

main :: IO ()
main = do putStrLn ""
          putStrLn "--- BENVENUTO NEL SISTEMA BANCARIO ---"
          n <- leggiNumeroConti
          conti <- creaConti n 1 []
          putStrLn ""
          putStrLn "Conti creati:"
          stampaContiDettaglio conti
          menu conti

{- La funzione leggiNumeroConti legge il numero di conti da creare:
   - il risultato è il numero letto;
   Richiede un numero intero positivo (> 0). -}

leggiNumeroConti :: IO Int
leggiNumeroConti = do putStr "Quanti conti vuoi creare? "
                      input <- getLine
                      case readMaybe input of
                          Just n | n > 0 -> return n
                          _ -> do putStrLn "ERRORE: inserire un numero intero positivo."
                                  leggiNumeroConti

{- La funzione stampaContiDettaglio stampa l'elenco dei conti con tutti i dettagli:
   - il suo unico argomento è la lista dei conti da stampare.
   Per ogni conto mostra: numero, intestatario e saldo. -}

stampaContiDettaglio :: [Conto] -> IO ()
stampaContiDettaglio [] = return ()
stampaContiDettaglio (Conto num int saldo _ : rest) = do printf "  - Conto %d (%s): saldo = %.2f\n" num int saldo
                                                         stampaContiDettaglio rest

{- La funzione stampaContiRapidi stampa un riepilogo essenziale dei conti utili per le operazioni:
   - il suo unico argomento è la lista dei conti da stampare. -}

stampaContiRapidi :: [Conto] -> IO ()
stampaContiRapidi conti = stampaContiRapidiAux conti 1


{- La funzione stampaContiRapidiAux è una funzione ausiliaria che stampa i conti mantenendo un contatore:
   - il suo primo argomento è la lista dei conti da stampare;
   - il suo secondo argomento è il contatore che tiene traccia del numero di conti stampati.
   Questa funzione è necessaria per sapere quando è stato raggiunto il quinto conto e andare a capo. -}

stampaContiRapidiAux :: [Conto] -> Int -> IO ()
stampaContiRapidiAux [] _ = putStrLn ""
stampaContiRapidiAux [Conto num int _ _] _ = printf "[%d|%s]\n" num int
stampaContiRapidiAux (Conto num int _ _ : rest) contatore | contatore `mod` 5 == 0 = do printf "[%d|%s]\n" num int
                                                                                        stampaContiRapidiAux rest (contatore + 1)
                                                          | otherwise              = do printf "[%d|%s]\t" num int
                                                                                        stampaContiRapidiAux rest (contatore + 1)

{- La funzione stampaStorico stampa l'elenco delle transazioni di un conto:
   - il suo unico argomento è la lista delle transazioni da stampare;
   Per ogni transazione mostra: tipo e importo. -}

stampaStorico :: [Transazione] -> IO ()
stampaStorico [] = return ()
stampaStorico (Trans imp tipo : rest) = do printf "  - %s: %.2f euro\n" (mostraTipo tipo) imp
                                           stampaStorico rest

{- La funzione mostraTipo converte un tipo di transazione in stringa:
   - il suo unico argomento è il tipo di transazione;
   - il suo secondo argomento (nel risultato) è la stringa corrispondente. -}

mostraTipo :: Tipo -> String
mostraTipo Deposito        = "deposito"
mostraTipo Prelievo        = "prelievo"
mostraTipo BonificoUscita  = "bonifico_uscita"
mostraTipo BonificoEntrata = "bonifico_entrata"

{- La funzione stampaRisultatoFiltro stampa l'elenco dei conti con saldo superiore alla soglia:
   - il suo unico argomento è la lista dei conti da stampare. -}

stampaRisultatoFiltro :: [Conto] -> IO ()
stampaRisultatoFiltro [] = putStrLn "  Nessun conto soddisfa il criterio di ricerca"
stampaRisultatoFiltro conti = stampaContiDettaglio conti

{- La funzione menu gestisce le operazioni richieste dall'utente:
   - il suo unico argomento è la lista dei conti corrente. -}

menu :: [Conto] -> IO ()
menu conti = do scelta <- leggiScelta conti
                gestisciScelta scelta conti

{- La funzione gestisciScelta gestisce la scelta dell'utente:
   - il suo primo argomento è la scelta effettuata;
   - il suo secondo argomento è la lista dei conti corrente.
   La scelta 7 termina il programma, le altre eseguono l'operazione corrispondente. -}

gestisciScelta :: Int -> [Conto] -> IO ()
gestisciScelta 7 _ = do putStrLn ""
                        putStrLn "Arrivederci!"
gestisciScelta scelta conti = do contiNuovi <- esegui scelta conti
                                 menu contiNuovi

{- La funzione esegui esegue l'operazione corrispondente alla scelta dell'utente:
   - il primo argomento è il numero dell'operazione (1-7);
   - il secondo argomento è la lista dei conti corrente;
   - il suo terzo argomento (nel risultato) è la lista dei conti aggiornata.
   Le operazioni 4, 5 e 6 sono di sola lettura e lasciano invariata la lista. -}

esegui :: Int -> [Conto] -> IO [Conto]
-- Operazione 1: Deposito
esegui 1 conti = do putStrLn ""
                    putStrLn "--- DEPOSITO ---"
                    num <- leggiIdConto "Numero conto: " conti
                    importo <- leggiImportoPositivo "Importo da depositare: "
                    case deposita num importo conti of
                        Nothing -> return conti
                        Just contiNuovi -> do printf "Deposito di %.2f effettuato sul conto numero %d.\n" importo num
                                              return contiNuovi
-- Operazione 2: Prelievo
esegui 2 conti = do putStrLn ""
                    putStrLn "--- PRELIEVO ---"
                    num <- leggiIdConto "Numero conto: " conti
                    case saldo num conti of
                        Nothing -> return conti
                        Just saldoDisponibile -> gestisciPrelievo saldoDisponibile num conti
-- Operazione 3: Bonifico
esegui 3 conti | length conti == 1 = do putStrLn "ERRORE: impossibile eseguire bonifico con un solo conto."
                                        return conti
esegui 3 conti = do putStrLn ""
                    putStrLn "--- BONIFICO ---"
                    numS <- leggiIdConto "Conto ordinante: " conti
                    case saldo numS conti of
                        Nothing -> return conti
                        Just saldoS -> gestisciBonifico saldoS numS conti
-- Operazione 4: Movimenti del conto
esegui 4 conti = do putStrLn ""
                    putStrLn "--- MOVIMENTI DEL CONTO ---"
                    num <- leggiIdConto "Numero conto: " conti
                    case storico num conti of
                        Nothing -> return conti
                        Just transazioni -> do stampaStorico transazioni
                                               return conti
-- Operazione 5: Ricerca per saldo
esegui 5 conti = do putStrLn ""
                    putStrLn "--- RICERCA PER SALDO ---"
                    soglia <- leggiSogliaSaldo "Mostra i conti con saldo maggiore di euro: "
                    stampaRisultatoFiltro (filtraPerSaldo soglia conti)
                    return conti
-- Operazione 6: Saldi completi
esegui 6 conti = do putStrLn ""
                    putStrLn "--- SALDI COMPLETI ---"
                    stampaContiDettaglio conti
                    return conti
-- Caso generico
esegui _ conti = return conti

{- La funzione gestisciPrelievo gestisce il prelievo controllando il saldo:
   - il primo argomento è il saldo disponibile;
   - il secondo argomento è il numero del conto;
   - il terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata. -}

gestisciPrelievo :: Double -> Int -> [Conto] -> IO [Conto]
gestisciPrelievo saldoDisponibile _ conti | saldoDisponibile == 0 = do putStrLn "Saldo 0: impossibile eseguire prelievo."
                                                                       return conti
gestisciPrelievo saldoDisponibile num conti = do importo <- leggiImportoValido "Importo da prelevare: " saldoDisponibile
                                                 case preleva num importo conti of
                                                     Nothing -> return conti
                                                     Just contiNuovi -> do printf "Prelievo di %.2f effettuato dal conto numero %d.\n" importo num
                                                                           return contiNuovi


{- La funzione gestisciBonifico gestisce il bonifico controllando il saldo:
   - il primo argomento è il saldo del conto sorgente;
   - il secondo argomento è il numero del conto sorgente;
   - il terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata. -}

gestisciBonifico :: Double -> Int -> [Conto] -> IO [Conto]
gestisciBonifico saldoS _ conti | saldoS == 0 = do putStrLn "Saldo 0: impossibile eseguire bonifico."
                                                   return conti
gestisciBonifico saldoS numS conti = do numD <- leggiContoDestinatario "Conto beneficiario: " numS conti
                                        importo <- leggiImportoValido "Importo del bonifico: " saldoS
                                        case bonifico numS numD importo conti of
                                            Nothing -> return conti
                                            Just contiNuovi -> do printf "Bonifico di euro %.2f dal conto %d al conto %d effettuato.\n" importo numS numD
                                                                  return contiNuovi

{- La funzione leggiScelta mostra il menu e i conti, poi avvia la lettura:
   - il suo unico argomento è la lista dei conti corrente;
   - il suo secondo argomento (nel risultato) è la scelta dell'utente. -}

leggiScelta :: [Conto] -> IO Int
leggiScelta conti = do stampaMenu conti
                       leggiSceltaOperazione conti

{- La funzione stampaMenu stampa il menu delle operazioni e i conti disponibili con cui operare:
   - il suo unico argomento è la lista dei conti corrente. -}

stampaMenu :: [Conto] -> IO ()
stampaMenu conti = do putStrLn ""
                      putStrLn "--- MENU ---"
                      putStrLn "1. Deposita"
                      putStrLn "2. Preleva"
                      putStrLn "3. Bonifico"
                      putStrLn "4. Movimenti del conto"
                      putStrLn "5. Ricerca per saldo"
                      putStrLn "6. Visualizza tutti i saldi"
                      putStrLn "7. Esci"
                      putStrLn ""
                      putStrLn "Conti disponibili:"
                      stampaContiRapidi conti
                      putStrLn ""

{- La funzione leggiSceltaOperazione legge la scelta dell'utente:
   - il suo primo argomento è la lista dei conti corrente;
   - il suo secondo argomento (nel risultato)è il numero della scelta valida effettuata. -}

leggiSceltaOperazione :: [Conto] -> IO Int
leggiSceltaOperazione conti = do putStr "Scegli operazione (digita 1-7): "
                                 input <- getLine
                                 case readMaybe input of
                                     Just n | n >= 1 && n <= 7 -> return n
                                     _ -> do putStrLn "ERRORE: inserire un numero tra 1 e 7."
                                             leggiSceltaOperazione conti

{- La funzione leggiImportoPositivo legge un importo positivo:
   - il suo unico argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento (nel risultato) è l'importo letto. -}

leggiImportoPositivo :: String -> IO Double
leggiImportoPositivo msg = do putStr msg
                              input <- getLine
                              case readMaybe input of
                                  Just imp | imp > 0 -> return imp
                                  _ -> do putStrLn "ERRORE: inserire un importo positivo."
                                          leggiImportoPositivo msg

{- La funzione leggiImportoValido legge un importo compreso tra 0 e un massimo:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento è il valore massimo consentito;
   - il suo terzo argomento (nel risultato) è l'importo letto. -}

leggiImportoValido :: String -> Double -> IO Double
leggiImportoValido msg massimo = do putStr msg
                                    input <- getLine
                                    case readMaybe input of
                                        Just imp | imp > 0 && imp < massimo -> return imp
                                        _ -> do printf "ERRORE: inserire un valore maggiore di 0 e minore di %.2f.\n" massimo
                                                leggiImportoValido msg massimo

{- La funzione leggiSogliaSaldo legge un numero non negativo per la ricerca per saldo:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento (nel risultato) è il numero letto. -}

leggiSogliaSaldo :: String -> IO Double
leggiSogliaSaldo msg = do putStr msg
                          input <- getLine
                          case readMaybe input of
                              Just soglia | soglia >= 0 -> return soglia
                              _ -> do putStrLn "ERRORE: inserire un numero non negativo"
                                      leggiSogliaSaldo msg

{- La funzione leggiIntestatario legge un intestatario valido:
   - il suo unico argomento (nel risultato) è l'intestatario letto. -}

leggiIntestatario :: IO String
leggiIntestatario = do input <- getLine
                       case validoIntestatario input of
                           True  -> return input
                           False -> do putStrLn "ERRORE: intestatario non valido (deve contenere almeno una lettera)."
                                       putStr "Inserisci nuovamente l'intestatario: "
                                       leggiIntestatario

{- La funzione leggiIdConto legge un numero di conto esistente:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento è la lista dei conti;
   - il suo terzo argomento (nel risultato) è il numero del conto letto. -}

leggiIdConto :: String -> [Conto] -> IO Int
leggiIdConto msg conti = do putStr msg
                            input <- getLine
                            case readMaybe input of
                                Just num | esisteConto num conti -> return num
                                _ -> do putStrLn "ERRORE: conto inesistente."
                                        leggiIdConto msg conti

{- La funzione leggiContoDestinatario legge un conto destinatario:
   - il suo primo argomento è il messaggio da mostrare all'utente;
   - il suo secondo argomento è il numero del conto sorgente;
   - il suo terzo argomento è la lista dei conti;
   - il suo quarto argomento (nel risultato) è il numero del conto destinatario letto.
   Il conto destinatario deve essere esistente e diverso dal conto sorgente. -}

leggiContoDestinatario :: String -> Int -> [Conto] -> IO Int
leggiContoDestinatario msg numS conti = do putStr msg
                                           input <- getLine
                                           case readMaybe input of
                                               Just numD | esisteConto numD conti && numD /= numS -> return numD
                                               _ -> do putStrLn "ERRORE: conto inesistente o coincidente con il conto ordinante."
                                                       leggiContoDestinatario msg numS conti

{- La funzione esisteConto verifica se un conto è presente nella lista:
   - il primo argomento è il numero del conto da cercare;
   - il secondo argomento è la lista dei conti.
   Il risultato è True se il conto esiste, False altrimenti. -}

esisteConto :: Int -> [Conto] -> Bool
esisteConto _ [] = False
esisteConto num (Conto n _ _ _ : rest) | num == n  = True
                                       | otherwise = esisteConto num rest

{- La funzione creaConti crea N conti con numeri casuali:
   - il primo argomento è il numero di conti ancora da creare;
   - il secondo argomento è il contatore per la numerazione progressiva;
   - il terzo argomento è la lista dei conti accumulata;
   - il suo quarto argomento (nel risultato) è la lista dei conti completa. 
   L'uso di reverse consente di stampare in ordine i contri creati. -}

creaConti :: Int -> Int -> [Conto] -> IO [Conto]
creaConti 0 _ contiAcc = return (reverse contiAcc)
creaConti n contatore contiAcc = do printf "Inserisci l'intestatario del conto numero %d: " contatore
                                    intestatario <- leggiIntestatario
                                    numero <- generaNumeroCasuale contiAcc
                                    let nuovoConto = Conto numero intestatario 0 []
                                    creaConti (n - 1) (contatore + 1) (nuovoConto : contiAcc)

{- La funzione generaNumeroCasuale genera un numero di conto casuale non ancora usato:
   - il suo unico argomento è la lista dei conti esistenti;
   - il risultato è il numero generato.
   I numeri generati sono in ordine crescente: ogni nuovo conto ha un numero maggiore del precedente. -}

generaNumeroCasuale :: [Conto] -> IO Int
generaNumeroCasuale conti = do let ultimo = ultimoNumero conti
                                   min = ultimo + 1
                               tempo <- getPOSIXTime
                               let seed = floor (tempo * 1000000)
                                   nextRand = (seed * 1103515245 + 12345) `mod` 2^31
                                   num = min + (nextRand `mod` (9000 - min))
                               generaNumeroCasualeAux conti min num

{- La funzione generaNumeroCasualeAux è una funzione ausiliaria che tenta un secondo numero casuale:
   - il suo primo argomento è la lista dei conti esistenti;
   - il suo secondo argomento è il valore minimo consentito;
   - il suo terzo argomento è il numero da verificare.
   Se il numero è libero lo restituisce, altrimenti tenta con un secondo numero basato sul tempo. -}

generaNumeroCasualeAux :: [Conto] -> Int -> Int -> IO Int
generaNumeroCasualeAux conti min num | not (esisteConto num conti) = return num
                                     | otherwise                   = do tempo <- getPOSIXTime
                                                                        let seed = floor (tempo * 1000000 + 1)
                                                                            nextRand = (seed * 1103515245 + 12345) `mod` 2^31
                                                                            num2 = min + (nextRand `mod` (9000 - min))
                                                                        generaNumeroCasualeFallback conti min num2

{- La funzione generaNumeroCasualeFallback è una funzione ausiliaria che usa il fallback sequenziale:
   - il suo primo argomento è la lista dei conti esistenti;
   - il suo secondo argomento è il valore minimo consentito;
   - il suo terzo argomento è il numero da verificare.
   Se il numero è libero lo restituisce, altrimenti usa il generatore sequenziale. -}

generaNumeroCasualeFallback :: [Conto] -> Int -> Int -> IO Int
generaNumeroCasualeFallback conti min num | not (esisteConto num conti) = return num
                                          | otherwise                   = return (generaNumeroSequenziale conti min)

{- La funzione ultimoNumero restituisce il numero più alto tra i conti esistenti:
   - il suo unico argomento è la lista dei conti.
   Se non ci sono conti, restituisce 999 (così il primo conto parte da 1000) -}

ultimoNumero :: [Conto] -> Int
ultimoNumero [] = 999
ultimoNumero conti = maximum [n | Conto n _ _ _ <- conti]

{- La funzione generaNumeroSequenziale genera un numero di conto sequenziale non ancora usato (fallback):
   - il suo primo argomento è la lista dei conti esistenti;
   - il suo secondo argomento è il tentativo corrente (minimo consentito);
   - il suo terzo argomento (nel risultato) è il numero generato.
   Viene usata come ultima risorsa quando i numeri casuali generati sono tutti occupati. -}

generaNumeroSequenziale :: [Conto] -> Int -> Int
generaNumeroSequenziale conti tentativo | tentativo < 9999 && not (esisteConto tentativo conti) = tentativo
                                        | tentativo < 9999 = generaNumeroSequenziale conti (tentativo + 1)
                                        | otherwise        = 9999

{- La funzione filtraPerSaldo restituisce la lista dei conti con saldo maggiore della soglia:
   - il primo argomento è la soglia;
   - il secondo argomento è la lista dei conti;
   - il suo terzo argomento (nel risultato) è la lista dei conti filtrati. -}

filtraPerSaldo :: Double -> [Conto] -> [Conto]
filtraPerSaldo _ [] = []
filtraPerSaldo s (c@(Conto _ _ saldo _) : rest) | saldo > s = c : filtraPerSaldo s rest
                                                | otherwise = filtraPerSaldo s rest

{- La funzione validoIntestatario verifica che una stringa contenga almeno una lettera:
   - il suo unico argomento è la stringa da verificare.
   Il risultato è True se contiene almeno una lettera, False altrimenti -}

validoIntestatario :: String -> Bool
validoIntestatario [] = False
validoIntestatario s = any isAlpha s

{- La funzione cercaConto cerca un conto per numero all'interno della lista:
   - il primo argomento è il numero del conto da cercare;
   - il secondo argomento è la lista dei conti;
   - il risultato è una coppia contenente: il conto trovato e la lista rimanente.
   - restituisce Nothing se il conto non esiste. -}

cercaConto :: Int -> [Conto] -> Maybe (Conto, [Conto])
cercaConto _ [] = Nothing
cercaConto num (c@(Conto n _ _ _) : rest) | num == n  = Just (c, rest)
                                          | otherwise = case cercaConto num rest of
                                                          Nothing -> Nothing
                                                          Just (conto, resto) -> Just (conto, c : resto)

{- La funzione sostituisciConto sostituisce un conto nella lista mantenendo la posizione:
   - il suo primo argomento è il conto aggiornato;
   - il suo secondo argomento è la lista dei conti corrente;
   - il risultato è la lista dei conti aggiornata. -}

sostituisciConto :: Conto -> [Conto] -> [Conto]
sostituisciConto _ [] = []
sostituisciConto contoAgg@(Conto num _ _ _) (c@(Conto n _ _ _) : rest) | num == n  = contoAgg : rest
                                                                       | otherwise = c : sostituisciConto contoAgg rest

{- La funzione deposita aggiunge un importo positivo al saldo del conto specificato:
   - il primo argomento è il numero del conto su cui depositare;
   - il secondo argomento è l'importo da depositare;
   - il terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata.
   Il conto viene sostituito nella sua posizione originale, mantenendo l'ordine della lista.
   Restituisce Nothing se il conto non esiste o l'importo non è positivo. -}

deposita :: Int -> Double -> [Conto] -> Maybe [Conto]
deposita _ importo _ | importo <= 0 = Nothing
deposita num importo conti =
    case cercaConto num conti of
        Nothing -> Nothing
        Just (Conto n int saldo trans, _) ->
            let nuovoSaldo = saldo + importo
                nuovaTrans = Trans importo Deposito
                contoAgg = Conto n int nuovoSaldo (nuovaTrans : trans)
            in Just (sostituisciConto contoAgg conti)

{- La funzione preleva sottrae un importo positivo dal saldo del conto specificato:
   - il primo argomento è il numero del conto da cui prelevare;
   - il secondo argomento è l'importo da prelevare;
   - il terzo argomento è la lista dei conti corrente;
   - il suo quarto argomento (nel risultato) è la lista dei conti aggiornata.
   Il conto viene sostituito nella sua posizione originale, mantenendo l'ordine della lista.
   Restituisce Nothing se: il conto non esiste, l'importo non è positivo, o il saldo è insufficiente. -}

preleva :: Int -> Double -> [Conto] -> Maybe [Conto]
preleva _ importo _ | importo <= 0 = Nothing
preleva num importo conti =
    case cercaConto num conti of
        Nothing -> Nothing
        Just (Conto n int saldo trans, _) | saldo < importo -> Nothing
                                          | otherwise ->
                                              let nuovoSaldo = saldo - importo
                                                  nuovaTrans = Trans importo Prelievo
                                                  contoAgg = Conto n int nuovoSaldo (nuovaTrans : trans)
                                              in Just (sostituisciConto contoAgg conti)

{- La funzione bonifico trasferisce un importo positivo dal conto sorgente al conto destinatario:
   - il primo argomento è il numero del conto sorgente;
   - il secondo argomento è il numero del conto destinatario;
   - il terzo argomento è l'importo da trasferire;
   - il quarto argomento è la lista dei conti corrente;
   - il suo quinto argomento (nel risultato) è la lista dei conti aggiornata.
   I conti vengono sostituiti nella loro posizione originale, mantenendo l'ordine della lista.
   Restituisce Nothing se: i conti coincidono, uno dei due non esiste, l'importo non è positivo, o il saldo è insufficiente. -}

bonifico :: Int -> Int -> Double -> [Conto] -> Maybe [Conto]
bonifico numS numD importo _ | numS == numD || importo <= 0 = Nothing
bonifico numS numD importo conti =
    case cercaConto numS conti of
        Nothing -> Nothing
        Just (Conto numSorg intS saldoS transS, _) | saldoS < importo -> Nothing
                                                   | otherwise ->
                                                       case cercaConto numD conti of
                                                           Nothing -> Nothing
                                                           Just (Conto numDest intD saldoD transD, _) ->
                                                               let nuovoSaldoS = saldoS - importo
                                                                   nuovoSaldoD = saldoD + importo
                                                                   transSorg = Trans importo BonificoUscita
                                                                   transDest = Trans importo BonificoEntrata
                                                                   contoSorgAgg = Conto numSorg intS nuovoSaldoS (transSorg : transS)
                                                                   contoDestAgg = Conto numDest intD nuovoSaldoD (transDest : transD)
                                                                   contiTemp = sostituisciConto contoSorgAgg conti
                                                               in Just (sostituisciConto contoDestAgg contiTemp)

{- La funzione saldo restituisce il saldo attuale del conto specificato:
   - il primo argomento è il numero del conto;
   - il secondo argomento è la lista dei conti;
   - il suo terzo argomento (nel risultato) è il saldo del conto.
   Restituisce Nothing se il conto non esiste. -}

saldo :: Int -> [Conto] -> Maybe Double
saldo _ [] = Nothing
saldo num (Conto n _ saldoConto _ : rest) | num == n  = Just saldoConto
                                          | otherwise = saldo num rest

{- La funzione storico restituisce la lista delle transazioni del conto specificato:
   - il primo argomento è il numero del conto;
   - il secondo argomento è la lista dei conti;
   - il suo terzo argomento (nel risultato) è la lista delle transazioni.
   Restituisce Nothing se il conto non esiste. -}

storico :: Int -> [Conto] -> Maybe [Transazione]
storico _ [] = Nothing
storico num (Conto n _ _ trans : rest) | num == n  = Just trans
                                       | otherwise = storico num rest