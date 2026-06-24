{- Programma Haskell per la gestione di un sistema bancario semplice. -}

import Data.Char (isAlpha)
import Data.Maybe (fromJust)
import Text.Printf
import Text.Read (readMaybe)
-- import System.Random (randomRIO)  -- RIMOSSO
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Tipi di dato

data Tipo =
      Deposito
    | Prelievo
    | BonificoUscita
    | BonificoEntrata
    deriving (Show, Eq)

data Transazione =
    Trans Double Tipo
    deriving (Show, Eq)

data Conto =
    Conto Int String Double [Transazione]
    deriving (Show, Eq)

main :: IO ()
main = do
    putStrLn ""
    putStrLn "--- BENVENUTO NEL SISTEMA BANCARIO ---"
    n <- leggiNumeroConti
    conti <- creaConti n 1 []
    putStrLn ""
    putStrLn "Conti creati:"
    stampaContiDettaglio conti
    menu conti

{- La funzione leggiNumeroConti legge il numero di conti da creare:
   - il risultato è il numero letto. -}

leggiNumeroConti :: IO Int
leggiNumeroConti = do
    putStr "Quanti conti vuoi creare? "
    input <- getLine

    case readMaybe input of
        Just n | n > 0 -> return n
        _ -> do
            putStrLn "ERRORE: inserire un numero intero positivo."
            leggiNumeroConti

{- La funzione stampaContiDettaglio stampa l'elenco dei conti con tutti i relativi dettagli:
   - il suo unico argomento è la lista dei conti da stampare. -}

stampaContiDettaglio :: [Conto] -> IO ()
stampaContiDettaglio [] = return ()

stampaContiDettaglio (Conto num intestatario saldo _ : rest) = do
    printf "  - Conto %d (%s): saldo = %.2f\n"
           num intestatario saldo
    stampaContiDettaglio rest

{- La funzione stampaContiRapidi stampa un riepilogo essenziale dei conti utili per le operazioni:
   - il suo unico argomento è la lista dei conti da stampare.
   Dopo 5 conti va a capo per una migliore leggibilità. -}

stampaContiRapidi :: [Conto] -> IO ()
stampaContiRapidi conti =
    stampaContiRapidiAux conti 1

stampaContiRapidiAux :: [Conto] -> Int -> IO ()

stampaContiRapidiAux [] _ =
    putStrLn ""

stampaContiRapidiAux [Conto num int _ _] _ = do
    printf "[%d|%s]\n" num int

stampaContiRapidiAux (Conto num int _ _ : rest) contatore
    | contatore `mod` 5 == 0 = do
        printf "[%d|%s]\n" num int
        stampaContiRapidiAux rest (contatore + 1)

    | otherwise = do
        printf "[%d|%s]\t" num int
        stampaContiRapidiAux rest (contatore + 1)

{- La funzione stampaStorico stampa in modo leggibile l'elenco delle transazioni:
   - il suo unico argomento è la lista delle transazioni da stampare. -}

stampaStorico :: [Transazione] -> IO ()
stampaStorico [] = return ()

stampaStorico (Trans imp tipo : rest) = do
    printf "  - %s: %.2f euro\n"
           (mostraTipo tipo)
           imp

    stampaStorico rest

mostraTipo :: Tipo -> String

mostraTipo Deposito = "deposito"

mostraTipo Prelievo = "prelievo"

mostraTipo BonificoUscita = "bonifico_uscita"

mostraTipo BonificoEntrata = "bonifico_entrata"

{- La funzione stampaRisultatoFiltro stampa l'elenco dei conti con saldo superiore alla soglia:
   - il suo unico argomento è la lista dei conti da stampare. -}

stampaRisultatoFiltro :: [Conto] -> IO ()

stampaRisultatoFiltro [] =
    putStrLn "  Nessun conto soddisfa il criterio di ricerca"

stampaRisultatoFiltro conti =
    stampaContiDettaglio conti

{- La funzione menu gestisce le operazioni richieste dall'utente:
   - il suo unico argomento è la lista corrente dei conti. -}

menu :: [Conto] -> IO ()
menu conti = do
    scelta <- leggiScelta conti
    gestisciScelta scelta conti

{- La funzione gestisciScelta gestisce la scelta dell'utente:
   - il primo argomento è la scelta effettuata;
   - il secondo argomento è la lista dei conti corrente.
   La scelta 7 termina il programma. -}

gestisciScelta :: Int -> [Conto] -> IO ()

gestisciScelta 7 _ = do
    putStrLn ""
    putStrLn "Arrivederci!"

gestisciScelta scelta conti = do
    contiNuovi <- esegui scelta conti
    menu contiNuovi

{- La funzione leggiScelta mostra il menu e i conti,
   poi avvia la lettura della scelta. -}

leggiScelta :: [Conto] -> IO Int
leggiScelta conti = do
    stampaMenu conti
    leggiSceltaOperazione

{- La funzione stampaMenu stampa il menu delle operazioni e i conti disponibili. -}

stampaMenu :: [Conto] -> IO ()
stampaMenu conti = do
    putStrLn ""
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
   - il risultato è un numero compreso tra 1 e 7. -}

leggiSceltaOperazione :: IO Int
leggiSceltaOperazione = do
    putStr "Scegli operazione (digita 1-7): "
    input <- getLine
    case readMaybe input of
        Just n
            | n >= 1 && n <= 7 ->
                return n
        _ -> do
            putStrLn "ERRORE: inserire un numero tra 1 e 7."
            leggiSceltaOperazione

{- La funzione leggiImportoPositivo legge un importo positivo:
   - il suo argomento è il messaggio da mostrare all'utente. -}

leggiImportoPositivo :: String -> IO Double
leggiImportoPositivo msg = do
    putStr msg
    input <- getLine
    case readMaybe input of
        Just imp
            | imp > 0 ->
                return imp
        _ -> do
            putStrLn "ERRORE: inserire un importo positivo."
            leggiImportoPositivo msg

{- La funzione leggiImportoValido legge un importo compreso tra 0 e un massimo:
   - il primo argomento è il messaggio da mostrare all'utente;
   - il secondo argomento è il valore massimo consentito;
   - il risultato è l'importo letto. -}

leggiImportoValido :: String -> Double -> IO Double
leggiImportoValido msg massimo = do

    putStr msg
    input <- getLine
    case readMaybe input of
        Just imp
            | imp > 0
            , imp < massimo ->
                return imp
        _ -> do
            printf
                "ERRORE: inserire un valore maggiore di 0 e minore di %.2f.\n"
                massimo
            leggiImportoValido msg massimo

{- La funzione leggiSogliaSaldo legge un numero non negativo per la ricerca per saldo:
   - il primo argomento è il messaggio da mostrare all'utente;
   - il risultato è il numero letto. -}

leggiSogliaSaldo :: String -> IO Double
leggiSogliaSaldo msg = do
    putStr msg
    input <- getLine
    case readMaybe input of
        Just soglia
            | soglia >= 0 ->
                return soglia
        _ -> do
            putStrLn "ERRORE: inserire un numero non negativo"
            leggiSogliaSaldo msg

{- La funzione leggiIntestatario legge un intestatario valido:
   - il risultato è l'intestatario letto. -}

leggiIntestatario :: IO String
leggiIntestatario = do
    input <- getLine
    case validoIntestatario input of
        True ->
            return input
        False -> do
            putStrLn
                "ERRORE: intestatario non valido (deve contenere almeno una lettera)."
            putStr "Inserisci nuovamente l'intestatario: "
            leggiIntestatario

{- La funzione leggiIdConto legge un numero di conto esistente:
   - il primo argomento è il messaggio da mostrare all'utente;
   - il secondo argomento è la lista dei conti;
   - il risultato è il numero del conto letto. -}

leggiIdConto :: String -> [Conto] -> IO Int
leggiIdConto msg conti = do
    putStr msg
    input <- getLine
    case readMaybe input of
        Just num
            | esisteConto num conti ->
                return num
        _ -> do
            putStrLn "ERRORE: conto inesistente."
            leggiIdConto msg conti

{- La funzione leggiContoDestinatario legge un conto destinatario:
   - il primo argomento è il messaggio da mostrare all'utente;
   - il secondo argomento è il numero del conto sorgente;
   - il terzo argomento è la lista dei conti;
   - il risultato è il numero del conto destinatario letto.
   Il conto destinatario deve essere esistente e diverso dal conto sorgente. -}

leggiContoDestinatario :: String -> Int -> [Conto] -> IO Int
leggiContoDestinatario msg numS conti = do
    putStr msg
    input <- getLine
    case readMaybe input of
        Just numD
            | esisteConto numD conti
            , numD /= numS ->
                return numD
        _ -> do
            putStrLn
                "ERRORE: conto inesistente o coincidente con il conto ordinante."
            leggiContoDestinatario msg numS conti

{- La funzione esisteConto verifica se un conto è presente nella lista:
   - il primo argomento è il numero del conto da cercare;
   - il secondo argomento è la lista dei conti. -}

esisteConto :: Int -> [Conto] -> Bool
esisteConto _ [] = False
esisteConto num (Conto n _ _ _ : rest)
    | num == n = True
    | otherwise = esisteConto num rest

{- La funzione creaConti crea N conti con numeri casuali:
   - il primo argomento è il numero di conti ancora da creare;
   - il secondo argomento è il contatore progressivo;
   - il terzo argomento è la lista dei conti accumulata;
   - il risultato è la lista completa dei conti. -}

creaConti :: Int -> Int -> [Conto] -> IO [Conto]
creaConti 0 _ contiAcc = return contiAcc

creaConti n contatore contiAcc = do
    printf
        "Inserisci l'intestatario del conto numero %d: "
        contatore
    intestatario <- leggiIntestatario
    numero <- generaNumeroCasuale contiAcc
    let nuovoConto =
            Conto numero intestatario 0 []
    creaConti
        (n - 1)
        (contatore + 1)
        (nuovoConto : contiAcc)

{- La funzione generaNumeroCasuale genera un numero di conto casuale non ancora usato:
   - il suo argomento è la lista dei conti esistenti;
   - il risultato è il numero generato. -}

generaNumeroCasuale :: [Conto] -> IO Int
generaNumeroCasuale conti = do
    tempo <- getPOSIXTime
    let seed = floor (tempo * 1000000)
        -- Semplice generatore pseudo-casuale lineare (LCG)
        nextRand seed' = (seed' * 1103515245 + 12345) `mod` 2^31
        rand = nextRand seed
        num = 1000 + (rand `mod` 9000)
    if not (esisteConto num conti)
        then return num
        else return (generaNumeroSequenziale conti 1000)

{- La funzione generaNumeroSequenziale genera un numero di conto
   sequenziale non ancora usato (fallback):
   - il primo argomento è la lista dei conti esistenti;
   - il secondo argomento è il tentativo corrente;
   - il risultato è il numero generato. -}

generaNumeroSequenziale :: [Conto] -> Int -> Int

generaNumeroSequenziale conti tentativo
    | tentativo < 10000
    , not (esisteConto tentativo conti) =
        tentativo

generaNumeroSequenziale conti tentativo
    | tentativo < 10000 =
        generaNumeroSequenziale
            conti
            (tentativo + 1)

generaNumeroSequenziale _ _ = 9999

{- La funzione filtraPerSaldo restituisce la lista dei conti con saldo maggiore della soglia:
   - il primo argomento è la soglia;
   - il secondo argomento è la lista dei conti;
   - il risultato è la lista dei conti filtrati. -}

filtraPerSaldo :: Double -> [Conto] -> [Conto]

filtraPerSaldo _ [] = []

filtraPerSaldo s (c@(Conto _ _ saldo _) : rest)
    | saldo > s =
        c : filtraPerSaldo s rest

filtraPerSaldo s (Conto _ _ saldo _ : rest)
    | saldo <= s =
        filtraPerSaldo s rest

{- La funzione validoIntestatario verifica che una stringa
   contenga almeno una lettera.
   La funzione restituisce False se la stringa è vuota
   o contiene solo caratteri non alfabetici. -}

validoIntestatario :: String -> Bool
validoIntestatario [] = False
validoIntestatario testo = contieneLettera testo

contieneLettera :: String -> Bool
contieneLettera [] = False
contieneLettera (c : rest)
    | isAlpha c = True
    | otherwise = contieneLettera rest


{- La funzione isLettera verifica che un codice ASCII
   corrisponda a una lettera A-Z o a-z. -}

isLettera :: Int -> Bool
isLettera c
    | c >= 65, c <= 90 = True
isLettera c
    | c >= 97, c <= 122 = True
isLettera _ = False

{- La funzione cercaConto cerca un conto per numero all'interno della lista:
   - il primo argomento è il numero del conto da cercare;
   - il secondo argomento è la lista dei conti;
   - il risultato è una coppia contenente:
       * il conto trovato;
       * la lista rimanente.
   La funzione restituisce Nothing se il conto non esiste. -}

cercaConto :: Int -> [Conto] -> Maybe (Conto, [Conto])
cercaConto _ [] = Nothing

cercaConto num (c@(Conto n _ _ _) : rest)
    | num == n =
        Just (c, rest)
    | otherwise =
        case cercaConto num rest of
            Nothing -> Nothing
            Just (conto, resto) ->
                Just (conto, c : resto)

{- La funzione deposita aggiunge un importo positivo
   al saldo del conto specificato:
   - il primo argomento è il numero del conto;
   - il secondo argomento è l'importo;
   - il terzo argomento è la lista dei conti;
   - il risultato è la lista aggiornata.
   La funzione restituisce Nothing se il conto non esiste
   oppure se l'importo non è positivo. -}

deposita :: Int -> Double -> [Conto] -> Maybe [Conto]
deposita _ importo _
    | importo <= 0 =
        Nothing

deposita num importo conti =
    case cercaConto num conti of
        Nothing -> Nothing
        Just (Conto n int saldo trans, resto) ->
            let nuovoSaldo = saldo + importo
                nuovaTransazione = Trans importo Deposito
                nuovoConto =
                    Conto
                        n
                        int
                        nuovoSaldo
                        (nuovaTransazione : trans)
            in Just (nuovoConto : resto)

{- La funzione preleva sottrae un importo positivo
   dal saldo del conto specificato:
   - il primo argomento è il numero del conto;
   - il secondo argomento è l'importo;
   - il terzo argomento è la lista dei conti;
   - il risultato è la lista aggiornata.
   La funzione restituisce Nothing se:
   - il conto non esiste;
   - l'importo non è positivo;
   - il saldo è insufficiente. -}

preleva :: Int -> Double -> [Conto] -> Maybe [Conto]

preleva _ importo _
    | importo <= 0 = Nothing

preleva num importo conti =
    case cercaConto num conti of
        Nothing -> Nothing
        Just (Conto n int saldo trans, resto)
            | saldo < importo -> Nothing
            | otherwise ->
                let nuovoSaldo = saldo - importo
                    nuovaTransazione = Trans importo Prelievo
                    nuovoConto =
                        Conto
                            n
                            int
                            nuovoSaldo
                            (nuovaTransazione : trans)
                in Just (nuovoConto : resto)

{- La funzione bonifico trasferisce un importo positivo
   dal conto sorgente al conto destinatario:
   - il primo argomento è il conto sorgente;
   - il secondo argomento è il conto destinatario;
   - il terzo argomento è l'importo;
   - il quarto argomento è la lista dei conti;
   - il risultato è la lista aggiornata.
   La funzione restituisce Nothing se:
   - i conti coincidono;
   - uno dei conti non esiste;
   - l'importo non è positivo;
   - il saldo del conto sorgente è insufficiente. -}

bonifico ::
    Int ->
    Int ->
    Double ->
    [Conto] ->
    Maybe [Conto]

bonifico numS numD importo _
    | numS == numD = Nothing
    | importo <= 0 = Nothing

bonifico numS numD importo conti =
    case cercaConto numS conti of
        Nothing -> Nothing
        Just
            ( Conto numSorg intS saldoS transS
            , restoSenzaS
            )
            | saldoS < importo ->
                Nothing
            | otherwise ->
                case cercaConto numD restoSenzaS of
                    Nothing -> Nothing
                    Just
                        ( Conto numDest intD saldoD transD
                        , restoFinale
                        ) ->
                            let nuovoSaldoS = saldoS - importo
                                nuovoSaldoD = saldoD + importo
                                transazioneS =
                                    Trans
                                        importo
                                        BonificoUscita
                                transazioneD =
                                    Trans
                                        importo
                                        BonificoEntrata
                                contoSorgente =
                                    Conto
                                        numSorg
                                        intS
                                        nuovoSaldoS
                                        (transazioneS : transS)
                                contoDestinatario =
                                    Conto
                                        numDest
                                        intD
                                        nuovoSaldoD
                                        (transazioneD : transD)
                            in Just( contoSorgente : contoDestinatario : restoFinale )

{- La funzione saldo restituisce il saldo attuale
   del conto specificato:
   - il primo argomento è il numero del conto;
   - il secondo argomento è la lista dei conti;
   - il risultato è il saldo del conto.
   La funzione restituisce Nothing se il conto non esiste. -}

saldo :: Int -> [Conto] -> Maybe Double
saldo _ [] = Nothing

saldo num (Conto n _ saldoConto _ : rest)
    | num == n = Just saldoConto
    | otherwise = saldo num rest

{- La funzione storico restituisce la lista delle transazioni
   del conto specificato:
   - il primo argomento è il numero del conto;
   - il secondo argomento è la lista dei conti;
   - il risultato è la lista delle transazioni.
   La funzione restituisce Nothing se il conto non esiste. -}

storico :: Int -> [Conto] -> Maybe [Transazione]
storico _ [] =Nothing

storico num (Conto n _ _ trans : rest)
    | num == n = Just trans
    | otherwise = storico num rest

{- La funzione gestisciPrelievo gestisce il prelievo controllando il saldo:
   - il primo argomento è il saldo disponibile;
   - il secondo argomento è il numero del conto;
   - il terzo argomento è la lista dei conti;
   - il risultato è la lista aggiornata. -}

gestisciPrelievo :: Double -> Int -> [Conto] -> IO [Conto]
gestisciPrelievo saldoDisponibile _ conti
    | saldoDisponibile == 0 = do
        putStrLn
            "Saldo 0: impossibile eseguire prelievo."
        return conti

gestisciPrelievo saldoDisponibile num conti = do
    importo <-
        leggiImportoValido
            "Importo da prelevare: "
            saldoDisponibile
    case preleva num importo conti of
        Nothing -> return conti
        Just contiNuovi -> do
            printf
                "Prelievo di %.2f effettuato dal conto numero %d.\n"
                importo
                num
            return contiNuovi

{- La funzione gestisciBonifico gestisce il bonifico controllando il saldo:
   - il primo argomento è il saldo del conto sorgente;
   - il secondo argomento è il numero del conto sorgente;
   - il terzo argomento è la lista dei conti;
   - il risultato è la lista aggiornata. -}

gestisciBonifico ::
    Double ->
    Int ->
    [Conto] ->
    IO [Conto]

gestisciBonifico saldoS _ conti
    | saldoS == 0 = do
        putStrLn
            "Saldo 0: impossibile eseguire bonifico."
        return conti

gestisciBonifico saldoS numS conti = do
    numD <-
        leggiContoDestinatario
            "Conto beneficiario: "
            numS
            conti
    importo <-
        leggiImportoValido
            "Importo del bonifico: "
            saldoS
    case bonifico numS numD importo conti of
        Nothing -> return conti
        Just contiNuovi -> do
            printf
                "Bonifico di euro %.2f dal conto %d al conto %d effettuato.\n"
                importo
                numS
                numD
            return contiNuovi

{- La funzione esegui esegue l'operazione corrispondente alla scelta dell'utente:
   - il primo argomento è il numero dell'operazione;
   - il secondo argomento è la lista dei conti;
   - il risultato è la lista aggiornata.
   Le operazioni 4, 5 e 6 non modificano i conti. -}

esegui :: Int -> [Conto] -> IO [Conto]
esegui 1 conti = do
    putStrLn ""
    putStrLn "--- DEPOSITO ---"
    num <-
        leggiIdConto
            "Numero conto: "
            conti
    importo <-
        leggiImportoPositivo
            "Importo da depositare: "
    case deposita num importo conti of
        Nothing -> return conti
        Just contiNuovi -> do
            printf
                "Deposito di %.2f effettuato sul conto numero %d.\n"
                importo
                num
            return contiNuovi

esegui 2 conti = do
    putStrLn ""
    putStrLn "--- PRELIEVO ---"
    num <-
        leggiIdConto
            "Numero conto: "
            conti

    case saldo num conti of
        Nothing ->
            return conti
        Just saldoDisponibile ->
            gestisciPrelievo
                saldoDisponibile
                num
                conti

esegui 3 conti
    | length conti == 1 = do
        putStrLn
            "ERRORE: impossibile eseguire bonifico con un solo conto."
        return conti

esegui 3 conti = do
    putStrLn ""
    putStrLn "--- BONIFICO ---"
    numS <-
        leggiIdConto
            "Conto ordinante: "
            conti
    case saldo numS conti of
        Nothing -> return conti
        Just saldoS ->
            gestisciBonifico
                saldoS
                numS
                conti

esegui 4 conti = do
    putStrLn ""
    putStrLn "--- MOVIMENTI DEL CONTO ---"
    num <-
        leggiIdConto
            "Numero conto: "
            conti
    case storico num conti of
        Nothing -> return conti
        Just transazioni -> do
            stampaStorico transazioni
            return conti

esegui 5 conti = do
    putStrLn ""
    putStrLn "--- RICERCA PER SALDO ---"
    soglia <-
        leggiSogliaSaldo
            "Mostra i conti con saldo maggiore di: "
    stampaRisultatoFiltro
        (filtraPerSaldo soglia conti)
    return conti

esegui 6 conti = do
    putStrLn ""
    putStrLn "--- SALDI COMPLETI ---"
    stampaContiDettaglio conti
    return conti

esegui _ conti = return conti