# Haskell-like Lazy Interpreter in Standard ML

**Descrizione (Intro):**
Questo progetto implementa un interprete per un linguaggio simile a Haskell scritto in Standard ML. Il core dell'interprete supporta paradigmi funzionali essenziali tra cui l'astrazione di funzioni, l'aritmetica, la logica booleana e, soprattutto, la **valutazione pigra (lazy evaluation)** con memoizzazione. Include un gestore robusto per la ricorsione mutua all'interno di espressioni `let` e dichiarazioni top-level.

## 2. Caratteristiche Principali (Key Features)

* **Valutazione Pigra & Memoizzazione:** Le espressioni non vengono valutate finché non sono necessarie. I risultati vengono salvati (memoizzati) per evitare ricalcoli futuri, utilizzando strutture dati `VThunk`.
* **Gestione Avanzata dell'Ambiente:** Utilizzo di `ref` in SML per gestire lo stato mutabile necessario per la memoizzazione e per la chiusura degli ambienti ricorsivi.
* **Ricorsione Mutua (Tying the Knot):** Implementazione della tecnica "tying the knot" per permettere a funzioni ed espressioni `let` di riferirsi a se stesse o l'una all'altra durante la definizione, manipolando i riferimenti all'ambiente (`env ref`).
* **Architettura Modulare:** Separazione netta tra Lexer, Core Interpreter (AST, Parser, Evaluator) e Main Driver (REPL).

## 3. Architettura del Progetto

* **`lexer.sml`**: Gestisce l'analisi lessicale convertendo il testo grezzo in un flusso di token. Supporta interi, identificatori, keyword e letterali stringa/char.
* **`interpreter.sml`**: Il cuore del sistema. Definisce l'Abstract Syntax Tree (AST), il parser ricorsivo e l'evaluator. Qui risiede la logica per i tipi di valore come `VClosure` e `VThunk`.
* **`main.sml`**: Gestisce la sessione interattiva, l'input dell'utente e il pipeline di esecuzione (Lexing -> Parsing -> Evaluation) mantenendo lo stato globale.

## 4. Esempio di Utilizzo (Snippet di Codice)

```haskell
// Esempio di valutazione pigra e ricorsione
let 
  isEven = \n -> if n == 0 then true else isOdd (n - 1);
  isOdd  = \n -> if n == 0 then false else isEven (n - 1);
in
  isEven 4
// Output: true (valutato solo quando richiesto)
```

## 5. Dettagli Tecnici (Per chi vuole approfondire)

"Il sistema utilizza un tipo value personalizzato che include VThunk. Un thunk contiene l'espressione da valutare e un riferimento all'ambiente di definizione. Quando forzato (forceValue), il risultato viene calcolato e memorizzato in un memo ref mutabile."

## 6. Limitazioni e Roadmap
* **Sistema di Tipi:** Attualmente il type-checker è rudimentale e controlla solo discrepanze ovvie; manca un'inferenza completa stile Hindley-Milner.

* **Subset del Linguaggio:** Mancano pattern matching, tipi di dati algebrici e list comprehension.

* **Segnalazione Errori:** I messaggi di errore potrebbero essere più precisi riguardo alla posizione nel codice sorgente.
