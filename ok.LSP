;;;------------------------------------------------------------------------------;;;
;;; CreaTavole_v17.lsp                                                         ;;;
;;;                                                                            ;;;
;;; Script Ultra-Semplificato.                                                 ;;;
;;; - Si concentra ESCLUSIVAMENTE sulla creazione e configurazione dei layout. ;;;
;;; - NON crea più alcun file DSD.                                             ;;;
;;; - Fornisce istruzioni finali per la pubblicazione manuale.                 ;;;
;;;------------------------------------------------------------------------------;;;

;;;==============================================================================
;;; Funzioni di Utilità
;;;==============================================================================

;;; Divide una stringa in base a un separatore.
(defun str-split (str sep / pos result)
  (while (setq pos (vl-string-search sep str))
    (setq result (cons (substr str 1 pos) result))
    (setq str (substr str (+ (strlen sep) pos 1)))
  )
  (reverse (cons str result))
)

;;; Verifica se un layout esiste già.
(defun LayoutEsiste-p (layoutName /)
  (if (member (strcase layoutName) (mapcar 'strcase (layoutlist)))
    T
    nil
  )
)

;;; Rimuove i caratteri non validi da un nome di layout.
(defun SanificaNomeLayout (nome / nomePulito)
  (setq nomePulito nome)
  (foreach char '("/" "\\" "<" ">" "?" "\"" ":" ";" "*" "|" "," "=" "`")
    (while (vl-string-search char nomePulito)
      (setq nomePulito (vl-string-subst "-" char nomePulito))
    )
  )
  nomePulito
)

;;; Aggiorna gli attributi di un blocco specifico in un dato layout.
(defun AggiornaAttributi (nomeLayout nomeBlocco dati / blocco vla-blocco vla-attributi)
  (setvar "CTAB" nomeLayout)
  (if (setq blocco (ssget "_X" (list '(0 . "INSERT") (cons 2 nomeBlocco) (cons 410 nomeLayout))))
    (progn
      (setq vla-blocco (vlax-ename->vla-object (ssname blocco 0)))
      (if (and vla-blocco (= (type vla-blocco) 'VLA-OBJECT))
        (progn
          (setq vla-attributi (vlax-invoke vla-blocco 'GetAttributes))
          (foreach attributo vla-attributi
            (cond
              ((= "ATT_NUMERO" (strcase (vla-get-TagString attributo)))
               (vla-put-TextString attributo (nth 0 dati))
              )
              ((= "ATT_TITOLO" (strcase (vla-get-TagString attributo)))
               (vla-put-TextString attributo (nth 1 dati))
              )
              ((= "ATT_SCALA" (strcase (vla-get-TagString attributo)))
               (vla-put-TextString attributo (nth 2 dati))
              )
            )
          )
          (vla-update vla-blocco)
        )
      )
    )
  )
)

;;;==============================================================================
;;; Funzione Principale del Programma
;;;==============================================================================
(defun c:CreaTavole ( / fileDati delimitatore layoutModello nomeBlocco f riga dati numeroTavola titoloTavola scalaTavola datiAggiornati userInput cancellaLayout titoloSanificato nomeLayoutCompleto listaLayoutCreati old_cmdecho old_filedia)
  (vl-load-com)
  (setq old_cmdecho (getvar "CMDECHO"))
  (setq old_filedia (getvar "FILEDIA"))

  (if (setq fileDati (getfiled "Seleziona il file di testo (Dati.txt)" "" "txt" 16))
    (progn
      (setq userInput (getstring (strcat "\nInserisci il carattere delimitatore <.>: ")))
      (if (or (not userInput) (= "" userInput)) (setq delimitatore ".") (setq delimitatore userInput))
      
      (setq userInput (getstring T (strcat "\nInserisci il nome del Layout Modello <00>: ")))
      (if (or (not userInput) (= "" userInput)) (setq layoutModello "00") (setq layoutModello userInput))
      
      (setq userInput (getstring T (strcat "\nInserisci il nome del blocco cartiglio <BLOCCO_CARTIGLIO_A4>: ")))
      (if (or (not userInput) (= "" userInput)) (setq nomeBlocco "BLOCCO_CARTIGLIO_A4") (setq nomeBlocco userInput))
      
      (setvar "CMDECHO" 0)

      (if (not (LayoutEsiste-p layoutModello))
        (alert (strcat "ERRORE: Il layout modello '" layoutModello "' non esiste."))
        (progn
          (setq cancellaLayout (getstring (strcat "\nVuoi eliminare i layout esistenti (escluso modello)? [S/N] <N>: ")))
          (if (member (strcase cancellaLayout) '("S" "SI"))
            (progn
              (princ "\nInizio eliminazione layout esistenti...")
              (setvar "CTAB" layoutModello)
              (foreach l (layoutlist)
                (if (and (/= (strcase l) "MODEL") (/= (strcase l) (strcase layoutModello)))
                  (command "_.LAYOUT" "_DELETE" l)
                )
              )
              (princ " completata.")
            )
          )

          (if (setq f (open fileDati "r"))
            (progn
              (setq listaLayoutCreati nil)
              (princ "\nInizio creazione tavole: ")
              (while (setq riga (read-line f))
                (setq dati (str-split riga delimitatore))
                (if (>= (length dati) 2)
                  (progn
                    (setq numeroTavola (vl-string-trim " " (nth 0 dati)))
                    (setq titoloTavola (vl-string-trim " " (nth 1 dati)))
                    (if (and (> (length dati) 2) (nth 2 dati) (/= "" (vl-string-trim " " (nth 2 dati))))
                      (setq scalaTavola (vl-string-trim " " (nth 2 dati)))
                      (setq scalaTavola "-")
                    )
                    (setq datiAggiornati (list numeroTavola titoloTavola scalaTavola))
                    (setq titoloSanificato (SanificaNomeLayout titoloTavola))
                    (setq nomeLayoutCompleto (strcat "Tav " numeroTavola " " titoloSanificato))
                    
                    (if (not (LayoutEsiste-p nomeLayoutCompleto))
                      (progn
                        (command "_.LAYOUT" "_COPY" layoutModello nomeLayoutCompleto)
                        (AggiornaAttributi nomeLayoutCompleto nomeBlocco datiAggiornati)
                        (setq listaLayoutCreati (cons nomeLayoutCompleto listaLayoutCreati))
                        (princ ".")
                      )
                    )
                  )
                )
              )
              (close f)
              (princ "\nCreazione layout completata.\n")

              ;; --- NUOVO MESSAGGIO FINALE ---
              (if listaLayoutCreati
                (alert (strcat "Operazione completata con successo.\n\n"
                               "Per stampare in serie i layout creati:\n\n"
                               "1. Digita il comando PUBBLICA e premi Invio.\n"
                               "2. AutoCAD caricherà automaticamente tutti i layout del disegno corrente.\n"
                               "3. Rimuovi dall'elenco i layout che non desideri stampare (es. 'Modello').\n"
                               "4. Imposta le opzioni di pubblicazione (es. 'Pubblica in: PDF') e avvia la stampa."))
                (alert "Nessun nuovo layout è stato creato. Operazione terminata.")
              )
            )
            (alert (strcat "ERRORE: Impossibile aprire il file: " fileDati))
          )
        )
      )
    )
    (princ "\nOperazione annullata dall'utente.")
  )

  (setvar "CMDECHO" old_cmdecho)
  (setvar "FILEDIA" old_filedia)
  (princ)
)

(princ "\n\nScript 'CreaTavole_v17.lsp' caricato. Digitare 'CREATAVOLE' per avviare.")
(princ)