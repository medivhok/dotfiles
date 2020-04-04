;;; config/finances/config.el -*- lexical-binding: t; -*-

(defvar medivhok/ledger-payement-prompt
  (concat "%^{prompt"
          "|Expenses:Assurances Vie"
          "|Expenses:Assurances Maison"
          "|Expenses:Assurances Auto"
          "|Liabilities:Bell"
          "|Liabilities:Capital One"
          "|Liabilities:Hydro"
          "|Expenses:Hypothèque"
          "|Expenses:Paiements Auto"
          "|Expenses:SAAQ"
          "}"))

(defvar medivhok/ledger-purchase-prompt
  (concat "%^{prompt"
          "|Expenses:Autres"
          "|Expenses:Épicerie"
          "|Expenses:Gas"
          "|Expenses:Hobby"
          "|Expenses:Prescriptions"
          "|Expenses:Restaurants"
          "|Expenses:Santé"
          "|Expenses:Valérie"
          "}"))

(defvar medivhok/ledger-payement-method-prompt
  (concat "%^{prompt"
          "|Assets:Tangerine:Opérations"
          "|Liabilities:Capital One"
          "}"))

(defvar medivhok/org-ledger-payement-template
  (concat
   "%(org-read-date) %^{Payee}  ; :paiement:\n"
   "    " medivhok/ledger-payement-prompt "  $ %^{Montant}\n"
   "    Assets:Tangerine:Opérations")
   "Template for payement transaction with ledger.")

(defvar medivhok/org-ledger-purchase-template
  (concat
   "%(org-read-date) * %^{Payee}  ; :achat:\n"
   "    " medivhok/ledger-purchase-prompt "  $ %^{Montant}\n"
   "    " medivhok/ledger-payement-method-prompt)
  "Template for cash transaction with ledger.")

(after! org
  (cl-pushnew '("l" "Ledger") org-capture-templates)
  (cl-pushnew `("la" "Achat" plain (file ,(format "~/Documents/Budget/ledger/transactions-%s.ledger" (format-time-string "%Y")))
                ,medivhok/org-ledger-purchase-template
                :empty-lines 1
                :immediate-finish t) org-capture-templates)
  (cl-pushnew `("lp" "Paiement" plain (file ,(format "~/Documents/Budget/ledger/transactions-%s.ledger" (format-time-string "%Y")))
                ,medivhok/org-ledger-payement-template
                :empty-lines 1
                :immediate-finish t) org-capture-templates))
