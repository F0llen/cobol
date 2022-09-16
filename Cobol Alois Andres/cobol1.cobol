                                                                        00001000
                                                                        00010000
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTPAA1.
      *===============================================================*
      * NOM DU PROGRAMME : COBTPAA1                                   *
      *---------------------------------------------------------------*
      *               O B J E T  D U  P R O G R A M M E               *
      *---------------------------------------------------------------*
      * SQUELETTE DE PROGRAMME COBOL                                  *
      *---------------------------------------------------------------*
      *     H I S T O R I Q U E  D E S  M O D I F I C A T I O N S     *
      *---------------------------------------------------------------*
      *   DATE   |  AUTEUR  |  REF.|              OBJET               *
      *---------------------------------------------------------------*
      * JJ/MM/AA | ........ |      | CREATION                         *
      * JJ/MM/AA | ........ |      |                                  *
      *===============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*
      *                     DEFINITION DES FICHIERS                   *
      *---------------------------------------------------------------*
       FILE-CONTROL.
           SELECT FICMVTS ASSIGN TO FICMVTS.
       DATA DIVISION.
      *---------------------------------------------------------------*
      *                    DESCRIPTION DES FICHIERS                   *
      *---------------------------------------------------------------*
       FILE SECTION.
       FD FICMVTS
           RECORDING MODE IS F.
       01     FD-FICMVTS.
         05   FD-FICMVTS-NUMCPT PIC X(10).
         05   FD-FICMVTS-DATE   PIC 9(08).
         05   FD-FICMVTS-CODE   PIC X(01).
         05   FD-FICMVTS-MONTANT PIC 9(08)V99.
         05   FILLER            PIC X(21).

      *---------------------------------------------------------------*
      *              DESCRIPTION DES VARIABLES DE TRAVAIL             *
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE STATUTS FICHIERS / RUPTURES                *
      *      ---------------------------------------------------------*
       01     WS-VARIABLES-RUPTURE.
         05   WS-EOF-FICMVTS     PIC X VALUE 'N'.
         05   WS-NUMCPT-PREC     PIC 9(10).

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE CALCULS                                    *
      *      ---------------------------------------------------------*
       01     WS-VARIABLES-CALCUL.
         05   WS-CUMUL-DPOT       PIC  9(10)V9(02) COMP-3.
         05   WS-CUMUL-RTRAIT     PIC  9(10)V9(02) COMP-3.
         05   WS-CUMUL-CB         PIC  9(10)V9(02) COMP-3.
         05   WS-TOTAL            PIC S9(10)V9(02) COMP-3.
      *      ---------------------------------------------------------*
      *-----< VARIABLES DE COMPTEURS                                  *
      *      ---------------------------------------------------------*
       01     CP-VARIABLES-COMPTEURS.
         05   CP-ERREUR           PIC  9(4) COMP VALUE 0.
         05   CP-TRAITE           PIC  9(4) COMP VALUE 0.
         05   CP-COMPTE-TOTAL     PIC  9(4) COMP VALUE 0.
      *      ---------------------------------------------------------*
      *-----< VARIABLES D'EDITION                                     *
      *      ---------------------------------------------------------*
       01     ED-ETAT.
         05   ED-LIGNE1.
           10 FILLER                     PIC X(29)
                                VALUE 'NUMERO DE COMPTE         :  '.
           10 ED-NUMCPT-1                           PIC X(10).

         05   ED-LIGNE3.
           10 FILLER  PIC X(31) VALUE '  CUMUL DES RETRAITS     :  '.
           10 ED-CUMUL-R                 PIC  ZZZBZZZBZZ9,V99.


         05   ED-LIGNE4.
           10 FILLER  PIC X(31) VALUE '  CUMUL DES CARTES BLEUES:  '.
           10 ED-CUMUL-C                 PIC  ZZZBZZZBZZ9,V99.


         05   ED-LIGNE5.
           10 FILLER  PIC X(31) VALUE '  CUMUL DES DEPOTS       :  '.
           10 ED-CUMUL-D                 PIC  ZZZBZZZBZZ9,V99.


         05   ED-LIGNE6.
           10 FILLER  PIC X(30) VALUE '  TOTAL DES OPERATIONS   :  '.
           10 ED-TOTAL                   PIC +ZZZBZZZBZZ9,V99.

         05   ED-LIGNE9.
           10 FILLER                     PIC X(31)
                                VALUE 'COMPTE                       : '.
           10 ED-NUMCPT-3                PIC X(10).

         05   ED-LIGNE10.
           10 FILLER  PIC X(31) VALUE 'NOMBRE DE MOUVEMENTS TRAITES : '.
           10 ED-MOUV-TRAITES            PIC 9(4).

         05   ED-LIGNE11.
           10 FILLER  PIC X(31) VALUE 'NOMBRE DE MOUVEMENTS ERRONES : '.
           10 ED-MOUV-ERRONES            PIC 9(4).

       PROCEDURE DIVISION.

      *===============================================================*
      *                       A L G O R I T H M E                     *
      *===============================================================*

      *---------------------------------------------------------------*
      *          000 A 599     TRAITEMENT PRINCIPAL                   *
      *---------------------------------------------------------------*

      *      ---------------------------------------------------------*
      *-----< BANQUE                                     *
      *      ---------------------------------------------------------*
       100-BANQUE.
           PERFORM 600-OUVRIR-FICMVTS
           PERFORM 610-LIRE-FICMVTS

           PERFORM 200-COMPTE
             UNTIL WS-EOF-FICMVTS = 'O'

           PERFORM 620-FERMER-FICMVTS
           PERFORM 999-ARRET-PROGRAMME
           .
      *      ---------------------------------------------------------*
      *-----< COMPTE                                      *
      *      ---------------------------------------------------------*
       200-COMPTE.
           PERFORM 700-TRT-NOUVEAU-NUM

           PERFORM 300-MOUVEMENT
             UNTIL FD-FICMVTS-NUMCPT NOT = WS-NUMCPT-PREC
                OR WS-EOF-FICMVTS = 'O'
           PERFORM 800-PREPA-AFFICHAGE-RELEVE
           .
      *      ---------------------------------------------------------*
      *-----< MOUVEMENT                                               *
      *      ---------------------------------------------------------*
       300-MOUVEMENT.
           PERFORM 710-CUMUL-MOUVEMENT
           PERFORM 610-LIRE-FICMVTS
           .

      *---------------------------------------------------------------*
      *         600 A 699     GESTION DES ENTREES / SORTIES           *
      *---------------------------------------------------------------*
       600-OUVRIR-FICMVTS.
           OPEN INPUT FICMVTS
           .

       610-LIRE-FICMVTS.
           READ FICMVTS
             AT END MOVE 'O' TO WS-EOF-FICMVTS
           END-READ
           .

       620-FERMER-FICMVTS.
           CLOSE FICMVTS
           .

      *---------------------------------------------------------------*
      *          700 A 799     REGLES DE GESTION COMPLEXES            *
      *---------------------------------------------------------------*

       700-TRT-NOUVEAU-NUM.
           MOVE FD-FICMVTS-NUMCPT TO WS-NUMCPT-PREC
           .

       710-CUMUL-MOUVEMENT.
           EVALUATE FD-FICMVTS-CODE
                WHEN 'D'
                    ADD FD-FICMVTS-MONTANT TO WS-CUMUL-DPOT
                WHEN 'R'
                    ADD FD-FICMVTS-MONTANT TO WS-CUMUL-RTRAIT
                WHEN 'C'
                    ADD FD-FICMVTS-MONTANT TO WS-CUMUL-CB
                WHEN OTHER
                    DISPLAY 'COMPTE' FD-FICMVTS-NUMCPT
                    'CODE MOUVEMENT' FD-FICMVTS-CODE 'ERRONE'
                    ADD 1 TO CP-ERREUR
           END-EVALUATE
           ADD 1 TO CP-TRAITE
           .

      *---------------------------------------------------------------*
      *         800 A 899      AFFICHAGES ET EDITIONS                 *
      *---------------------------------------------------------------*

       800-PREPA-AFFICHAGE-RELEVE.
           COMPUTE WS-TOTAL = WS-CUMUL-DPOT - WS-CUMUL-RTRAIT -
           WS-CUMUL-CB

           MOVE WS-TOTAL TO ED-TOTAL
           MOVE WS-CUMUL-RTRAIT TO ED-CUMUL-R
           MOVE WS-CUMUL-DPOT TO ED-CUMUL-D
           MOVE WS-CUMUL-CB TO ED-CUMUL-C
           MOVE WS-NUMCPT-PREC TO ED-NUMCPT-3
           MOVE WS-NUMCPT-PREC TO ED-NUMCPT-1
           MOVE CP-TRAITE TO ED-MOUV-TRAITES
           MOVE CP-ERREUR TO ED-MOUV-ERRONES

           DISPLAY ED-LIGNE1
           DISPLAY SPACE
           DISPLAY ED-LIGNE3
           DISPLAY ED-LIGNE4
           DISPLAY ED-LIGNE5
           DISPLAY ED-LIGNE6
           DISPLAY SPACE
           DISPLAY SPACE
           DISPLAY ED-LIGNE9
           DISPLAY ED-LIGNE10
           DISPLAY ED-LIGNE11
           DISPLAY SPACE
           DISPLAY SPACE

           ADD 1 TO CP-COMPTE-TOTAL
           INITIALIZE CP-TRAITE
           INITIALIZE CP-ERREUR
           .

      *---------------------------------------------------------------*
      *          900 A 999     GESTION DES ANOMALIES                  *
      *                        ET DE LA FIN DU TRAITEMENT             *
      *---------------------------------------------------------------*
       999-ARRET-PROGRAMME.
           DISPLAY 'NOMBRE DE COMPTES TRAITES     :' CP-COMPTE-TOTAL
           STOP RUN
           .

      *===============================================================*
      *                 F I N  D U  P R O G R A M M E                 *
      *===============================================================*
