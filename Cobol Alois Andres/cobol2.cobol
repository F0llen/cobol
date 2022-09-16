       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTPAA2.
      *===============================================================*
      * NOM DU PROGRAMME : COBTPAA2                                   *
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
           SELECT FICETAT ASSIGN TO FICETAT.
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

       FD FICETAT
           RECORDING MODE IS F.
       01     FD-FICETAT.
         05   FD-FICETAT-NUMCPT PIC X(10).
         05   FD-FICETAT-DATE   PIC 9(08).
         05   FD-FICETAT-CODE   PIC X(01).
         05   FD-FICETAT-MONTANT PIC 9(08)V99.
         05   FILLER            PIC X(51).

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
      *-----< VARIABLES DE DATE                                       *
      *      ---------------------------------------------------------*
       01     WS-DONNEES-TEMPORELLES.
         05   WS-DATE.
           10 WS-AA          PIC 99.
           10 WS-MM          PIC 99.
           10 WS-JJ          PIC 99.
         05   WS-DATE-TRAITEMENT.
           10 WS-JJ          PIC 99/.
           10 WS-MM          PIC 99/.
           10 FILLER         PIC XX VALUE '20'.
           10 WS-AA          PIC 99.

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
         05   CP-PAGE             PIC  9(4) COMP VALUE 1.
         05   CP-NB-CPT           PIC  9 COMP VALUE 0.
      *      ---------------------------------------------------------*
      *-----< VARIABLES D'EDITION                                     *
      *      ---------------------------------------------------------*
       01     ED-ZONE-ED                 PIC X(80).

       01     ED-LIGNE1.
         05   FILLER                     PIC X(13)
                     VALUE 'EDITION DU : '.
         05   ED-DATE                    PIC X(10).
         05   FILLER                     PIC X(30)
                     VALUE '                            '.
         05   FILLER                     PIC X(16)
                     VALUE '               '.
         05   FILLER                     PIC X(7)
                     VALUE 'PAGE : '.
         05   ED-PAGE                    PIC Z(4).

       01     ED-LIGNE4.
         05   FILLER                     PIC X(29)
                                VALUE '                             '.
         05   FILLER                     PIC X(23)
                                VALUE 'EDITION DES OPERATIONS'.

       01     ED-LIGNE7.
         05   FILLER                     PIC X(29)
                                VALUE 'NUMERO DE COMPTE         :  '.
         05   ED-NUMCPT-7                PIC X(10).

       01     ED-LIGNE9.
         05   FILLER  PIC X(31) VALUE '  CUMUL DES RETRAITS     :  '.
         05   FILLER  PIC X(35)
                        VALUE '                                   '.
         05   ED-CUMUL-R                 PIC  ZZZBZZZBZZ9,V99.

       01     ED-LIGNE10.
         05   FILLER  PIC X(31) VALUE '  CUMUL DES CARTES BLEUES:  '.
         05   FILLER  PIC X(35)
                        VALUE '                                   '.
         05   ED-CUMUL-C                 PIC  ZZZBZZZBZZ9,V99.


       01     ED-LIGNE11.
         05   FILLER  PIC X(31) VALUE '  CUMUL DES DEPOTS       :  '.
         05   FILLER  PIC X(35)
                        VALUE '                                   '.
         05   ED-CUMUL-D                 PIC  ZZZBZZZBZZ9,V99.


       01     ED-LIGNE13.
         05   FILLER  PIC X(30) VALUE '  TOTAL DES OPERATIONS   :  '.
         05   FILLER  PIC X(35)
                        VALUE '                                   '.
         05   ED-TOTAL                   PIC +ZZZBZZZBZZ9,V99.

       01     ED-LIGNE-S1.
         05   FILLER  PIC X(31) VALUE 'COMPTE                       : '.
         05   ED-NUMCPT-S1               PIC X(10).

       01     ED-LIGNE-S2.
         05   FILLER  PIC X(31) VALUE 'NOMBRE DE MOUVEMENTS TRAITES : '.
         05   ED-MOUV-TRAITES            PIC 9(4).

       01     ED-LIGNE-S3.
         05   FILLER  PIC X(31) VALUE 'NOMBRE DE MOUVEMENTS ERRONES : '.
         05   ED-MOUV-ERRONES            PIC 9(4).


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
           PERFORM 720-DATE
           PERFORM 600-OUVRIR-FICMVTS
           PERFORM 610-LIRE-FICMVTS
           PERFORM 630-OUVRIR-FICETAT

           PERFORM 820-EDIT-DEBUT
           PERFORM 200-COMPTE
             UNTIL WS-EOF-FICMVTS = 'O'

           PERFORM 620-FERMER-FICMVTS
           PERFORM 640-FERMER-FICETAT
           PERFORM 999-ARRET-PROGRAMME
           .
      *      ---------------------------------------------------------*
      *-----< COMPTE                                      *
      *      ---------------------------------------------------------*
       200-COMPTE.
           PERFORM 730-INIT-CUMUL
           PERFORM 700-TRT-NOUVEAU-NUM
           PERFORM 300-MOUVEMENT
             UNTIL FD-FICMVTS-NUMCPT NOT = WS-NUMCPT-PREC
                OR WS-EOF-FICMVTS = 'O'

           PERFORM 800-PREPA-AFFICHAGE-RELEVE
           PERFORM 810-EDIT-OPE
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

       630-OUVRIR-FICETAT.
           OPEN OUTPUT FICETAT
           .

       640-FERMER-FICETAT.
           CLOSE FICETAT
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
                    DISPLAY 'COMPTE ' FD-FICMVTS-NUMCPT
                    ' CODE MOUVEMENT ' FD-FICMVTS-CODE ' ERRONE '
                    DISPLAY SPACE
                    ADD 1 TO CP-ERREUR
           END-EVALUATE
           ADD 1 TO CP-TRAITE
           .

       720-DATE.
           ACCEPT WS-DATE FROM DATE
           MOVE CORR WS-DATE TO WS-DATE-TRAITEMENT
           MOVE WS-DATE-TRAITEMENT TO ED-DATE
           .

       730-INIT-CUMUL.
           INITIALIZE WS-CUMUL-DPOT
           INITIALIZE WS-CUMUL-RTRAIT
           INITIALIZE WS-CUMUL-CB
           .

      *---------------------------------------------------------------*
      *         800 A 899      AFFICHAGES ET EDITIONS                 *
      *---------------------------------------------------------------*

       800-PREPA-AFFICHAGE-RELEVE.
           MOVE CP-TRAITE TO ED-MOUV-TRAITES
           MOVE CP-ERREUR TO ED-MOUV-ERRONES
           MOVE WS-NUMCPT-PREC TO ED-NUMCPT-S1

           DISPLAY ED-LIGNE-S1
           DISPLAY ED-LIGNE-S2
           DISPLAY ED-LIGNE-S3
           DISPLAY SPACE

           ADD 1 TO CP-COMPTE-TOTAL
           INITIALIZE CP-TRAITE
           INITIALIZE CP-ERREUR
           .

       810-EDIT-OPE.
           COMPUTE WS-TOTAL = WS-CUMUL-DPOT - WS-CUMUL-RTRAIT -
           WS-CUMUL-CB
           MOVE WS-NUMCPT-PREC TO ED-NUMCPT-7
           MOVE WS-TOTAL TO ED-TOTAL
           MOVE WS-CUMUL-RTRAIT TO ED-CUMUL-R
           MOVE WS-CUMUL-DPOT TO ED-CUMUL-D
           MOVE WS-CUMUL-CB TO ED-CUMUL-C

           WRITE FD-FICETAT
              FROM ED-LIGNE7 AFTER 3 LINES
           END-WRITE.

           WRITE FD-FICETAT
              FROM ED-LIGNE9 AFTER 2 LINES
           END-WRITE.

           WRITE FD-FICETAT
              FROM ED-LIGNE10 AFTER 1 LINES
           END-WRITE.

           WRITE FD-FICETAT
              FROM ED-LIGNE11 AFTER 1 LINES
           END-WRITE.

           WRITE FD-FICETAT
              FROM ED-LIGNE13 AFTER 2 LINES
           END-WRITE.

           ADD 1 TO CP-NB-CPT
           IF CP-NB-CPT IS GREATER THAN 5
             THEN INITIALIZE CP-NB-CPT
             ADD 1 TO CP-PAGE
             PERFORM 820-EDIT-DEBUT
           END-IF

           .

       820-EDIT-DEBUT.
           MOVE CP-PAGE TO ED-PAGE
           WRITE FD-FICETAT
            FROM ED-LIGNE1 AFTER PAGE
           END-WRITE.
           WRITE FD-FICETAT
            FROM ED-LIGNE4 AFTER 3 LINES
           END-WRITE
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
