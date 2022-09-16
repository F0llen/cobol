       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTP3A2.
      *===============================================================*
      * NOM DU PROGRAMME : COBTP3A2                                   *
      *---------------------------------------------------------------*
      *               O B J E T  D U  P R O G R A M M E               *
      *---------------------------------------------------------------*
      *                TP RAPPROCHEMENT SEQUENTIEL                    *
      *---------------------------------------------------------------*
      *     H I S T O R I Q U E  D E S  M O D I F I C A T I O N S     *
      *---------------------------------------------------------------*
      *   DATE   |  AUTEUR  |  REF.|              OBJET               *
      *---------------------------------------------------------------*
      * 01/08/22 |  ALOIS.A |      |      DEVELOPPEMENT SYSOUT        *
      * 29/07/22 |  ALOIS.A |      |      DEVELOPPEMENT FICCPTS       *
      *===============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*
      *                     DEFINITION DES FICHIERS                   *
      *---------------------------------------------------------------*
       FILE-CONTROL.
           SELECT FICMVTS ASSIGN TO FICMVTS FILE STATUS IS FS-FICMVTS.
           SELECT FICETAT ASSIGN TO FICETAT.
           SELECT FICCPTE ASSIGN TO FICCPTE FILE STATUS IS FS-FICCPTE.
           SELECT FICCPTS ASSIGN TO FICCPTS.
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
         05   FILLER            PIC X(80).

       FD FICCPTE
           RECORDING MODE IS F.
       01     FD-FICCPTE.
         05   FD-FICCPTE-NUMCPT PIC X(10).
         05   FD-FICCPTE-DATE-OUV PIC 9(08).
         05   FD-FICCPTE-SOLDE  PIC S9(09)V99 COMP-3.
         05   FD-FICCPTE-DATE-MAJ PIC 9(08).
         05   FILLER            PIC X(28).

       FD FICCPTS
           RECORDING MODE IS F.
       01     FD-FICCPTS.
         05   FILLER            PIC X(60).

      *---------------------------------------------------------------*
      *              DESCRIPTION DES VARIABLES DE TRAVAIL             *
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE COMPTES / FILE STATUS                      *
      *      ---------------------------------------------------------*
       01  WS-NUMCPT-PREC     PIC 9(10).
       01  WS-FS.
         05   FS-FICMVTS           PIC X(2).
         05   FS-FICCPTE           PIC X(2).

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE CALCULS                                    *
      *      ---------------------------------------------------------*
       01  WS-VARIABLES-CALCUL.
         05 WS-CUMUL-DPOT          PIC 9(08)V99.
         05 WS-CUMUL-RTRAIT        PIC 9(08)V99.
         05 WS-CUMUL-CB            PIC 9(08)V99.
         05 WS-ANCIEN-SOLDE        PIC S9(09)V99 COMP-3.
         05 WS-NOUV-SOLDE          PIC S9(09)V99 COMP-3.
         05 WS-TOTAL               PIC S9(10)V99 COMP-3.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE CALCULS                                    *
      *      ---------------------------------------------------------*
       01  WS-DATE-TEMP.
         05 WS-DATE-SYST.
           10 WS-JJ   PIC 99.
           10 WS-MM   PIC 99.
           10 WS-AA   PIC 9999.
         05 WS-DATE-TRAITEMENT.
           10 WS-JJ   PIC 99/.
           10 WS-MM   PIC 99/.
           10 WS-AA   PIC 9999.

       01  WS-DONNEES-COMP.
         05 WS-DATE-COMPILE.
           10 WS-JJ   PIC 99/.
           10 WS-MM   PIC 99/.
           10 WS-AA   PIC 99.
         05 WS-HEURE-COMPILE.
           10 WS-HH   PIC 99.
           10 FILLER  PIC X.
           10 WS-MN   PIC 99.
           10 FILLER  PIC X.
           10 WS-SC   PIC 99.

       01 WS-DATE.
         05 WS-DATE-COMP.
           10 WS-JJ-COMP PIC 99/.
           10 WS-MM-COMP PIC 99/.
           10 WS-AA-COMP PIC 99.
         05 WS-DATE-COMP2.
           10 WS-JJ-COMP PIC 99/.
           10 WS-MM-COMP PIC 99/.
           10 FILLER PIC XX VALUE '20'.
           10 WS-AA-COMP PIC 99.
         05 WS-HEURE-COM.
           10 WS-HH PIC 99.
           10 FILLER PIC X(1) VALUE ':'.
           10 WS-MN PIC 99.
           10 FILLER PIC X VALUE ':'.
           10 WS-SC PIC 99.

       01 WS-HEURE-DEBUT.
         05 WS-HH PIC 99.
         05 WS-MN PIC 99.
         05 WS-SC PIC 99.
       01 WS-HEURE-FIN.
         05 WS-HH PIC 99.
         05 WS-MN PIC 99.
         05 WS-SC PIC 99.

       01 WS-TRAITEMENT-DEBUT.
         05 WS-DATE-DEBUT.
           10 WS-JJ PIC 99.
           10 WS-MM PIC 99.
           10 WS-AA PIC 99.
         05 WS-DATE-DEBUT2.
           10 WS-JJ PIC 99/.
           10 WS-MM PIC 99/.
           10 FILLER PIC XX VALUE '20'.
           10 WS-AA PIC 99.
       01 WS-TRAITEMENT-FIN.
         05 WS-DATE-FIN.
           10 WS-JJ PIC 99.
           10 WS-MM PIC 99.
           10 WS-AA PIC 99.
         05 WS-DATE-FIN2.
           10 WS-JJ PIC 99/.
           10 WS-MM PIC 99/.
           10 FILLER PIC XX VALUE '20'.
           10 WS-AA PIC 99.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE COMPTEURS                                  *
      *      ---------------------------------------------------------*
       01 CP-VARIABLES-COMPTEURS.
         05 CP-COMPTES         PIC 9(4) COMP VALUE 0.
         05 CP-ERREUR          PIC 9(4) COMP VALUE 0.
         05 CP-MODIF           PIC 9(4) COMP VALUE 0.
         05 CP-CREA            PIC 9(4) COMP VALUE 0.
         05 CP-REPORT          PIC 9(4) COMP VALUE 0.
         05 CP-LECTURE-MVTS    PIC 9(4) COMP VALUE 0.
         05 CP-LECTURE-CPTE    PIC 9(4) COMP VALUE 0.
         05 CP-ECRITURE-CPTS   PIC 9(4) COMP VALUE 0.
         05 CP-PAGE            PIC 9(4) COMP VALUE 1.
         05 CP-MOUVEMENTS      PIC 9(4) COMP VALUE 0.

      *      ---------------------------------------------------------*
      *-----< VARIABLES D'AFFICHAGE                                   *
      *      ---------------------------------------------------------*
       01 AF-ETAT.
         05 AF-LIGNE1.
           10 FILLER PIC X(29)
                               VALUE 'NUMERO DE COMPTE          : '.
           10 AF-NUMCPT-1 PIC X(10).

         05 AF-LIGNE2.
           10 FILLER PIC X(29) VALUE '  ANCIEN SOLDE            : '.
           10 FILLER PIC X(36).
           10 AF-ANCIEN-S PIC +ZZZBZZZBZZ9,V99.

         05 AF-LIGNE3.
           10 FILLER PIC X(29) VALUE '  CUMUL DES RETRAITS      : '.
           10 FILLER PIC X(37).
           10 AF-CUMUL-R PIC ZZZBZZZBZZ9,V99.

         05 AF-LIGNE4.
           10 FILLER PIC X(29) VALUE '  CUMUL DES CARTES BLEUES : '.
           10 FILLER PIC X(37).
           10 AF-CUMUL-C  PIC ZZZBZZZBZZ9,V99.

         05 AF-LIGNE5.
           10 FILLER PIC X(29) VALUE '  CUMUL DES DEPOTS        : '.
           10 FILLER PIC X(37).
           10 AF-CUMUL-D    PIC ZZZBZZZBZZ9,V99.

         05 AF-LIGNE6.
           10 FILLER PIC X(31) VALUE 'COMPTE : '.
           10 AF-NUMCPT-3 PIC X(10).

          5 AF-LIGNE7.
           10 FILLER PIC X(29) VALUE '  TOTAL DES OPERATIONS    : '.
           10 FILLER PIC X(36).
           10 AF-TOTAL PIC +ZZZBZZZBZZ9,V99.

         05 AF-LIGNE8.
           10 FILLER PIC X(29) VALUE '  NOUVEAU SOLDE           : '.
           10 FILLER PIC X(36).
           10 AF-NOUVEAU-S PIC +ZZZBZZZBZZ9,V99.

      *      ---------------------------------------------------------*
      *-----< VARIABLES D'EDITION                                     *
      *      ---------------------------------------------------------*
       01 ED-ZONE-ED PIC X(80).
       01 ED-ZONE-CPTS PIC X(60).

       01 ED-ENTETE1.
         05 FILLER PIC X(13) VALUE 'EDITION DU : '.
         05 ED-DATE-ED PIC 99/99/9999.
         05 FILLER PIC X(46).
         05 FILLER PIC X(07) VALUE 'PAGE : '.
         05 ED-PAGE PIC 9(04).

       01 ED-DATE-DERNIER-MVT PIC 99/99/9999.

       01 ED-ENTETE2.
         05 FILLER PIC X(29).
         05 FILLER PIC X(22) VALUE 'EDITION DES OPERATIONS'.

       01 ED-FICCPTS.
         05 ED-FICCPTS-NUMCPT PIC X(10).
         05 ED-FICCPTS-DATE-OUV PIC 9(08).
         05 ED-FICCPTS-SOLDE PIC S9(09)V99 COMP-3.
         05 ED-FICCPTS-DATE-MAJ PIC 9(08).
         05 FILLER PIC X(22).

       PROCEDURE DIVISION.
      *===============================================================*
      *                     A L G O R I T H M E                       *
      *===============================================================*

      *---------------------------------------------------------------*
      *           000 A 599   TRAITEMENT PRINCIPAL                    *
      *---------------------------------------------------------------*

      *      ---------------------------------------------------------*
      *-----< BANQUE                                                  *
      *      ---------------------------------------------------------*

       100-BANQUE.
           PERFORM 600-OUVRIR-FICMVTS
           PERFORM 601-OUVRIR-FICCPTE
           PERFORM 602-OUVRIR-FICCPTS
           PERFORM 603-OUVRIR-FICETAT
           PERFORM 604-LIRE-FICMVTS
           PERFORM 605-LIRE-FICCPTE

           PERFORM 800-DATE
           PERFORM 701-INIT-COMPTEURS

           PERFORM 200-COMPTE
                    UNTIL FS-FICMVTS = '10' AND FS-FICCPTE = '10'

           PERFORM 801-DATE2
           PERFORM 805-AFFICHAGE-SYSOUT
           PERFORM 606-FERMER-FICCPTE
           PERFORM 607-FERMER-FICCPTS
           PERFORM 608-FERMER-FICETAT
           PERFORM 609-FERMER-FICMVTS
           PERFORM 999-ARRET-PROGRAMME
           .

      *      ---------------------------------------------------------*
      *-----< COMPTE                                                  *
      *      ---------------------------------------------------------*
       200-COMPTE.
           PERFORM 700-TRT-NOUVEAU-NUM
           PERFORM 610-ACTION-FICCPTS

           WRITE FD-FICCPTS FROM ED-FICCPTS
           PERFORM 802-AFFICHAGE-ENTETE
           PERFORM 803-AFFICHAGE-CORPS
      *    PERFORM 804-AFFICHAGE-SYSOUT

           ADD 1 TO CP-ECRITURE-CPTS
           .
      *      ---------------------------------------------------------*
      *-----< MOUVEMENT                                               *
      *      ---------------------------------------------------------*
       300-MOUVEMENT.
           EVALUATE TRUE
                WHEN FD-FICMVTS-CODE = 'D'
                     ADD FD-FICMVTS-MONTANT TO WS-CUMUL-DPOT
                     MOVE FD-FICMVTS-DATE TO ED-DATE-DERNIER-MVT
                WHEN FD-FICMVTS-CODE = 'R'
                     ADD FD-FICMVTS-MONTANT TO WS-CUMUL-RTRAIT
                     MOVE FD-FICMVTS-DATE TO ED-DATE-DERNIER-MVT
                WHEN FD-FICMVTS-CODE = 'C'
                     ADD FD-FICMVTS-MONTANT TO WS-CUMUL-CB
                     MOVE FD-FICMVTS-DATE TO ED-DATE-DERNIER-MVT
                WHEN OTHER
                     DISPLAY 'COMPTE ' FD-FICMVTS-NUMCPT ' : CODE MVT '
                     FD-FICMVTS-CODE ' ERRONE'
                     ADD 1 TO CP-ERREUR
           END-EVALUATE
           ADD 1 TO CP-MOUVEMENTS
           PERFORM 604-LIRE-FICMVTS
           .

      *      ---------------------------------------------------------*
      *-----<      600 A 699     GESTIONS DES ENTREES/SORTIES         *
      *      ---------------------------------------------------------*
       600-OUVRIR-FICMVTS.
           OPEN INPUT FICMVTS
           IF FS-FICMVTS NOT = 00
              DISPLAY ' ERREUR OUVERTURE FICMVTS   FS-FICMVTS = '
              FS-FICMVTS
              PERFORM 999-ARRET-PROGRAMME
           END-IF
           .
       601-OUVRIR-FICCPTE.
           OPEN INPUT FICCPTE
           IF FS-FICCPTE NOT = 00
              DISPLAY ' ERREUR OUVERTURE FICCPTE   FS-FICCPTE = '
              FS-FICCPTE
              PERFORM 999-ARRET-PROGRAMME
           END-IF
           .
       602-OUVRIR-FICCPTS.
           OPEN OUTPUT FICCPTS
           .
       603-OUVRIR-FICETAT.
           OPEN OUTPUT FICETAT
           .
       604-LIRE-FICMVTS.
           READ FICMVTS
           IF FS-FICMVTS = '10'
              MOVE HIGH-VALUE TO FD-FICMVTS-NUMCPT
           ELSE
              ADD 1 TO CP-LECTURE-MVTS
           END-IF
           .
       605-LIRE-FICCPTE.
           READ FICCPTE
           IF FS-FICCPTE = '10'
              MOVE HIGH-VALUE TO FD-FICCPTE-NUMCPT
           ELSE
              ADD 1 TO CP-LECTURE-CPTE
           END-IF
           .
       606-FERMER-FICCPTE.
           CLOSE FICCPTE
           .
       607-FERMER-FICCPTS.
           CLOSE FICCPTS
           .
       608-FERMER-FICETAT.
           CLOSE FICETAT
           .
       609-FERMER-FICMVTS.
           CLOSE FICMVTS
           .
       610-ACTION-FICCPTS.
           EVALUATE TRUE
                WHEN FD-FICMVTS-NUMCPT > FD-FICCPTE-NUMCPT
                     PERFORM 611-RECOPIE
                WHEN FD-FICMVTS-NUMCPT < FD-FICCPTE-NUMCPT
                     PERFORM 612-CREATION
                WHEN FD-FICMVTS-NUMCPT = FD-FICCPTE-NUMCPT
                     PERFORM 613-MODIFICATION
                WHEN OTHER
                     DISPLAY 'ERREUR'
           END-EVALUATE
           .

       611-RECOPIE.
           MOVE FD-FICCPTE TO ED-FICCPTS
           COMPUTE WS-TOTAL = WS-CUMUL-DPOT - WS-CUMUL-RTRAIT
           - WS-CUMUL-CB
           MOVE ED-FICCPTS TO ED-ZONE-CPTS
           MOVE FD-FICCPTE-SOLDE TO WS-ANCIEN-SOLDE
           MOVE FD-FICCPTE-SOLDE TO WS-NOUV-SOLDE
           ADD 1 TO CP-REPORT
           PERFORM 605-LIRE-FICCPTE
           .

       612-CREATION.
           INITIALIZE WS-ANCIEN-SOLDE
           MOVE FD-FICMVTS-NUMCPT TO ED-FICCPTS-NUMCPT
           MOVE FD-FICMVTS-DATE TO ED-FICCPTS-DATE-OUV
           PERFORM 300-MOUVEMENT
             UNTIL FD-FICMVTS-NUMCPT NOT = ED-FICCPTS-NUMCPT
                OR FS-FICMVTS = '10'
           COMPUTE WS-TOTAL = WS-CUMUL-DPOT - WS-CUMUL-RTRAIT
           - WS-CUMUL-CB
           MOVE WS-TOTAL TO ED-FICCPTS-SOLDE
           MOVE WS-TOTAL TO WS-NOUV-SOLDE
           MOVE ED-DATE-DERNIER-MVT TO ED-FICCPTS-DATE-MAJ
           MOVE ED-FICCPTS TO ED-ZONE-CPTS
           ADD 1 TO CP-CREA
           .

       613-MODIFICATION.
           MOVE FD-FICMVTS-NUMCPT TO ED-FICCPTS-NUMCPT
           MOVE FD-FICCPTE-DATE-OUV TO ED-FICCPTS-DATE-OUV
           PERFORM 300-MOUVEMENT
             UNTIL FD-FICMVTS-NUMCPT NOT = ED-FICCPTS-NUMCPT
               OR FS-FICMVTS = '10'
           MOVE FD-FICCPTE-SOLDE TO WS-ANCIEN-SOLDE
           COMPUTE WS-TOTAL = WS-CUMUL-DPOT - WS-CUMUL-CB
           - WS-CUMUL-RTRAIT
           COMPUTE WS-NOUV-SOLDE = WS-TOTAL + WS-ANCIEN-SOLDE
           MOVE WS-NOUV-SOLDE TO ED-FICCPTS-SOLDE
           MOVE FD-FICMVTS-DATE TO ED-DATE-DERNIER-MVT
           DISPLAY FD-FICMVTS-DATE 'DATE'
           MOVE ED-DATE-DERNIER-MVT TO ED-FICCPTS-DATE-MAJ
           MOVE ED-FICCPTS TO ED-ZONE-CPTS
           ADD 1 TO CP-MODIF
           PERFORM 605-LIRE-FICCPTE
           .
      *      ---------------------------------------------------------*
      *-----<      700 A 799     REGLES DE GESTIONS COMPLEXES         *
      *      ---------------------------------------------------------*
       700-TRT-NOUVEAU-NUM.
           MOVE FD-FICMVTS-NUMCPT TO WS-NUMCPT-PREC
           INITIALIZE WS-CUMUL-DPOT
           INITIALIZE WS-CUMUL-RTRAIT
           INITIALIZE WS-CUMUL-CB
           INITIALIZE CP-ERREUR
           .
       701-INIT-COMPTEURS.
           INITIALIZE CP-PAGE
           INITIALIZE CP-COMPTES
           INITIALIZE CP-CREA
           INITIALIZE CP-MODIF
           INITIALIZE CP-REPORT
           INITIALIZE CP-ERREUR
           INITIALIZE CP-LECTURE-MVTS
           INITIALIZE CP-LECTURE-CPTE
           INITIALIZE CP-ECRITURE-CPTS
           .

      *      ---------------------------------------------------------*
      *-----<      800 A 899     AFFICHAGES ET EDITIONS               *
      *      ---------------------------------------------------------*

       800-DATE.
           DISPLAY '     '
           .

       801-DATE2.
           DISPLAY '     '
           .

       802-AFFICHAGE-ENTETE.
           IF CP-COMPTES = 5
              ADD 1 TO CP-PAGE
              MOVE ED-ENTETE1 TO ED-ZONE-ED
              WRITE FD-FICETAT FROM ED-ZONE-ED AFTER PAGE
              END-WRITE
              MOVE ED-ENTETE2 TO ED-ZONE-ED
              WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 3 LINES
              INITIALIZE CP-COMPTES
           END-IF
           .

       803-AFFICHAGE-CORPS.
           MOVE ED-FICCPTS-NUMCPT TO AF-NUMCPT-1
           MOVE AF-LIGNE1 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 3 LINES

           MOVE WS-ANCIEN-SOLDE TO AF-ANCIEN-S
           MOVE AF-LIGNE2 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 2 LINES

           MOVE WS-CUMUL-RTRAIT TO AF-CUMUL-R
           MOVE AF-LIGNE3 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 2 LINES

           MOVE WS-CUMUL-CB TO AF-CUMUL-C
           MOVE AF-LIGNE4 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 1 LINE

           MOVE WS-CUMUL-DPOT TO AF-CUMUL-D
           MOVE AF-LIGNE5 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 1 LINE

           MOVE WS-TOTAL TO AF-TOTAL
           MOVE AF-LIGNE7 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 2 LINES

           MOVE WS-NOUV-SOLDE TO AF-NOUVEAU-S
           MOVE AF-LIGNE8 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 2 LINES

           ADD 1 TO CP-COMPTES.

       804-AFFICHAGE-SYSOUT.
           DISPLAY 'SYSOUT'
           .




       805-AFFICHAGE-SYSOUT.
           DISPLAY 'COBTPAA3 - DERNIERE COMPILATION : ' 'A COMPLETER'
           DISPLAY 'DEBUT DU TRAITEMENT : ' 'A COMPLETER' ' A '
           DISPLAY 'NOMBRE DE CREATIONS          : ' CP-CREA
           DISPLAY 'NOMBRE DE MODIFICATIONS      : ' CP-MODIF
           DISPLAY 'NOMBRE DE RECOPIES           : ' CP-REPORT
           DISPLAY 'NOMBRE DE MOUVEMENTS ERRONES : ' CP-ERREUR
           DISPLAY 'NOMBRE DE LECTURE FICCPTE    : ' CP-LECTURE-CPTE
           DISPLAY 'NOMBRE DE LECTURE FICMVTS    : ' CP-LECTURE-MVTS
           DISPLAY 'NOMBRE DE ECRITURE FICCPTS   : ' CP-ECRITURE-CPTS
           DISPLAY 'FIN NORMALE'
           DISPLAY 'FIN DE TRAITEMENT   : ' 'A COMPLETER' ' A '
           'A COMPLETER'
           .
      *---------------------------------------------------------------*
      *          900 A 999     GESTION DES ANOMALIES                  *
      *                        ET DE LA FIN DU TRAITEMENT             *
      *---------------------------------------------------------------*
       999-ARRET-PROGRAMME.
           STOP RUN
           .

      *===============================================================*
      *                 F I N  D U  P R O G R A M M E                 *
      *===============================================================*
