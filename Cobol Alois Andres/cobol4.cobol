       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTP4AA.
      *===============================================================*
      * NOM DU PROGRAMME : COBTP4AA                                   *
      *---------------------------------------------------------------*
      *               O B J E T  D U  P R O G R A M M E               *
      *---------------------------------------------------------------*
      *                          TP VSAM                              *
      *---------------------------------------------------------------*
      *     H I S T O R I Q U E  D E S  M O D I F I C A T I O N S     *
      *---------------------------------------------------------------*
      *   DATE   |  AUTEUR  |  REF.|              OBJET               *
      *---------------------------------------------------------------*
      * 10/08/22 |  ALOIS.A |      |          DEBUGING FS 92          *
      *   /  /   |  ALOIS.A |      |                                  *
      *===============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *---------------------------------------------------------------*
      *                     DEFINITION DES FICHIERS                   *
      *---------------------------------------------------------------*
       FILE-CONTROL.
           SELECT FICETAT ASSIGN TO FICETAT.
           SELECT FICMAJ  ASSIGN TO FICMAJ FILE STATUS IS FS-FICMAJ.
           SELECT FICCPT  ASSIGN TO FICCPT
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS FD-FICCPT-NUMCPT
           FILE STATUS IS FS-FICCPT.
       DATA DIVISION.
      *---------------------------------------------------------------*
      *                    DESCRIPTION DES FICHIERS                   *
      *---------------------------------------------------------------*
       FILE SECTION.
       FD FICETAT
           RECORDING MODE IS F.
       01     FD-FICETAT.
         05   FILLER              PIC X(79).

       FD FICCPT.
       01   FD-ENREG-FICCPT.
         05 FD-FICCPT-NUMCPT          PIC X(10).
         05 FD-FICCPT-DATE-OUV        PIC 9(08).
         05 FD-FICCPT-SOLDE           PIC S9(09)V9(02) COMP-3.
         05 FD-FICCPT-DATE-MAJ        PIC 9(08).
         05 FD-FICCPT-NOM             PIC X(20).
         05 FILLER                    PIC X(08).

       FD FICMAJ
           RECORDING MODE IS F.
       01     FD-FICMAJ.
         05   FD-FICMAJ-NUMCPT    PIC X(10).
         05   FD-FICMAJ-CODE      PIC X.
         05   FD-FICMAJ-DATE-OUV  PIC 9(08).
         05   FD-FICMAJ-SOLDE     PIC S9(09)V99 COMP-3.
         05   FD-FICMAJ-DATE-MAJ  PIC 9(08).
         05   FD-FICMAJ-NOM       PIC X(20).
         05   FD-FICMAJ-IND-OUV   PIC X.
           88 IND-OUV-OK          VALUE 'O'.
           88 IND-OUV-KO          VALUE 'N'.
         05   FD-FICMAJ-IND-SOLDE PIC X.
           88 IND-SOLDE-OK        VALUE 'O'.
           88 IND-SOLDE-KO        VALUE 'N'.
         05   FD-FICMAJ-IND-MAJ   PIC X.
           88 IND-MAJ-OK          VALUE 'O'.
           88 IND-MAJ-KO          VALUE 'N'.
         05   FD-FICMAJ-IND-NOM   PIC X.
           88 IND-NOM-OK          VALUE 'O'.
           88 IND-NOM-KO          VALUE 'N'.
         05   FILLER              PIC X(03).

      *---------------------------------------------------------------*
      *              DESCRIPTION DES VARIABLES DE TRAVAIL             *
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE COMPTES / FILE STATUS                      *
      *      ---------------------------------------------------------*
       01  WS-NUMCPT-PREC          PIC 9(10).

       01  WS-FICCPT-ENREG.
         05 WS-FICCPT-NUMCPT          PIC X(10).
         05 WS-FICCPT-DATE-OUV        PIC 9(08).
         05 WS-FICCPT-SOLDE           PIC S9(09)V9(02) COMP-3.
         05 WS-FICCPT-DATE-MAJ        PIC 9(08).
         05 WS-FICCPT-NOM             PIC X(20).
         05 FILLER                    PIC X(08).

       01  WS-FS.
         05   FS-FICMAJ            PIC 9(2).
         05   FS-FICCPT            PIC 9(2).
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
         05   WS-HEURE.
           10 WS-HH          PIC 99..
           10 WS-MN          PIC 99..
           10 WS-SC          PIC 99.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE CALCULS                                    *
      *      ---------------------------------------------------------*
       01     WS-VARIABLES-CALCUL.
         05   WS-CUMUL-DPOT       PIC  9(10)V9(02).
         05   WS-CUMUL-RTRAIT     PIC  9(10)V9(02).
         05   WS-CUMUL-CB         PIC  9(10)V9(02).
         05   WS-TOTAL            PIC S9(10)V9(02) COMP-3.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE COMPTEURS                                  *
      *      ---------------------------------------------------------*
       01 CP-VARIABLES-COMPTEURS.
         05 CP-CREA-OK    PIC 9(4) COMP VALUE 0.
         05 CP-CREA-KO    PIC 9(4) COMP VALUE 0.
         05 CP-MODIF-OK   PIC 9(4) COMP VALUE 0.
         05 CP-MODIF-KO   PIC 9(4) COMP VALUE 0.
         05 CP-SUPPR-OK   PIC 9(4) COMP VALUE 0.
         05 CP-SUPPR-KO   PIC 9(4) COMP VALUE 0.
         05 CP-ACT-ERR    PIC 9(4) COMP VALUE 0.
         05 CP-TOT-ERR    PIC 9(4) COMP VALUE 0.
         05 CP-TOT-ACT    PIC 9(4) COMP VALUE 0.

      *      ---------------------------------------------------------*
      *-----< VARIABLES D'AFFICHAGE                                   *
      *      ---------------------------------------------------------*
       01 AF-STATUT       PIC X(6).

      *      ---------------------------------------------------------*
      *-----< VARIABLES D'EDITION                                     *
      *      ---------------------------------------------------------*
       01 ED-ZONE-ED      PIC X(80).

       01 ED-ENTETE1.
         05 FILLER PIC X(13) VALUE 'EDITION DU : '.
         05 ED-DATE-ED PIC 99/99/9999.
         05 FILLER PIC X(46).
         05 FILLER PIC X(07) VALUE 'PAGE : '.
         05 ED-PAGE PIC 9(04).

       01 ED-ENTETE2.
         05 FILLER PIC X(29).
         05 FILLER PIC X(23) VALUE 'MISE A JOUR DES COMPTES'.

       01 ED-ENTETE3.
         05 FILLER PIC X(32) VALUE 'NO COMPTE   DATE OUV   DATE MAJ '.
         05 FILLER PIC X(22) VALUE '      SOLDE           '.
         05 FILLER PIC X(21) VALUE 'NOM CLIENT     STATUT'.

       01 ED-FICETAT.
         05 ED-FICMAJ-NUMCPT    PIC X(10).
         05 FILLER              PIC X.
         05 ED-FICMAJ-DATE-OUV  PIC 9(08).
         05 FILLER              PIC X.
         05 ED-FICMAJ-DATE-MAJ  PIC 9(08).
         05 FILLER              PIC X.
         05 ED-FICMAJ-SOLDE     PIC +ZZZBZZZBZZ9,V99.
         05 FILLER              PIC X.
         05 ED-FICMAJ-NOM       PIC X(20).
         05 FILLER              PIC X.
         05 ED-FICMAJ-CODE      PIC X.
         05 FILLER              PIC X.
         05 ED-STATUT           PIC X(6).

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
           PERFORM 600-OUVRIR-FICMAJ
           PERFORM 601-OUVRIR-FICCPT
           PERFORM 609-OUVRIR-FICETAT
           PERFORM 700-INITIALISATION
           PERFORM 602-LIRE-FICMAJ
           PERFORM 802-ENTETE-FICETAT
           PERFORM 200-COMPTE UNTIL FS-FICMAJ = 10
           PERFORM 604-FERMER-FICMAJ
           PERFORM 605-FERMER-FICCPT
           PERFORM 610-FERMER-FICETAT
           PERFORM 800-COMPTEURS
           PERFORM 999-ARRET-PROGRAMME
           .

      *      ---------------------------------------------------------*
      *-----< COMPTE                                                  *
      *      ---------------------------------------------------------*
       200-COMPTE.
           MOVE FD-FICMAJ-NUMCPT TO FD-FICCPT-NUMCPT
           PERFORM 608-LIRE-FICCPT
           PERFORM 710-EVALUATE-CODE

           PERFORM 803-AFFICHAGE-FICETAT
           PERFORM 602-LIRE-FICMAJ
           ADD 1 TO CP-TOT-ACT
           .

      *      ---------------------------------------------------------*
      *-----<      600 A 699     GESTIONS DES ENTREES/SORTIES         *
      *      ---------------------------------------------------------*
       600-OUVRIR-FICMAJ.
           OPEN INPUT FICMAJ
           IF FS-FICMAJ NOT = 00
             DISPLAY ' ERREUR OUVERTURE FICMAJ   FS-FICMAJ = ' FS-FICMAJ
             PERFORM 999-ARRET-PROGRAMME
           END-IF
           .

       601-OUVRIR-FICCPT.
           OPEN I-O FICCPT
           IF FS-FICCPT NOT = 00
             DISPLAY ' ERREUR OUVERTURE FICCPT   FS-FICCPT = ' FS-FICCPT
             PERFORM 999-ARRET-PROGRAMME
           END-IF
           .

       602-LIRE-FICMAJ.
           READ FICMAJ
           END-READ
           .

       604-FERMER-FICMAJ.
           CLOSE FICMAJ
           .

       605-FERMER-FICCPT.
           CLOSE FICCPT
           .

       606-ECRIRE-FICCPT.
           WRITE FD-ENREG-FICCPT FROM WS-FICCPT-ENREG
           .

       607-REECRIRE-FICCPT.
           REWRITE FD-ENREG-FICCPT FROM WS-FICCPT-ENREG
           .

       608-LIRE-FICCPT.
           READ FICCPT
           END-READ
           .

       609-OUVRIR-FICETAT.
           OPEN OUTPUT FICETAT
           .

       610-FERMER-FICETAT.
           CLOSE FICETAT
           .

       611-CREA-COMPTE.
           IF FS-FICCPT = 00
                   MOVE 'A TORT' TO AF-STATUT
                   ADD 1 TO CP-CREA-KO
                   ADD 1 TO CP-TOT-ERR
           ELSE IF FS-FICCPT = 23
                   MOVE 'OK' TO AF-STATUT
                   MOVE FD-FICMAJ-NUMCPT TO WS-FICCPT-NUMCPT
                   MOVE FD-FICMAJ-DATE-OUV TO WS-FICCPT-DATE-OUV
                   MOVE FD-FICMAJ-SOLDE TO WS-FICCPT-SOLDE
                   MOVE FD-FICMAJ-DATE-MAJ TO WS-FICCPT-DATE-MAJ
                   MOVE FD-FICMAJ-NOM TO WS-FICCPT-NOM
                   PERFORM 606-ECRIRE-FICCPT
                   ADD 1 TO CP-CREA-OK
           ELSE
               DISPLAY 'ERREUR DE ORDRE D ACCES FICHIER SUR FICCPT'
               ' - FS : ' FS-FICCPT
               ADD 1 TO CP-TOT-ERR
           END-IF
           .

       612-MODIF-COMPTE.
           READ FICCPT INTO WS-FICCPT-ENREG
           IF FS-FICCPT = 00
                 MOVE 'OK' TO AF-STATUT
                 MOVE FD-FICMAJ-NUMCPT TO WS-FICCPT-NUMCPT
                 IF IND-OUV-OK
                    THEN MOVE FD-FICMAJ-DATE-OUV TO WS-FICCPT-DATE-OUV
                 END-IF
                 IF IND-SOLDE-OK
                    THEN MOVE FD-FICMAJ-SOLDE TO WS-FICCPT-SOLDE
                 END-IF
                 IF IND-MAJ-OK
                    THEN MOVE FD-FICMAJ-DATE-MAJ TO WS-FICCPT-DATE-MAJ
                 END-IF
                 IF IND-NOM-OK
                    THEN MOVE FD-FICMAJ-NOM TO WS-FICCPT-NOM
                 END-IF
                 PERFORM 607-REECRIRE-FICCPT
                 ADD 1 TO CP-MODIF-OK
           ELSE IF FS-FICCPT = 23
                 MOVE 'A TORT' TO AF-STATUT
                 ADD 1 TO CP-MODIF-KO
                 ADD 1 TO CP-TOT-ERR
           ELSE
              DISPLAY 'ERREUR DE ORDRE D ACCES FICHIER SUR FICCPT'
               ' - FS : ' FS-FICCPT
              ADD 1 TO CP-TOT-ERR
           END-IF
           .

       613-SUPPR-COMPTE.
           IF FS-FICCPT = 00
                 MOVE 'OK' TO AF-STATUT
                 DELETE FICCPT
                 ADD 1 TO CP-SUPPR-OK
           ELSE IF FS-FICCPT = 23
                 MOVE 'A TORT' TO AF-STATUT
                 ADD 1 TO CP-SUPPR-KO
                 ADD 1 TO CP-TOT-ERR
           ELSE
              DISPLAY 'ERREUR DE ORDRE D ACCES FICHIER SUR FICCPT'
               ' - FS : ' FS-FICCPT
              ADD 1 TO CP-TOT-ERR
           END-IF
           .

       614-ANOMALIE-CODE.
           MOVE 'ERRONE' TO AF-STATUT
           ADD 1 TO CP-ACT-ERR
           ADD 1 TO CP-TOT-ERR
           .

      *      ---------------------------------------------------------*
      *-----<      700 A 799     REGLES DE GESTIONS COMPLEXES         *
      *      ---------------------------------------------------------*
       700-INITIALISATION.
           INITIALIZE CP-CREA-OK
           INITIALIZE CP-CREA-KO
           INITIALIZE CP-MODIF-OK
           INITIALIZE CP-MODIF-KO
           INITIALIZE CP-SUPPR-OK
           INITIALIZE CP-SUPPR-KO
           INITIALIZE CP-ACT-ERR
           INITIALIZE CP-TOT-ERR
           INITIALIZE CP-TOT-ACT
           .

       710-EVALUATE-CODE.
           EVALUATE FD-FICMAJ-CODE
               WHEN 'C'
                   PERFORM 611-CREA-COMPTE
               WHEN 'M'
                   PERFORM 612-MODIF-COMPTE
               WHEN 'S'
                   PERFORM 613-SUPPR-COMPTE
               WHEN OTHER
                   PERFORM 614-ANOMALIE-CODE
           END-EVALUATE
           .

      *      ---------------------------------------------------------*
      *-----<      800 A 899     AFFICHAGES ET EDITIONS               *
      *      ---------------------------------------------------------*
       800-COMPTEURS.
           DISPLAY 'COMPTEUR CREATIONS OK     : ' CP-CREA-OK
           DISPLAY 'COMPTEUR CREATIONS KO     : ' CP-CREA-KO
           DISPLAY 'COMPTEUR MODIFICATIONS OK : ' CP-MODIF-OK
           DISPLAY 'COMPTEUR MODIFICATIONS KO : ' CP-MODIF-KO
           DISPLAY 'COMPTEUR SUPRESSIONS OK   : ' CP-SUPPR-OK
           DISPLAY 'COMPTEUR SUPRESSIONS KO   : ' CP-SUPPR-KO
           DISPLAY 'COMPTEUR ACTIONS ERRONEES : ' CP-ACT-ERR
           DISPLAY 'COMPTEUR TOTAL ERREURS    : ' CP-TOT-ERR
           DISPLAY 'COMPTEUR TOTAL ACTIONS    : ' CP-TOT-ACT
           .

       802-ENTETE-FICETAT.
           MOVE ED-ENTETE1 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED
           MOVE ED-ENTETE2 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 3 LINES
           MOVE ED-ENTETE3 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 3 LINES
           .

       803-AFFICHAGE-FICETAT.
           MOVE FD-FICMAJ-NUMCPT TO ED-FICMAJ-NUMCPT
           MOVE FD-FICMAJ-DATE-OUV TO ED-FICMAJ-DATE-OUV
           MOVE FD-FICMAJ-DATE-MAJ TO ED-FICMAJ-DATE-MAJ
           MOVE FD-FICMAJ-SOLDE TO ED-FICMAJ-SOLDE
           MOVE FD-FICMAJ-NOM TO ED-FICMAJ-NOM
           MOVE FD-FICMAJ-CODE TO ED-FICMAJ-CODE
           MOVE AF-STATUT TO ED-STATUT
           MOVE ED-FICETAT TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED
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
