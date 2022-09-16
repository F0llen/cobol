       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2TP4AA.
      *===============================================================*
      * NOM DU PROGRAMME : DB2TP4AA                                   *
      *---------------------------------------------------------------*
      *               O B J E T  D U  P R O G R A M M E               *
      *---------------------------------------------------------------*
      *                          TP VSAM                              *
      *---------------------------------------------------------------*
      *     H I S T O R I Q U E  D E S  M O D I F I C A T I O N S     *
      *---------------------------------------------------------------*
      *   DATE   |  AUTEUR  |  REF.|              OBJET               *
      *---------------------------------------------------------------*
      *   /  /   |  ALOIS.A |      |                                  *
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
       DATA DIVISION.
      *---------------------------------------------------------------*
      *                    DESCRIPTION DES FICHIERS                   *
      *---------------------------------------------------------------*
       FILE SECTION.
       FD FICETAT
           RECORDING MODE IS F.
       01     FD-FICETAT.
         05   FILLER              PIC X(79).

       FD FICMAJ
           RECORDING MODE IS F.
       01     FD-FICMAJ.
         05   FD-FICMAJ-NUMCPT    PIC X(10).
         05   FD-FICMAJ-CODE      PIC X.
         05   FD-FICMAJ-DATE-OUV.
           10 WS-SSAA             PIC 9(4).
           10 WS-MM               PIC 9(2).
           10 WS-JJ               PIC 9(2).
         05   FD-FICMAJ-SOLDE     PIC S9(09)V99 COMP-3.
         05   FD-FICMAJ-DATE-MAJ.
           10 WS-SSAA             PIC 9(4).
           10 WS-MM               PIC 9(2).
           10 WS-JJ               PIC 9(2).
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
         05   FILLER              PIC 9(3).

      *---------------------------------------------------------------*
      *              DESCRIPTION DES VARIABLES DE TRAVAIL             *
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *      ---------------------------------------------------------*
      *-----< COPIE TABLE COMPTE                                      *
      *      ---------------------------------------------------------*
       EXEC SQL
         INCLUDE TCOMPTE
       END-EXEC
       .

      *      ---------------------------------------------------------*
      *-----< INCLUSION SQLCODE                                       *
      *      ---------------------------------------------------------*
       EXEC SQL
         INCLUDE SQLCA
       END-EXEC
       .

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE FILE STATUS / INDICATEUR                   *
      *      ---------------------------------------------------------*
       01  WS-FS.
         05   FS-FICMAJ            PIC 9(2).

       01   INDICATEUR.
         05 INDICSOLDE             PIC S9(4) COMP.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE DATE                                       *
      *      ---------------------------------------------------------*
       01     WS-DONNEES-TEMPORELLES.

         05   WS-BUF-DATE.
           10 WS-SSAA        PIC 9(4).
           10 FILLER         PIC X VALUE '-'.
           10 WS-MM          PIC 9(2).
           10 FILLER         PIC X VALUE '-'.
           10 WS-JJ          PIC 9(2).

         05   WS-DATE-SYST.
           10 WS-AA          PIC 9(4).
           10 WS-MM          PIC 99.
           10 WS-JJ          PIC 99.
         05   WS-DATE-TRAITEMENT.
           10 WS-JJ          PIC 99/.
           10 WS-MM          PIC 99/.
           10 FILLER         PIC XX VALUE '20'.
           10 WS-AA          PIC 99.

       01     WS-DONNEES-COMP.
         05   WS-DATE-COMPILE.
           10 WS-JJ          PIC 99/.
           10 WS-MM          PIC 99/.
           10 WS-AA          PIC 99.
         05   WS-HEURE-COMPILE.
           10 WS-HH          PIC 99.
           10 FILLER         PIC X VALUE ':'.
           10 WS-MN          PIC 99.
           10 FILLER         PIC X VALUE ':'.
           10 WS-SC          PIC 99.

       01     WS-DATE.
         05   WS-DATE-COMP.
           10 WS-JJ-COMP     PIC 99/.
           10 WS-MM-COMP     PIC 99/.
           10 WS-AA-COMP     PIC 99.
         05   WS-DATE-COMP2.
           10 WS-MM-COMP     PIC 99/.
           10 WS-JJ-COMP     PIC 99/.
           10 FILLER         PIC XX VALUE '20'.
           10 WS-AA-COMP     PIC 99.
         05   WS-HEURE-P.
           10 WS-HH          PIC 99.
           10 FILLER         PIC X VALUE ':'.
           10 WS-MN          PIC 99.
           10 FILLER         PIC X VALUE ':'.
           10 WS-SC          PIC 99.

       01     WS-HEURE-DEB.
         05   WS-HH          PIC 99.
         05   WS-MN          PIC 99.
         05   WS-SC          PIC 99.

       01     WS-HEURE-FIN.
         05   WS-HH          PIC 99.
         05   WS-MN          PIC 99.
         05   WS-SC          PIC 99.

       01     WS-TRAITEMENT-DEB.
         05   WS-DATE-DEB.
           10 WS-AA          PIC 9(4).
           10 WS-MM          PIC 99.
           10 WS-JJ          PIC 99.
         05   WS-DATE-DEB1.
           10 WS-JJ          PIC 99/.
           10 WS-MM          PIC 99/.
           10 FILLER         PIC XX VALUE '20'.
           10 WS-AA          PIC 99.

       01     WS-TRAITEMENT-FIN.
         05   WS-DATE-FIN.
           10 WS-AA          PIC 9(4).
           10 WS-MM          PIC 99.
           10 WS-JJ          PIC 99.
         05   WS-DATE-FIN1.
           10 WS-JJ          PIC 99/.
           10 WS-MM          PIC 99/.
           10 FILLER         PIC XX VALUE '20'.
           10 WS-AA          PIC 99.

       01     WS-DATE-OUV.
         05   WS-JJ          PIC 99/.
         05   WS-MM          PIC 99/.
         05   FILLER         PIC XX VALUE '20'.
         05   WS-AA          PIC 99.

       01     WS-DATE-MAJ.
         05   WS-JJ          PIC 99/.
         05   WS-MM          PIC 99/.
         05   FILLER         PIC XX VALUE '20'.
         05   WS-AA          PIC 99.

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
       01 AF-CODE         PIC 9.
       01 AF-NUMERCPT     PIC X(10).
       01 AF-FICMAJ-CODE  PIC X.

      *      ---------------------------------------------------------*
      *-----< VARIABLES D'EDITION                                     *
      *      ---------------------------------------------------------*
       01 ED-ZONE-ED      PIC X(80).

       01 ED-ENTETE1.
         05 FILLER PIC X(13) VALUE 'EDITION DU : '.
         05 ED-DATE-ED PIC 99/99/9999.
         05 FILLER PIC X(45).
         05 FILLER PIC X(07) VALUE 'PAGE : '.
         05 ED-PAGE PIC 9(04).

       01 ED-ENTETE2.
         05 FILLER PIC X(29).
         05 FILLER PIC X(23) VALUE 'MISE A JOUR DES COMPTES'.

       01 ED-ENTETE3.
         05 FILLER PIC X(31) VALUE 'NO COMPTE  DATE OUV   DATE MAJ '.
         05 FILLER PIC X(22) VALUE '      SOLDE           '.
         05 FILLER PIC X(24) VALUE 'NOM CLIENT        STATUT'.

       01 ED-FICETAT.
         05 ED-FICMAJ-NUMCPT    PIC X(10).
         05 FILLER              PIC X.
         05 ED-FICMAJ-DATE-OUV  PIC 99/99/9999.
         05 FILLER              PIC X.
         05 ED-FICMAJ-DATE-MAJ  PIC 99/99/9999.
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
           PERFORM 609-OUVRIR-FICETAT
           PERFORM 700-INITIALISATION
           PERFORM 602-LIRE-FICMAJ
           PERFORM 810-DATE-HEURE1

           PERFORM 802-ENTETE-FICETAT
           PERFORM 200-COMPTE UNTIL FS-FICMAJ = 10

           PERFORM 811-DATE-HEURE2
           PERFORM 820-DEBUT-SYSOUT
           PERFORM 815-AFFICHAGE-ANOM
           PERFORM 604-FERMER-FICMAJ
           PERFORM 610-FERMER-FICETAT
           PERFORM 800-COMPTEURS-FIN
           PERFORM 999-ARRET-PROGRAMME
           .

      *      ---------------------------------------------------------*
      *-----< COMPTE                                                  *
      *      ---------------------------------------------------------*
       200-COMPTE.
           PERFORM 605-LIRE-COMPTE
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

       602-LIRE-FICMAJ.
           READ FICMAJ
           END-READ
           .

       604-FERMER-FICMAJ.
           CLOSE FICMAJ
           .

       605-LIRE-COMPTE.
           MOVE FD-FICMAJ-NUMCPT TO NUMERCPT
           MOVE CORR FD-FICMAJ-DATE-OUV TO WS-BUF-DATE
           MOVE WS-BUF-DATE TO DATECREA
           MOVE CORR FD-FICMAJ-DATE-MAJ TO WS-BUF-DATE
           MOVE WS-BUF-DATE TO DATEMAJ
           EXEC SQL
                SELECT NUMERCPT, NOMCLIEN, DATECREA, SOLDECPT, DATEMAJ
                INTO :NUMERCPT, :NOMCLIEN, :DATECREA,
                :SOLDECPT :INDICSOLDE, :DATEMAJ
                FROM TCOMPTE020
                WHERE NUMERCPT = :NUMERCPT
           END-EXEC
           PERFORM 630-TEST-SQL-CODE
           .

       609-OUVRIR-FICETAT.
           OPEN OUTPUT FICETAT
           .

       610-FERMER-FICETAT.
           CLOSE FICETAT
           .

       611-CREA-COMPTE.
           IF SQLCODE = 0
                   MOVE 'A TORT' TO AF-STATUT
                   ADD 1 TO CP-CREA-KO
                   ADD 1 TO CP-TOT-ERR
           ELSE IF SQLCODE = 100
              EXEC SQL
                INSERT INTO TCOMPTE020 (NUMERCPT, NOMCLIEN, DATECREA,
                SOLDECPT, DATEMAJ)
                VALUES (:NUMERCPT, :NOMCLIEN, :DATECREA,
                :SOLDECPT :INDICSOLDE, :DATEMAJ)
              END-EXEC
              PERFORM 630-TEST-SQL-CODE
              ADD 1 TO CP-CREA-OK
           ELSE
               DISPLAY 'ERREUR REQUETE SQL - SQLCODE : ' SQLCODE
               ADD 1 TO CP-TOT-ERR
           END-IF
           .

       612-MODIF-COMPTE.
           IF SQLCODE = 0
                 MOVE 'OK' TO AF-STATUT
                 IF IND-OUV-OK
                    PERFORM 621-MODIF-DATE-OUV
                 END-IF
                 IF IND-SOLDE-OK
                    PERFORM 622-MODIF-SOLDE
                 END-IF
                 IF IND-MAJ-OK
                    PERFORM 623-MODIF-DATE-MAJ
                 END-IF
                 IF IND-NOM-OK
                    PERFORM 624-MODIF-NOM
                 END-IF
                 ADD 1 TO CP-MODIF-OK
           ELSE IF SQLCODE = 100
                 MOVE 'A TORT' TO AF-STATUT
                 ADD 1 TO CP-MODIF-KO
                 ADD 1 TO CP-TOT-ERR
           ELSE
              DISPLAY 'ERREUR DE REQUETE SQL - SQLCODE : ' SQLCODE
              ADD 1 TO CP-TOT-ERR
           END-IF
           .

       613-SUPPR-COMPTE.
           IF SQLCODE = 0
                 MOVE 'OK' TO AF-STATUT
                 EXEC SQL
                   DELETE FROM TCOMPTE020
                   WHERE NUMERCPT = :NUMERCPT
                 END-EXEC
                 PERFORM 630-TEST-SQL-CODE
                 ADD 1 TO CP-SUPPR-OK
           ELSE IF SQLCODE = 100
                 MOVE 'A TORT' TO AF-STATUT
                 ADD 1 TO CP-SUPPR-KO
                 ADD 1 TO CP-TOT-ERR
           ELSE
              DISPLAY 'ERREUR REQUETE SQL - SQLCODE : ' SQLCODE
              ADD 1 TO CP-TOT-ERR
           END-IF
           .

       614-ANOMALIE-CODE.
           MOVE 'ERRONE' TO AF-STATUT
           MOVE 1 TO AF-CODE
           MOVE NUMERCPT TO AF-NUMERCPT
           MOVE FD-FICMAJ-CODE TO AF-FICMAJ-CODE
           ADD 1 TO CP-ACT-ERR
           ADD 1 TO CP-TOT-ERR
           .


       621-MODIF-DATE-OUV.
           MOVE FD-FICMAJ-DATE-OUV TO DATECREA
           EXEC SQL
             UPDATE TCOMPTE020
             SET DATECREA = :DATECREA
             WHERE NUMERCPT = :NUMERCPT
           END-EXEC
           PERFORM 630-TEST-SQL-CODE
           .

       622-MODIF-SOLDE.
           MOVE FD-FICMAJ-SOLDE TO SOLDECPT
           EXEC SQL
             UPDATE TCOMPTE020
             SET SOLDECPT = :SOLDECPT :INDICSOLDE
             WHERE NUMERCPT = :NUMERCPT
           END-EXEC
           PERFORM 630-TEST-SQL-CODE
           .

       623-MODIF-DATE-MAJ.
           MOVE FD-FICMAJ-DATE-MAJ TO DATEMAJ
           EXEC SQL
             UPDATE TCOMPTE020
             SET DATEMAJ = :DATEMAJ
             WHERE NUMERCPT = :NUMERCPT
           END-EXEC
           PERFORM 630-TEST-SQL-CODE
           .

       624-MODIF-NOM.
           MOVE FD-FICMAJ-NOM TO NOMCLIEN
           EXEC SQL
             UPDATE TCOMPTE020
             SET NOMCLIEN = :NOMCLIEN
             WHERE NUMERCPT = :NUMERCPT
           END-EXEC
           PERFORM 630-TEST-SQL-CODE
           .

       630-TEST-SQL-CODE.
           EVALUATE TRUE
               WHEN SQLCODE = 0
                    CONTINUE
               WHEN SQLCODE < 0
                    DISPLAY ' ERROR ON NUMERCPT = ' FD-FICMAJ-NUMCPT
                            ' NOM CLIENT ' NOMCLIEN
                    DISPLAY ' SQLCODE = ' SQLCODE
               WHEN SQLCODE = 100
                    CONTINUE
               WHEN SQLCODE > 0 AND NOT = 100
                    DISPLAY ' WARNING '
                    DISPLAY ' SQLCODE = ' SQLCODE
               WHEN OTHER
                    DISPLAY ' SQLCODE = ' SQLCODE
            END-EVALUATE
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
       800-COMPTEURS-FIN.
           DISPLAY 'NOMBRE DE CREATIONS OK    : ' CP-CREA-OK
           DISPLAY 'NOMBRE DE CREATIONS KO    : ' CP-CREA-KO
           DISPLAY 'NOMBRE DE MODIFICATIONS OK: ' CP-MODIF-OK
           DISPLAY 'NOMBRE DE MODIFICATIONS KO: ' CP-MODIF-KO
           DISPLAY 'NOMBRE DE SUPRESSIONS OK  : ' CP-SUPPR-OK
           DISPLAY 'NOMBRE DE SUPRESSIONS KO  : ' CP-SUPPR-KO
           DISPLAY 'NOMBRE DE ACTIONS ERRONEES: ' CP-ACT-ERR
           DISPLAY 'TOTAL ERREURS             : ' CP-TOT-ERR
           DISPLAY 'TOTAL ACTIONS             : ' CP-TOT-ACT
           DISPLAY 'FIN NORMALE'
           MOVE CORR WS-HEURE-FIN TO WS-HEURE-P
           DISPLAY 'FIN DE TRAITEMENT   : ' WS-DATE-FIN1 ' A '
                   WS-HEURE-P
           .

       802-ENTETE-FICETAT.
           ACCEPT WS-DATE-SYST FROM DATE YYYYMMDD
           MOVE CORR WS-DATE-SYST TO WS-DATE-TRAITEMENT
           MOVE WS-DATE-TRAITEMENT TO ED-DATE-ED
           MOVE ED-ENTETE1 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED
           MOVE ED-ENTETE2 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 3 LINES
           MOVE ED-ENTETE3 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED AFTER 3 LINES
           .

       803-AFFICHAGE-FICETAT.
           MOVE FD-FICMAJ-NUMCPT TO ED-FICMAJ-NUMCPT
           MOVE CORR FD-FICMAJ-DATE-OUV TO WS-DATE-OUV
           MOVE CORR FD-FICMAJ-DATE-MAJ TO WS-DATE-MAJ
           MOVE WS-DATE-OUV TO ED-FICMAJ-DATE-OUV
           MOVE WS-DATE-MAJ TO ED-FICMAJ-DATE-MAJ
           MOVE FD-FICMAJ-SOLDE TO ED-FICMAJ-SOLDE
           MOVE FD-FICMAJ-NOM TO ED-FICMAJ-NOM
           MOVE FD-FICMAJ-CODE TO ED-FICMAJ-CODE
           MOVE AF-STATUT TO ED-STATUT
           MOVE ED-FICETAT TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED
           .

       810-DATE-HEURE1.
           ACCEPT WS-HEURE-DEB FROM TIME
           ACCEPT WS-DATE-DEB FROM DATE YYYYMMDD
           MOVE CORR WS-DATE-DEB TO WS-DATE-DEB1
           .

       811-DATE-HEURE2.
           ACCEPT WS-HEURE-FIN FROM TIME
           ACCEPT WS-DATE-FIN FROM DATE YYYYMMDD
           MOVE CORR WS-DATE-FIN TO WS-DATE-FIN1
           .

       815-AFFICHAGE-ANOM.
           IF AF-CODE = 1
             DISPLAY 'COMPTE ' AF-NUMERCPT ' : CODE ACTION '
                   AF-FICMAJ-CODE ' ERRONE'
           END-IF
           DISPLAY SPACE
           .

       820-DEBUT-SYSOUT.
           MOVE WHEN-COMPILED TO WS-DONNEES-COMP
           MOVE WS-DONNEES-COMP TO WS-DATE-COMP
           MOVE CORR WS-DATE-COMP TO WS-DATE-COMP2
           MOVE CORR WS-HEURE-COMPILE TO WS-HEURE-P
           DISPLAY 'COBTP4AA - DERNIERE COMPILATION : ' WS-DATE-COMP2
                   ' A ' WS-HEURE-P
           MOVE CORR WS-HEURE-DEB TO WS-HEURE-P
           DISPLAY 'DEBUT DE TRAITEMENT : ' WS-DATE-DEB1 ' A '
                   WS-HEURE-P
           DISPLAY SPACE
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
