       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2TP8AA.
      *===============================================================*
      * NOM DU PROGRAMME : DB2TP8AA                                   *
      *---------------------------------------------------------------*
      *               O B J E T  D U  P R O G R A M M E               *
      *---------------------------------------------------------------*
      *                          TP CURSEUR                          *
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
       DATA DIVISION.
      *---------------------------------------------------------------*
      *                    DESCRIPTION DES FICHIERS                   *
      *---------------------------------------------------------------*
       FILE SECTION.
       FD FICETAT
           RECORDING MODE IS F.
       01     FD-FICETAT.
         05   FILLER              PIC X(79).

      *---------------------------------------------------------------*
      *              DESCRIPTION DES VARIABLES DE TRAVAIL             *
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.

      *      ---------------------------------------------------------*
      *-----< SQLCODE                                                 *
      *      ---------------------------------------------------------*
        EXEC SQL
          INCLUDE TCOMPTE
        END-EXEC
        .

        EXEC SQL
          INCLUDE SQLCA
        END-EXEC
        .

        COPY CCALDAT.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE DATE                                       *
      *      ---------------------------------------------------------*
       01       WS-DATES-SYSIN.

         05     WS-SYSIN       PIC X(8).

         05     WS-DATE-SYSIN1 PIC X(8).

         05     WS-DATE-SYSIN2.
           10   WS-SSAA.
             15 WS-SS          PIC X(2).
             15 WS-AA          PIC X(2).
           10   WS-MM          PIC X(2).
           10   WS-JJ          PIC X(2).

         05     WS-DATE-SYSIN3.
           10   WS-SSAA.
             15 WS-SS          PIC X(2).
             15 WS-AA          PIC X(2).
           10   FILLER         PIC X VALUE '-'.
           10   WS-MM          PIC X(2).
           10   FILLER         PIC X VALUE '-'.
           10   WS-JJ          PIC X(2).

         05     WS-DATE-SYSIN4 PIC X(10).

       01     WS-SCALDAT-DATES.

         05   WS-SCALDAT1.
           10 WS-SSAA-SCALDAT1 PIC X(4).
           10 WS-MM-SCALDAT1   PIC X(2).
           10 WS-JJ-SCALDAT1   PIC X(2).

         05   WS-SCALDAT2.
           10 WS-SSAA-SCALDAT2 PIC X(4).
           10 FILLER           PIC X.
           10 WS-MM-SCALDAT2   PIC X(2).
           10 FILLER           PIC X.
           10 WS-JJ-SCALDAT2   PIC X(2).

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE TABLE ET BUFFER                            *
      *      ---------------------------------------------------------*
       01   WS-TABLE.
         05 WS-NUMERCPT   PIC X(10).
         05 WS-DATECREA   PIC X(10).
         05 WS-SOLDECPT   PIC S9(8)V9(2) COMP-3.

       01   WS-BUFFER     PIC S9(8)V9(2).
      *      ---------------------------------------------------------*
      *-----< VARIABLES D'EDITION                                     *
      *      ---------------------------------------------------------*
       01 ED-ZONE-ED      PIC X(80).

       01 ED-ENTETE1.
         05 FILLER PIC X(6) VALUE 'NUMCPT'.
         05 FILLER PIC X(20).
         05 FILLER PIC X(5) VALUE 'SOLDE'.
         05 FILLER PIC X(20).
         05 FILLER PIC X(13) VALUE 'DATE CREATION'.

       01 ED-FICETAT.
         05 ED-NUMCPT  PIC X(10).
         05 FILLER     PIC X(10).
         05 ED-SOLDE   PIC +ZZZBZZZBZZ9,V99.
         05 FILLER     PIC X(11).
         05 ED-DATE    PIC X(30).

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
           PERFORM 610-LIRE-SYSIN
           PERFORM 630-SYSIN-VIDE
           PERFORM 600-DECLARER-CURSEUR
           PERFORM 601-OUVRIR-CURSEUR
           PERFORM 602-LIRE-CURSEUR
           PERFORM 620-OUVRIR-FICETAT
           PERFORM 800-ENTETE-FICETAT
           PERFORM 200-COMPTE
             UNTIL SQLCODE = 100
           PERFORM 603-FERMER-CURSEUR
           PERFORM 621-FERMER-FICETAT
           PERFORM 999-ARRET-PROGRAMME
           .

      *      ---------------------------------------------------------*
      *-----< COMPTE                                                  *
      *      ---------------------------------------------------------*
       200-COMPTE.
           PERFORM 701-ATTRIB-DATE
           PERFORM 700-APPEL-SCALDAT
           PERFORM 801-AFFICHAGE-FICETAT
           PERFORM 602-LIRE-CURSEUR
           .

      *---------------------------------------------------------------*
      *           600 A 699   GESTION DES ENTREES/SORTIES             *
      *---------------------------------------------------------------*

       600-DECLARER-CURSEUR.
           EXEC SQL
             DECLARE  CURSEUR1 CURSOR FOR
             SELECT   NUMERCPT, DATECREA, SOLDECPT
             FROM     TCOMPTE020
             WHERE    DATECREA > :WS-DATE-SYSIN4
           END-EXEC
           .

       601-OUVRIR-CURSEUR.
           EXEC SQL
             OPEN CURSEUR1
           END-EXEC
           .

       602-LIRE-CURSEUR.
           EXEC SQL
             FETCH CURSEUR1
               INTO :WS-NUMERCPT, :WS-DATECREA, :WS-SOLDECPT
           END-EXEC
           .

       603-FERMER-CURSEUR.
           EXEC SQL
             CLOSE CURSEUR1
           END-EXEC
           .

       610-LIRE-SYSIN.
           ACCEPT WS-DATE-SYSIN1 FROM SYSIN.
           MOVE WS-DATE-SYSIN1 TO WS-DATE-SYSIN2
           MOVE CORR WS-DATE-SYSIN2 TO WS-DATE-SYSIN3
           MOVE WS-DATE-SYSIN3 TO WS-DATE-SYSIN4
           .

       620-OUVRIR-FICETAT.
           OPEN OUTPUT FICETAT
           .

       621-FERMER-FICETAT.
           CLOSE FICETAT
           .

       630-SYSIN-VIDE.
           MOVE LOW-VALUE TO WS-SYSIN
           ACCEPT WS-SYSIN FROM SYSIN
           IF WS-SYSIN = LOW-VALUE
              DISPLAY 'ERREUR : SYSIN VIDE, FIN DE PROGRAMME.'
              PERFORM 999-ARRET-PROGRAMME
           END-IF
           .
      *      ---------------------------------------------------------*
      *-----<      700 A 799     REGLES DE GESTIONS COMPLEXES         *
      *      ---------------------------------------------------------*
       700-APPEL-SCALDAT.
           CALL 'SCALDAT' USING SCALDAT-COMMAREA
           IF SCALDAT-STATUS NOT = '00'
              DISPLAY SCALDAT-MESSAGE
           END-IF
           .

       701-ATTRIB-DATE.
           MOVE WS-DATECREA TO WS-SCALDAT2
           MOVE WS-SSAA-SCALDAT2 TO WS-SSAA-SCALDAT1
           MOVE WS-MM-SCALDAT2 TO WS-MM-SCALDAT1
           MOVE WS-JJ-SCALDAT2 TO WS-JJ-SCALDAT1
           MOVE WS-SCALDAT1 TO SCALDAT-DATE
           .

      *      ---------------------------------------------------------*
      *-----<      800 A 899     AFFICHAGES ET EDITIONS               *
      *      ---------------------------------------------------------*

       800-ENTETE-FICETAT.
           MOVE ED-ENTETE1 TO ED-ZONE-ED
           WRITE FD-FICETAT FROM ED-ZONE-ED
           .

       801-AFFICHAGE-FICETAT.
           MOVE WS-NUMERCPT TO ED-NUMCPT
           MOVE WS-SOLDECPT TO WS-BUFFER
           MOVE WS-BUFFER TO ED-SOLDE
           MOVE SCALDAT-DATETENDUE TO ED-DATE
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
