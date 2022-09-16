       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTP5AA.
      *===============================================================*
      * NOM DU PROGRAMME : COBTP5AA                                   *
      *---------------------------------------------------------------*
      *               O B J E T  D U  P R O G R A M M E               *
      *---------------------------------------------------------------*
      *                          TP TABLEAUX                          *
      *---------------------------------------------------------------*
      *     H I S T O R I Q U E  D E S  M O D I F I C A T I O N S     *
      *---------------------------------------------------------------*
      *   DATE   |  AUTEUR  |  REF.|              OBJET               *
      *---------------------------------------------------------------*
      * 11/08/22 |  ALOIS.A |      |          DEBUT DE PROG           *
      *   /  /   |  ALOIS.A |      |                                  *
      *===============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *===============================================================*
      *                     DEFINITION DES FICHIERS                   *
      *===============================================================*
       FILE-CONTROL.
           SELECT FICCPT ASSIGN TO FICCPT.
       DATA DIVISION.
      *===============================================================*
      *                    DESCRIPTION DES FICHIERS                   *
      *===============================================================*
       FILE SECTION.
       FD FICCPT
           RECORDING MODE IS F.
       01    FD-FICCPT.
         05  FD-FICCPT-NUMCPT   PIC X(10).
         05  FD-FICCPT-DATE-OUV PIC 9(08).
         05  FD-FICCPT-SOLDE    PIC S9(09)V99 COMP-3.
         05  FD-FICCPT-DATE-MAJ PIC 9(08).
         05  FD-FICCPT-NOM      PIC X(20).
         05  FILLER             PIC X(8).

      *===============================================================*
      *              DESCRIPTION DES VARIABLES DE TRAVAIL             *
      *===============================================================*
       WORKING-STORAGE SECTION.

      *      ---------------------------------------------------------*
      *-----< VARIABLES DE TABLEAUX                                   *
      *      ---------------------------------------------------------*

       01     WS-TICF-CPTES.
         05   WS-TICF-POSTE OCCURS 6 TIMES.
           10 WS-TICF-NUMCPT   PIC X(10).
           10 WS-TICF-DATE-OUV PIC 9(08).
           10 WS-TICF-SOLDE    PIC S9(09)V99 COMP-3.
           10 WS-TICF-DATE-MAJ PIC 9(08).
           10 WS-TICF-NOM      PIC X(20).

       01     WS-TIXF-CPTES.
         05   WS-TIXF-POSTE OCCURS 6 TIMES INDEXED BY IX-TIXF.
           10 WS-TIXF-NUMCPT   PIC X(10).
           10 WS-TIXF-DATE-OUV PIC 9(08).
           10 WS-TIXF-SOLDE    PIC S9(09)V99 COMP-3.
           10 WS-TIXF-DATE-MAJ PIC 9(08).
           10 WS-TIXF-NOM      PIC X(20).

       01   INDICES.
         05 IC-TICF       PIC S9(4) COMP.
         05 IC-TICF-MAX   PIC S9(4) COMP VALUE +5.
         05 IC-TICF-CHARG PIC S9(4) COMP.

       01   WS-INDICES.
         05 IX-TIXF-AUX   PIC S9(4) COMP.
         05 IX-TIXF-MAX   PIC S9(4) COMP VALUE +5.
         05 IX-TIXF-CHARG PIC S9(4) COMP.

       01   WS-SOLDES.
         05 WS-SOLDE-MIN  PIC S9(09)V99 COMP-3.
         05 WS-SOLDE-MAX  PIC S9(09)V99 COMP-3.

       01   WS-SYSIN.
         05 WS-SYSIN-TYPE-DEM   PIC X(02).
         05 WS-SYSIN-NUMCPT     PIC X(10).

       PROCEDURE DIVISION.
      *===============================================================*
      *                     A L G O R I T H M E                       *
      *===============================================================*

       100-PROGRAMME.
           PERFORM 600-OUVRIR-FICCPT
           PERFORM 601-LIRE-FICCPT
           PERFORM 799-RECUP-SYSIN

      *    PERFORM 701-CHARGE-TICF-DEMANDE-01
      *    PERFORM 702-AFFICHAGE-TICF-DEMANDE-02
      *    PERFORM 703-SOLDES-TICF-DEMANDE-03
           PERFORM 705-RECHERCHE-TICF-DEMANDE-05
      *    PERFORM 706-CHARGE-TIXF-DEMANDE-06
      *    PERFORM 707-AFFICHAGE-TIXF-DEMANDE-07
      *    PERFORM 708-SOLDES-TIXF-DEMANDE-08

           PERFORM 602-FERMER-FICCPT
           PERFORM 999-ARRET-PROGRAMME
           .

      *      ---------------------------------------------------------*
      *-----<      600 A 699     GESTIONS DES ENTREES/SORTIES         *
      *      ---------------------------------------------------------*

       600-OUVRIR-FICCPT.
           OPEN INPUT FICCPT
           .

       601-LIRE-FICCPT.
           READ FICCPT
           .

       602-FERMER-FICCPT.
           CLOSE FICCPT
           .

      *      ---------------------------------------------------------*
      *-----<      700 A 799     REGLES DE GESTIONS COMPLEXES         *
      *      ---------------------------------------------------------*

       701-CHARGE-TICF-DEMANDE-01.
           PERFORM VARYING IC-TICF
                 FROM 1 BY 1
                 UNTIL IC-TICF > 6
              MOVE FD-FICCPT-NUMCPT TO WS-TICF-NUMCPT(IC-TICF)
              MOVE FD-FICCPT-DATE-OUV TO WS-TICF-DATE-OUV(IC-TICF)
              MOVE FD-FICCPT-SOLDE TO WS-TICF-SOLDE(IC-TICF)
              MOVE FD-FICCPT-DATE-MAJ TO WS-TICF-DATE-MAJ(IC-TICF)
              MOVE FD-FICCPT-NOM TO WS-TICF-NOM(IC-TICF)
              PERFORM 601-LIRE-FICCPT
           END-PERFORM
           SUBTRACT 1 FROM IC-TICF GIVING IC-TICF-CHARG
           .

       702-AFFICHAGE-TICF-DEMANDE-02.
           PERFORM VARYING IC-TICF
                 FROM 1 BY 1
                 UNTIL IC-TICF > 6
              DISPLAY 'NUMERO DE COMPTE : ' WS-TICF-NUMCPT(IC-TICF)
              DISPLAY 'DATE D OUVERTURE : ' WS-TICF-DATE-OUV(IC-TICF)
              DISPLAY 'SOLDE DU COMPTE  : ' WS-TICF-SOLDE(IC-TICF)
              DISPLAY 'DATE DE MAJ      : ' WS-TICF-DATE-MAJ(IC-TICF)
              DISPLAY 'NOM DU CLIENT    : ' WS-TICF-NOM(IC-TICF)
           END-PERFORM
           .

       703-SOLDES-TICF-DEMANDE-03.
           MOVE FD-FICCPT-SOLDE TO WS-SOLDE-MIN
           MOVE FD-FICCPT-SOLDE TO WS-SOLDE-MAX
           PERFORM VARYING IC-TICF
                 FROM 1 BY 1
                 UNTIL IC-TICF > 6
              IF FD-FICCPT-SOLDE > WS-SOLDE-MAX
                      THEN MOVE FD-FICCPT-SOLDE TO WS-SOLDE-MAX
              ELSE IF FD-FICCPT-SOLDE < WS-SOLDE-MIN
                      THEN MOVE FD-FICCPT-SOLDE TO WS-SOLDE-MIN
                   END-IF
              END-IF
              PERFORM 601-LIRE-FICCPT
           END-PERFORM
           DISPLAY 'SOLDE MIN : ' WS-SOLDE-MIN
           DISPLAY 'SOLDE MAX : ' WS-SOLDE-MAX
           .

       705-RECHERCHE-TICF-DEMANDE-05.
           PERFORM VARYING IC-TICF
                 FROM 1 BY 1
                 UNTIL WS-TICF-NUMCPT(IC-TICF) = WS-SYSIN-NUMCPT
                 OR IC-TICF > 6
           END-PERFORM
           MOVE IC-TICF TO IC-TICF-CHARG
           DISPLAY 'NUMERO DE COMPTE : ' WS-TICF-NUMCPT(IC-TICF-CHARG)
           DISPLAY 'DATE D OUVERTURE : ' WS-TICF-DATE-OUV(IC-TICF-CHARG)
           DISPLAY 'SOLDE DU COMPTE  : ' WS-TICF-SOLDE(IC-TICF-CHARG)
           DISPLAY 'DATE DE MAJ      : ' WS-TICF-DATE-MAJ(IC-TICF-CHARG)
           DISPLAY 'NOM DU CLIENT    : ' WS-TICF-NOM(IC-TICF-CHARG)
           .

       706-CHARGE-TIXF-DEMANDE-06.
           SET IX-TIXF-AUX TO IX-TIXF
           PERFORM VARYING IX-TIXF
                 FROM 1 BY 1
                 UNTIL IX-TIXF > 6
              MOVE FD-FICCPT-NUMCPT TO WS-TIXF-NUMCPT(IX-TIXF)
              MOVE FD-FICCPT-DATE-OUV TO WS-TIXF-DATE-OUV(IX-TIXF)
              MOVE FD-FICCPT-SOLDE TO WS-TIXF-SOLDE(IX-TIXF)
              MOVE FD-FICCPT-DATE-MAJ TO WS-TIXF-DATE-MAJ(IX-TIXF)
              MOVE FD-FICCPT-NOM TO WS-TIXF-NOM(IX-TIXF)
              PERFORM 601-LIRE-FICCPT
           END-PERFORM
           SET IX-TIXF DOWN BY +1
           SET IX-TIXF-CHARG TO IX-TIXF
           .

       707-AFFICHAGE-TIXF-DEMANDE-07.
           PERFORM VARYING IX-TIXF
                 FROM 1 BY 1
                 UNTIL IX-TIXF > 6
              DISPLAY 'NUMERO DE COMPTE : ' WS-TIXF-NUMCPT(IX-TIXF)
              DISPLAY 'DATE D OUVERTURE : ' WS-TIXF-DATE-OUV(IX-TIXF)
              DISPLAY 'SOLDE DU COMPTE  : ' WS-TIXF-SOLDE(IX-TIXF)
              DISPLAY 'DATE DE MAJ      : ' WS-TIXF-DATE-MAJ(IX-TIXF)
              DISPLAY 'NOM DU CLIENT    : ' WS-TIXF-NOM(IX-TIXF)
           END-PERFORM
           .

       799-RECUP-SYSIN.
           ACCEPT WS-SYSIN FROM SYSIN
           .

       708-SOLDES-TIXF-DEMANDE-08.
           MOVE FD-FICCPT-SOLDE TO WS-SOLDE-MIN
           MOVE FD-FICCPT-SOLDE TO WS-SOLDE-MAX
           PERFORM VARYING IX-TIXF
                 FROM 1 BY 1
                 UNTIL IX-TIXF > 6
              IF FD-FICCPT-SOLDE > WS-SOLDE-MAX
                      THEN MOVE FD-FICCPT-SOLDE TO WS-SOLDE-MAX
              ELSE IF FD-FICCPT-SOLDE < WS-SOLDE-MIN
                      THEN MOVE FD-FICCPT-SOLDE TO WS-SOLDE-MIN
                   END-IF
              END-IF
              PERFORM 601-LIRE-FICCPT
           END-PERFORM
           DISPLAY 'SOLDE MIN : ' WS-SOLDE-MIN
           DISPLAY 'SOLDE MAX : ' WS-SOLDE-MAX
           .

      *709-RECHERCHE-TIXF-DEMANDE-09.
      *    .
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
