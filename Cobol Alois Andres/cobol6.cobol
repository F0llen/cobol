       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTP6AA.
      *===============================================================*
      * NOM DU PROGRAMME : COBTP6AA                                   *
      *---------------------------------------------------------------*
      *               O B J E T  D U  P R O G R A M M E               *
      *---------------------------------------------------------------*
      *                     TP SOUS-PROGRAMME                         *
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
       DATA DIVISION.
      *===============================================================*
      *                    DESCRIPTION DES FICHIERS                   *
      *===============================================================*
       FILE SECTION.

      *===============================================================*
      *              DESCRIPTION DES VARIABLES DE TRAVAIL             *
      *===============================================================*
       WORKING-STORAGE SECTION.

       01 NOM-VARIABLE PIC X(8).
      *      ---------------------------------------------------------*
      *-----< INCLUSION CONTENU SOUS-PROG                             *
      *      ---------------------------------------------------------*
       COPY CMODULE.

       PROCEDURE DIVISION.
      *===============================================================*
      *                     A L G O R I T H M E                       *
      *===============================================================*

       100-FONCTION.
           PERFORM 300-INIT-VAR
           PERFORM 200-APPEL-STATIQUE
           PERFORM 201-APPEL-DYNAMIQUE
           PERFORM 800-SYSOUT
           PERFORM 999-ARRET-PROGRAMME
           .

       200-APPEL-STATIQUE.
           CALL 'SPTP6AA' USING MODULE-COMMAREA
           .

       201-APPEL-DYNAMIQUE.
           MOVE 'SPTP6AA' TO NOM-VARIABLE
           CALL NOM-VARIABLE USING MODULE-COMMAREA
           .

       300-INIT-VAR.
           MOVE 6 TO VARIABLE1
           MOVE 9 TO VARIABLE2
           .

       800-SYSOUT.
           DISPLAY VARIABLE3
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
