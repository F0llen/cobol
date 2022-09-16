       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPTP6AA.
      *===============================================================*
      * NOM DU PROGRAMME : SPTP6AA.                                   *
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
       DATA DIVISION.
       LINKAGE SECTION.

       COPY CMODULE.

       PROCEDURE DIVISION
           USING MODULE-COMMAREA.
      *===============================================================*
      *                     A L G O R I T H M E                       *
      *===============================================================*

       100-APPEL-STATIQUE.
           COMPUTE VARIABLE3 = VARIABLE2 - VARIABLE1
           GOBACK
           .

      *102-APPEL-ENTRER-STATIQUE.
      *    ENTRY 'ENTRE1' USING MODULE-COMMAREA
      *    COMPUTE VARIABLE3 = VARIABLE2 + VARIABLE1
      *    GOBACK
      *    .

      *===============================================================*
      *                 F I N  D U  P R O G R A M M E                 *
      *===============================================================*
