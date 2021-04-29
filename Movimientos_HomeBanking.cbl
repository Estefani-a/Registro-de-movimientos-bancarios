       IDENTIFICATION DIVISION.
       PROGRAM-ID.SassoneEstefaniaEj1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT OPTIONAL MOVIMIENTOS
       ASSIGN TO "C:\Users\Usuario\OneDrive\Escritorio\movimientos.dat"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MOVIMIENTOS.
       01 MOVIMIENTOS-REGISTRO.
               05 HOJA_NRO PIC 9(1).
               05 COD_CLI PIC X(10).
               05 TIPOCTA PIC X(10).
               05 SUCURSAL PIC X(8).
               05 CUENTA PIC X(15).
               05 DIGITO PIC X(5).
               05 FECHA_MOV PIC X(13).
               05 COD_OPER PIC X(9).
               05 IMPORTE PIC X(15).
               05 CANAL  PIC X(2).


       WORKING-STORAGE SECTION.

       01 LINEA1.
               05 TEXTOFECHA_REV PIC X(30) VALUE 'Fecha:'.
               05 MUESTRA_FECHA_REV PIC X(10).
               05 ESPACIO PIC X(40) VALUE " ".
               05 TEXTONRO_HOJA PIC X(30)  VALUE 'Hoja nro '.
               05 MUESTRA-NRO_HOJA PIC 9(1).


       01 LINEA2.
               05 SUBLINEA3-1 PIC X(20) VALUE " ".
               05 SUBLINEA3-2 PIC X(11) VALUE "LISTADO DE".
               05 SUBLINEA3-3 PIC X(16) VALUE "MOVIMIENTOS POR".
               05 SUBLINEA3-4 PIC X(15) VALUE "HOMEBANCKING".

       01 LINEA3.
               05 NDECUENTA PIC x(18) VALUE "Numero de cuenta".
               05 TEXTOFECHAMOV PIC X(12) VALUE 'Fecha'.
               05 TEXTOCODIGO PIC X(24) VALUE 'Codigo de operacion'.
               05 TEXTOIMPORTE PIC X(20) VALUE 'Importe'.

       01 PRESENTACION.
                 05 MUESTRA_SUCURSAL PIC X(5).
                 05 MUESTRA_CUENTA PIC X(9).
                 05 MUESTRA_DIGITO PIC X(4).
                 05 MUESTRA_FECHA_MOV PIC X(18).
                 05 MUESTRA_COD_OPER PIC X(18).
                 05 MUESTRA_IMPORTE PIC X(15).

       01  FIN_ARCHIVO PIC X.
       01  MAXIMO-REGISTROS PIC 99.
       01  GUARDA-ENTER PIC X .

      ******************************************************************
           PROCEDURE DIVISION.

           EMPIEZA-PROGRAMA.

           PERFORM APERTURA_ARCHIVO.
           MOVE ZEROES TO MAXIMO-REGISTROS.
           MOVE "1" TO FIN_ARCHIVO.
           PERFORM LEER-SIG-REG.

           DISPLAY LINEA1.
           DISPLAY LINEA2.
           DISPLAY "CODIGO DE CLIENTE:" COD_CLI.
           DISPLAY "TIPO DE CUENTA:" TIPOCTA.
           DISPLAY " "
           DISPLAY LINEA3.

           PERFORM MOSTRAR-REGISTROS

           UNTIL FIN_ARCHIVO = "0".
           PERFORM CERRAR-ARCHIVO.
           STOP RUN.
      ****************************************************************
      * APERTURA DE ARCHIVOS
      ****************************************************************

           APERTURA_ARCHIVO.
              OPEN INPUT MOVIMIENTOS.
      *****************************************************************
      * CERRAR ARCHIVOS
      *****************************************************************
           CERRAR-ARCHIVO.
             CLOSE MOVIMIENTOS.

      *****************************************************************
      * MOSTRAR-ENCABEZADO
      *****************************************************************
           MOSTRAR_ENCABEZADO.
              DISPLAY LINEA1.
              DISPLAY LINEA2.
              DISPLAY "CODIGO DE CLIENTE:" COD_CLI.
              DISPLAY "TIPO DE CUENTA:" TIPOCTA.
              DISPLAY " "
              DISPLAY LINEA3.

      ****************************************************************
      * MOSTRAR-REGISTROS
      ****************************************************************
           MOSTRAR-REGISTROS.
               IF MAXIMO-REGISTROS = 7
                   PERFORM MOSTRAR_ENCABEZADO
                   MOVE 0 TO MAXIMO-REGISTROS.

               PERFORM MOSTRAR-CAMPOS.
               PERFORM LEER-SIG-REG.

      ****************************************************************
      * MOSTRAR-CAMPOS
      ****************************************************************
           MOSTRAR-CAMPOS.

                IF MAXIMO-REGISTROS = 7
                  PERFORM PULSAR_ENTER.


           MOVE SUCURSAL TO MUESTRA_SUCURSAL.
           MOVE CUENTA TO MUESTRA_CUENTA.
           MOVE DIGITO TO MUESTRA_DIGITO.
           MOVE FECHA_MOV TO MUESTRA_FECHA_MOV.
           MOVE COD_OPER TO MUESTRA_COD_OPER.
           MOVE IMPORTE TO MUESTRA_IMPORTE.

           DISPLAY PRESENTACION.
           ADD 1 TO MAXIMO-REGISTROS.

      *****************************************************************
      * LEE SIGUENTE REGISTRO
      *****************************************************************

           LEER-SIG-REG.
           READ MOVIMIENTOS NEXT RECORD
           AT END
           MOVE "0" TO FIN_ARCHIVO.

      *****************************************************************
      * PULSAR ENTER
      *****************************************************************
           PULSAR_ENTER.

           DISPLAY
           "Presione la tecla ENTER para ver la siguiente pagina.".
           ACCEPT GUARDA-ENTER.
           MOVE ZEROES TO MAXIMO-REGISTROS.
