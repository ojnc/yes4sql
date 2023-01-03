REPORT yjncsqlconsole MESSAGE-ID zmsgs.

* SQLCONSOLE for SAPGUI
*

DATA:
*     reference to wrapper class of control based on OO Framework
  g_editor           TYPE REF TO cl_gui_textedit,
*     reference to custom container: necessary to bind TextEdit Control
  g_editor_container TYPE REF TO cl_gui_custom_container,
*     other variables
  g_ok_code          LIKE sy-ucomm,         " return code from screen
  g_repid            LIKE sy-repid.

DATA: rows(8) TYPE c.

DATA: code     TYPE TABLE OF rssource-line.

CONSTANTS: c_line_length TYPE i VALUE 255.

* define table type for data exchange
TYPES: BEGIN OF mytable_line,
         line(c_line_length) TYPE c,
       END OF mytable_line.

* table to exchange text
DATA gt_mytable TYPE TABLE OF mytable_line.
DATA gt_sql     TYPE TABLE OF mytable_line.

DATA: myline LIKE LINE OF gt_mytable.

DATA: tblr_table        TYPE REF TO cl_salv_table.

DATA: first         TYPE i,
      numcols       TYPE i,
      aggfun        TYPE i,
      numrows       TYPE i,
      numhdrs       TYPE i,
      rownum        TYPE i,
      mystring      TYPE string,
      mystring2     TYPE string,
      nrows(8)      TYPE n,
      myoff         TYPE i,
      mylen         TYPE i,
      repheader(50) TYPE c,
      w_result(1)   TYPE c,
      w_rc          TYPE i,
      wtxt1(80),
      prog(8)       TYPE c.

TYPES: BEGIN OF t_token,
         token(40) TYPE c,
       END   OF t_token.

DATA: gt_token TYPE STANDARD TABLE OF  t_token,
      wa_token TYPE t_token,
      w_hash   TYPE i.

FIELD-SYMBOLS: <fs_token> TYPE t_token.


*    CL_ABAP_CHAR_UTILITIES=>NEWLINE.
CONSTANTS: con_tab TYPE c VALUE cl_abap_char_utilities=>horizontal_tab .
*    HORIZONTAL_TAB
*    VERTICAL_TAB
*    NEWLINE
*    CR_LF
*    FORM_FEED
*    BACKSPACE
*    SPACE_STR

* necessary to flush the automation queue
CLASS cl_gui_cfw DEFINITION LOAD.

PARAMETERS: p_pass  TYPE char8   OBLIGATORY LOWER CASE.

*&---------------------------------------------------------------------*
*                       INITIALIZATION EVENT                           *
*&---------------------------------------------------------------------*
INITIALIZATION.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK screen-name EQ 'P_PASS'.
    MOVE: 1 TO screen-invisible.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  MOVE `Possible Wrong SQL - see ` TO wtxt1.

  PERFORM f_encrypt.

  IF w_hash <> 141824.  "ojnc
    MESSAGE e999(zmsgs) WITH 'Password Mismatch'.
  ENDIF.

  MOVE '1000' TO rows.

  CALL SCREEN 100.

************************************************************************
*   P B O
************************************************************************
MODULE pbo OUTPUT.

  SET PF-STATUS `MAIN100`.
  SET TITLEBAR  `TITLEYES4SQL`.

  IF g_editor IS INITIAL.

*   initilize local variable with sy-repid, since sy-repid doesn`t work
*    as parameter directly.
    g_repid = sy-repid.

*   create control container
    CREATE OBJECT g_editor_container
      EXPORTING
        container_name              = `MYEDIT`
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      add your handling
    ENDIF.


*   create calls constructor, which initializes, creats and links
*    a TextEdit Control
    CREATE OBJECT g_editor
      EXPORTING
        parent                     = g_editor_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.
    IF sy-subrc NE 0.
      CALL FUNCTION `POPUP_TO_INFORM`
        EXPORTING
          titel = g_repid
          txt2  = `Create Object Failed`
          txt1  = `to make TextEditor Control`.
      LEAVE PROGRAM.
    ENDIF.

  ENDIF.                               " Editor is initial

* remember: there is an automatic flush at the end of PBO!

ENDMODULE.                             " PBO


************************************************************************
*   P A I
************************************************************************
MODULE pai INPUT.

  CASE g_ok_code.

    WHEN `EXIT`.
      PERFORM exit_program.

    WHEN `EXEC`.
*     retrieve table from control
      CLEAR gt_mytable.

      CALL METHOD g_editor->get_text_as_r3table
        IMPORTING
          table  = gt_mytable
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc NE 0.
        CALL FUNCTION `POPUP_TO_INFORM`
          EXPORTING
            titel = g_repid
            txt2  = `Get_Text_As_R3Table Failed`
            txt1  = `Unable to Store SQL`.
        LEAVE PROGRAM.
      ENDIF.

*     if you would like to work with the table contents
*     perform a explicit flush here allthough the method
*     flushes internally (at least up to release 4.6D).
*     The reason: don`t rely on internal flushes of control
*     wrappers. These might vanish in the future leading to a
*     malfunction of your transaction. The additional flush here
*     does no harm. The autmation queue is empty and NO additional
*     roundtrip to the frontend will be triggered.

      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          OTHERS = 1.

      IF sy-subrc NE 0.
        CALL FUNCTION `POPUP_TO_INFORM`
          EXPORTING
            titel = g_repid
            txt2  = `cl_gui_cfw=>flush Failed`
            txt1  = `Exiting Program`.
        LEAVE PROGRAM.
      ENDIF.

      PERFORM f_runsql.

  ENDCASE.

  CLEAR g_ok_code.
ENDMODULE.                             " PAI


*&---------------------------------------------------------------------*
*&      Form  EXIT_PROGRAM
*&---------------------------------------------------------------------*
FORM exit_program.
* Destroy Control.
  IF NOT g_editor IS INITIAL.
    CALL METHOD g_editor->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc NE 0.
      CALL FUNCTION `POPUP_TO_INFORM`
        EXPORTING
          titel = g_repid
          txt2  = `g_editor->free Failed`
          txt1  = `Exiting Program`.
      LEAVE PROGRAM.
    ENDIF.
*   free ABAP object also
    FREE g_editor.
  ENDIF.

* destroy container
  IF NOT g_editor_container IS INITIAL.
    CALL METHOD g_editor_container->free
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
*         MESSAGE E002 WITH F_RETURN.
    ENDIF.
*   free ABAP object also
    FREE g_editor_container.
  ENDIF.


* finally flush
  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc NE 0.
    CALL FUNCTION `POPUP_TO_INFORM`
      EXPORTING
        titel = g_repid
        txt2  = `cl_gui_cfw=>flush Failed`
        txt1  = `Exiting Program`.
    LEAVE PROGRAM.
  ENDIF.

  LEAVE PROGRAM.

ENDFORM.                               " EXIT_PROGRAM

*&---------------------------------------------------------------------*
*&      Form  F_RUNSQL
*&---------------------------------------------------------------------*
FORM f_runsql.

  IF rows IS INITIAL.
    MOVE 99999999 TO rows.
  ENDIF.

  CLEAR: code.

  MOVE 0 TO : first.

  REFRESH gt_sql.

  LOOP AT gt_mytable INTO myline.
    IF strlen( myline ) = 0.
      CONTINUE.
    ENDIF.

    REPLACE ALL OCCURRENCES OF con_tab  IN myline WITH space.

    CONCATENATE ` ` myline ` ` INTO myline.

    IF first = 0.
        FIND ` Select ` IN myline IGNORING CASE.
        IF sy-subrc <> 0.
          FIND ` With ` IN myline IGNORING CASE.
          IF sy-subrc <> 0.
            CALL FUNCTION `POPUP_TO_INFORM`
              EXPORTING
                titel = g_repid
                txt2  = `SELECT DMLs Only Please!`
                txt1  = `Correct & Retry`.
            RETURN.
          ENDIF.
        ENDIF.

      SPLIT myline AT space INTO TABLE gt_token.

      LOOP AT gt_token ASSIGNING <fs_token>.

        IF strlen( <fs_token>-token ) = 0.
          CONTINUE.
        ENDIF.

        TRANSLATE <fs_token>-token TO UPPER CASE.

        IF NOT ( <fs_token>-token  = 'SELECT' OR  <fs_token>-token  = 'WITH' ).
          CALL FUNCTION `POPUP_TO_INFORM`
            EXPORTING
              titel = g_repid
              txt2  = `SELECT DMLs Only Please!`
              txt1  = `Correct & Retry`.
          RETURN.
        ENDIF.

        EXIT.

      ENDLOOP.

      MOVE 1 TO first.
    ENDIF.


    FIND ` SUM(` IN myline IGNORING CASE.
    IF sy-subrc = 0.
      MOVE 1 TO aggfun.
    ENDIF.
    FIND ` MAX(` IN myline IGNORING CASE.
    IF sy-subrc = 0.
      MOVE 1 TO aggfun.
    ENDIF.
    FIND ` MIN(` IN myline IGNORING CASE.
    IF sy-subrc = 0.
      MOVE 1 TO aggfun.
    ENDIF.
    FIND ` AVG(` IN myline IGNORING CASE.
    IF sy-subrc = 0.
      MOVE 1 TO aggfun.
    ENDIF.
    FIND ` COUNT(` IN myline IGNORING CASE.
    IF sy-subrc = 0.
      MOVE 1 TO aggfun.
    ENDIF.

    APPEND myline TO gt_sql.

  ENDLOOP.

  IF lines( gt_sql ) = 0.
    CALL FUNCTION `POPUP_TO_INFORM`
      EXPORTING
        titel = g_repid
        txt2  = `No SQL found`
        txt1  = `Correct & Retry`.
    RETURN.
  ENDIF.

  APPEND `PROGRAM jncsProgram.` TO code.
  APPEND `` TO code.

  APPEND `DATA: tblr_table      TYPE REF TO cl_salv_table, ` TO code.
  APPEND `      tblr_disp       TYPE REF TO cl_salv_display_settings, ` TO code.
  APPEND `      tblr_columns    TYPE REF TO cl_salv_columns_table, ` TO code.
  APPEND `      tblr_column     TYPE REF TO cl_salv_column_table, ` TO code.
  APPEND `      tblr_events     TYPE REF TO cl_salv_events_table, ` TO code.
  APPEND `      tblr_functions  TYPE REF TO cl_salv_functions_list, ` TO code.
  APPEND `      tblr_filters    TYPE REF TO cl_salv_filters, ` TO code.
  APPEND `      tbls_color      TYPE lvc_s_colo. ` TO code.

  APPEND `CONSTANTS: gc_true  TYPE sap_bool VALUE 'X', ` TO code.
  APPEND `           gc_false TYPE sap_bool VALUE space. ` TO code.

  APPEND `DATA: exsm TYPE string.`          TO code.
  APPEND `DATA: exlm TYPE string.`          TO code.
  APPEND `DATA: myex TYPE REF TO CX_ROOT.`  TO code.

  APPEND `` TO code.
  APPEND `" PERFORM DoSQL.` TO code.
  APPEND `` TO code.

  APPEND `*&--------------------------------------------------------------------*` TO code.
  APPEND `*&      Form  DoSQL` TO code.
  APPEND `*&--------------------------------------------------------------------*` TO code.
  APPEND `FORM DoSQL.` TO code.
  APPEND `` TO code.

  IF rows IS INITIAL.
    MOVE '99999999' TO rows.
  ELSE.
    MOVE rows TO nrows.
  ENDIF.

  IF nrows IS INITIAL.
    MOVE 99999999 TO nrows.
  ENDIF.

  APPEND `` TO code.
  APPEND `  TRY.` TO code.
  APPEND `` TO code.

  LOOP AT gt_sql INTO myline.
    APPEND myline TO code.
  ENDLOOP.
  APPEND `into TABLE @DATA(lt_tab)` TO code.
  IF aggfun = 0.
    CONCATENATE `  UP TO ` nrows `rows` INTO mystring SEPARATED BY space.
    APPEND mystring TO code.
  ENDIF.
  APPEND `.` TO code.

  APPEND `  CATCH CX_ROOT INTO myex.` TO code.
  APPEND `    exsm =  myex->if_message~get_text( ).` TO code.
  APPEND `    exlm =  myex->if_message~get_longtext( ).` TO code.

  APPEND `    CALL FUNCTION ``POPUP_TO_INFORM``` TO code.
  APPEND `      EXPORTING` TO code.
  APPEND `          titel = ``jncDynamicSub``` TO code.
  APPEND `          txt1  = ``Generate SUBROUTINE POOL Succeeded BUT SQL failed``` TO code.
  APPEND `          txt2  = 'Possible Wrong SQL '` TO code.
  APPEND `          txt3  = exsm` TO code.
  APPEND `          txt4  = exlm.` TO code.
  APPEND `  ENDTRY.` TO code.

  APPEND `` TO code.

  APPEND `TRY. ` TO code.
  APPEND `    CALL METHOD cl_salv_table=>factory ` TO code.
  APPEND `      IMPORTING ` TO code.
  APPEND `        r_salv_table = tblr_table ` TO code.
  APPEND `      CHANGING ` TO code.
  APPEND `        t_table      = lt_tab. ` TO code.
  APPEND `  CATCH cx_salv_msg. ` TO code.
  APPEND `ENDTRY. ` TO code.

  APPEND `*... activate ALV generic Functions ` TO code.
  APPEND `tblr_functions = tblr_table->get_functions( ). ` TO code.
  APPEND `tblr_functions->set_all( gc_true ). ` TO code.
  APPEND ` ` TO code.
  APPEND `tblr_functions = tblr_table->get_functions( ). ` TO code.
  APPEND `tblr_functions->set_all( gc_true ). ` TO code.
*   APPEND `tblr_functions->set_default( gc_true ). ` TO code.
  APPEND ` ` TO code.
  APPEND `tblr_disp = tblr_table->get_display_settings( ). ` TO code.
  APPEND ` ` TO code.
  APPEND `tblr_disp->set_striped_pattern( 'X' ). ` TO code.
  APPEND `tblr_disp->set_list_header( 'SAPGUI SQL Console' ). ` TO code.
  APPEND ` ` TO code.
  APPEND `tblr_columns = tblr_table->get_columns( ). ` TO code.
  APPEND `tblr_columns->set_optimize( gc_true ). ` TO code.
  APPEND `tblr_columns->set_key_fixation( if_salv_c_bool_sap=>true ). ` TO code.
  APPEND ` ` TO code.
  APPEND ` ` TO code.
  APPEND `try. ` TO code.
  APPEND `    tblr_column ?= tblr_columns->get_column( 'MANDT' ). ` TO code.
  APPEND `    tblr_column->set_technical( if_salv_c_bool_sap=>true ). ` TO code.
  APPEND `  catch cx_salv_not_found.   ` TO code.
  APPEND `endtry.   ` TO code.


  APPEND `tblr_table->display( ). ` TO code.

  APPEND `` TO code.
  APPEND `ENDFORM.` TO code.

  IF first = 0.
    CALL FUNCTION `POPUP_TO_INFORM`
      EXPORTING
        titel = g_repid
        txt2  = `Open SQL Without a FROM`
        txt1  = `Correct & retry`.
    RETURN.
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  GENERATE SUBROUTINE POOL code NAME DATA(prog)
           MESSAGE                   DATA(mess)
           INCLUDE                   DATA(incl)
           LINE                      DATA(line)
           WORD                      DATA(wrd)
           OFFSET                    DATA(off)
           MESSAGE-ID                DATA(mid)
           SHORTDUMP-ID              DATA(sid).

  IF sy-subrc <> 0.
    CALL FUNCTION `POPUP_TO_INFORM`
      EXPORTING
        titel = g_repid
        txt2  = `Generate SUBROUTINE POOL Failed`
        txt1  = wtxt1.
    cl_demo_output=>display(
      |MESSAGE:      { mess }\n| &&
      |INCLUDE:      { incl }\n| &&
      |LINE:         { line }\n| &&
      |WORD:         { wrd  }\n| &&
      |OFFSET:       { off  }\n| &&
      |MESSAGE-ID:   { CONV string( mid ) }\n| &&
      |SHORTDUMP-ID: { sid }| ).
    PERFORM f_dumpcode.
  ELSE.
    PERFORM dosql IN PROGRAM (prog).
    IF sy-subrc <> 0.
      CALL FUNCTION `POPUP_TO_INFORM`
        EXPORTING
          titel = g_repid
          txt2  = `Generate SUBROUTINE POOL Succeeded BUT Call failed`
          txt1  = wtxt1.
      PERFORM f_dumpcode.
      cl_demo_output=>display(
        |MESSAGE:      { mess }\n| &&
        |INCLUDE:      { incl }\n| &&
        |LINE:         { line }\n| &&
        |WORD:         { wrd  }\n| &&
        |OFFSET:       { off  }\n| &&
        |MESSAGE-ID:   { CONV string( mid ) }\n| &&
        |SHORTDUMP-ID: { sid }| ).
    ENDIF.
  ENDIF.

ENDFORM.        "F_RUNSQL

*&--------------------------------------------------------------------*
*&      Form  f_dumpcode
*&--------------------------------------------------------------------*
FORM f_dumpcode.

  CALL METHOD g_editor->set_text_as_r3table
    EXPORTING
      table  = code
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc NE 0.
    CALL FUNCTION `POPUP_TO_INFORM`
      EXPORTING
        titel = g_repid
        txt2  = `Set_Text_As_R3Table Failed`
        txt1  = `Unable to show CODE`.
  ENDIF.
ENDFORM.

*&--------------------------------------------------------------------*
*&      Form  f_encrypt
*&--------------------------------------------------------------------*
FORM f_encrypt.
  DATA: w_char TYPE c,
        offset TYPE i,
        w_int  TYPE i.

  FIELD-SYMBOLS: <fs_byte> TYPE x. "hana 2 bytes LITTLE ENDIAN

  CLEAR w_hash.

  DO 8 TIMES.
    COMPUTE offset = sy-index - 1.
    MOVE p_pass+offset(1) TO w_char.
    ASSIGN w_char TO <fs_byte> CASTING.
    MOVE <fs_byte> TO w_int.
    ADD  w_int TO w_hash.
  ENDDO.
ENDFORM.                    "f_encrypt
