REPORT yes4sql MESSAGE-ID zmsgs.

* SQL tool for SAP ABAP Programmers - BOTH OPEN & NATIVE SQLs
*
* Objective - to see JOINS in SAP to confirm or discover relationships
*             and to see Data side by Side

* Read "SAP Table and Field search strategies"
*   https://blogs.sap.com/2019/06/05/methods-to-find-the-tables-in-sap/
*   https://eursap.eu/2021/05/21/sap-tip-finding-a-table-name-from-a-field-name/
*  and many other excellent resources to navigate the cryptic tables & Colums of SAP
*
* SE16 is the best for 1 table inspection & you can open several sessions if you have more than 1 table.
* You may "hate joins" and prefer looping matches on Internal Tables.
* However if you wish to see the relationships in DATA VISIBLE format NOTHING succeeds like JOINs

* SQL Must be SELECT or WITH
* List of Columns Selected given in a BEGIN END nest
*  1st Field 2nd type(optional) "comments
*  if type is omitted then type is taken to be same as field
*  there should not be any line break by word wrap in text copntrol

* SUM MIN MAX AVG COUNT COUNT( *) supported
* SUM( table~COLUMN )
* OPEN SQL & NATIVE SQL syntax must be correct

* If you use NATIVE SQL make sure you have :SY-MANDT filter in WHERE Clause

* ABAP program in memory shown for diagnostics and possible reuse

* JOINs and SUBQUERIES are NOT ALLOWED for
*           Pooled Tables, Clustered Tables & Projection Views
*   Even AGGREGATE Functions are NOT ALLOWED!   -- thse restrictions are inherent in SAP

* So this tool is more useful for TRANSPARENT TABLES only!
* For Pool & Cluster use SE16

*  Author Jayanta Narayan Choudhuri
*         Flat 302
*         395 Jodhpur Park
*         Kolkata 700 068
*       Email ssscal@gmail.com

*  TextEdit Control Tool Code Copied from SAP Standard Example saptextedit_demo_3
*  This is FREE software with FULL responsibility on USER & anyone changing sourcecode!

DATA:
*     reference to wrapper class of control based on OO Framework
  g_editor           TYPE REF TO cl_gui_textedit,
*     reference to custom container: necessary to bind TextEdit Control
  g_editor_container TYPE REF TO cl_gui_custom_container,
*     other variables
  g_ok_code          LIKE sy-ucomm,         " return code from screen
  g_repid            LIKE sy-repid.

DATA: rows(8) TYPE c,
      isopen  TYPE c,     "Radio Button
      isnatv  TYPE c,
      delim   TYPE c.

DATA:  code     TYPE TABLE OF rssource-line.
*      prog(8)  TYPE c,
*      msg(120) TYPE c,
*      lin(3)   TYPE c,
*      wrd(10)  TYPE c,
*      off(3)   TYPE c.

DATA: onelinecode LIKE LINE OF code.

TYPE-POOLS : slis.

DATA : fcat TYPE slis_t_fieldcat_alv.
DATA : wcat LIKE LINE OF fcat.

CONSTANTS: c_line_length TYPE i VALUE 255.

* define table type for data exchange
TYPES: BEGIN OF mytable_line,
         line(c_line_length) TYPE c,
       END OF mytable_line.

* table to exchange text
DATA gt_mytable TYPE TABLE OF mytable_line.
DATA gt_sql     TYPE TABLE OF mytable_line.

DATA: myline    LIKE LINE OF gt_mytable,
      oneline   LIKE LINE OF gt_mytable,
      lv_rowcnt TYPE i.

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
      wfilename     TYPE string,
      wtxt1(80),
      p_dir         TYPE string,
      wcomment      TYPE string.

TYPES: BEGIN OF t_fldtyp,
         fld     TYPE string,
         typ     TYPE string,
         hdr(10) TYPE c,
       END OF t_fldtyp.

DATA: gt_fldtyp TYPE STANDARD TABLE OF  t_fldtyp,
      wa_fldtyp TYPE t_fldtyp.

FIELD-SYMBOLS: <fs_fldtyp> TYPE t_fldtyp.

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

TYPES: BEGIN OF t_obj_table,
         line(255) TYPE x,
       END OF t_obj_table.

DATA: obj_table TYPE STANDARD TABLE OF t_obj_table,
      sel_flash TYPE STANDARD TABLE OF t_obj_table,
      music_mp3 TYPE STANDARD TABLE OF t_obj_table.

DATA: sel_flash_url(255),
      music_mp3_url(255).

TYPES: BEGIN OF t_song,
         song TYPE char12,
       END OF t_song.

DATA: gt_song TYPE STANDARD TABLE OF t_song,
      wa_song TYPE t_song,
      songnum TYPE i.

DATA: dockingbotm TYPE REF TO  cl_gui_docking_container,
      html_viewer TYPE REF TO  cl_gui_html_viewer.

PARAMETERS: p_pass  TYPE char8   OBLIGATORY LOWER CASE.

*&---------------------------------------------------------------------*
*                       INITIALIZATION EVENT                           *
*&---------------------------------------------------------------------*
INITIALIZATION.
*  MOVE 'PAGAL' TO wa_song-song.
*  APPEND wa_song TO gt_song.
*  MOVE 'RAGA' TO wa_song-song.
*  APPEND wa_song TO gt_song.
*  MOVE 'GIRI' TO wa_song-song.
*  APPEND wa_song TO gt_song.
*  MOVE 'WOODEN' TO wa_song-song.
*  APPEND wa_song TO gt_song.
*  MOVE 'MUMBAI' TO wa_song-song.
*  APPEND wa_song TO gt_song.
*  MOVE 'NIRALA' TO wa_song-song.
*  APPEND wa_song TO gt_song.
*  MOVE 'GOHOME' TO wa_song-song.
*  APPEND wa_song TO gt_song.
*  MOVE 'LONELY' TO wa_song-song.
*  APPEND wa_song TO gt_song.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK screen-name EQ 'P_PASS'.
    MOVE: 1 TO screen-invisible.
    MODIFY SCREEN.
  ENDLOOP.

*  IF sy-batch <> 'X'.
*
*    REFRESH obj_table.
*    IMPORT obj_table FROM DATABASE zcplyblobs(fl) ID 'BIJOYA'.
*    sel_flash = obj_table.
*
*    CALL FUNCTION 'QF05_RANDOM_INTEGER'
*      EXPORTING
*        ran_int_max   = 8
*        ran_int_min   = 1
*      IMPORTING
*        ran_int       = songnum
*      EXCEPTIONS
*        invalid_input = 1
*        OTHERS        = 2.
*
*    IF songnum NOT BETWEEN 1 AND 8.
*      MOVE 2 TO songnum.
*    ENDIF.
*
*    READ TABLE gt_song INTO wa_song INDEX songnum.
*
*    REFRESH obj_table.
*    IMPORT obj_table FROM DATABASE zcplyblobs(m3) ID wa_song-song.
*    music_mp3 = obj_table.
*
*    IF dockingbotm IS INITIAL.
*
*      CREATE OBJECT dockingbotm
*        EXPORTING
*          repid     = sy-repid
*          dynnr     = sy-dynnr
*          side      = dockingbotm->dock_at_bottom
*          ratio     = 90
*          extension = 1000.
*
*      IF html_viewer IS INITIAL.
*        CREATE OBJECT html_viewer
*          EXPORTING
*            parent = dockingbotm.
*      ENDIF.
*
*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          filetype                = 'BIN'
*          filename                = 'c:/tmp/Pagal.mp3'
*        TABLES
*          data_tab                = music_mp3
*        EXCEPTIONS
*          file_open_error         = 1
*          file_read_error         = 2
*          no_batch                = 3
*          gui_refuse_filetransfer = 4
*          invalid_type            = 5
*          no_authority            = 6
*          unknown_error           = 7
*          bad_data_format         = 8
*          header_not_allowed      = 9
*          separator_not_allowed   = 10
*          header_too_long         = 11
*          unknown_dp_error        = 12
*          access_denied           = 13
*          dp_out_of_memory        = 14
*          disk_full               = 15
*          dp_timeout              = 16
*          OTHERS                  = 17.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*      CALL METHOD html_viewer->load_data
*        EXPORTING
*          url                  = 'jncBijoya4.swf'
*          type                 = 'application'
*          subtype              = 'x-shockwave-flash'
*        IMPORTING
*          assigned_url         = sel_flash_url
*        CHANGING
*          data_table           = sel_flash
*        EXCEPTIONS
*          dp_invalid_parameter = 1
*          dp_error_general     = 2.
*
*      IF sy-subrc <> 0.
*        MESSAGE e999 WITH 'Load Flash Failed'.
*      ENDIF.
*
*      html_viewer->show_data( url = sel_flash_url ).
*
*      cl_gui_cfw=>flush( ).
*
*    ENDIF.
*
*  ENDIF.
*

START-OF-SELECTION.

*
*  IF html_viewer IS NOT INITIAL.
*    html_viewer->free( ).
*  ENDIF.

*  MOVE pp_dir TO p_dir.
*
*  CALL METHOD cl_gui_frontend_services=>directory_exist
*    EXPORTING
*      directory            = p_dir
*    RECEIVING
*      result               = w_result
*    EXCEPTIONS
*      cntl_error           = 1
*      error_no_gui         = 2
*      wrong_parameter      = 3
*      not_supported_by_gui = 4
*      OTHERS               = 5.
*
*  IF sy-subrc <> 0 OR w_result <> 'X'.
*    CALL METHOD cl_gui_frontend_services=>directory_create
*      EXPORTING
*        directory                = p_dir
*      CHANGING
*        rc                       = w_rc
*      EXCEPTIONS
*        directory_create_failed  = 1
*        cntl_error               = 2
*        error_no_gui             = 3
*        directory_access_denied  = 4
*        directory_already_exists = 5
*        path_not_found           = 6
*        unknown_error            = 7
*        not_supported_by_gui     = 8
*        wrong_parameter          = 9
*        OTHERS                   = 5.
*  ENDIF.
*
*  FIND '\' IN p_dir.
*
*  IF sy-subrc = 0.
*    CONCATENATE p_dir '\jnc.ab4' INTO wfilename.
*  ELSE.
*    CONCATENATE p_dir '/jnc.ab4' INTO wfilename.
*  ENDIF.
*
*  CONCATENATE `Possible Wrong SQL - see ` wfilename INTO wtxt1.

  MOVE `Possible Wrong SQL - see ` TO wtxt1.

  PERFORM f_encrypt.

  IF w_hash <> 141824.  "ojnc
    MESSAGE e999(zmsgs) WITH 'Password Mismatch'.
  ENDIF.

  MOVE `X` TO isnatv.
  CLEAR isopen.

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


************************************************************************
*  F O R M S
************************************************************************

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

  CLEAR: code, gt_fldtyp.

  MOVE 0 TO : first, numhdrs.
  LOOP AT gt_mytable INTO myline.
    ADD 1 TO numhdrs.
    IF strlen( myline ) = 0.
      CONTINUE.
    ENDIF.

    REPLACE ALL OCCURRENCES OF con_tab  IN myline WITH space.

    CONCATENATE ` ` myline ` ` INTO myline.
    IF first = 0.
      FIND ` Begin ` IN myline IGNORING CASE.
      IF sy-subrc <> 0.
        CALL FUNCTION `POPUP_TO_INFORM`
          EXPORTING
            titel = g_repid
            txt2  = `BEGIN of Types Missing`
            txt1  = `Correct & Retry`.
        RETURN.
      ENDIF.
      MOVE 1 TO first.
      PERFORM f_get_comment.
      MOVE wcomment TO repheader.
      CONTINUE.
    ENDIF.

    FIND ` END ` IN myline IGNORING CASE.
    IF sy-subrc = 0.
      EXIT.
    ENDIF.

    FIND ` Select ` IN myline IGNORING CASE.
    IF sy-subrc = 0.
      CALL FUNCTION `POPUP_TO_INFORM`
        EXPORTING
          titel = g_repid
          txt2  = `END of Types is missing`
          txt1  = `Correct & Retry`.
      RETURN.
    ENDIF.

    SPLIT myline AT space INTO TABLE gt_token.

    CLEAR: numcols, wa_fldtyp.


    LOOP AT gt_token ASSIGNING <fs_token>.

      IF strlen( <fs_token>-token ) = 0.
        CONTINUE.
      ENDIF.

      ADD 1 TO numcols.

      IF numcols = 1.
        MOVE <fs_token>-token TO wa_fldtyp-fld.
      ENDIF.

      IF numcols = 2 AND <fs_token>-token+0(1) <> '"'.
        MOVE <fs_token>-token TO wa_fldtyp-typ.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF wa_fldtyp-typ IS INITIAL.
      MOVE wa_fldtyp-fld TO wa_fldtyp-typ.
    ENDIF.

    TRANSLATE wa_fldtyp-fld TO LOWER CASE.
    TRANSLATE wa_fldtyp-typ TO LOWER CASE.

    PERFORM f_get_comment.

    MOVE wcomment TO wa_fldtyp-hdr.

    APPEND wa_fldtyp TO gt_fldtyp.

  ENDLOOP.

  IF lines( gt_fldtyp ) = 0.
    CALL FUNCTION `POPUP_TO_INFORM`
      EXPORTING
        titel = g_repid
        txt2  = `No Fields and Types found`
        txt1  = `Correct & Retry`.
    RETURN.
  ENDIF.


  MOVE 0 TO : first, aggfun.
  REFRESH gt_sql.

  LOOP AT gt_mytable INTO myline.
    IF strlen( myline ) = 0.
      CONTINUE.
    ENDIF.

    IF sy-tabix <= numhdrs.
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

  APPEND ` DATA : BEGIN OF i_tab OCCURS 0,` TO code.

  LOOP AT gt_fldtyp INTO wa_fldtyp.
    CONCATENATE  `         ` wa_fldtyp-fld  ` TYPE ` wa_fldtyp-typ `,` INTO mystring.
    APPEND mystring TO code.
  ENDLOOP.

  APPEND `      END OF i_tab.` TO code.
  APPEND `` TO code.

  APPEND `DATA : r_tab LIKE LINE OF i_tab,` TO code.
  APPEND `       l_kount    TYPE i.` TO code.
  APPEND `` TO code.

  APPEND ` DATA : mystring TYPE string.`          TO code.
  APPEND ` DATA : mytitle  TYPE lvc_title.`       TO code.
  APPEND `` TO code.

  APPEND `DATA: rows      TYPE i.` TO code.
  APPEND `` TO code.

  APPEND ` DATA: exsm TYPE string.`          TO code.
  APPEND ` DATA: exlm TYPE string.`          TO code.
  APPEND ` DATA: myex TYPE REF TO CX_ROOT.`  TO code.

  APPEND `` TO code.

  APPEND ` DATA: lo_sql    TYPE REF TO cl_sql_statement,` TO code.
  APPEND `       lo_result TYPE REF TO cl_sql_result_set,` TO code.
  APPEND `       lo_exc    TYPE REF TO cx_sql_exception,` TO code.
  APPEND `       lt_cols   TYPE adbc_column_tab,` TO code.
  APPEND `       lv_query  TYPE string,` TO code.
  APPEND `       lo_output TYPE REF TO data,` TO code.
  APPEND `       dref2     TYPE REF TO data.` TO code.

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

  CONCATENATE `  MOVE` nrows `TO rows.` INTO mystring SEPARATED BY space.
  APPEND mystring TO code.
  APPEND `` TO code.

  CONCATENATE `  MOVE '` repheader `' TO mytitle.` INTO mystring.
  APPEND mystring TO code.
  APPEND `` TO code.


  APPEND `` TO code.
  APPEND `  TRY.` TO code.
  APPEND `` TO code.

  IF isopen <> `X`.
    CLEAR oneline.
    myline = ` lv_query = `.
    lv_rowcnt = lines( gt_sql ).
    LOOP AT gt_sql INTO oneline.
      IF sy-tabix = 1.
        CONCATENATE myline ` CONDENSE(``` oneline ```` INTO myline SEPARATED BY space.
        APPEND myline TO code.
      ELSE.
        CONCATENATE ` && ``` oneline  ```` INTO myline SEPARATED BY space.
        APPEND myline TO code.
      ENDIF.
    ENDLOOP.
    APPEND ` ).` TO code.
  ENDIF.
  APPEND `` TO code.

  IF isopen = `X`.
    LOOP AT gt_sql INTO myline.
      APPEND myline TO code.
    ENDLOOP.

    APPEND `INTO TABLE @i_tab` TO code.
    IF aggfun = 0.
      CONCATENATE `  UP TO ` nrows `rows` INTO mystring SEPARATED BY space.
      APPEND mystring TO code.
    ENDIF.
    APPEND `.` TO code.
  ELSE.

    APPEND ` TRY.` TO code.
    APPEND `     CREATE OBJECT lo_sql. ` TO code.
    APPEND `     GET REFERENCE OF I_TAB[] INTO lo_output. ` TO code.
    APPEND `     lo_result = lo_sql->execute_query( lv_query ). ` TO code.
    APPEND `     lo_result->set_param_table( itab_ref = lo_output  ). ` TO code.
    CONCATENATE `     lo_result->next_package( UPTO = ` nrows ` ).` INTO mystring SEPARATED BY space.
    APPEND mystring TO code.
    APPEND `   CATCH cx_sql_exception INTO lo_exc. ` TO code.
    APPEND `     MESSAGE lo_exc TYPE 'I' DISPLAY LIKE 'E'. ` TO code.
    APPEND ` ENDTRY. ` TO code.
    APPEND ` ` TO code.

  ENDIF.

  APPEND `` TO code.
  APPEND `    PERFORM zjnc_dump_list  USING ``I_TAB[]`` ``I_TAB`` mytitle.` TO code.
  APPEND `` TO code.
  APPEND `  CATCH CX_ROOT INTO myex.` TO code.
  APPEND `    exsm =  myex->if_message~get_text( ).` TO code.
  APPEND `    exlm =  myex->if_message~get_longtext( ).` TO code.
  APPEND `` TO code.

  APPEND `    CALL FUNCTION ``POPUP_TO_INFORM``` TO code.
  APPEND `      EXPORTING` TO code.
  APPEND `          titel = ``jncDynamicSub``` TO code.
  APPEND `          txt1  = ``Generate SUBROUTINE POOL Succeeded BUT SQL failed``` TO code.
  CONCATENATE `          txt2  = 'Possible Wrong SQL - see ` wfilename `'` INTO mystring.
  APPEND mystring TO code.
  APPEND `          txt3  = exsm` TO code.
  APPEND `          txt4  = exlm.` TO code.
  APPEND `  ENDTRY.` TO code.
  APPEND `ENDFORM.  "DoSQL.` TO code.

  APPEND `` TO code.
  APPEND `*&--------------------------------------------------------------------*` TO code.
  APPEND `*&      Form  ZJNC_DUMP_LIST  Our Good Old ALV list -  RECOMMENDED!` TO code.
  APPEND `*&--------------------------------------------------------------------*` TO code.
  APPEND `FORM zjnc_dump_list  USING value(p_it_name) TYPE c` TO code.
  APPEND `                           value(p_wa_name) TYPE c` TO code.
  APPEND `                           value(p_heading) TYPE c.` TO code.
  APPEND `` TO code.
  APPEND `  TYPE-POOLS: slis.` TO code.
  APPEND `` TO code.
  APPEND `  DATA:` TO code.
  APPEND `    stru_ref    TYPE REF TO cl_abap_structdescr,` TO code.
  APPEND `    comp_tab    TYPE abap_compdescr_tab,` TO code.
  APPEND `    one_comp    TYPE abap_compdescr,` TO code.
  APPEND `    one_name    TYPE string,` TO code.
  APPEND `    type_ref    TYPE REF TO cl_abap_typedescr,` TO code.
  APPEND `    is_ddic     TYPE abap_bool,` TO code.
  APPEND `    lt_ddic     TYPE dd_x031l_table,` TO code.
  APPEND `    wa_ddic     TYPE x031l,` TO code.
  APPEND `    lt_fcat     TYPE slis_t_fieldcat_alv,` TO code.
  APPEND `    wa_fcat     TYPE slis_fieldcat_alv,` TO code.
  APPEND `    ls_layo     TYPE slis_layout_alv,` TO code.
  APPEND `    l_alv       TYPE REF TO cl_gui_alv_grid.` TO code.
  APPEND `` TO code.
  APPEND `  FIELD-SYMBOLS: <fs_type>  TYPE ANY,` TO code.
  APPEND `                 <fs_table> TYPE STANDARD TABLE,` TO code.
  APPEND `                 <fs_line>  TYPE ANY.` TO code.
  APPEND `` TO code.
  APPEND `  ASSIGN (p_it_name) TO <fs_table>.` TO code.
  APPEND `` TO code.
  APPEND `  ASSIGN (p_wa_name) TO <fs_line>.` TO code.
  APPEND `` TO code.
  APPEND `  ls_layo-colwidth_optimize = ``X``.` TO code.
  APPEND `  ls_layo-zebra = ``X``.` TO code.
  APPEND `  ls_layo-window_titlebar = p_heading.` TO code.
  APPEND `  ls_layo-box_tabname   = p_it_name.` TO code.
  APPEND `` TO code.
  APPEND `  stru_ref ?= cl_abap_structdescr=>describe_by_data( <fs_line> ).` TO code.
  APPEND `` TO code.
  APPEND `  comp_tab = stru_ref->components.` TO code.
  APPEND `` TO code.
  APPEND `  LOOP AT comp_tab INTO one_comp.` TO code.
  APPEND `    CLEAR wa_fcat.` TO code.
  APPEND `    wa_fcat-tabname   = p_it_name.` TO code.
  APPEND `    wa_fcat-fieldname = one_comp-name.` TO code.
  APPEND `` TO code.
  APPEND `    CONCATENATE p_wa_name ``-`` one_comp-name INTO one_name.` TO code.
  APPEND `` TO code.
  APPEND `    ASSIGN (one_name) TO <fs_type>.` TO code.
  APPEND `` TO code.
  APPEND `    type_ref ?= cl_abap_typedescr=>describe_by_data( <fs_type> ).` TO code.
  APPEND `` TO code.
  APPEND `    is_ddic = type_ref->is_ddic_type( ).` TO code.
  APPEND `` TO code.
  APPEND `    IF is_ddic = abap_true.` TO code.
  APPEND `      lt_ddic = type_ref->get_ddic_object( ).` TO code.
  APPEND `` TO code.
  APPEND `      LOOP AT lt_ddic INTO wa_ddic.` TO code.
  APPEND `        CLEAR wa_ddic-tabname.` TO code.
  APPEND `        SELECT SINGLE` TO code.
  APPEND `               dd03l~tabname` TO code.
  APPEND `          INTO wa_ddic-tabname` TO code.
  APPEND `          FROM dd03l` TO code.
  APPEND `         WHERE dd03l~fieldname = wa_ddic-fieldname` TO code.
  APPEND `           AND dd03l~tabname NOT LIKE ``/%``.  " only normal namespace` TO code.
  APPEND `` TO code.
  APPEND `` TO code.
  APPEND `        wa_fcat-ref_tabname    = wa_ddic-tabname.` TO code.
  APPEND `        wa_fcat-ref_fieldname  = wa_ddic-fieldname.` TO code.
  APPEND `` TO code.
  APPEND `        SELECT SINGLE` TO code.
  APPEND `               dd04t~scrtext_s` TO code.
  APPEND `               dd04t~scrtext_m` TO code.
  APPEND `               dd04t~scrtext_l` TO code.
  APPEND `          INTO (wa_fcat-seltext_s, wa_fcat-seltext_m, wa_fcat-seltext_l)` TO code.
  APPEND `          FROM dd04t` TO code.
  APPEND `         WHERE dd04t~rollname   = wa_ddic-fieldname` TO code.
  APPEND `           AND dd04t~ddlanguage = sy-langu.` TO code.
  APPEND `` TO code.
  APPEND `      ENDLOOP.` TO code.
  APPEND `    ELSE.` TO code.
  APPEND `      MOVE one_comp-name TO wa_fcat-seltext_l.` TO code.
  APPEND `    ENDIF.` TO code.
  APPEND `` TO code.

  LOOP AT gt_fldtyp ASSIGNING <fs_fldtyp>.
    IF <fs_fldtyp>-hdr IS NOT INITIAL.
      TRANSLATE <fs_fldtyp>-fld TO UPPER CASE.
      CONCATENATE `    IF one_comp-name = '` <fs_fldtyp>-fld `'.` INTO mystring.
      APPEND mystring TO code.

      CONCATENATE `      MOVE '` <fs_fldtyp>-hdr `' TO  wa_fcat-seltext_l.` INTO mystring.
      APPEND mystring TO code.

*     APPEND `      CLEAR: wa_fcat-ref_tabname, wa_fcat-ref_fieldname.` TO code.

      APPEND `    ENDIF.` TO code.
    ENDIF.
  ENDLOOP.

  APPEND `` TO code.
  APPEND `    MOVE wa_fcat-seltext_l TO: wa_fcat-seltext_s, wa_fcat-seltext_m.` TO code.
  APPEND `    MOVE 'L' TO wa_fcat-ddictxt.` TO code.
  APPEND `` TO code.
  APPEND `    APPEND wa_fcat TO lt_fcat.` TO code.
  APPEND `` TO code.
  APPEND `  ENDLOOP.` TO code.
  APPEND `` TO code.
  APPEND `  CALL FUNCTION ``REUSE_ALV_GRID_DISPLAY``` TO code.
  APPEND `    EXPORTING` TO code.
  APPEND `      is_layout   = ls_layo` TO code.
  APPEND `      it_fieldcat = lt_fcat` TO code.
  APPEND `    TABLES` TO code.
  APPEND `      t_outtab    = <fs_table>.` TO code.
  APPEND `` TO code.
  APPEND `ENDFORM.                    "ZJNC_DUMP_LIST` TO code.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


*  GENERATE SUBROUTINE POOL code   NAME    prog
*                                  MESSAGE msg
*                                  LINE    lin
*                                  WORD    wrd
*                                  OFFSET  off.

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
      cl_demo_output=>display(
        |MESSAGE:      { mess }\n| &&
        |INCLUDE:      { incl }\n| &&
        |LINE:         { line }\n| &&
        |WORD:         { wrd  }\n| &&
        |OFFSET:       { off  }\n| &&
        |MESSAGE-ID:   { CONV string( mid ) }\n| &&
        |SHORTDUMP-ID: { sid }| ).
      PERFORM f_dumpcode.
    ENDIF.
  ENDIF.


ENDFORM.        "F_RUNSQL

*&--------------------------------------------------------------------*
*&      Form  f_get_comment
*&--------------------------------------------------------------------*
FORM f_get_comment.
  CLEAR: wcomment, myoff.
  FIND `"` IN myline MATCH OFFSET myoff.
  IF myoff IS NOT INITIAL.
    mylen = strlen( myline ).
    mylen = mylen - myoff.
    ADD 1 TO myoff.
    IF mylen <> 0.
      MOVE myline+myoff(mylen) TO wcomment.
      CONDENSE wcomment.
      REPLACE ALL OCCURRENCES OF `'` IN wcomment WITH ''.
    ENDIF.
  ENDIF.
ENDFORM.                    "f_get_comment

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
