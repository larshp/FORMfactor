REPORT zformfactor.

* See https://github.com/larshp/FORMfactor/

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2015 Lars Hvam Petersen
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

DATA: gv_ok_code LIKE sy-ucomm.

DEFINE _raise.
  raise exception type lcx_exception
    exporting
      iv_text = &1.                                         "#EC NOTEXT
END-OF-DEFINITION.

PARAMETERS: p_prog TYPE rpy_prog-progname DEFAULT 'ZTEST01' OBLIGATORY.

********************************************************************************

CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA mv_text TYPE string.

    METHODS constructor
      IMPORTING iv_text TYPE string.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
  ENDMETHOD.                    "CONSTRUCTOR

ENDCLASS.

CLASS lcl_logic DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run RAISING lcx_exception.

    CLASS-DATA: gv_input  TYPE string READ-ONLY,
                gv_output TYPE string READ-ONLY.

  PRIVATE SECTION.
    CLASS-METHODS:
      loop
        RAISING lcx_exception,
      handle_statement
        IMPORTING iv_statement        TYPE string
        RETURNING VALUE(rv_statement) TYPE string
        RAISING   lcx_exception,
      parse_statement
        IMPORTING iv_statement        TYPE string
        RETURNING VALUE(rv_statement) TYPE string
        RAISING   lcx_exception,
      pretty_print,
      read_report.

    CLASS-DATA: gt_source TYPE TABLE OF abaptxt255.

ENDCLASS.

CLASS lcl_logic IMPLEMENTATION.

  METHOD read_report.

    DATA: ls_result TYPE zcl_aoc_parser=>st_result.


    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = p_prog
        with_lowercase   = abap_true
      TABLES
        source_extended  = gt_source
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4. "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    CONCATENATE LINES OF gt_source INTO gv_input
      SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.

  METHOD loop.

    DATA: lv_statement TYPE string,
          ls_source    LIKE LINE OF gt_source.


    LOOP AT gt_source INTO ls_source.
      CONCATENATE lv_statement ls_source-line cl_abap_char_utilities=>newline
        INTO lv_statement.

      IF lv_statement = cl_abap_char_utilities=>newline.
        CONCATENATE gv_output cl_abap_char_utilities=>newline INTO gv_output.
        CLEAR lv_statement.
      ELSEIF lv_statement CP '*.+'.
        lv_statement = handle_statement( lv_statement ).
        CONCATENATE gv_output lv_statement INTO gv_output.
        CLEAR lv_statement.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD handle_statement.

    DATA: lv_condensed TYPE string.


    rv_statement = iv_statement.
    lv_condensed = iv_statement.
    CONDENSE lv_condensed.

    IF lv_condensed CP 'FORM *' OR lv_condensed CP 'PERFORM *'.
      rv_statement = parse_statement( iv_statement ).
    ENDIF.

  ENDMETHOD.

  METHOD parse_statement.

    DATA: lt_code      TYPE TABLE OF string,
          lv_statement TYPE string,
          ls_result    TYPE zcl_aoc_parser=>st_result.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF ls_result-tokens.


    lv_statement = iv_statement.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_statement WITH space.
    APPEND lv_statement TO lt_code.
    ls_result = zcl_aoc_parser=>run( lt_code ).
    IF ls_result-match = abap_false.
      _raise 'Error parsing code'.
    ENDIF.

    READ TABLE ls_result-tokens ASSIGNING <ls_token> WITH KEY type = 'T'.
    ASSERT sy-subrc = 0.
    CASE <ls_token>-rulename.
      WHEN 'FORM'.
        BREAK-POINT.
      WHEN 'PERFORM_INTERN'.
        BREAK-POINT.
      WHEN OTHERS.
        BREAK-POINT.
    ENDCASE.

    rv_statement = 'parse, todo'.

  ENDMETHOD.

  METHOD pretty_print.

    DATA: ls_rseumod TYPE rseumod,
          lv_option  TYPE c LENGTH 5.


    CALL FUNCTION 'RS_WORKBENCH_CUSTOMIZING'
      EXPORTING
        choice          = 'WB'
        suppress_dialog = 'X'
      IMPORTING
        setting         = ls_rseumod.
    IF ls_rseumod-lowercase = 'X'.
      lv_option = 'LOWER'.
    ELSEIF ls_rseumod-lowercase = 'G'.
      lv_option = 'HIKEY'.
    ELSEIF ls_rseumod-lowercase = 'L'.
      lv_option = 'LOKEY'.
    ELSE.
      lv_option = 'UPPER'.
    ENDIF.

*    CALL FUNCTION 'CREATE_PRETTY_PRINT_FORMAT'
*      EXPORTING
*        mode          = lv_option
**      TABLES
**       source        = lt_pretty
*      EXCEPTIONS
*        syntax_errors = 1.
*if sy-subrc <> 0.
** todo
*endif.

  ENDMETHOD.

  METHOD run.

    read_report( ).
    loop( ).
    pretty_print( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run RAISING lcx_exception,
      pbo,
      pai.

  PRIVATE SECTION.
    CLASS-DATA:
      go_splitter TYPE REF TO cl_gui_splitter_container,
      go_text1    TYPE REF TO cl_gui_textedit,
      go_text2    TYPE REF TO cl_gui_textedit.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD pbo.

    DATA: lo_left  TYPE REF TO cl_gui_container,
          lo_right TYPE REF TO cl_gui_container.

    IF NOT go_splitter IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT go_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 1
        columns = 2.

    lo_left  = go_splitter->get_container( row = 1 column = 1 ).
    lo_right = go_splitter->get_container( row = 1 column = 2 ).

    CREATE OBJECT go_text1
      EXPORTING
        parent = lo_left.
    go_text1->set_toolbar_mode( 0 ).
    go_text1->set_statusbar_mode( 0 ).
    go_text1->set_readonly_mode( 1 ).
    go_text1->set_font_fixed( ).
    go_text1->set_textstream( lcl_logic=>gv_input ).

    CREATE OBJECT go_text2
      EXPORTING
        parent = lo_right.
    go_text2->set_toolbar_mode( 0 ).
    go_text2->set_statusbar_mode( 0 ).
    go_text2->set_readonly_mode( 1 ).
    go_text2->set_font_fixed( ).
    go_text2->set_textstream( lcl_logic=>gv_output ).

    SET TITLEBAR 'TITLE_3000'.
    SET PF-STATUS 'STATUS_3000'.

  ENDMETHOD.

  METHOD pai.

    CASE gv_ok_code.
      WHEN 'BACK'.
        CLEAR gv_ok_code.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.

  METHOD run.

    CALL SCREEN 3000.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run.

ENDCLASS.                    "lcl_app DEFINITION

CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    DATA: lx_exception TYPE REF TO lcx_exception.

    TRY.
        lcl_logic=>run( ).
        lcl_gui=>run( ).
      CATCH lcx_exception INTO lx_exception.
        MESSAGE lx_exception->mv_text TYPE 'E'.
    ENDTRY.

  ENDMETHOD.                    "run

ENDCLASS.                    "lcl_app IMPLEMENTATION

START-OF-SELECTION.
  lcl_app=>run( ).

MODULE status_3000 OUTPUT.
  lcl_gui=>pbo( ).
ENDMODULE.

MODULE user_command_3000 INPUT.
  lcl_gui=>pai( ).
ENDMODULE.