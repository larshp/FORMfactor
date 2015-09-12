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

* todo, pre-FORM comments, another pass, leeloo multipass?

DATA: gv_ok_code LIKE sy-ucomm.

CONSTANTS: gc_tab     TYPE c LENGTH 2 VALUE '  ',
           gc_newline TYPE c VALUE cl_abap_char_utilities=>newline.

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

*----------------------------------------------------------------------*
*       CLASS lcx_exception IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
  ENDMETHOD.                    "CONSTRUCTOR

ENDCLASS.                    "lcx_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_parameter DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_parameter DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_name TYPE string,
      get_name
        RETURNING VALUE(rv_name) TYPE string,
      set_type
        IMPORTING iv_type TYPE string,
      render
        IMPORTING iv_no_type     TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rv_code) TYPE string.

  PRIVATE SECTION.
    DATA: mv_name TYPE string,
          mv_type TYPE string.

ENDCLASS.                    "lcl_parameter DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_parameter IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_parameter IMPLEMENTATION.

  METHOD constructor.

    mv_name = iv_name.

  ENDMETHOD.                    "constructor

  METHOD get_name.
    rv_name = mv_name.
  ENDMETHOD.                    "get_name

  METHOD set_type.
    mv_type = iv_type.
  ENDMETHOD.                    "set_type

  METHOD render.
    IF iv_no_type = abap_true.
      rv_code = mv_name.
    ELSE.
      CONCATENATE mv_name 'TYPE' mv_type
        INTO rv_code SEPARATED BY space.
    ENDIF.

    CONCATENATE gc_tab gc_tab gc_tab gc_tab gc_tab rv_code
      INTO rv_code RESPECTING BLANKS.
  ENDMETHOD.                    "render

ENDCLASS.                    "lcl_parameter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_parameter_list DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_parameter_list DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      size
        RETURNING VALUE(rv_size) TYPE i,
      add
        IMPORTING iv_name             TYPE string
        RETURNING VALUE(ro_parameter) TYPE REF TO lcl_parameter,
      get
        IMPORTING iv_index            TYPE i
        RETURNING VALUE(ro_parameter) TYPE REF TO lcl_parameter,
      render
        IMPORTING iv_no_type     TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rv_code) TYPE string.

  PRIVATE SECTION.
    DATA: mt_list TYPE TABLE OF REF TO lcl_parameter.

ENDCLASS.                    "lcl_parameter_list DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_parameter_list IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_parameter_list IMPLEMENTATION.

  METHOD size.
    rv_size = lines( mt_list ).
  ENDMETHOD.                    "size

  METHOD get.

    READ TABLE mt_list INDEX iv_index INTO ro_parameter.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD add.

    CREATE OBJECT ro_parameter
      EXPORTING
        iv_name = iv_name.
    APPEND ro_parameter TO mt_list.

  ENDMETHOD.                    "add

  METHOD render.

    DATA: lt_code      TYPE TABLE OF string,
          lo_parameter TYPE REF TO lcl_parameter.


    LOOP AT mt_list INTO lo_parameter.
      APPEND lo_parameter->render( iv_no_type ) TO lt_code.
    ENDLOOP.
    CONCATENATE LINES OF lt_code INTO rv_code SEPARATED BY gc_newline.

  ENDMETHOD.                    "render

ENDCLASS.                    "lcl_parameter_list IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_method DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_method DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_name TYPE string,
      get_importing
        RETURNING VALUE(ro_list) TYPE REF TO lcl_parameter_list,
      get_changing
        RETURNING VALUE(ro_list) TYPE REF TO lcl_parameter_list,
      get_exception
        RETURNING VALUE(ro_list) TYPE REF TO lcl_parameter_list,
      get_name
        RETURNING VALUE(rv_name) TYPE string,
      add_code
        IMPORTING iv_code TYPE clike,
      get_code
        RETURNING VALUE(rv_code) TYPE string,
      render
        RETURNING VALUE(rv_code) TYPE string,
      apply
        IMPORTING
          it_values      TYPE string_table
        RETURNING
          VALUE(rv_code) TYPE string.

  PRIVATE SECTION.
* todo, TABLES parameters?
    DATA:
      mv_name       TYPE string,
      mo_importing  TYPE REF TO lcl_parameter_list,
      mo_changing   TYPE REF TO lcl_parameter_list,
      mo_exceptions TYPE REF TO lcl_parameter_list,
      mv_code       TYPE string.

ENDCLASS.                    "lcl_method DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_method IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_method IMPLEMENTATION.

  METHOD apply.

    DATA: lo_parameter TYPE REF TO lcl_parameter,
          lt_values    LIKE it_values,
          lv_name      TYPE string,
          lv_len       TYPE i,
          lv_value     LIKE LINE OF lt_values.


    ASSERT lines( it_values ) = mo_importing->size( ) + mo_changing->size( ).

    lt_values = it_values.

    rv_code = mv_name && '(' && gc_newline.

    IF mo_importing->size( ) > 0.
      rv_code = rv_code && 'EXPORTING' && gc_newline.
      DO mo_importing->size( ) TIMES.
        lo_parameter = mo_importing->get( sy-index ).

        lv_value = lt_values[ 1 ].
        lv_name = lo_parameter->get_name( ).
        CONCATENATE rv_code lv_name ' = ' lv_value gc_newline
          INTO rv_code RESPECTING BLANKS.
        DELETE lt_values INDEX 1.
      ENDDO.
    ENDIF.

    IF mo_changing->size( ) > 0.
      rv_code = rv_code && 'CHANGING' && gc_newline.

      DO mo_changing->size( ) TIMES.
        lo_parameter = mo_changing->get( sy-index ).

        lv_value = lt_values[ 1 ].
        lv_name = lo_parameter->get_name( ).
        CONCATENATE rv_code lv_name ' = ' lv_value gc_newline
          INTO rv_code RESPECTING BLANKS.
        DELETE lt_values INDEX 1.
      ENDDO.
    ENDIF.

    lv_len = strlen( rv_code ) - 1.
    CONCATENATE rv_code(lv_len) ').' gc_newline
      INTO rv_code SEPARATED BY space.

  ENDMETHOD.

  METHOD get_code.

    rv_code = mv_code.

  ENDMETHOD.                    "get_code

  METHOD add_code.

    CONCATENATE mv_code iv_code INTO mv_code.

  ENDMETHOD.                    "add_code

  METHOD render.

    DATA: lv_len TYPE i.


    CONCATENATE gc_tab gc_tab gc_tab mv_name gc_newline
      INTO rv_code RESPECTING BLANKS.
    IF mo_importing->size( ) > 0.
      CONCATENATE rv_code gc_tab gc_tab gc_tab gc_tab 'IMPORTING' gc_newline
        INTO rv_code RESPECTING BLANKS.
      rv_code = rv_code && mo_importing->render( ) && gc_newline.
    ENDIF.
    IF mo_changing->size( ) > 0.
      CONCATENATE rv_code gc_tab gc_tab gc_tab gc_tab 'CHANGING' gc_newline
        INTO rv_code RESPECTING BLANKS.
      rv_code = rv_code && mo_changing->render( ) && gc_newline.
    ENDIF.
    IF mo_exceptions->size( ) > 0.
      CONCATENATE rv_code gc_tab gc_tab gc_tab gc_tab 'RAISING' gc_newline
        INTO rv_code RESPECTING BLANKS.
      rv_code = rv_code && mo_exceptions->render( abap_true ) && gc_newline.
    ENDIF.

    lv_len = strlen( rv_code ) - 1.
    rv_code = rv_code(lv_len).

  ENDMETHOD.                    "render

  METHOD get_name.
    rv_name = mv_name.
  ENDMETHOD.                    "get_name

  METHOD get_importing.

    ro_list = mo_importing.

  ENDMETHOD.                    "get_importing

  METHOD get_changing.

    ro_list = mo_changing.

  ENDMETHOD.                    "get_changing

  METHOD get_exception.

    ro_list = mo_exceptions.

  ENDMETHOD.                    "get_exception

  METHOD constructor.

    mv_name = iv_name.

    CREATE OBJECT mo_importing.
    CREATE OBJECT mo_changing.
    CREATE OBJECT mo_exceptions.

  ENDMETHOD.                    "constructor

ENDCLASS.                    "lcl_method IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING iv_name TYPE string,
      add_method
        IMPORTING iv_name          TYPE string
        RETURNING VALUE(ro_method) TYPE REF TO lcl_method,
      get_method
        IMPORTING iv_name          TYPE string
        RETURNING VALUE(ro_method) TYPE REF TO lcl_method
        RAISING   lcx_exception,
      get_name
        RETURNING VALUE(rv_name) TYPE string,
      render
        RETURNING VALUE(rv_code) TYPE string.

  PRIVATE SECTION.

    DATA: mv_name    TYPE string,
          mt_methods TYPE TABLE OF REF TO lcl_method.

    METHODS:
      render_definition
        RETURNING VALUE(rv_code) TYPE string,
      render_implementation
        RETURNING VALUE(rv_code) TYPE string.

ENDCLASS.                    "lcl_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_class IMPLEMENTATION.

  METHOD constructor.

    mv_name = iv_name.

  ENDMETHOD.                    "constructor

  METHOD render.

    rv_code = render_definition( ) && render_implementation( ).

  ENDMETHOD.

  METHOD get_name.

    rv_name = mv_name.

  ENDMETHOD.

  METHOD get_method.

    DATA: lo_method TYPE REF TO lcl_method.


    LOOP AT mt_methods INTO lo_method.
      IF lo_method->get_name( ) = iv_name.
        ro_method = lo_method.
      ENDIF.
    ENDLOOP.

    IF ro_method IS INITIAL.
      _raise 'method not found'.
    ENDIF.

  ENDMETHOD.                    "get_method

  METHOD add_method.

    CREATE OBJECT ro_method
      EXPORTING
        iv_name = iv_name.

    APPEND ro_method TO mt_methods.

  ENDMETHOD.                    "add_method

  METHOD render_implementation.

    DATA: lv_name   TYPE string,
          lo_method TYPE REF TO lcl_method.


    CONCATENATE 'CLASS' mv_name 'IMPLEMENTATION' INTO rv_code SEPARATED BY space.
    rv_code = rv_code && '.' && gc_newline && gc_newline.

    LOOP AT mt_methods INTO lo_method.
      lv_name = lo_method->get_name( ).
      CONCATENATE rv_code space 'METHOD' lv_name
        INTO rv_code SEPARATED BY space.
      CONCATENATE rv_code '.' gc_newline INTO rv_code.
      rv_code = rv_code && lo_method->get_code( ).
      CONCATENATE rv_code gc_tab 'ENDMETHOD.' gc_newline gc_newline
        INTO rv_code RESPECTING BLANKS.
    ENDLOOP.

    rv_code = rv_code && gc_newline && 'ENDCLASS.' && gc_newline.

  ENDMETHOD.                    "render_implementation

  METHOD render_definition.

    DATA: lv_comma_newline TYPE c LENGTH 2,
          lt_code          TYPE TABLE OF string,
          lv_code          TYPE string,
          lo_method        TYPE REF TO lcl_method.


    CONCATENATE 'CLASS' mv_name 'DEFINITION' 'FINAL'
      INTO rv_code SEPARATED BY space.
    CONCATENATE rv_code '.' gc_newline gc_newline
      INTO rv_code.
    CONCATENATE rv_code gc_tab 'PUBLIC SECTION.' gc_newline gc_newline
      INTO rv_code RESPECTING BLANKS.
    CONCATENATE rv_code gc_tab gc_tab 'CLASS-METHODS:' gc_newline
      INTO rv_code RESPECTING BLANKS.

    LOOP AT mt_methods INTO lo_method.
      APPEND lo_method->render( ) TO lt_code.
    ENDLOOP.
    IF sy-subrc = 0.
      CONCATENATE ',' gc_newline INTO lv_comma_newline.
      CONCATENATE LINES OF lt_code INTO lv_code SEPARATED BY lv_comma_newline.
      CONCATENATE rv_code lv_code '.' gc_newline INTO rv_code.
    ENDIF.

    rv_code = rv_code && 'ENDCLASS.' && gc_newline && gc_newline.

  ENDMETHOD.                    "render_definition

ENDCLASS.                    "lcl_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_source DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_source DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      read
        IMPORTING iv_program       TYPE rpy_prog-progname
        RETURNING VALUE(rv_source) TYPE string,
      pretty_print
        IMPORTING iv_source        TYPE string
        RETURNING VALUE(rv_source) TYPE string
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_source DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_source IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_source IMPLEMENTATION.

  METHOD pretty_print.

    DATA: ls_rseumod TYPE rseumod,
          lt_pretty  TYPE TABLE OF string,
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

    SPLIT iv_source AT gc_newline INTO TABLE lt_pretty.

    CALL FUNCTION 'CREATE_PRETTY_PRINT_FORMAT'
      EXPORTING
        mode          = lv_option
      TABLES
        source        = lt_pretty
      EXCEPTIONS
        syntax_errors = 1.
    IF sy-subrc <> 0.
      _raise 'pretty printer error'.
    ENDIF.

    CONCATENATE LINES OF lt_pretty INTO rv_source SEPARATED BY gc_newline.

  ENDMETHOD.                    "pretty_print

  METHOD read.

    DATA: lt_source TYPE TABLE OF abaptxt255.


    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = iv_program
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.                             "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    CONCATENATE LINES OF lt_source INTO rv_source
      SEPARATED BY gc_newline.

  ENDMETHOD.                    "read

ENDCLASS.                    "lcl_source IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_logic DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_logic DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run RAISING lcx_exception.

    CLASS-DATA: gv_input  TYPE string READ-ONLY,
                gv_output TYPE string READ-ONLY.

  PRIVATE SECTION.

    CLASS-DATA:
      go_class  TYPE REF TO lcl_class,
      go_method TYPE REF TO lcl_method.

    CLASS-METHODS:
      add_code
        IMPORTING iv_code TYPE clike,
      loop
        IMPORTING iv_source TYPE string
        RAISING   lcx_exception,
      handle_statement
        IMPORTING iv_statement        TYPE string
        RETURNING VALUE(rv_statement) TYPE string
        RAISING   lcx_exception,
      build_signature
        IMPORTING iv_statement TYPE string
        RAISING   lcx_exception,
      parse_statement
        IMPORTING iv_statement        TYPE string
        RETURNING VALUE(rv_statement) TYPE string
        RAISING   lcx_exception,
      handle_form
        IMPORTING is_result TYPE zcl_aoc_parser=>st_result
        RAISING   lcx_exception,
      handle_perform
        IMPORTING is_result           TYPE zcl_aoc_parser=>st_result
        RETURNING VALUE(rv_statement) TYPE string
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_logic DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_logic IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_logic IMPLEMENTATION.

  METHOD build_signature.

    DATA: lt_code      TYPE TABLE OF string,
          lv_statement TYPE string,
          lo_method    TYPE REF TO lcl_method,
          lo_parameter TYPE REF TO lcl_parameter,
          lo_list      TYPE REF TO lcl_parameter_list,
          ls_result    TYPE zcl_aoc_parser=>st_result.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF ls_result-tokens.


    lv_statement = iv_statement.
    REPLACE ALL OCCURRENCES OF gc_newline IN lv_statement WITH space.
    APPEND lv_statement TO lt_code.
    ls_result = zcl_aoc_parser=>run( lt_code ).
    IF ls_result-match = abap_false.
      _raise 'Error parsing code'.
    ENDIF.

    LOOP AT ls_result-tokens ASSIGNING <ls_token>.
      CASE <ls_token>-value.
        WHEN zcl_aoc_parser=>c_role-formdefid.
          lo_method = go_class->add_method( <ls_token>-code ).
        WHEN 'FORM_USING'.
          lo_list = lo_method->get_importing( ).
        WHEN 'FORM_CHANGING'.
          lo_list = lo_method->get_changing( ).
        WHEN 'FORM_RAISING'.
          lo_list = lo_method->get_exception( ).
        WHEN zcl_aoc_parser=>c_role-fielddefid OR zcl_aoc_parser=>c_role-classexctypeid.
          lo_parameter = lo_list->add( <ls_token>-code ).
        WHEN zcl_aoc_parser=>c_role-typeid.
          lo_parameter->set_type( <ls_token>-code ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.                    "build_signature

  METHOD handle_form.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF is_result-tokens.


    READ TABLE is_result-tokens ASSIGNING <ls_token>
      WITH KEY value = zcl_aoc_parser=>c_role-formdefid.
    ASSERT sy-subrc = 0.

    go_method = go_class->get_method( <ls_token>-code ).

  ENDMETHOD.                    "handle_form

  METHOD handle_perform.

    DATA: lt_values TYPE string_table,
          lv_method TYPE string.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF is_result-tokens.


    READ TABLE is_result-tokens ASSIGNING <ls_token>
      WITH KEY value = zcl_aoc_parser=>c_role-formid.
    ASSERT sy-subrc = 0.
    lv_method = <ls_token>-code.

    LOOP AT is_result-tokens ASSIGNING <ls_token>
        WHERE value = zcl_aoc_parser=>c_role-fieldid.
      APPEND <ls_token>-code TO lt_values.
    ENDLOOP.

    rv_statement = go_class->get_name( ) &&
      '=>' &&
      go_class->get_method( lv_method )->apply( lt_values ).

  ENDMETHOD.                    "handle_perform

  METHOD add_code.
    IF go_method IS BOUND.
      go_method->add_code( iv_code ).
    ELSE.
      CONCATENATE gv_output iv_code INTO gv_output.
    ENDIF.
  ENDMETHOD.                    "add_code

  METHOD loop.

    DATA: lv_statement TYPE string,
          lt_source    TYPE TABLE OF string,
          lv_line      LIKE LINE OF lt_source.


    SPLIT iv_source AT gc_newline INTO TABLE lt_source.

    LOOP AT lt_source INTO lv_line.
      CONCATENATE lv_statement lv_line gc_newline
        INTO lv_statement.
      CONDENSE lv_statement.

* todo, use proper parser?
      IF lv_statement CP 'FORM *.+'.
        build_signature( lv_statement ).
        CLEAR lv_statement.
      ELSEIF lv_statement CP '*.+'
          OR ( strlen( lv_statement ) >= 8 AND lv_statement(8) = 'ENDFORM.' )
          OR lv_statement = gc_newline
          OR lv_statement CP '#**'.
        CLEAR lv_statement.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_source INTO lv_line.
      CONCATENATE lv_statement lv_line gc_newline
        INTO lv_statement.

      IF lv_statement = gc_newline
          OR lv_statement CP '#**'.
        add_code( lv_statement ).
        CLEAR lv_statement.
      ELSEIF lv_statement CP '*.+'
          OR ( strlen( lv_statement ) >= 8 AND lv_statement(8) = 'ENDFORM.' ).
        lv_statement = handle_statement( lv_statement ).
        add_code( lv_statement ).
        CLEAR lv_statement.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "loop

  METHOD handle_statement.

    DATA: lv_condensed TYPE string.


    rv_statement = iv_statement.
    lv_condensed = iv_statement.
    CONDENSE lv_condensed.

    IF lv_condensed CP 'REPORT *'.
      CLEAR rv_statement.
    ELSEIF lv_condensed CP 'FORM *'
        OR lv_condensed CP 'ENDFORM.*'
        OR lv_condensed CP 'PERFORM *'.
      rv_statement = parse_statement( iv_statement ).
    ENDIF.

  ENDMETHOD.                    "handle_statement

  METHOD parse_statement.

    DATA: lt_code      TYPE TABLE OF string,
          lv_statement TYPE string,
          ls_result    TYPE zcl_aoc_parser=>st_result.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF ls_result-tokens.


    lv_statement = iv_statement.
    REPLACE ALL OCCURRENCES OF gc_newline IN lv_statement WITH space.
    APPEND lv_statement TO lt_code.
    ls_result = zcl_aoc_parser=>run( lt_code ).
    IF ls_result-match = abap_false.
      _raise 'Error parsing code'.
    ENDIF.

    READ TABLE ls_result-tokens ASSIGNING <ls_token>
      WITH KEY type = zcl_aoc_parser=>c_type-terminal.
    ASSERT sy-subrc = 0.
    CASE <ls_token>-rulename.
      WHEN 'FORM'.
        handle_form( ls_result ).
      WHEN 'PERFORM_INTERN'.
        rv_statement = handle_perform( ls_result ).
      WHEN 'ENDFORM'.
        CLEAR go_method.
      WHEN OTHERS.
        ASSERT 1 = 1 + 1.
    ENDCASE.

  ENDMETHOD.                    "parse_statement

  METHOD run.

    CREATE OBJECT go_class
      EXPORTING
        iv_name = 'LCL_APP'.

    gv_input = lcl_source=>read( p_prog ).
    loop( gv_input ).
* todo, report name
    gv_output =
      'REPORT zfoobar.' && gc_newline && gc_newline &&
      go_class->render( ) &&
      gv_output.
    gv_output = lcl_source=>pretty_print( gv_output ).

  ENDMETHOD.                    "run

ENDCLASS.                    "lcl_logic IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_gui IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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

    lo_left  = go_splitter->get_container( row    = 1
                                           column = 1 ).
    lo_right = go_splitter->get_container( row    = 1
                                           column = 2 ).

    CREATE OBJECT go_text1
      EXPORTING
        parent = lo_left.
    go_text1->set_toolbar_mode( 0 ).
    go_text1->set_statusbar_mode( 0 ).
    go_text1->set_readonly_mode( 1 ).
    go_text1->set_font_fixed( ).
    go_text1->set_wordwrap_behavior(
      wordwrap_mode = cl_gui_textedit=>wordwrap_off ).
    go_text1->set_textstream( lcl_logic=>gv_input ).

    CREATE OBJECT go_text2
      EXPORTING
        parent = lo_right.
    go_text2->set_toolbar_mode( 0 ).
    go_text2->set_statusbar_mode( 0 ).
    go_text2->set_readonly_mode( 1 ).
    go_text2->set_font_fixed( ).
    go_text1->set_wordwrap_behavior(
      wordwrap_mode = cl_gui_textedit=>wordwrap_off ).
    go_text2->set_textstream( lcl_logic=>gv_output ).

    SET TITLEBAR 'TITLE_3000'.
    SET PF-STATUS 'STATUS_3000'.

  ENDMETHOD.                    "pbo

  METHOD pai.

    CASE gv_ok_code.
      WHEN 'BACK'.
        CLEAR gv_ok_code.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.                    "pai

  METHOD run.

    CALL SCREEN 3000.

  ENDMETHOD.                    "run

ENDCLASS.                    "lcl_gui IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run.

ENDCLASS.                    "lcl_app DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
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

*----------------------------------------------------------------------*
*  MODULE status_3000 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE status_3000 OUTPUT.
  lcl_gui=>pbo( ).
ENDMODULE.                    "status_3000 OUTPUT

*----------------------------------------------------------------------*
*  MODULE user_command_3000 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_3000 INPUT.
  lcl_gui=>pai( ).
ENDMODULE.                    "user_command_3000 INPUT