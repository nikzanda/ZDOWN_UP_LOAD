*&---------------------------------------------------------------------*
*& Report ZDOWN_UP_LOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdown_up_load
NO STANDARD PAGE HEADING
LINE-SIZE  255.
*----------------------------------------------------------------------
* Declare Database Objects
*----------------------------------------------------------------------
TABLES:
  dokil,
  trdir.
*----------------------------------------------------------------------
* Constants
CONSTANTS:
  mc_trdir_identifier(72)  TYPE c VALUE '%&%& RDIR',
  mc_report_identifier(72) TYPE c VALUE '%&%& REPO',
  mc_text_identifier(72)   TYPE c VALUE '%&%& TEXP',
  mc_thead_identifier(72)  TYPE c VALUE '%&%& HEAD',
  mc_doc_identifier(72)    TYPE c VALUE '%&%& DOKL',
  mc_trdir_short(4)        TYPE c VALUE 'RDIR',
  mc_report_short(4)       TYPE c VALUE 'REPO',
  mc_text_short(4)         TYPE c VALUE 'TEXP',
  mc_thead_short(4)        TYPE c VALUE 'HEAD',
  mc_doc_short(4)          TYPE c VALUE 'DOKP'.

*----------------------------------------------------------------------
*----------------------------------------------------------------------
* Declare Module level data structures
*----------------------------------------------------------------------
DATA: BEGIN OF mtab_program_source OCCURS 0,
        line(1000) TYPE c,
      END OF mtab_program_source.

DATA: mtab_program_trdir LIKE trdir OCCURS 0 WITH HEADER LINE.

DATA: mtab_program_texts LIKE textpool OCCURS 0 WITH HEADER LINE.

DATA: mstr_thead LIKE thead.

DATA: BEGIN OF mtab_program_file OCCURS 0,
        line(275) TYPE c,
      END OF mtab_program_file.

DATA: BEGIN OF mtab_directory OCCURS 0,
        name     LIKE trdir-name,
        desc(72) TYPE c,
        savename LIKE rlgrap-filename,
      END OF mtab_directory.

DATA: BEGIN OF mtab_program_documentation OCCURS 0,
        line(255) TYPE c,
      END OF mtab_program_documentation.

*----------------------------------------------------------------------
* Selection Screen
*----------------------------------------------------------------------

*-- Options for upload/download of programs
SELECTION-SCREEN BEGIN OF BLOCK frm_options WITH FRAME TITLE TEXT-udl.
PARAMETERS:
rb_down RADIOBUTTON GROUP udl DEFAULT 'X'.       " Download reports
SELECTION-SCREEN BEGIN OF BLOCK frm_trdir WITH FRAME TITLE TEXT-dir.
SELECT-OPTIONS:
s_name  FOR trdir-name,              " Program Name
s_subc  FOR trdir-subc               " Program Type
DEFAULT 'F' OPTION EQ SIGN E," Exclude Functions by default
s_cnam  FOR trdir-cnam               " Created by
DEFAULT sy-uname,
s_unam  FOR trdir-unam,              " Last Changed by
s_cdat  FOR trdir-cdat,              " Creation date
s_udat  FOR trdir-udat.              " Last update date
SELECTION-SCREEN END OF BLOCK frm_trdir.
*-- Options for uploading programs
PARAMETERS:
rb_up   RADIOBUTTON GROUP udl.       " Upload reports
SELECTION-SCREEN BEGIN OF BLOCK frm_upload WITH FRAME TITLE TEXT-upl.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) TEXT-sng.
PARAMETERS:
rb_file RADIOBUTTON GROUP how DEFAULT 'X'.
SELECTION-SCREEN COMMENT 33(42) TEXT-fna.
SELECTION-SCREEN END OF LINE.
PARAMETERS:
rb_list RADIOBUTTON GROUP how.
SELECTION-SCREEN END OF BLOCK frm_upload.
SELECTION-SCREEN END OF BLOCK frm_options.

*-- Options for up/downloading programs
SELECTION-SCREEN BEGIN OF BLOCK frm_filen WITH FRAME TITLE TEXT-fil.
PARAMETERS:
  rb_dos  RADIOBUTTON GROUP fil DEFAULT 'X', " Save to local
  rb_unix RADIOBUTTON GROUP fil,       " Save to UNIX
  p_path  LIKE rlgrap-filename         " Path to save files to
  DEFAULT 'C:\Users\Nicol�\Documents\SAP\SAP GUI\test\'.
SELECTION-SCREEN END OF BLOCK frm_filen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-low.
  CALL FUNCTION 'F4_PROGRAM'
    EXPORTING
      object             = s_name-low
      suppress_selection = 'X'
    IMPORTING
      result             = s_name-low
    EXCEPTIONS
      OTHERS             = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_name-high.
  CALL FUNCTION 'F4_PROGRAM'
    EXPORTING
      object             = s_name-high
      suppress_selection = 'X'
    IMPORTING
      result             = s_name-high
    EXCEPTIONS
      OTHERS             = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_unam-low.
  PERFORM get_name USING 'S_UNAM-LOW'
  CHANGING s_unam-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_unam-high.
  PERFORM get_name USING 'S_UNAM-HIGH'
  CHANGING s_unam-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cnam-low.
  PERFORM get_name USING 'S_CNAM-LOW'
  CHANGING s_cnam-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cnam-high.
  PERFORM get_name USING 'S_CNAM-HIGH'
  CHANGING s_cnam-high.

TOP-OF-PAGE.
  IF rb_list = 'X'.
    FORMAT COLOR COL_HEADING.

    NEW-LINE.
    WRITE: AT 3 TEXT-h01,
    AT 15 TEXT-h03.
    FORMAT COLOR OFF.
  ENDIF.

AT LINE-SELECTION.
  CHECK rb_list = 'X'.                 " only do in list mode
  READ LINE sy-curow FIELD VALUE mtab_directory-savename.

*-- Read file into an internal table
  PERFORM read_report_from_disk TABLES mtab_program_file
  USING  mtab_directory-savename.
*-- Split table into TADIR entry, report lines, and report text
  PERFORM split_incoming_file TABLES mtab_program_file
    mtab_program_source
    mtab_program_texts
    mtab_program_documentation
  CHANGING trdir
    mstr_thead.
*-- Save all of the data
  PERFORM insert_new_report TABLES mtab_program_source
    mtab_program_texts
    mtab_program_documentation
  USING  trdir
        mstr_thead.

*----------------------------------------------------------------------
* Start of processing
*----------------------------------------------------------------------
START-OF-SELECTION.
  FORMAT COLOR COL_NORMAL.

  IF rb_down = 'X'.
    PERFORM download_reports.
  ELSEIF rb_up = 'X'.
    PERFORM upload_reports.
  ENDIF.

END-OF-SELECTION.

  IF rb_down = 'X'.
    CONCATENATE p_path
    'directory.txt'
    INTO p_path.
    PERFORM save_table_to_file TABLES mtab_directory
    USING  p_path.
  ENDIF.

*---------------------------------------------------------------------*
*       FORM UPLOAD_REPORTS                                           *
*---------------------------------------------------------------------*
FORM upload_reports.

*-- Can upload a reports entered in selection criteria or
*-- select from a list.  List can be from index.txt in same directory
*-- (created by the download) or by reading the first line of each file
*-- in the directory.

  IF rb_file = 'X'. " Upload single program from a file
*-- Read file into an internal table
    PERFORM read_report_from_disk TABLES mtab_program_file
    USING  p_path.
*-- Split table into TADIR entry, report lines, and report text
    PERFORM split_incoming_file TABLES mtab_program_file
      mtab_program_source
      mtab_program_texts
      mtab_program_documentation
    CHANGING trdir
      mstr_thead.
*-- Save all of the data
    PERFORM insert_new_report TABLES mtab_program_source
      mtab_program_texts
      mtab_program_documentation
    USING  trdir
          mstr_thead.

  ELSEIF rb_list = 'X'. " Show list for user to choose from
*-- get list of report names/descriptions from directory text
    CONCATENATE p_path
    'directory.txt'
    INTO p_path.

    PERFORM read_report_from_disk TABLES mtab_directory
    USING  p_path.

    SORT mtab_directory.

*-- Write out list of report names/descriptions
    LOOP AT mtab_directory.
      WRITE:
      / mtab_directory-name UNDER TEXT-h01,
      mtab_directory-desc UNDER TEXT-h03,
      mtab_directory-savename.

    ENDLOOP.
*-- Process user selections for reports to upload.
  ENDIF.

ENDFORM.                               " upload_reports
*---------------------------------------------------------------------*
*       FORM DOWNLOAD_REPORTS                                         *
*---------------------------------------------------------------------*
*       From the user selections, get all programs that meet the      *
*       criteria, and save them in ftab_program_directory.            *
*       Also save the report to disk.                                 *
*---------------------------------------------------------------------*
FORM download_reports.

  DATA:
        lc_full_filename LIKE rlgrap-filename.

*-- The table is put into an internal table because the program will
*-- abend if multiple transfers to a dataset occur within a SELECT/
*-- ENDSELCT (tested on 3.1H)

  SELECT * FROM  trdir
  INTO TABLE mtab_program_trdir
  WHERE  name  IN s_name
  AND    subc  IN s_subc
  AND    cnam  IN s_cnam
  AND    unam  IN s_unam
  AND    cdat  IN s_cdat
  AND    udat  IN s_udat.

  LOOP AT mtab_program_trdir.

*-- Clear out text and source code tables
    CLEAR:
    mtab_program_file,
    mtab_program_source,
    mtab_program_texts,
    mtab_program_documentation.

    REFRESH:
    mtab_program_file,
    mtab_program_source,
    mtab_program_texts,
    mtab_program_documentation.

*-- Get the report
    READ REPORT mtab_program_trdir-name INTO mtab_program_source.

*-- Get the text for the report
    READ TEXTPOOL mtab_program_trdir-name INTO mtab_program_texts.

*-- Get the documentation for the report
    CLEAR dokil.
    SELECT * UP TO 1 ROWS FROM dokil
    WHERE  id          = 'RE'
    AND    object      = mtab_program_trdir-name
    AND    langu       = sy-langu
    AND    typ         = 'E'
    ORDER BY version DESCENDING.
    ENDSELECT.
*-- Documentation exists for this object
    IF sy-subrc = 0.
      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = dokil-id
          langu   = dokil-langu
          object  = dokil-object
          typ     = dokil-typ
          version = dokil-version
        IMPORTING
          head    = mstr_thead
        TABLES
          line    = mtab_program_documentation
        EXCEPTIONS
          OTHERS  = 1.

    ENDIF.

*-- Put the report code and texts into a single file

*-- Put the identifier line in so that the start of the TRDIR line
*-- is marked
    CONCATENATE mc_trdir_identifier
    mtab_program_trdir-name
    INTO mtab_program_file-line.
    APPEND mtab_program_file.

*-- Add the TRDIR line
    mtab_program_file-line = mtab_program_trdir.
    APPEND mtab_program_file.

*-- Put the identifier line in so that the start of the report code
*-- is marked
    CONCATENATE mc_report_identifier
    mtab_program_trdir-name
    INTO mtab_program_file-line.
    APPEND mtab_program_file.

*-- Add the report code
    LOOP AT mtab_program_source.
      mtab_program_file = mtab_program_source.
      APPEND mtab_program_file.
    ENDLOOP.

*-- Put the identifier line in so that the start of the report text
*-- is marked
    CONCATENATE mc_text_identifier
    mtab_program_trdir-name
    INTO mtab_program_file-line.
    APPEND mtab_program_file.

*-- Add the report texts
    LOOP AT mtab_program_texts.
      mtab_program_file = mtab_program_texts(264).
      APPEND mtab_program_file.
    ENDLOOP.

*-- Put the identifier line in so that the start of the THEAD record
*-- is marked
    CONCATENATE mc_thead_identifier
    mtab_program_trdir-name
    INTO mtab_program_file-line.
    APPEND mtab_program_file.

    mtab_program_file = mstr_thead.
    APPEND mtab_program_file.

*-- Put the identifier line in so that the start of the report
*-- documentation is marked
    CONCATENATE mc_doc_identifier
    mtab_program_trdir-name
    INTO mtab_program_file-line.
    APPEND mtab_program_file.

*-- Add the report documentation
    LOOP AT mtab_program_documentation.
      mtab_program_file = mtab_program_documentation.
      APPEND mtab_program_file.
    ENDLOOP.

*-- Make the fully pathed filename that report will be saved to
    CONCATENATE p_path
    mtab_program_trdir-name
    '.txt'
    INTO lc_full_filename.

    PERFORM save_table_to_file TABLES mtab_program_file
    USING  lc_full_filename.

*-- Write out message with Program Name/Description
    READ TABLE mtab_program_texts WITH KEY id = 'R'.
    IF sy-subrc = 0.
      mtab_directory-name = mtab_program_trdir-name.
      mtab_directory-desc = mtab_program_texts-entry.
      mtab_directory-savename = lc_full_filename.
      APPEND mtab_directory.

      WRITE: / mtab_program_trdir-name,
      mtab_program_texts-entry(65) COLOR COL_HEADING.
    ELSE.
      mtab_directory-name = mtab_program_trdir-name.
      mtab_directory-desc = 'No description available'.
      mtab_directory-savename = lc_full_filename.
      APPEND mtab_directory.

      WRITE: / mtab_program_trdir-name.
    ENDIF.

  ENDLOOP.
ENDFORM.                               " BUILD_PROGRAM_DIRECTORY
*---------------------------------------------------------------------*
*       FORM SAVE_TABLE_TO_FILE                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FTAB_TABLE                                                    *
*  -->  F_FILENAME                                                    *
*---------------------------------------------------------------------*
FORM save_table_to_file TABLES ftab_table
USING  f_filename.

  IF rb_dos = 'X'.                  " Save file to presentation server
    CALL FUNCTION 'WS_DOWNLOAD'
      EXPORTING
        filename = f_filename
        filetype = 'ASC'
      TABLES
        data_tab = ftab_table
      EXCEPTIONS
        OTHERS   = 4.

    IF sy-subrc NE 0.
      WRITE: / 'Error opening dataset' COLOR COL_NEGATIVE,
      f_filename COLOR COL_NEGATIVE.
    ENDIF.
  ELSE.                                " Save file to application serve
    OPEN DATASET f_filename FOR OUTPUT ENCODING UTF-8 IN TEXT MODE.
    IF sy-subrc = 0.
      LOOP AT ftab_table.
        TRANSFER ftab_table TO f_filename.
        IF sy-subrc NE 0.
          WRITE: / 'Error writing record to file;' COLOR COL_NEGATIVE,
          f_filename COLOR COL_NEGATIVE.
        ENDIF.
      ENDLOOP.
    ELSE.
      WRITE: / 'Error opening dataset' COLOR COL_NEGATIVE,
      f_filename COLOR COL_NEGATIVE.
    ENDIF.
  ENDIF.                               " End RB_DOS
ENDFORM.                               " SAVE_PROGRAM
*---------------------------------------------------------------------*
*       FORM READ_REPORT_FROM_DISK                                    *
*---------------------------------------------------------------------*
*       Read report into internal table.  Can read from local or      *
*       remote computer                                               *
*---------------------------------------------------------------------*
FORM read_report_from_disk TABLES ftab_table
USING  f_filename.

  DATA:
        lc_message(128) TYPE c.

  CLEAR   ftab_table.
  REFRESH ftab_table.

  IF rb_dos = 'X'.
    TRANSLATE f_filename USING '/\'.   " correct slash for Dos PC file
    CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
        filename            = f_filename
        filetype            = 'ASC'
      TABLES
        data_tab            = ftab_table
      EXCEPTIONS
        conversion_error    = 1
        file_open_error     = 2
        file_read_error     = 3
        invalid_table_width = 4
        invalid_type        = 5
        no_batch            = 6
        unknown_error       = 7
        OTHERS              = 8.
    IF sy-subrc >< 0.
      WRITE: / 'Error reading file from local PC' COLOR COL_NEGATIVE.
    ENDIF.
  ELSEIF rb_unix = 'X'.
    TRANSLATE f_filename USING '\/'.   " correct slash for unix
    OPEN DATASET f_filename FOR INPUT MESSAGE lc_message ENCODING UTF-8 IN TEXT MODE.
    IF sy-subrc = 0.
      DO.
        READ DATASET f_filename INTO ftab_table.
        IF sy-subrc = 0.
          APPEND ftab_table.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      CLOSE DATASET f_filename.
    ELSE.
      WRITE: / 'Error reading file from remote computer'
      COLOR COL_NEGATIVE,
      / lc_message,
      / f_filename.
      sy-subrc = 4.
    ENDIF.
  ENDIF.


ENDFORM.                               " READ_REPORT_FROM_DISK

*---------------------------------------------------------------------*
*       FORM SPLIT_INCOMING_FILE                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FTAB_PROGRAM_FILE                                             *
*  -->  FTAB_PROGRAM_SOURCE                                           *
*  -->  `                                                             *
*  -->  FTAB_PROGRAM_TEXTS                                            *
*---------------------------------------------------------------------*
FORM split_incoming_file TABLES ftab_program_file
  STRUCTURE mtab_program_file
  ftab_program_source
  STRUCTURE mtab_program_source
  ftab_program_texts
  STRUCTURE mtab_program_texts
  ftab_program_documentation
  STRUCTURE mtab_program_documentation
CHANGING fstr_trdir
  fstr_thead.

  DATA:
    lc_datatype(4)  TYPE c,             " Type of data, REPO, TEXP, RDIR
    lc_program_file LIKE mtab_program_file.

  LOOP AT ftab_program_file.
    lc_program_file = ftab_program_file.
    CASE lc_program_file(9).
      WHEN mc_trdir_identifier.
        lc_datatype = mc_trdir_short.
      WHEN mc_report_identifier.
        lc_datatype = mc_report_short.
      WHEN mc_text_identifier.
        lc_datatype = mc_text_short.
      WHEN mc_doc_identifier.
        lc_datatype = mc_doc_short.
      WHEN mc_thead_identifier.
        lc_datatype = mc_thead_short.
      WHEN OTHERS. " Actual contents of report, trdir, or text
        CASE lc_datatype.
          WHEN mc_trdir_short.
            fstr_trdir = ftab_program_file.
          WHEN mc_report_short.
            ftab_program_source = ftab_program_file.
            APPEND ftab_program_source.
          WHEN mc_text_short.
            ftab_program_texts = ftab_program_file(264).
            APPEND ftab_program_texts.
          WHEN mc_thead_short.
            fstr_thead = ftab_program_file.
          WHEN mc_doc_short.
            ftab_program_documentation = ftab_program_file.
            APPEND ftab_program_documentation.
        ENDCASE.
    ENDCASE.
  ENDLOOP.
ENDFORM.                               " SPLIT_INCOMING_FILE
*---------------------------------------------------------------------*
*       FORM INSERT_NEW_REPORT                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FTAB_PROGRAM_SOURCE                                           *
*  -->  FTAB_PROGRAM_TEXTS                                            *
*  -->  F_TRDIR                                                       *
*---------------------------------------------------------------------*
FORM insert_new_report TABLES ftab_program_source
  STRUCTURE mtab_program_source
  ftab_program_texts
  STRUCTURE mtab_program_texts
  ftab_program_documentation
  STRUCTURE mtab_program_documentation
USING  fstr_trdir LIKE trdir
      fstr_thead LIKE mstr_thead.
  DATA:
    lc_obj_name  LIKE e071-obj_name,
    lc_line2(40) TYPE c,
    lc_answer(1) TYPE c.

*-- read trdir to see if the report already exists, if it does, prompt
*-- user to overwrite or abort.
  SELECT SINGLE * FROM trdir WHERE name = fstr_trdir-name.
  IF sy-subrc = 0.                     " Already exists
    CONCATENATE 'want to overwrite report'
    fstr_trdir-name
    INTO lc_line2 SEPARATED BY space.

    CONCATENATE lc_line2
    '?'
    INTO lc_line2.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption  = 'N'
        textline1      = 'The selected report already exists, do you'
        textline2      = lc_line2
        titel          = 'Report already exists'
        cancel_display = space
      IMPORTING
        answer         = lc_answer
      EXCEPTIONS
        OTHERS         = 1.
  ELSE.
    lc_answer = 'J'.
  ENDIF.

  IF lc_answer = 'J'.
*-- Create the TADIR entry.  (TRDIR entry created by INSERT REPORT)
    lc_obj_name = trdir-name.

    CALL FUNCTION 'TR_TADIR_POPUP_ENTRY_E071'
      EXPORTING
        wi_e071_pgmid     = 'R3TR'
        wi_e071_object    = 'PROG'
        wi_e071_obj_name  = lc_obj_name
        wi_tadir_devclass = '$TMP'
      EXCEPTIONS
        exit              = 3
        OTHERS            = 4.

    IF sy-subrc = 0.
*-- Create Report
      INSERT REPORT fstr_trdir-name FROM ftab_program_source.
*-- Create Texts
      INSERT TEXTPOOL fstr_trdir-name FROM ftab_program_texts
      LANGUAGE sy-langu.
*-- Save Documentation
      CALL FUNCTION 'DOCU_UPDATE'
        EXPORTING
          head    = fstr_thead
          state   = 'A'
          typ     = 'E'
          version = '1'
        TABLES
          line    = ftab_program_documentation
        EXCEPTIONS
          OTHERS  = 1.

    ELSE.
      WRITE: / 'Error updating the TADIR entry' COLOR COL_NEGATIVE,
      'Program' COLOR COL_NEGATIVE INTENSIFIED OFF,
      fstr_trdir-name, 'was not loaded into SAP.'
      COLOR COL_NEGATIVE INTENSIFIED OFF.
    ENDIF.
  ELSE.
    WRITE: / fstr_trdir-name COLOR COL_NEGATIVE,
    'was not uploaded into SAP.  Action cancelled by user'
    COLOR COL_NEGATIVE INTENSIFIED OFF.
  ENDIF.
ENDFORM.                               " INSERT_NEW_REPORT
*---------------------------------------------------------------------*
*       FORM GET_NAME                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(F_FIELD)                                                *
*  -->  F_NAME                                                        *
*---------------------------------------------------------------------*
FORM get_name USING VALUE(f_field)
CHANGING f_name.

  DATA: ltab_fields LIKE dynpread OCCURS 0 WITH HEADER LINE,
        lc_prog     LIKE d020s-prog,
        lc_dnum     LIKE d020s-dnum.

  TRANSLATE f_field TO UPPER CASE.

  REFRESH ltab_fields.
  ltab_fields-fieldname = f_field.
  APPEND ltab_fields.
  lc_prog =  sy-repid .
  lc_dnum =  sy-dynnr .
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = lc_prog
      dynumb     = lc_dnum
    TABLES
      dynpfields = ltab_fields
    EXCEPTIONS
      OTHERS     = 01.
  READ TABLE ltab_fields INDEX 1.
  IF sy-subrc EQ 0.
    f_name = ltab_fields-fieldvalue.
    REFRESH ltab_fields.
  ENDIF.

  CALL FUNCTION 'F4_USER'
    EXPORTING
      object = f_name
    IMPORTING
      result = f_name.

ENDFORM.                               " GET_NAME

*TEXPZKBPROGS
* IDIR     File Download Options (File Selection)
* IFIL     File Options
* IFNA     Enter filename below (under File Options)
* IH01     Prog Name
* IH03     Program Description
* ISNG     Upload a single file
* IUDL     Upload to SAP/Download from SAP
* IUPL     File Upload Options
* R        Backup/Restore program source code with texts
* P_PATH          Path to save programs to
* RB_DOS          Files on local computer
* RB_DOWN         Download Programs
* RB_FILE         Upload a single file
* RB_LIST         Select program(s) from a list
* RB_UNIX         Files on remote computer
* RB_UP           Upload Programs to SAP
* S_CDAT           Date Created
* S_CNAM          Created by UserID
* S_NAME          Program Name
* S_SUBC          Program Type
* S_UDAT          Date Changed
* S_UNAM          Last Changed by UserID
*HEADZKBPROGS
* DOKU      ZHRBDC54
*DOKLZKBPROGS

*--- End of Program
%&%& TEXPZDOWN_UP_LOAD
IDIR     File Download Options (File Selection)
IFIL     File Options
IFNA     Enter filename below (under File Options)
IH01     Prog Name
IH03     Program Description
ISNG     Upload a single file
IUDL     Upload to SAP/Download from SAP
IUPL     File Upload Options
R        Program ZDOWN_UP_LOAD
SP_PATH          Path to save programs to
SRB_DOS          Files on local computer
SRB_DOWN         Download Programs
SRB_FILE         Upload a single file
SRB_LIST         Select program(s) from a list
SRB_UNIX         Files on remote computer
SRB_UP           Upload Programs to SAP
SS_CDAT          Date Created
SS_CNAM          Created by UserID
SS_NAME          Program Name
SS_SUBC          Program Type
SS_UDAT          Date Changed
SS_UNAM          Last Changed by UserID
%&%& HEADZDOWN_UP_LOAD
                                                                                                                                                               00000                00000000000000                0000000000000000000000  0
%&%& DOKLZDOWN_UP_LOAD
