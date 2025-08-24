CLASS zcl_work_order_crud_han_0217 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    DATA: ls_work_order  TYPE ztworkorder_0217,
          ls_order_log   TYPE ztorderhist_0217,
          lt_work_order  TYPE TABLE OF ztworkorder_0217,
          lv_description TYPE zde_change_description_0217,
          lv_initialdate TYPE zde_date_0217,
          lv_enddate     TYPE zde_date_0217,
          lv_customer    TYPE zde_customer_id_0217,
          lv_status      TYPE zde_order_status_0217,
          lv_priority    TYPE zde_order_priority_0217,
          mo_validate    TYPE REF TO zcl_work_order_validator_0217.

    TYPES tt_workorder   TYPE STANDARD TABLE OF ztworkorder_0217.


    METHODS create_Work_order   IMPORTING is_work_order    TYPE ztworkorder_0217
                                EXPORTING rv_message       TYPE string
                                RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS update_work_order   IMPORTING is_work_order    TYPE ztworkorder_0217
                                          is_update_order  TYPE ztworkorder_0217
                                EXPORTING rv_message       TYPE string
                                RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS delete_work_order   IMPORTING iv_work_order_id TYPE zde_order_id_0217
                                          iv_status        TYPE zde_order_status_0217
                                EXPORTING rv_message       TYPE string
                                RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS read_work_order     IMPORTING iv_initialdate   TYPE zde_date_0217
                                          iv_enddate       TYPE zde_date_0217
                                          iv_customer      TYPE zde_customer_id_0217
                                          iv_status        TYPE zde_order_status_0217
                                          iv_priority      TYPE zde_order_priority_0217
                                EXPORTING rt_workorder     TYPE tt_workorder
                                          rv_message       TYPE string
                                RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS autority_check     IMPORTING iv_activity      TYPE c
                               EXPORTING rv_message       TYPE string
                               RETURNING VALUE(rv_result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_han_0217 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

  ENDMETHOD.


  METHOD create_work_order.

* Validation and creation work order

    CREATE OBJECT mo_validate.

    DATA(lv_autority_check) = mo_validate->autority_check( EXPORTING     iv_activity   = '01'
                                                           IMPORTING     rv_message    = rv_message ).
    IF lv_autority_check EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.


    CREATE OBJECT mo_validate.

    DATA(lv_order_validate) = mo_validate->validate_create_order( EXPORTING     iv_customer_id      = is_work_order-customer_id
                                                                                iv_technician_id    = is_work_order-technician_id
                                                                                iv_priority         = is_work_order-priority
                                                                                iv_status           = is_work_order-status
                                                                  IMPORTING     rv_message          = rv_message ).
    IF lv_order_validate EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    TRY.
        INSERT ztworkorder_0217 FROM @is_work_order.

      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        rv_message = lx_sql_db->get_text( ).
    ENDTRY.

    IF sy-subrc EQ 0.
      rv_result = abap_true.
    ELSE.
      rv_result  = abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD update_work_order.

    CREATE OBJECT mo_validate.

    DATA(lv_autority_check) = mo_validate->autority_check( EXPORTING     iv_activity   = '02'
                                                           IMPORTING     rv_message    = rv_message ).
    IF lv_autority_check EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

*   Validate and update order.
    CREATE OBJECT mo_validate.

    DATA(lv_validate_uporder) = mo_validate->validate_update_order( EXPORTING   iv_work_order_id    = is_work_order-work_order_id
                                                                                iv_status           = is_work_order-status
                                                                    IMPORTING   rv_message          = rv_message  ).

    IF lv_validate_uporder EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE
    FROM ztworkorder_0217
    FIELDS *
    WHERE work_order_id EQ @is_work_order-work_order_id
    INTO @ls_work_order.

    IF is_update_order-technician_id NE ''.
      lv_description = |'Technical updated from { ls_work_order-technician_id } to { is_update_order-technician_id }'| .
      ls_work_order-technician_id = is_update_order-technician_id.

    ENDIF.

    IF is_update_order-priority NE ''.
      lv_description = |'Priority updated from { ls_work_order-priority } to { is_update_order-priority }'| .
      ls_work_order-priority = is_update_order-priority.

    ENDIF.

    IF is_update_order-status NE ''.
      lv_description = |'Status updated from { ls_work_order-status } to { is_update_order-status }'| .
      ls_work_order-status = is_update_order-status.

    ENDIF.

    IF is_update_order-description NE ''.
      lv_description = |'description updated from { ls_work_order-description } to { is_update_order-description }'| .
      ls_work_order-description = is_update_order-description.
    ENDIF.


    DATA(lv_order_validate) = mo_validate->validate_create_order(   EXPORTING   iv_customer_id      = is_work_order-customer_id
                                                                                iv_technician_id    = is_work_order-technician_id
                                                                                iv_priority         = is_work_order-priority
                                                                                iv_status           = is_work_order-status
                                                                    IMPORTING   rv_message          = rv_message  ).

    IF lv_order_validate EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    TRY.

        UPDATE ztworkorder_0217 FROM @ls_work_order.

        SELECT COUNT( * )
        FROM ztorderhist_0217
        WHERE work_order_id EQ @is_work_order-work_order_id
        INTO @DATA(lv_records).

        ls_order_log = VALUE ztorderhist_0217(  history_id          = is_work_order-work_order_id && lv_records
                                                work_order_id       = is_work_order-work_order_id
                                                modification_date   = cl_abap_context_info=>get_system_date( )
                                                change_description  = lv_description ).

        INSERT ztorderhist_0217 FROM @ls_order_log.

        IF sy-subrc EQ 0.
          rv_result = abap_true.
        ELSE.
          rv_result = abap_false.
        ENDIF.

      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        rv_message = lx_sql_db->get_text( ).
    ENDTRY.

  ENDMETHOD.

  METHOD delete_work_order.

    CREATE OBJECT mo_validate.

    DATA(lv_autority_check) = mo_validate->autority_check( EXPORTING     iv_activity   = '06'
                                                           IMPORTING     rv_message    = rv_message ).
    IF lv_autority_check EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    CREATE OBJECT mo_validate.

    DATA(lv_validate_delete) = mo_validate->validate_delete_order( EXPORTING    iv_work_order_id = iv_work_order_id
                                                                                iv_status        = iv_status
                                                                   IMPORTING    rv_message       = rv_message  ).

    IF lv_validate_delete EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    TRY.

        ls_work_order = VALUE ztworkorder_0217( work_order_id = iv_work_order_id ).

        DELETE ztworkorder_0217 FROM @ls_work_order.

        SELECT COUNT( * )
        FROM ztorderhist_0217
        WHERE work_order_id EQ @iv_work_order_id
        INTO @DATA(lv_records).

        DATA: lv_user_id TYPE sy-uname.
        lv_user_id = sy-uname.


        ls_order_log = VALUE ztorderhist_0217(  history_id          = iv_work_order_id && lv_records
                                                work_order_id       = iv_work_order_id
                                                modification_date   = cl_abap_context_info=>get_system_date( )
                                                change_description  = |User { lv_user_id } has deleted WO: { iv_work_order_id } | ).

        INSERT ztorderhist_0217 FROM @ls_order_log.

        IF sy-subrc EQ 0.
          rv_result = abap_true.
        ELSE.
          rv_result = abap_false.
        ENDIF.

      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        rv_message = lx_sql_db->get_text( ).
    ENDTRY.

  ENDMETHOD.



  METHOD read_work_order.

    CREATE OBJECT mo_validate.

    DATA(lv_autority_check) = mo_validate->autority_check( EXPORTING     iv_activity   = '03'
                                                           IMPORTING     rv_message    = rv_message ).
    IF lv_autority_check EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.


    IF iv_initialdate EQ ''.
      lv_initialdate = '19000101'.
    ELSE.
      lv_initialdate = iv_initialdate.
    ENDIF.


    IF iv_enddate EQ ''.
      lv_enddate = cl_abap_context_info=>get_system_date( ).
    ELSE.
      lv_enddate = iv_enddate.
    ENDIF.

    IF iv_customer EQ ''.
      lv_customer = '%'.
    ELSE.
      lv_customer = iv_customer.
    ENDIF.

    IF iv_status EQ ''.
      lv_status = '%'.
    ELSE.
      lv_status = iv_status.
    ENDIF.

    IF iv_priority EQ ''.
      lv_priority = '%'.
    ELSE.
      lv_priority = iv_priority.
    ENDIF.


    TRY.
        SELECT
        FROM ztworkorder_0217
        FIELDS *
        WHERE creation_date GE @lv_initialdate
            AND customer_id EQ ANY ( SELECT FROM ztworkorder_0217
                               FIELDS customer_id
                               WHERE customer_id LIKE @lv_customer )
            AND status EQ ANY ( SELECT FROM ztworkorder_0217
                               FIELDS status
                               WHERE status LIKE @lv_status )
            AND priority EQ ANY ( SELECT FROM ztworkorder_0217
                               FIELDS priority
                               WHERE priority LIKE @lv_priority )
           INTO TABLE @lt_work_order.


        IF sy-subrc EQ 0.
          rt_workorder = lt_work_order.
          rv_result = abap_true.
        ELSE.
          rv_message = 'No records found for these search parameters'.
          rv_result = abap_false.
        ENDIF.

      CATCH cx_sy_open_sql_db INTO DATA(lx_sql_db).
        rv_message = lx_sql_db->get_text( ).
    ENDTRY.


  ENDMETHOD.

  METHOD autority_check.



  ENDMETHOD.


ENDCLASS.
