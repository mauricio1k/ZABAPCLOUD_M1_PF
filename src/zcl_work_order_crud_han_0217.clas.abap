CLASS zcl_work_order_crud_han_0217 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: ls_work_order  TYPE ztworkorder_0217,
          ls_order_log   TYPE ztorderhist_0217,
          lv_description TYPE zde_change_description_0217,
          mo_validate    TYPE REF TO zcl_work_order_validator_0217.

    METHODS create_Work_order IMPORTING iv_work_order_id  TYPE zde_order_id_0217
                                        iv_customer_id    TYPE zde_customer_id_0217
                                        iv_technician_id  TYPE zde_technician_id_0217
                                        iv_priority       TYPE zde_order_priority_0217
                                        iv_status         TYPE zde_order_status_0217
                                        iv_description    TYPE zde_order_description_0217
                                        iv_creation_date  TYPE zde_date_0217

*                              EXPORTING ev_order_validate TYPE abap_bool
                              RETURNING VALUE(rv_result)  TYPE abap_bool.

    METHODS update_work_order IMPORTING is_work_order    TYPE ztworkorder_0217
                                        is_update_order  TYPE ztworkorder_0217

                              RETURNING VALUE(rv_result) TYPE abap_bool.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_han_0217 IMPLEMENTATION.

  METHOD create_work_order.

* Validation and creation work order

    CREATE OBJECT mo_validate.

    DATA(lv_order_validate) = mo_validate->validate_create_order(   iv_customer_id      = iv_customer_id
                                                                    iv_technician_id    = iv_technician_id
                                                                    iv_priority         = iv_priority ).
    IF lv_order_validate EQ abap_false.
       rv_result = abap_false.
*      ev_order_validate = abap_false.
      RETURN.
    ENDIF.

    ls_work_order = VALUE ztworkorder_0217( work_order_id  = iv_work_order_id
                                            customer_id    = iv_customer_id
                                            technician_id  = iv_technician_id
                                            creation_date  = iv_creation_date
                                            status         = iv_status
                                            priority       = iv_priority
                                            description    = iv_description ).

    INSERT ztworkorder_0217 FROM @ls_work_order.

    IF sy-subrc EQ 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD update_work_order.

*   Validate and update order.
    CREATE OBJECT mo_validate.

    DATA(lv_validate_uporder) = mo_validate->validate_update_order( iv_work_order_id    = is_work_order-work_order_id
                                                                    iv_status           = is_work_order-status  ).

    IF lv_validate_uporder EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE
    FROM ztworkorder_0217
    FIELDS *
    WHERE work_order_id = @is_work_order-work_order_id
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


    DATA(lv_order_validate) = mo_validate->validate_create_order(   iv_customer_id      = ls_work_order-customer_id
                                                                    iv_technician_id    = ls_work_order-technician_id
                                                                    iv_priority         = ls_work_order-priority ).

    IF lv_order_validate EQ abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

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

  ENDMETHOD.

ENDCLASS.
