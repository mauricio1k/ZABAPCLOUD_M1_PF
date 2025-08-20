CLASS zcl_work_order_crud_han_0217 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: ls_work_order TYPE ztworkorder_0217,
          mo_validate   TYPE REF TO zcl_work_order_validator_0217.

    METHODS create_Work_order IMPORTING iv_work_order_id  TYPE zde_order_id_0217
                                        iv_customer_id    TYPE zde_customer_id_0217
                                        iv_technician_id  TYPE zde_technician_id_0217
                                        iv_priority       TYPE zde_order_priority_0217
                                        iv_status         TYPE zde_order_status_0217
                                        iv_description    TYPE zde_order_description_0217
                                        iv_creation_date  TYPE zde_date_0217

                              EXPORTING ev_order_validate TYPE abap_bool
                              RETURNING VALUE(rv_result)  TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_han_0217 IMPLEMENTATION.

  METHOD create_work_order.

* Validation for creation

    CREATE OBJECT mo_validate.

    DATA(lv_order_validate) = mo_validate->validate_create_order(   iv_customer_id      = iv_customer_id
                                                                    iv_technician_id    = iv_technician_id
                                                                    iv_priority         = iv_priority ).
    IF lv_order_validate EQ abap_false.
      ev_order_validate = abap_false.
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

ENDCLASS.
