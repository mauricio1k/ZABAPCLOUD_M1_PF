CLASS zcl_work_order_validator_0217 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      validate_create_order IMPORTING iv_customer_id   TYPE zde_customer_id_0217
                                      iv_technician_id TYPE zde_technician_id_0217
                                      iv_priority      TYPE zde_order_priority_0217
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_update_order IMPORTING iv_work_order_id TYPE zde_order_id_0217
                                      iv_status        TYPE zde_order_status_0217
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_delete_order IMPORTING iv_work_order_id TYPE zde_order_id_0217
                                      iv_status        TYPE zde_order_status_0217
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,

      validate_status_and_priority IMPORTING iv_status       TYPE zde_order_status_0217
                                             iv_priority     TYPE zde_order_priority_0217
                                   RETURNING VALUE(rv_valid) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS check_customer_exists
      IMPORTING
        iv_customer_id  TYPE zde_customer_id_0217
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS check_technician_exists
      IMPORTING
        iv_technician_id TYPE zde_technician_id_0217
      RETURNING
        VALUE(r_result)  TYPE abap_bool.

    METHODS check_priority_valid
      IMPORTING
        iv_priority     TYPE zde_order_priority_0217
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS check_order_exists
      IMPORTING
        iv_work_order_id TYPE zde_order_id_0217
      RETURNING
        VALUE(r_result)  TYPE abap_bool.

    METHODS check_order_history
      IMPORTING
        iv_work_order_id TYPE zde_order_id_0217
      RETURNING
        VALUE(r_result)  TYPE abap_bool.

ENDCLASS.

CLASS zcl_work_order_validator_0217 IMPLEMENTATION.

  METHOD validate_create_order.

    " Check if customer exists
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

*    " Check if priority is valid
    DATA(lv_priority_valid) = check_priority_valid( iv_priority ).
    IF lv_priority_valid IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_update_order.

    " Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)
    IF iv_status NE 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_delete_order.

    " Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is "PE" (Pending)
    IF iv_status NE 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS NOT INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_status_and_priority.

  ENDMETHOD.




  METHOD check_customer_exists.

    SELECT SINGLE FROM ztcustomer_0217
    FIELDS customer_id
    WHERE customer_id EQ @iv_customer_id
    INTO @DATA(lv_customer_id).

    r_result = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD check_technician_exists.

    SELECT SINGLE
    FROM zttechnician0217
    FIELDS technician_id
    WHERE technician_id EQ @iv_technician_id
    INTO @DATA(lv_technician_id).

    r_result = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_true ELSE abap_false ).


  ENDMETHOD.


  METHOD check_priority_valid.

    SELECT SINGLE
    FROM ztpriority_0217
    FIELDS priority_code
    WHERE priority_code EQ @iv_priority
    INTO @DATA(lv_priority_valid).

    r_result = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD check_order_exists.

    SELECT SINGLE
    FROM ztworkorder_0217
    FIELDS work_order_id
    WHERE work_order_id EQ @iv_work_order_id
    INTO @DATA(lv_order_exist).

    r_result = COND abap_bool( WHEN sy-subrc EQ 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD check_order_history.

    SELECT SINGLE
    FROM ztORDERHIST_0217
    FIELDS  history_id
    WHERE work_order_id EQ @iv_work_order_id
    INTO @DATA(lv_records).

    r_result = COND abap_bool( WHEN sy-subrc NE 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.

ENDCLASS.
