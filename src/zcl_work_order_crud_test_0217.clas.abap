CLASS zcl_work_order_crud_test_0217 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    DATA: mo_order_crud TYPE REF TO zcl_work_order_crud_han_0217.

    METHODS:
      test_create_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_test_0217 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
  test_create_work_order( out ).

  ENDMETHOD.

  METHOD test_create_work_order.

    CREATE OBJECT mo_order_crud.

    DATA(ls_work_order) = VALUE ztworkorder_0217(
    work_order_id  = '0000000001'
    customer_id    = '00000001'
    technician_id  = '12345678'
    creation_date  = cl_abap_context_info=>get_system_date( )
    status         = 'PE'
    priority       = 'A'
    description    = 'Check the bucket elevator belt tension 101'
  ).

    DATA(lv_result) = mo_order_crud->create_work_order( iv_work_order_id   = ls_work_order-work_order_id
                                                    iv_customer_id         = ls_work_order-customer_id
                                                    iv_technician_id       = ls_work_order-technician_id
                                                    iv_priority            = ls_work_order-priority
                                                    iv_status              = ls_work_order-status
                                                    iv_description         = ls_work_order-description
                                                    iv_creation_date       = ls_work_order-creation_date ).

    IF lv_result = abap_true.
      io_out->write( |The work order was created | ).
    ELSE.
      io_out->write( |Work order has not been created | ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
