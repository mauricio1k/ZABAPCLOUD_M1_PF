CLASS zcl_work_order_crud_test_0217 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    DATA: mo_order_crud TYPE REF TO zcl_work_order_crud_han_0217.

    DATA: lt_technician TYPE STANDARD TABLE OF zttechnician0217,
          lt_customer   TYPE STANDARD TABLE OF ztcustomer_0217,
          lt_priority   TYPE STANDARD TABLE OF ztpriority_0217,
          lt_status     TYPE STANDARD TABLE OF ztstatus_0217.


    METHODS:
      update_other_tables,
      test_create_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_work_order_crud_test_0217 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    test_create_work_order( out ).
*    update_other_tables(  ).

  ENDMETHOD.

  METHOD test_create_work_order.

    CREATE OBJECT mo_order_crud.

    DATA(ls_work_order) = VALUE ztworkorder_0217(
    work_order_id  = '0000000002'
    customer_id    = '10000003'
    technician_id  = '00000004'
    creation_date  = cl_abap_context_info=>get_system_date( )
    status         = 'PE'
    priority       = 'A'
    description    = 'Check the gearbox breather and oil level'
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

  METHOD update_other_tables.

    lt_technician = VALUE #( ( technician_id  = '00000001'
                               name           = 'Kevin Salgado'
                               specialty      = 'Mechatronic'   )

                           ( technician_id    = '00000002'
                               name           = 'Diego Prieto'
                               specialty      = 'Mechanical Engineer'   )

                           ( technician_id    = '00000004'
                               name           = 'Barandon Rodríguez'
                               specialty      = 'Development analyst'  )

                           ( technician_id    = '00000005'
                               name           = 'Juan Carlos Muñoz'
                               specialty      = 'Electronic Engineer'   )

                           ( technician_id    = '00000006'
                               name           = 'Camilo Rodriguez'
                               specialty      = 'Industrial Engineer'   ) ).

    INSERT zttechnician0217 FROM TABLE @lt_technician.



    lt_customer = VALUE #( (  customer_id  = '10000001'
                               name        = 'Barton, Wunsch and Thompson'
                               address     = '47 W 13th St, New York, NY 10011, USA'
                               phone       = '(206) 342-8631' )

                           (  customer_id  = '10000002'
                               name        = 'Cummerata - Corkery'
                               address     = '20 Cooper Square, New York, NY 10003, USA'
                               phone       = '(717) 550-1675' )

                           (  customer_id  = '10000003'
                               name        = 'Kling Group'
                               address     = '1 E 2nd St, New York, NY 10003, USA'
                               phone       = '(248) 762-0356' )

                           (  customer_id  = '10000004'
                               name        = 'Champlin - Wunsch'
                               address     = '75 3rd Ave, New York, NY 10003, USA'
                               phone       = '(253) 644-2182' )

                           (  customer_id  = '10000005'
                               name        = 'Russel - Toy'
                               address     = 'Metrotech Center, Brooklyn, NY 11201, USA'
                               phone       = '(212) 658-3916' ) ).

    INSERT ztcustomer_0217 FROM TABLE @lt_customer.



    lt_priority = VALUE #( (  priority_code        = 'A'
                              priority_description = 'High Priotity' )

                           ( priority_code         = 'B'
                              priority_description = 'Low Priotity' ) ).

    INSERT ztpriority_0217 FROM TABLE @lt_priority.



    lt_status = VALUE #( ( status_code          = 'PE'
                            status_description  = 'Pending' )

                         ( status_code          = 'CO'
                            status_description  = 'Completed' ) ).

    INSERT ztstatus_0217 FROM TABLE @lt_status.



  ENDMETHOD.

ENDCLASS.
