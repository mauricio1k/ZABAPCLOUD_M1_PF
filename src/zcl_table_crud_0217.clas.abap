CLASS zcl_table_crud_0217 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_table_crud_0217 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA(ls_worder) = VALUE ztworkorder_0217( work_order_id = '0000000002' ).

    DELETE ztworkorder_0217 FROM @ls_worder.

  ENDMETHOD.

ENDCLASS.
