CLASS zcl_table_crud_0217 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    "DATA: lt_technician TYPE STANDARD TABLE OF zttechnician0217.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_table_crud_0217 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*    DATA(ls_worder) = VALUE ztworkorder_0217( work_order_id = '0000000002' ).
*
*    DELETE ztworkorder_0217 FROM @ls_worder.


*    SELECT COUNT( * )
*    FROM ztworkorder_0217
*    WHERE work_order_id EQ '0000000002'
*    INTO @DATA(lv_records).
*
*    out->write( lv_records ).

*     lt_technician = VALUE #( ( technician_id  = '00000003'
*                               name           = 'Oscar Jurado'
*                               specialty      = 'Software Engineer'   ) ).
*
*     INSERT zttechnician0217 FROM TABLE @lt_technician.
*
*    DATA: lv_initialdate TYPE zde_date_0217,
*          lv_enddate     TYPE zde_date_0217,
*          lv_customer    TYPE zde_customer_id_0217,
*          lv_status      TYPE zde_order_status_0217,
*          lv_priority    TYPE zde_order_priority_0217.
*
*
*    lv_initialdate = ''. " '20250820'.
*    lv_enddate  = '20250825'.
*    lv_customer = '10000005'.
*    lv_status   = 'PE'.
*    lv_priority = 'A'.
*
*    IF lv_initialdate EQ ''.
*      lv_initialdate = '19000101'.
*    ENDIF.
*
*
*    IF lv_enddate EQ ''.
*      lv_enddate = cl_abap_context_info=>get_system_date( ).
*    ENDIF.
*
*    IF lv_customer EQ ''.
*      lv_customer = '%'.
*    ENDIF.
*
*    IF lv_status EQ ''.
*      lv_status = '%'.
*    ENDIF.
*
*    IF lv_priority EQ ''.
*      lv_priority = '%'.
*    ENDIF.
*
*    SELECT
*    FROM ztworkorder_0217
*    FIELDS *
*    WHERE creation_date GE @lv_initialdate
*        AND customer_id EQ ANY ( SELECT FROM ztworkorder_0217
*                           FIELDS customer_id
*                           WHERE customer_id LIKE @lv_customer )
*        AND status EQ ANY ( SELECT FROM ztworkorder_0217
*                           FIELDS status
*                           WHERE status EQ @lv_status )
*        AND priority EQ ANY ( SELECT FROM ztworkorder_0217
*                           FIELDS priority
*                           WHERE priority EQ @lv_priority )
*    INTO TABLE @DATA(lt_wolist).
*    out->write( lt_wolist ).


*    SELECT
*    FROM ztworkorder_0217
*    FIELDS *
*    WHERE creation_date GE @lv_initialdate
*        AND customer_id EQ ANY ( SELECT FROM ztworkorder_0217
*                           FIELDS customer_id
*                           WHERE customer_id LIKE '%' )
*    INTO TABLE @DATA(lt_wolist0).
*    out->write( lt_wolist0 ).
*
*
*
*    SELECT
*    FROM ztworkorder_0217
*    FIELDS *
*    WHERE creation_date GE @lv_initialdate
*        AND EXISTS ( SELECT FROM ztworkorder_0217
*                           FIELDS customer_id
*                           WHERE customer_id EQ @lv_customer )
*    INTO TABLE @DATA(lt_wolist2).
*    out->write( lt_wolist2 ).
*
*    SELECT
*    FROM ztworkorder_0217
*    FIELDS *
*    WHERE creation_date IN ( SELECT FROM ztworkorder_0217
*                             FIELDS creation_date
*                             WHERE  creation_date GE @lv_initialdate
*                             AND    creation_date LE @lv_enddate )
*    INTO TABLE @DATA(lt_wolist3).
*
*    out->write( lt_wolist3 ).

    "DATA lv_country_code TYPE land1 VALUE 'en'.
    DATA lv_order_id TYPE zde_order_id_0217 VALUE '0000000004'.

    AUTHORITY-CHECK OBJECT 'ZAOORD0217'
*       ID 'ZAFWO0217' FIELD lv_order_id,
       ID 'actvt'      FIELD '01'.


*    AUTHORITY-CHECK OBJECT 'dmo/trvl'
*       ID '/dmo/cntry' FIELD lv_country_code
*       ID 'actvt'      FIELD '01'.

    DATA(lv_granted) = COND #( WHEN sy-subrc = 0 THEN abap_true
                                                 ELSE abap_false ).

    IF lv_granted EQ abap_true.
      out->write( 'yes' ).
    ELSE.
      out->write( 'no' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
