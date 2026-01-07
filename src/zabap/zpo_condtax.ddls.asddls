@AbapCatalog.sqlViewName: 'ZVPO_CONDTAX'
@AbapCatalog.compiler.compareFilter: true
//@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PO Condition TAX Amount'
define view ZPO_condtax as select from I_PurOrdItmPricingElementAPI01 as cond
inner join I_PurchaseOrderItemAPI01 as po_item on cond.PurchaseOrder = po_item.PurchaseOrder 
and cond.PurchaseOrderItem = po_item.PurchaseOrderItem 
{

key cond.PurchaseOrder,
 cond.PurchaseOrderItem,
     po_item.TaxCode,
     cond.ConditionType,
@Semantics.currencyCode: true
cond.ConditionCurrency, 
@Semantics.amount.currencyCode: 'ConditionCurrency'
cond.ConditionAmount,

( case  po_item.TaxCode when  'A1' then '05'   
                        when 'A2' then  '12' 
                        when 'A3' then  '18' 
                        when 'A4' then  '28'
                        when 'A5' then  '05'   
                        when 'A6' then  '12' 
                        when 'A7' then  '18' 
                        when 'A8' then  '28'                                               
                        when 'I1' then  '05'  
                        when 'I2' then  '12' 
                        when 'I3' then  '18' 
                        when 'I4' then  '28'     
                       else ''  end ) as rate 


} where ( cond.ConditionType  = 'FGW1' or cond.ConditionType  = 'FQU1'  or cond.ConditionType  = 'FVA1'  or
          cond.ConditionType  = 'ZFV1' or cond.ConditionType  = 'ZFVG'  or cond.ConditionType  = 'ZFRA'  or   
          cond.ConditionType  = 'ZIN1' or cond.ConditionType  = 'ZIN2'  or cond.ConditionType  = 'ZOH1'  or 
          cond.ConditionType  = 'ZOH2' or cond.ConditionType  = 'ZOHG'  or cond.ConditionType  = 'ZOHA'  or 
          cond.ConditionType  = 'ZPK2' or cond.ConditionType  = 'ZPK1'  or cond.ConditionType  = 'ZPKG'  or  
          cond.ConditionType  = 'ZPKA' ) and 
        ( po_item.TaxCode  = 'A1'      or po_item.TaxCode     =  'A2'   or
          po_item.TaxCode     = 'A3'   or    po_item.TaxCode  = 'A4'    or po_item.TaxCode     =  'A5'   or
          po_item.TaxCode     = 'A6'   or    po_item.TaxCode  = 'A7'    or po_item.TaxCode     =  'A8'   or
          po_item.TaxCode     = 'I1'   or    po_item.TaxCode  = 'I2'    or po_item.TaxCode     =  'I3'   or
          po_item.TaxCode     = 'I4' )
