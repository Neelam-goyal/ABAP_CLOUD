@AbapCatalog.sqlViewName: 'ZI_PSCRAPV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product Scrap data'
define view ZI_PRODUCTDSCRAP
with parameters
 @Environment.systemField: #SYSTEM_DATE
    p_startdate : fis_budat,
    @Environment.systemField: #SYSTEM_DATE
    p_enddate   : fis_budat

  as select from I_MaterialDocumentItem_2 as a
{
  key a.Material,
  key a.Plant,
      @Semantics.unitOfMeasure: true
      a.MaterialBaseUnit,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      sum( case a.GoodsMovementType
      when '555' then a.QuantityInBaseUnit end ) as scrap_qty


}
where
  (
    a.GoodsMovementType = '555'
  )
  and  a.Material          is not initial
  and  a.PostingDate       >= $parameters.p_startdate
  and  a.PostingDate       <= $parameters.p_enddate
group by
  a.Material,
  a.Plant,
  a.MaterialBaseUnit
  
