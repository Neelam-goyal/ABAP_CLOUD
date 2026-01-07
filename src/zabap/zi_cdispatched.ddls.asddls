@AbapCatalog.sqlViewName: 'ZI_CDISPATCHEDV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer dispatched data'
define view ZI_CDISPATCHED
  with parameters
  @Environment.systemField: #SYSTEM_DATE
    p_startdate : fis_budat,
   @Environment.systemField: #SYSTEM_DATE   
    p_enddate   : fis_budat

  as select from I_MaterialDocumentItem_2 as a
    inner join   I_Customer               as b on a.Customer = b.Customer
    inner join   I_Plant                  as c on a.Plant = c.Plant
{
  key a.Customer,
  key a.Plant,
      b.CustomerName,
      c.PlantName,
      @Semantics.unitOfMeasure: true
      a.MaterialBaseUnit,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      sum( case a.GoodsMovementType
      when '601' then a.QuantityInBaseUnit
      when '602' then a.QuantityInBaseUnit * -1 end ) as dispatched_qty,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      sum( case a.GoodsMovementType
      when '653' then a.QuantityInBaseUnit end )      as return_qty


}
where
  (
       a.GoodsMovementType = '601'
    or a.GoodsMovementType = '602'
    or a.GoodsMovementType = '653'
  )
  and  a.Customer          is not initial
  and  a.PostingDate         >= $parameters.p_startdate
  and  a.PostingDate         <= $parameters.p_enddate
group by
  a.Customer,
  a.Plant,
  a.MaterialBaseUnit,
  b.CustomerName,
  c.PlantName
