@AbapCatalog.sqlViewName: 'ZI_PRODUCTV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product Data'
define view ZI_PRODUCTD
  with parameters
    @Environment.systemField: #SYSTEM_DATE
    p_startdate : fis_budat,
    @Environment.systemField: #SYSTEM_DATE
    p_enddate   : fis_budat

  as select from    I_MaterialDocumentItem_2                                                               as a
    inner join      I_ProductPlantBasic                                                                    as b on  a.Material = b.Product
                                                                                                                and a.Plant    = b.Plant
    inner join      I_ProductText                                                                          as d on  b.Product  = d.Product
                                                                                                                and d.Language = 'E'
    inner join      I_Plant                                                                                as c on a.Plant = c.Plant
   
{
  key a.Material,
  key a.Plant,
      d.ProductName,
      c.PlantName,
      b.ProductionSupervisor,
      @Semantics.unitOfMeasure: true
      a.MaterialBaseUnit,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      sum( case a.GoodsMovementType
      when '101' then a.QuantityInBaseUnit end ) as production_qty

}
where
  (
       a.GoodsMovementType = '101'
  )
  and  a.Material          is not initial
  and  a.OrderID           is not initial
  and  a.PostingDate       >= $parameters.p_startdate
  and  a.PostingDate       <= $parameters.p_enddate
group by
  a.Material,
  a.Plant,
  a.MaterialBaseUnit,
  d.ProductName,
  c.PlantName,
  b.ProductionSupervisor
