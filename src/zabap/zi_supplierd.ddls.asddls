@AbapCatalog.sqlViewName: 'ZI_SUPPLIERV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Supplier Data'
define view ZI_SUPPLIERD
   with parameters
  @Environment.systemField: #SYSTEM_DATE
    p_startdate : fis_budat,
   @Environment.systemField: #SYSTEM_DATE   
    p_enddate   : fis_budat

  as select from I_MaterialDocumentItem_2 as a
    inner join   I_Supplier               as b on a.Supplier = b.Supplier
    inner join   I_Plant                  as c on a.Plant = c.Plant
{
  key a.Supplier,
  key a.Plant,
      b.SupplierName,
      c.PlantName,
      @Semantics.unitOfMeasure: true
      a.MaterialBaseUnit,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      sum( case a.GoodsMovementType
      when '101' then a.QuantityInBaseUnit end )  as received_qty,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      sum( case a.GoodsMovementType
      when '350' then a.QuantityInBaseUnit end )      as rejected_qty


}
where
  (
       a.GoodsMovementType = '101'
    or a.GoodsMovementType = '350'
  )
  and  a.Supplier          is not initial
  and  a.PurchaseOrder     is not initial
  and  a.PostingDate         >= $parameters.p_startdate
  and  a.PostingDate         <= $parameters.p_enddate
group by
  a.Supplier,
  a.Plant,
  a.MaterialBaseUnit,
  b.SupplierName,
  c.PlantName
