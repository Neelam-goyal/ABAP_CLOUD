@AbapCatalog.sqlViewName: 'ZI_PRODUCTPV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'In-House PPM'
@Analytics: {
dataCategory: #CUBE,
internalName:#LOCAL
}
@ObjectModel: {
usageType: {
dataClass: #MIXED,
serviceQuality: #D,
sizeCategory: #XXL
},
supportedCapabilities: [ #ANALYTICAL_PROVIDER, #SQL_DATA_SOURCE, #CDS_MODELING_DATA_SOURCE ],
modelingPattern: #ANALYTICAL_CUBE
}
define view ZI_PRODUCT_PPM
  with parameters
    @Environment.systemField: #SYSTEM_DATE
    p_startdate : fis_budat,
    @Environment.systemField: #SYSTEM_DATE
    p_enddate   : fis_budat
  as select from    ZI_PRODUCTD (p_startdate:$parameters.p_startdate,p_enddate:$parameters.p_enddate)      as a
    left outer join ZI_PRODUCTDSCRAP (p_startdate:$parameters.p_startdate,p_enddate:$parameters.p_enddate) as b on  a.Material = b.Material
                                                                                                                and a.Plant    = b.Plant
{
  key a.Material,
  key a.Plant,
      a.ProductName,
      a.PlantName,
      a.ProductionSupervisor,
      @Semantics.unitOfMeasure: true
      a.MaterialBaseUnit,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      a.production_qty,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      b.scrap_qty,
      (division(b.scrap_qty,a.production_qty,5) * 1000000) as product_rppm
}
