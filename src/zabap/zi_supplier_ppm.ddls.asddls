@AbapCatalog.sqlViewName: 'ZI_SUPPLIERPV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Supplier PPM'
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
define view ZI_SUPPLIER_PPM
  with parameters
    @Environment.systemField: #SYSTEM_DATE
    p_startdate : fis_budat,
    @Environment.systemField: #SYSTEM_DATE
    p_enddate   : fis_budat
  as select from ZI_SUPPLIERD (p_startdate:$parameters.p_startdate,p_enddate:$parameters.p_enddate)
{
      @Consumption.valueHelpDefinition: [{ entity:{ element: 'Suppier',
                                                    name: 'I_SUPPLIER' }}]
  key Supplier,
  key Plant,
      SupplierName,
      PlantName,
      @Semantics.unitOfMeasure: true
      MaterialBaseUnit,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      received_qty,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      rejected_qty,
      (division(rejected_qty,received_qty,5) * 1000000) as supplier_rppm
}
