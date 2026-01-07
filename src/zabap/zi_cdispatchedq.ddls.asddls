@AbapCatalog.sqlViewName: 'ZI_CDISPATCHEDQV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer PPM Query'
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
define view ZI_CDISPATCHEDQ
  with parameters
    @Environment.systemField: #SYSTEM_DATE
    p_startdate : fis_budat,
    @Environment.systemField: #SYSTEM_DATE
    p_enddate   : fis_budat
  as select from ZI_CDISPATCHED (p_startdate:$parameters.p_startdate,p_enddate:$parameters.p_enddate)
{
      @Consumption.valueHelpDefinition: [{ entity:{ element: 'Customer',
                                                         name: 'I_CUSTOMER' }}]
  key Customer,
  key Plant,
      CustomerName,
      PlantName,
      @Semantics.unitOfMeasure: true
      MaterialBaseUnit,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      dispatched_qty,
      @Semantics.quantity.unitOfMeasure: 'MaterialBaseUnit'
      @DefaultAggregation: #SUM
      return_qty,
      (division(return_qty,dispatched_qty,5) * 1000000) as customer_rppm
}
