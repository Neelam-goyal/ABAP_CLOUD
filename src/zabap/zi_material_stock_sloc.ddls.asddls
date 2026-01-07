@AbapCatalog.sqlViewName: 'ZV_MATS_STOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Location wise material stock'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_MATERIAL_STOCK_SLOC as select from I_MaterialStock_2 as MARD
{
  key MARD.Material,
  key MARD.Plant,
  key MARD.StorageLocation,
  cast(sum( MARD.MatlWrhsStkQtyInMatlBaseUnit ) as abap.quan(13,2) ) as COMP_STOCK 
   
} group by MARD.Material, MARD.Plant, MARD.StorageLocation
