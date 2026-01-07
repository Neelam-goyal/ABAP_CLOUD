@AbapCatalog.sqlViewName: 'ZV_MAT_STOCK'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Stock'
define view ZI_MATERIAL_STOCK as select from I_MaterialStock_2 as MARD
{
  key MARD.Material,
  key MARD.Plant,
  sum( MARD.MatlWrhsStkQtyInMatlBaseUnit ) as COMP_STOCK
 
} group by MARD.Material, MARD.Plant
