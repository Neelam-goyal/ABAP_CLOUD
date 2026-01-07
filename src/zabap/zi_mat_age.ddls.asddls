@AbapCatalog.sqlViewName: 'ZV_MAT_AGE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Ageing Data'
define view ZI_MAT_AGE

  with parameters
    @Consumption: {
                     defaultValue: 'INR',
                     valueHelpDefinition: [{
                                              entity: {
                                                         name:'I_Currency',
                                                         element:'Currency'
                                                      }
                                          }]
                  }
  
  P_DisplayCurrency : nsdm_display_currency,

  @Environment.systemField: #SYSTEM_DATE
  p_start_date : sydate
    
  as select from I_StockQuantityCurrentValue_2(P_DisplayCurrency : $parameters.P_DisplayCurrency) as mard
  left outer join I_Batch as batch
  on mard.Product = batch.Material and
     mard.Plant   = batch.Plant and
     mard.Batch   = batch.Batch

left outer join I_ProductTypeText as ptype
on  mard.ProductType = ptype.ProductType and ptype.Language = 'E'   

left outer join I_Product as product
on  mard.Product = product.Product

left outer join I_ProductDescription as producttxt
on  mard.Product = producttxt.Product

left outer join I_ExtProdGrpText as ExtProdGrpText
on product.ExternalProductGroup = ExtProdGrpText.ExternalProductGroup and ExtProdGrpText.Language = 'E'

left outer join I_ProductPlantIntlTrd as prodtplant
on mard.Product = prodtplant.Product and mard.Plant = prodtplant.Plant

left outer join I_ProductPlantBasic as prodtplantbase
on mard.Product = prodtplantbase.Product and mard.Plant = prodtplantbase.Plant
{

  key mard.Product,
  key mard.Plant,
  key mard.StorageLocation,
  key mard.Batch,
  $parameters.p_start_date as CurrentDate,
  mard.Supplier,
  mard.SDDocument,
  mard.SDDocumentItem,
  mard.WBSElementInternalID,
  mard.Customer,
  mard.SpecialStockIdfgStockOwner,
  mard.InventoryStockType,
  mard.InventorySpecialStockType,
  mard.MaterialBaseUnit,
  mard.Currency,
  mard.DisplayCurrency,
  mard.ValuationAreaType,
  mard.MatlWrhsStkQtyInMatlBaseUnit,
  mard.StockValueInCCCrcy,
  
  case when mard.StockValueInCCCrcy <> 0
  then division( mard.StockValueInCCCrcy, mard.MatlWrhsStkQtyInMatlBaseUnit, 2 ) 
  end as UnitPrice,
  
  mard.StockValueInDisplayCurrency,
  mard.ProductGroup,
  mard.ProductType,
  batch.ManufactureDate,
  batch.LastGoodsReceiptDate,

  ptype.MaterialTypeName as PRODUCTGROUPNAME,  
  product.ExternalProductGroup,
  producttxt.ProductDescription, 
  ExtProdGrpText.ExternalProductGroupName,
  mard._StorageLocation.StorageLocationName,
  prodtplant.ConsumptionTaxCtrlCode,
  prodtplantbase.ProfitCenter,
  
  case when batch.ManufactureDate <> '00000000'
  then dats_days_between(batch.ManufactureDate, :p_start_date) 
  when batch.ManufactureDate = '00000000'
  then dats_days_between(batch.LastGoodsReceiptDate, :p_start_date) 
  end as NoOfDays

//  dats_days_between(batch.ManufactureDate, :p_start_date) as nodaysmfg,
//  dats_days_between(batch.LastGoodsReceiptDate, :p_start_date) as nodayrecpt  
  
  //mard._Currency,
  //mard._Customer,
  //mard._InventorySpecialStockType,
  //mard._InventoryStockType,
  //mard._Plant,
  //mard._Product,
  //mard._ProductGroup,
  //mard._ProductType,
  //mard._SpecialStockIdfgStockOwner,
  //mard._StorageLocation,
  //mard._Supplier,
  //mard._UnitOfMeasure

}
where mard.StockValueInCCCrcy > 0
