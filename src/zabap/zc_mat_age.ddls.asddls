@AbapCatalog.sqlViewName: 'ZCV_MAT_AGE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material Ageing Data'
define view ZC_MAT_AGE

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
    p_start_date      : sydate

  as select from ZI_MAT_AGE(P_DisplayCurrency : $parameters.P_DisplayCurrency, p_start_date : $parameters.p_start_date) as ag
{

      @EndUserText.label: 'Material'
  key ag.Product,

      @EndUserText.label: 'Plant'
  key ag.Plant,

      @EndUserText.label: 'Storage Location'
  key ag.StorageLocation,

      @EndUserText.label: 'Batch'
  key ag.Batch,

      @EndUserText.label: 'Product Description'
      ag.ProductDescription,


      @EndUserText.label: 'Current Date'
      ag.CurrentDate,

      @EndUserText.label: 'UOM'
      ag.MaterialBaseUnit,

      @EndUserText.label: 'Stock'
      ag.MatlWrhsStkQtyInMatlBaseUnit,

      @EndUserText.label: 'Stock Value'
      ag.StockValueInCCCrcy,

      @EndUserText.label: 'Unit Price'
      ag.UnitPrice,

      @EndUserText.label: 'Currency'
      ag.Currency,

      @EndUserText.label: 'Material Type'
      ag.ProductType,

      @EndUserText.label: 'Date of Posting'
      ag.ManufactureDate,

      @EndUserText.label: 'Received Date of Stcok'
      ag.LastGoodsReceiptDate,

      @EndUserText.label: 'No Of Days'
      ag.NoOfDays,

      @EndUserText.label: 'Material Type Description'
      ag.PRODUCTGROUPNAME,

      @EndUserText.label: 'Ext. Material group'
      ag.ExternalProductGroup,

      @EndUserText.label: 'Ext. Material group description'
      ag.ExternalProductGroupName,

      @EndUserText.label: 'Storage Location Description'
      ag.StorageLocationName,

      @EndUserText.label: 'HSN Code'
      ag.ConsumptionTaxCtrlCode,

      @EndUserText.label: 'Profit Center'
      ag.ProfitCenter,

      @EndUserText.label: 'AgeingCategires'
      case
      when ag.NoOfDays >= 1 and ag.NoOfDays <= 30
      then '0-30 Days'
      when ag.NoOfDays >= 31 and ag.NoOfDays <= 60
      then '31-60 Days'
      when ag.NoOfDays >= 61 and ag.NoOfDays <= 90
      then '61-90 Days'
      when ag.NoOfDays >= 91 and ag.NoOfDays <= 120
      then '91-120 Days'
      when ag.NoOfDays >= 121 and ag.NoOfDays <= 150
      then '121-150 Days'
      when ag.NoOfDays >= 151 and ag.NoOfDays <= 180
      then '151-180 Days'
      when ag.NoOfDays >= 181 and ag.NoOfDays <= 365
      then '181-365 Days'
      when ag.NoOfDays >365
      then 'More that 365 Days'
      end as AgeCategory


      //      ag.Supplier,
      //      ag.SDDocument,
      //      ag.SDDocumentItem,
      //      ag.WBSElementInternalID,
      //      ag.Customer,
      //      ag.SpecialStockIdfgStockOwner,
      //      ag.InventoryStockType,
      //      ag.InventorySpecialStockType,
      //      ag.StockValueInDisplayCurrency,
      //      ag.ProductGroup,
      //      ag.Currency,
      //      ag.DisplayCurrency,
      //      ag.ValuationAreaType

}
