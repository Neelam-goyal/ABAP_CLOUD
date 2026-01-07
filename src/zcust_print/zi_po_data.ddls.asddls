@AbapCatalog.sqlViewName: 'ZV_PO_DATA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PO Data'
define view ZI_PO_DATA
  as select from    I_PurchaseOrderItemAPI01 as ekpo

    left outer join I_PurchaseOrderAPI01     as ekko on ekpo.PurchaseOrder = ekko.PurchaseOrder

    left outer join I_Product                as mara on mara.Product = ekpo.Material

    left outer join I_ProductDescription     as makt on  makt.Product  = ekpo.Material
                                                     and makt.Language = 'E'
{

  key ekpo.PurchaseOrder,
  key ekpo.PurchaseOrderItem,
      ekpo.OrderQuantity,
      ekpo.OverdelivTolrtdLmtRatioInPct,
      ekpo.Material,
      ekpo.YY1_SampleINSPNo_PDI,
      ekpo.Plant,
      ekpo.CompanyCode,
      ekpo.SupplierMaterialNumber,
      ekpo.RequirementTracking,
      ekpo.RequisitionerName,
      ekpo.PurchaseOrderItem as mandipoitem,
      mara.ProductGroup,
      mara.ProductType,
      ekpo.DocumentCurrency,
      ekpo.ReferenceDeliveryAddressID,
      ekpo.Subcontractor,
      ekpo.YY1_Item_Remark_PDI,
      ekko.Customer,
      ekko.Supplier,
      ekko.SupplyingPlant,
      ekko.ValidityStartDate,
      ekko.ValidityEndDate,
      ekko.PurchaseOrderDate,
      ekko.PurchasingProcessingStatus,
      ekko.PurchaseOrderType,
      ekko.PaymentTerms,
      ekko.IncotermsClassification,
      ekko.IncotermsLocation1,
      ekko.YY1_Bardana_Deduction_PDH,
      ekko.YY1_Policy_No_PDH,
      ekko.YY1_Shipto_PDH,
      makt.ProductDescription,
      ekpo.TaxCode,
      ekpo.OrderPriceUnit,
      ekpo.NetAmount,
      ekpo.NetPriceAmount,
      ekpo.BaseUnit,
      ekpo.PurchaseOrderQuantityUnit,
      ekpo.Subtotal1Amount,
      ekpo.Subtotal6Amount

}
where
      ekpo.PurchasingDocumentDeletionCode = ''
//  and ekpo.IsCompletelyDelivered          = ''
