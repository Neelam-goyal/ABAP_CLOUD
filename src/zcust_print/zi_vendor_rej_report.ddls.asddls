@AbapCatalog.sqlViewName: 'ZI_VEND_REJ_R'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Vendor Rejection Report'
define view ZI_VENDOR_REJ_REPORT
  as select from    ZI_INSP_LOT_REJ as insplot
    left outer join ZI_GRN_DETAIL   as grn on  grn.MaterialDocument     = insplot.MaterialDocument
                                           and grn.MaterialDocumentYear = insplot.MaterialDocumentYear
                                           and grn.PurchaseOrder        = insplot.PurchasingDocument
                                           and grn.PurchaseOrderItem    = insplot.PurchasingDocumentItem
{

  key insplot.InspectionLot,
      @EndUserText.label: 'Material Document'
      insplot.MaterialDocument, 
      @EndUserText.label: 'Material Document Year' 
      insplot.MaterialDocumentYear,
      @EndUserText.label: 'Material Doument Item' 
      grn.MaterialDocumentItem,
      @EndUserText.label: 'Material'
      grn.Material,
      @EndUserText.label: 'Material Description'
      grn.ProductDescription,
      //@EndUserText.label: 'Storage Location' 
      //@EndUserText.label: 'Storage Loc. Name' 
      @EndUserText.label: 'Posting Date'      
      grn.PostingDate,
      @EndUserText.label: 'Stock Type'
      grn.InventoryStockType,
      //@EndUserText.label: 'Stock Type Name'
      @EndUserText.label: 'Delivery Note'
      grn.ReferenceDocument,
      @EndUserText.label: 'Document Date'
      grn.DocumentDate,
      @EndUserText.label: 'Purchase Order'
      grn.PurchaseOrder,
      @EndUserText.label: 'Purchase Order Item'
      grn.PurchaseOrderItem, 
      @EndUserText.label: 'Quantity'    
      insplot.InspectionLotQuantity,
      @EndUserText.label: 'Unit of Entry'
      grn.EntryUnit,
      @EndUserText.label: 'Supplier'
      insplot.Supplier,
      @EndUserText.label: 'Supplier Name'
      insplot.SupplierName,
      @EndUserText.label: 'Goods Movement Type'
      grn.GoodsMovementType,
      //@EndUserText.label: 'Goods Movement Type Text'
      @EndUserText.label: 'Batch'
      insplot.Batch,      
      @EndUserText.label: 'Accepted Qty'
      insplot.InspLotQtyToFree,
      @EndUserText.label: 'Rejected Qty'
      insplot.InspLotQtyToBlocked
      //@EndUserText.label: 'Rejected Location'
            
//      insplot.InspectionLotType,
//      insplot.Plant,
//      insplot.InspectionLotObjectText,
//      insplot.InspectionLotOrigin,
//      insplot.ManufacturingOrder,
//      insplot.InspectionLotUsageDecisionCode,
//      insplot.MatlDocLatestPostgDate,
//      insplot.DeliveryDocument,
//      insplot.SalesOrder,
//      insplot.InspectionLotUsageDecidedOn,
//      grn.MaterialDocumentHeaderText,
//      grn.BillOfLading,    
//      grn.QuantityInEntryUnit,
//      grn.TotalGoodsMvtAmtInCCCrcy,
//      grn.InventorySpecialStockType,
//      grn.PurchaseOrderDate,
//      grn.OrderQuantity,
//      grn.NetPriceAmount,
//      grn.QuantityInDeliveryQtyUnit,
//      grn.Country,
//      grn.StreetPrefixName1,
//      grn.StreetPrefixName2,
//      grn.StreetName,
//      grn.StreetSuffixName1,
//      grn.DistrictName,
//      grn.CityName,
//      grn.PostalCode,
//      grn.AddressRepresentationCode,
//      grn.AddressPersonID,
//      grn.ProductDescription

} //where grn.InventoryStockType = '01'
