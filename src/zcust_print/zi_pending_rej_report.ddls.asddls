@AbapCatalog.sqlViewName: 'ZV_PEND_REJ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pending Rejection Report'
define view ZI_PENDING_REJ_REPORT
  as select from    ZI_VENDOR_REJ_REPORT as REJ
    left outer join ZI_REJ_RET as grn on  grn.PurchaseOrder        = REJ.PurchaseOrder
                                      and grn.PurchaseOrderItem    = REJ.PurchaseOrderItem
                                      and grn.Supplier             = REJ.Supplier
                                      and grn.ReferenceDocument    = REJ.ReferenceDocument
                                      and grn.Batch                = REJ.Batch
{
  key REJ.InspectionLot,
      @EndUserText.label: 'Material Document'
      REJ.MaterialDocument,
      @EndUserText.label: 'Material Document Year'
      REJ.MaterialDocumentYear,
      @EndUserText.label: 'Material Doument Item'
      REJ.MaterialDocumentItem,
      @EndUserText.label: 'Material'
      REJ.Material,
      @EndUserText.label: 'Material Description'
      REJ.ProductDescription,
      @EndUserText.label: 'Posting Date'
      REJ.PostingDate,
      @EndUserText.label: 'Stock Type'
      REJ.InventoryStockType,
      @EndUserText.label: 'Delivery Note'
      REJ.ReferenceDocument,
      @EndUserText.label: 'Document Date'
      REJ.DocumentDate,
      @EndUserText.label: 'Purchase Order'
      REJ.PurchaseOrder,
      @EndUserText.label: 'Purchase Order Item'
      REJ.PurchaseOrderItem,
      @EndUserText.label: 'Quantity'
      REJ.InspectionLotQuantity,
      @EndUserText.label: 'Unit of Entry'
      REJ.EntryUnit,
      @EndUserText.label: 'Supplier'
      REJ.Supplier,
      @EndUserText.label: 'Supplier Name'
      REJ.SupplierName,
      @EndUserText.label: 'Goods Movement Type'
      REJ.GoodsMovementType,
      @EndUserText.label: 'Batch'
      REJ.Batch,
      @EndUserText.label: 'Accepted Qty'
      REJ.InspLotQtyToFree,
      @EndUserText.label: 'Rejected Qty'
      REJ.InspLotQtyToBlocked,
      @EndUserText.label: 'Returned Qty'
      grn.ret_qty,
      @EndUserText.label: 'Pending Returned Qty'
      ( REJ.InspLotQtyToBlocked - grn.ret_qty ) as pend_ret_qty

}
