@AbapCatalog.sqlViewName: 'ZV_RFQ_DATA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'RFQ Data'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_RFQ_DATA
  as select from    I_Requestforquotation_Api01 as rfq //I_RfqItem_Api01 
    left outer join I_RfqBidder_Api01           as rfqbid on rfqbid.RequestForQuotation = rfq.RequestForQuotation
{

  key rfq.RequestForQuotation,
  key rfqbid.PartnerCounter,
      rfqbid.Supplier,
      rfq.CreationDate
}
