@AbapCatalog.sqlViewName: 'ZV_GATE_OUT_F4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Gate out F4'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_GATE_OUT_F4 as select from zsd_gout_data as go
{

 key go.gentry_num,  
 key go.gentry_year, 
 key go.saledocument,
 key go.invoicenum, 
 key go.invoiceitem, 
 go.saledocitem ,
 go.mblnr,       
 go.mjahr

    
}
