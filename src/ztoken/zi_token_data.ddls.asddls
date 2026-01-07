@AbapCatalog.sqlViewName: 'ZV_TOKEN_DATA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS for token data'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_TOKEN_DATA
  as select from    zmm_token_data       as token
    left outer join ZI_GATE_ENTRY_REPORT as gerep on gerep.tokennum = token.tokennum
{

  key    token.tokennum                 as Tokennum,
  key    token.plant                    as Plant,
  key    token.ponum                    as PONum,
  key    token.poitem                   as POItem,
         gerep.mblnr                    as MigoNumber,
         gerep.mjahr                    as MigoYear,
         token.material                 as Material,
         token.material_desc            as MaterialDesc,
         token.material_group           as MaterialGroup,
         token.supplier                 as Supplier,
         token.supplier_name            as SupplierName,
         token.supplier_gstin           as SupplierGstin,
         token.supplier_pan             as SupplierPan,
         token.supplier_mobile          as SupplierMobile,
         token.drivername               as Drivername,
         token.vehicleno                as Vehicleno,
         token.vehicletype              as Vehicletype,
         token.vehiclname               as Vehiclname,
         token.tokendate                as Tokendate,
         token.tokentime                as Tokentime,
         token.transporter              as Transporter,
         token.remark                   as Remark,
         token.insplotno                as Insplotno,
         token.tokeninsp                as Tokeninsp,
         token.supervisor               as SuperVisor,
         token.contractor               as ConTractor,
         token.gatenetryno              as Gatenetryno,
         token.gatenetryyear            as Gatenetryyear,
         token.token_createdby          as TokenCreatedby,
         token.token_createdon          as TokenCreatedon,
         token.token_createdtime        as TokenCreatedtime,
         token.tokenchanged             as Tokenchanged,
         token.token_changedby          as TokenChangedby,
         token.token_changedon          as TokenChangedon,
         token.token_changedtime        as TokenChangedtime,
         token.tokendeleted             as Tokendeleted,
         token.token_deletedby          as TokenDeletedby,
         token.token_deletedon          as TokenDeletedon,
         token.token_deletedtime        as TokenDeletedtime,
         
//when gerep.mblnr is not null then 'Close'
         case when gerep.mblnr is initial then 'Open'
         when gerep.gentry_num is null then 'Open'
         else 
            'Close' end as tokenstatus
}
where
  token.tokendeleted = ''
