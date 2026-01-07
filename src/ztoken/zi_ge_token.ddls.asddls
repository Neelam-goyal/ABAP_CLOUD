@AbapCatalog.sqlViewName: 'ZV_GE_TOKEN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'GE token data'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_GE_TOKEN
  as select from zmm_ge_token as getoken
{

  key getoken.gentry_num        as GentryNum,
  key getoken.gentry_year       as GentryYear,
  key getoken.ponum             as Ponum,
  key getoken.poitem            as Poitem,
  key getoken.tokennum          as Tokennum,
  getoken.material          as Material,
  getoken.material_desc     as MaterialDesc,
  getoken.plant             as Plant,
  getoken.material_group    as MaterialGroup,
  getoken.supplier          as Supplier,
  getoken.supplier_name     as SupplierName,
  getoken.supplier_gstin    as SupplierGstin,
  getoken.supplier_pan      as SupplierPan,
  getoken.token_in_date     as TokenInDate,
  getoken.token_in_time     as TokenInTime,
  getoken.driver_name       as DriverName,
  getoken.vehicle_no        as VehicleNo,
  getoken.mobileno          as Mobileno,
  getoken.invoice_no        as InvoiceNo,
  getoken.invoice_date      as InvoiceDate,
  getoken.invoice_qty       as InvoiceQty,
  getoken.invoice_qty_uom   as InvoiceQtyUom,
  getoken.ewaybill_no       as EwaybillNo,
  getoken.transporter       as Transporter,
  getoken.lr_no             as LrNo,
  getoken.lr_date           as LrDate,
  getoken.bagtype           as BagType,
  getoken.total_bag_rec     as TotalBagRec,
  getoken.per_bag_wt        as PerBagWt,
  getoken.total_bag_wt      as TotalBagWt,
  getoken.excess_qty        as ExcessQty,
  getoken.total_bag_rec_uom as TotalBagRecUom,
  getoken.per_bag_wt_uom    as PerBagWtUom,
  getoken.total_bag_wt_uom  as TotalBagWtUom,
  getoken.excess_qty_uom    as ExcessQtyUom,
  getoken.grain_vech_stat   as GrainVechStat,
  getoken.gross_wt          as GrossWt,
  getoken.tare_wt           as TareWt,
  getoken.net_wt            as NetWt,
  getoken.net_wt_rec        as NetWtRec,
  getoken.net_rec_qty       as NetRecQty,
  getoken.gross_wt_uom      as GrossWtUom,
  getoken.tare_wt_uom       as TareWtUom,
  getoken.net_wt_uom        as NetWtUom,
  getoken.net_wt_rec_uom    as NetWtRecUom,
  getoken.net_rec_qty_uom   as NetRecQtyUom,
  getoken.wb_in_date        as WbInDate,
  getoken.wb_in_time        as WbInTime,
  getoken.wb_out_date       as WbOutDate,
  getoken.wb_out_time       as WbOutTime,
  getoken.unload_date       as UnloadDate,
  getoken.unload_time       as UnloadTime,
  getoken.wb_remark         as WbRemark,
  getoken.erdat             as Erdat,
  getoken.uzeit             as Uzeit,
  getoken.cuname            as Cuname,
  getoken.gedeleted         as Gedeleted

}
