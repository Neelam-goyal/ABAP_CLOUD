@AbapCatalog.sqlViewName: 'ZV_PROD_MASTER '
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product master'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_PROD_MASTER
  as select from    I_Product        as prod
    left outer join I_ProductPlantBasic as pplant on pplant.Product = prod.Product
{

  key prod.Product,
      prod.ProductType,
      prod._Text.ProductName,
      prod.BaseUnit,
      pplant.Plant,
      prod.ProductOID,
      prod.ProductGroup,
      prod.CreationDate,
      prod.CreationTime,
      prod.CreationDateTime,
      prod.CreatedByUser,
      prod.LastChangeDate,
      prod.LastChangedByUser

}
