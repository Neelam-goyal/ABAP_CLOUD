@AbapCatalog.sqlViewName: 'ZV_ROUT_DATA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Routing Data'
define view ZI_ROUT_DATA
  as select from    I_MfgBOOMaterialAssignment as mapl

    left outer join I_MfgBOOOperationChangeState  as op   on  op.BillOfOperationsType   = mapl.BillOfOperationsType
                                                       and op.BillOfOperationsGroup  = mapl.BillOfOperationsGroup
                                                       and op.BillOfOperationsVariant = mapl.BillOfOperationsVariant

    left outer join I_WorkCenterBySemanticKey  as wrk  on  wrk.Plant                = mapl.Plant
                                                       and wrk.WorkCenterInternalID = op.WorkCenterInternalID
     left outer join I_WorkCenterCostCenter as wcc
      on wcc.WorkCenterInternalID = wrk.WorkCenterInternalID
     and wcc.CostCenterAllocation = '0001'

    left outer join I_CostCenterText as cct
      on cct.CostCenter = wcc.CostCenter                                                  

    left outer join I_ProductDescription       as makt on  makt.Product  = mapl.Product
                                                       and makt.Language = 'E'
{

      @EndUserText.label: 'Material No.'
      key mapl.Product,
      
      @EndUserText.label: 'Plant'
      key mapl.Plant,
      
      @EndUserText.label: 'Routing Group'  
      key mapl.BillOfOperationsGroup,
      
      @EndUserText.label: 'Alternate Routing'  
      key mapl.BillOfOperationsVariant,
      
      key mapl.BOOToMaterialInternalID,
      key mapl.BillOfOperationsType,
  
      @EndUserText.label: 'Operation No.'     
      key op.Operation_2, //SubOperation_2,
          
      @EndUserText.label: 'Material Description'
      makt.ProductDescription,
      
      @EndUserText.label: 'Key Date'           
      op.ValidityStartDate,
      
      @EndUserText.label: 'Control Key'
      op.OperationControlProfile,
      
      @EndUserText.label: 'Operation Short Text'
      op.OperationText, //SubOperationText,
       @EndUserText.label: 'Cost Center'
       wcc.CostCenter,

     @EndUserText.label: 'Cost Center Description'
     cct.CostCenterDescription,
      
      @EndUserText.label: 'Base Qty.'
      op.OperationReferenceQuantity,
      
      @EndUserText.label: 'UOM For Activity/Op.'
      op.OperationUnit,
      
      @EndUserText.label: 'Work Center'
      wrk.WorkCenter,
      
      @EndUserText.label: 'Parameter 1'
      op.StandardWorkFormulaParam1,
      
      @EndUserText.label: 'Machine/ Repair&maint'
      op.StandardWorkQuantity1,
      
      @EndUserText.label: 'UOM:Machine/ Repair&maint'
      op.StandardWorkQuantityUnit1,
      
      @EndUserText.label: 'Parameter 2'
      op.StandardWorkFormulaParam2,
      
      @EndUserText.label: 'Labor'
      op.StandardWorkQuantity2,
      
      @EndUserText.label: 'UOM:Labor'
      op.StandardWorkQuantityUnit2,
      
      @EndUserText.label: 'Parameter 3'
      op.StandardWorkFormulaParam3,
      
      @EndUserText.label: 'Power'
      op.StandardWorkQuantity3,
      
      @EndUserText.label: 'UOM:Power'
      op.StandardWorkQuantityUnit3,
      
      @EndUserText.label: 'Parameter 4'
      op.StandardWorkFormulaParam4,
      
      @EndUserText.label: 'Consumable'
      op.StandardWorkQuantity4,
      
      @EndUserText.label: 'UOM:Consumable'
      op.StandardWorkQuantityUnit4,
      
      @EndUserText.label: 'Parameter 5'
      op.StandardWorkFormulaParam5,
      
      @EndUserText.label: 'Depreciation'
      op.StandardWorkQuantity5,
      
      @EndUserText.label: 'UOM:Depreciation'
      op.StandardWorkQuantityUnit5,
      
      @EndUserText.label: 'Parameter 6'
      op.StandardWorkFormulaParam6,
      
      @EndUserText.label: 'Setup'
      op.StandardWorkQuantity6,
      
      @EndUserText.label: 'UOM:Setup'
      op.StandardWorkQuantityUnit6                 

                  
//      op.BillOfOperationsSequence,
//      op.BOOOperationInternalID,
//      op.BOOSqncOpAssgmtIntVersionCntr,
//      op.BOOOpInternalVersionCounter,
//      op.SubOperation,
//      op.SuperiorOperationInternalID,
//      op.LongTextLanguageCode,
//      op.CreationDate,
//      op.CreatedByUser,
//      op.LastChangeDate,
//      op.LastChangedByUser,
//      op.ChangeNumber,      
//      op.ValidityEndDate,
//      op.IsDeleted,
//      op.IsImplicitlyDeleted,
//      op.OperationStandardTextCode,
//      op.WorkCenterInternalID,
//      op.WorkCenterTypeCode,
//      op.FactoryCalendar,
//      op.CapacityCategoryCode,
//      op.OperationStdWorkQtyGrpgCat,
//      op.NumberOfTimeTickets,
//      op.NumberOfConfirmationSlips,
//      op.EmployeeWageGroup,
//      op.EmployeeWageType,
//      op.EmployeeSuitability,
//      op.NumberOfEmployees,
//      op.BillOfOperationsRefType,
//      op.BillOfOperationsRefGroup,
//      op.BillOfOperationsRefVariant,
//      op.OperationSetupType,
//      op.OperationSetupGroupCategory,
//      op.OperationSetupGroup,
//      op.ControlRecipeDestination,
//      op.OpIsExtlyProcdWithSubcontrg,
//      op.PurchasingInfoRecord,
//      op.PurchasingOrganization,
//      op.PurchaseContract,
//      op.PurchaseContractItem,
//      op.PurchasingInfoRecdAddlGrpgName,
//      op.MaterialGroup,
//      op.PurchasingGroup,
//      op.Supplier,
//      op.PlannedDeliveryDuration,
//      op.NumberOfOperationPriceUnits,
//      op.OpExternalProcessingCurrency,
//      op.OpExternalProcessingPrice,
//      op.CostElement,
//      op.OperationCostingRelevancyType,
//      op.InspectionLotType,
//      op.OperationScrapPercent,
//      op.OpQtyToBaseQtyNmrtr,
//      op.OpQtyToBaseQtyDnmntr,    
//      op.CostCtrActivityType1,
//      op.PerfEfficiencyRatioCode1,
//      op.CostCtrActivityType2,
//      op.PerfEfficiencyRatioCode2,
//      op.CostCtrActivityType3,
//      op.PerfEfficiencyRatioCode3,
//      op.CostCtrActivityType4,
//      op.PerfEfficiencyRatioCode4,
//      op.CostCtrActivityType5,
//      op.PerfEfficiencyRatioCode5,
//      op.CostCtrActivityType6,
//      op.PerfEfficiencyRatioCode6,
//      op.BusinessProcess,
//      op.StartDateOffsetDurationUnit,
//      op.StartDateOffsetDuration,
//      op.EndDateOffsetDurationUnit,
//      op.EndDateOffsetDuration,
//      op.LeadTimeReductionStrategy,
//      wrk.WorkCenterIsToBeDeleted,
//      wrk.WorkCenterIsLocked,
//      wrk.WorkCenterIsMntndForCosting,
//      wrk.WorkCenterIsMntndForScheduling,
//      wrk.AdvancedPlanningIsSupported,
//      wrk.LaborTrackingIsRequired,
//      wrk.WorkCenterCategoryCode,
//      wrk.WorkCenterLocation,
//      wrk.WorkCenterLocationGroup,
//      wrk.WorkCenterUsage,
//      wrk.WorkCenterResponsible,
//      wrk.SupplyArea,
//      wrk.CapacityInternalID,
//      wrk.MachineType,
//      wrk.MatlCompIsMarkedForBackflush,
//      wrk.WorkCenterSetupType,
//      wrk.FreeDefinedTableFieldSemantic,
//      wrk.ObjectInternalID,
//      wrk.StandardTextInternalID,
//      wrk.PlanVersion,
//      wrk.StandardTextIDIsReferenced,
//      wrk.EmployeeWageTypeIsReferenced,
//      wrk.NmbrOfTimeTicketsIsReferenced,
//      wrk.EmployeeWageGroupIsReferenced,
//      wrk.EmplSuitabilityIsReferenced,
//      wrk.WorkCenterSetpTypeIsReferenced,
//      wrk.OpControlProfileIsReferenced,
//      wrk.NumberOfConfSlipsIsReferenced,
//      wrk.WorkCenterStdQueueDurnUnit,
//      wrk.WorkCenterStandardQueueDurn,
//      wrk.WorkCenterMinimumQueueDurnUnit,
//      wrk.WorkCenterMinimumQueueDuration,
//      wrk.WorkCenterStandardWorkQtyUnit1,
//      wrk.WorkCenterStandardWorkQtyUnit2,
//      wrk.WorkCenterStandardWorkQtyUnit3,
//      wrk.WorkCenterStandardWorkQtyUnit4,
//      wrk.WorkCenterStandardWorkQtyUnit5,
//      wrk.WorkCenterStandardWorkQtyUnit6,
//      wrk.StandardWorkQuantityUnit,
//      wrk.StandardWorkFormulaParamGroup,
//      wrk.WrkCtrStdValMaintRule1,
//      wrk.WrkCtrStdValMaintRule2,
//      wrk.WrkCtrStdValMaintRule3,
//      wrk.WrkCtrStdValMaintRule4,
//      wrk.WrkCtrStdValMaintRule5,
//      wrk.WrkCtrStdValMaintRule6

}
