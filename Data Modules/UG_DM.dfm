inherited UGDM: TUGDM
  object conFB: TFDConnection
    Params.Strings = (
      'ConnectionDef=VB Live')
    FetchOptions.AssignedValues = [evDetailDelay]
    FetchOptions.DetailDelay = 400
    FormatOptions.AssignedValues = [fvMapRules, fvDataSnapCompatibility]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtWideString
        TargetDataType = dtAnsiString
      end
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtCurrency
      end
      item
        SourceDataType = dtSingle
        TargetDataType = dtDouble
      end>
    FormatOptions.DataSnapCompatibility = True
    ResourceOptions.AssignedValues = [rvCmdExecMode, rvAutoReconnect]
    ResourceOptions.AutoReconnect = True
    UpdateOptions.AssignedValues = [uvAutoCommitUpdates]
    LoginPrompt = False
    Transaction = trnFB
    Left = 120
    Top = 10
  end
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 615
    Top = 10
  end
  object FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink
    Left = 615
    Top = 65
  end
  object FDStanStorageJSONLink: TFDStanStorageJSONLink
    Left = 615
    Top = 130
  end
  object FDStanStorageBinLink: TFDStanStorageBinLink
    Left = 615
    Top = 190
  end
  object qrySQL: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    FilterOptions = [foCaseInsensitive]
    Connection = conFB
    FormatOptions.AssignedValues = [fvDataSnapCompatibility]
    FormatOptions.DataSnapCompatibility = True
    Left = 185
    Top = 10
  end
  object cmdGeneric: TFDCommand
    Connection = conFB
    ActiveStoredUsage = [auDesignTime]
    Left = 250
    Top = 10
  end
  object sprGeneric: TFDStoredProc
    ActiveStoredUsage = [auDesignTime]
    Connection = conFB
    ResourceOptions.AssignedValues = [rvStorePrettyPrint]
    ResourceOptions.StorePrettyPrint = True
    StoredProcName = 'SP_GEN_BILLABLE_SUMMARY_TABLE'
    Left = 330
    Top = 10
  end
  object trnFB: TFDTransaction
    Connection = conFB
    Left = 120
    Top = 65
  end
end
