Unit ActionsReplace;

Interface

Uses
  AdvObjects, AdvPersistents, AdvFilers,
  Actions,
  DialogSearchReplace,
  cxGridCustomTableView, cxGridTableView;

Type
  TActionReplace = Class(TAction)
  Protected
    FOperation: TSearchReplaceOperation;

    FSearch: String;
    FReplace: String;
    FField: String;
  Public
    Procedure Assign(oObject : TAdvObject); Override;
    Procedure Define(oFiler : TAdvFiler); Override;

    Procedure Process(oRecord: TcxCustomGridRecord); Override;

    Function Preview : String; Override;

    Property Operation : TSearchReplaceOperation Read FOperation Write FOperation;

    Property Field : String     Read FField     Write FField;
    Property Replace : String   Read FReplace   Write FReplace;
    Property Search : String    Read FSearch    Write FSearch;
  End;

Implementation

Uses
  StringSupport, StringUtils,
  FormMain;

{----------------------------------------------------------------------------------------}
Procedure TActionReplace.Assign(oObject: TAdvObject);
Begin { Assign }
  Inherited;

  FField := TActionReplace(oObject).FField;
  FOperation := TActionReplace(oObject).FOperation;
  FReplace := TActionReplace(oObject).FReplace;
  FSearch := TActionReplace(oObject).FSearch;
End; { Assign }

{----------------------------------------------------------------------------------------}
Procedure TActionReplace.Define(oFiler: TAdvFiler);
Begin { Define }
  Inherited;

  oFiler['Field'].DefineString(FField);
  oFiler['Operation'].DefineEnumerated(FOperation);
  oFiler['Replace'].DefineString(FReplace);
  oFiler['Search'].DefineString(FSearch);
End; { Define }

{----------------------------------------------------------------------------------------}
Function TActionReplace.Preview: String;
Const
  ARR_OPERATIONS : Array[TSearchReplaceOperation] Of String =
    ('Search Only', 'Replace All', 'Lowercase', 'Uppercase', 'Inner Capitalise');
Begin { Preview }
  Result := Format('%s, %s, %s, %s', [FField, ARR_OPERATIONS[FOperation], FReplace, FSearch]);
End; { Preview }

{----------------------------------------------------------------------------------------}
Procedure TActionReplace.Process(oRecord: TcxCustomGridRecord);

Begin { Process }
  Inherited;

  // TODO  I'm in the middle of this and it doesn't compile
(*
  If oRecord.Values[oColumn.Index]<>Null Then
    sTemp := oRecord.Values[oColumn.Index]
  Else
    sTemp := '';

  sOriginalFile := StringUtils.Trim(oRecord.Values[colPath.Index]) +
                   StringUtils.Trim(oRecord.Values[colFilename.Index]) +
                   StringUtils.Trim(oRecord.Values[colFileExt.Index]);

  Case oMsg.Operation Of
    sroReplaceAll      : sTemp := StringReplace(sTemp, oMsg.Search, oMsg.Replace, [rfReplaceAll, rfIgnoreCase]);
    sroLowercase       : sTemp := Lower(sTemp);
    sroUppercase       : sTemp := Upper(sTemp);
    sroInnerCapitalise : sTemp := StringUtils.UpperFirst(sTemp);
  End; { Case }

  If oColumn.Index=colFilename.Index Then
  Begin { If }
    sFinalFile := StringUtils.Trim(oRecord.Values[colPath.Index]) +
                  StringUtils.Trim(sTemp) +
                  StringUtils.Trim(oRecord.Values[colFileExt.Index]);

    If frmMain.RenameFile(sOriginalFile, sFinalFile) Then
    Begin { If }
      tvGrid.DataController.Values[oRecord.RecordIndex, oColumn.Index] := Trim(sTemp);

      Inc(iFilesChanged)
    End; { If }
  End { If }
  Else
  Begin { Else }
    tvGrid.DataController.Values[oRecord.RecordIndex, oColumn.Index] := Trim(sTemp);

    // TODO  Change TAG Data here
  End; { Else }
*)
End; { Process }

End.
