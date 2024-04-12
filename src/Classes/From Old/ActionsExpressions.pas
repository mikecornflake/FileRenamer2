Unit ActionsExpressions;

Interface

Uses
  AdvObjects, AdvPersistents, AdvFilers,
  Actions,
  cxGridCustomTableView, cxGridTableView;

Type
  TActionExpression = Class(TAction)  // Designed as the base for all Expression Actions.  Do not use directly!
  Protected
    FExpression: String;
    FProcessedExpression: String;
  Public
    Procedure Assign(oObject : TAdvObject); Override;
    Procedure Define(oFiler : TAdvFiler); Override;

    Procedure Process(oRecord: TcxCustomGridRecord); Override;

    Function Preview : String; Override;

    Property Expression : String Read FExpression Write FExpression;
    Property ProcessedExpression : String Read FProcessedExpression Write FProcessedExpression;
  End;

  TActionExpressionRename = Class(TActionExpression)
  Public
    Procedure Initialise; Override;

    Procedure Process(oRecord: TcxCustomGridRecord); Override;

    Function Preview : String; Override;
  End;

  TActionApplyToTag = Class(TActionExpression)
  Private
    FTag: String;
  Public
    Procedure Initialise; Override;

    Procedure Assign(oObject : TAdvObject); Override;
    Procedure Define(oFiler : TAdvFiler); Override;

    Procedure Process(oRecord: TcxCustomGridRecord); Override;
    Function Preview : String; Override;

    Property Tag : String Read FTag Write FTag;
  End;

  TActionApplyToAllTags = Class(TActionExpression)
  Public
    Procedure Process(oRecord: TcxCustomGridRecord); Override;
    Function Preview : String; Override;
  End;


Implementation

Uses
  FormMain, StringUtils,
  SysUtils,
  AdvFactories;

{ TActionExpression }

{----------------------------------------------------------------------------------------}
Procedure TActionExpression.Define(oFiler: TAdvFiler);
Begin { Define }
  Inherited;

  oFiler['Expression'].DefineString(FExpression);
End; { Define }

{----------------------------------------------------------------------------------------}
Procedure TActionExpression.Assign(oObject: TAdvObject);
Begin { Assign }
  Inherited;

  FExpression := TActionExpression(oObject).FExpression;
End; { Assign }

{----------------------------------------------------------------------------------------}
Procedure TActionExpression.Process(oRecord: TcxCustomGridRecord);
Begin { Process }
  Inherited;

  FProcessedExpression := Interpret(frmMain.fmeFilenameTags.ReplaceKeywords(FExpression, oRecord));
End; { Process }

{----------------------------------------------------------------------------------------}
Function TActionExpression.Preview: String;
Begin { Preview }
  Result := FExpression;
End; { Preview }

{ TActionExpressionRename }

{----------------------------------------------------------------------------------------}
Procedure TActionExpressionRename.Initialise;
Begin { Initialise }
  Inherited;

  FName := 'Rename file';
  FDescription := 'Rename file according to Expression';
End; { Initialise }

{----------------------------------------------------------------------------------------}
Procedure TActionExpressionRename.Process(oRecord: TcxCustomGridRecord);
Var
  sExt : String;
  sFinalFile : String;
  sPath : String;
  sTemp : String;
Begin { Process }
  Inherited;

  sPath := Trim(oRecord.Values[frmMain.fmeFilenameTags.colPath.Index]);
  sExt := Trim(oRecord.Values[frmMain.fmeFilenameTags.colFileExt.Index]);

  If (Pos(':\', FProcessedExpression)=0) Then
    sFinalFile := sPath + Trim(FProcessedExpression) + sExt
  Else
    sFinalFile := Trim(FProcessedExpression) + sExt;

  If frmMain.RenameFile(FFilename, sFinalFile) Then
    frmMain.SetValue(oRecord.RecordIndex, frmMain.fmeFilenameTags.colFilename.Caption, sTemp);
End; { Process }

{----------------------------------------------------------------------------------------}
Function TActionExpressionRename.Preview: String;
Begin { Preview }
  Result := 'Filename = '+FExpression;
End; { Preview }

{ TActionApplyToTag }

{----------------------------------------------------------------------------------------}
Procedure TActionApplyToTag.Initialise;
Begin { Initialise }
  Inherited;

  FName := 'Edit Tag';
  FDescription := 'Replace Tag with result from Expression';

  FTag := frmMain.fmeFilenameTags.colTest.Caption;
End; { Initialise }

{----------------------------------------------------------------------------------------}
Procedure TActionApplyToTag.Assign(oObject: TAdvObject);
Begin { Assign }
  Inherited;

  FTag := TActionApplyToTag(oObject).FTag;
End; { Assign }

{----------------------------------------------------------------------------------------}
Procedure TActionApplyToTag.Define(oFiler: TAdvFiler);
Begin { Define }
  Inherited;

  oFiler['Tag'].DefineString(FTag)
End; { Define }

{----------------------------------------------------------------------------------------}
Procedure TActionApplyToTag.Process(oRecord: TcxCustomGridRecord);
Begin { Process }
  Inherited;

  frmMain.SetValue(oRecord.RecordIndex, FTag, FProcessedExpression);
End; { Process }

{----------------------------------------------------------------------------------------}
Function TActionApplyToTag.Preview: String;
Begin { Preview }
  Result := FTag+' = '+FExpression;
End; { Preview }

{ TActionApplyToAllTags }

{----------------------------------------------------------------------------------------}
Procedure TActionApplyToAllTags.Process(oRecord: TcxCustomGridRecord);
Var
  iColumn : Integer;
  oColumn : TcxGridColumn;
  oFilename : TcxGridColumn;
  sExt : String;
  sFinalFile : String;
  sOriginalExpression : String;
  sPath : String;
  sTemp : String;
Begin { Process }
  Inherited;

  sOriginalExpression := frmMain.fmeFilenameTags.ReplaceKeywords(FExpression, oRecord);

  // Update all the tags
  For iColumn := 0 To frmMain.fmeFilenameTags.tvGrid.ColumnCount-1 Do
  Begin { For }
    oColumn := frmMain.fmeFilenameTags.tvGrid.Columns[iColumn];

    If (oColumn.Editing) And (oColumn<>frmMain.fmeFilenameTags.colFilename) Then
    Begin { If }
      sTemp := Interpret(StringReplace(sOriginalExpression, '%_ALL%', VarToStr(oRecord.Values[oColumn.Index]), [rfReplaceAll, rfIgnoreCase]));

      frmMain.SetValue(oRecord.RecordIndex, oColumn.Caption, sTemp);
    End; { If }
  End; { For }

  // And now, rename the file
  oFilename := frmMain.fmeFilenameTags.colFilename;

  sPath := Trim(oRecord.Values[frmMain.fmeFilenameTags.colPath.Index]);
  sExt := Trim(oRecord.Values[frmMain.fmeFilenameTags.colFileExt.Index]);
  sTemp := Interpret(StringReplace(sOriginalExpression, '%_ALL%', VarToStr(oRecord.Values[oFilename.Index]), [rfReplaceAll, rfIgnoreCase]));

  If (Pos(':\', sTemp)=0) Then
    sFinalFile := sPath + Trim(sTemp) + sExt
  Else
    sFinalFile := Trim(sTemp) + sExt;

  If frmMain.RenameFile(FFilename, sFinalFile) Then
    frmMain.SetValue(oRecord.RecordIndex, oFilename.Caption, sTemp);
End; { Process }

{----------------------------------------------------------------------------------------}
Function TActionApplyToAllTags.Preview: String;
Begin { Preview }
  Result := 'All Tags = '+FExpression;
End; { Preview }

Initialization
  Factory.Register([TActionExpressionRename, TActionApplyToTag, TActionApplyToAllTags])

End.

