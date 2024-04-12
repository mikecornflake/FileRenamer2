Unit Actions;

Interface

Uses
  AdvObjects, AdvPersistents, AdvFilers, AdvLists, AdvFactories,
  cxGridCustomTableView;

Type
  TAction = Class(TAdvPersistent)
  Protected
    FDescription: String;
    FFilename: String;
    FName: String;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Assign(oObject : TAdvObject); Override;
    Procedure Define(oFiler : TAdvFiler); Override;

    Procedure Initialise; Virtual;
    Procedure Finalise; Virtual;

    Function Preview : String; Virtual;

    Procedure Process(oRecord: TcxCustomGridRecord); Virtual;

    Property Name : String        Read FName        Write FName;
    Property Description : String Read FDescription Write FDescription;

    Property Filename : String    Read FFilename    Write FFilename;
  End;

  TActionList = Class(TAdvList)
  Private
    Function GetItems(iIndex: Integer): TAction;
  Public
    Function Add(Value : TAction) : Integer;

    Property Items[iIndex : Integer] : TAction Read GetItems; Default;
  End;

  TActions = Class(TAction)
  Protected
    FList : TActionList;

    Function GetItem(iIndex: Integer): TAction;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Assign(oObject : TAdvObject); Override;
    Procedure Define(oFiler : TAdvFiler); Override;

    Procedure Initialise; Override;
    Procedure Finalise; Override;

    Function Preview : String; Override;
    Procedure Process(oRecord: TcxCustomGridRecord); Override;

    Procedure Delete(Index : Integer); 
    Function Add(Value : TAction) : Integer;
    Function Count : Integer;

    Property Items[iIndex : Integer] : TAction Read GetItem; Default;
  End;

Implementation

Uses
  FormMain, FrameFileTags, StringUtils,
  StringSupport;

Var
  lActionFactory : TAdvFactory;

{----------------------------------------------------------------------------------------}
Function ActionFactory : TAdvFactory;
Begin { ActionFactory }
  If Not Assigned(lActionFactory) Then
    lActionFactory := TAdvFactory.Create;

  Result := lActionFactory;
End; { ActionFactory }


{ TAction }

constructor TAction.Create;
begin
  inherited;

end;

destructor TAction.Destroy;
begin

  inherited;
end;

{----------------------------------------------------------------------------------------}
Procedure TAction.Initialise;
Begin { Initialise }
  FName := ClassName;
  FDescription := '<Description>';
End; { Initialise }

{----------------------------------------------------------------------------------------}
Procedure TAction.Finalise;
Begin { Finalise }
End; { Finalise }

{----------------------------------------------------------------------------------------}
Procedure TAction.Assign(oObject: TAdvObject);
Begin { Assign }
  Inherited;

  FName := TAction(oObject).FName;
  FDescription := TAction(oObject).FDescription;
End; { Assign }

{----------------------------------------------------------------------------------------}
Procedure TAction.Define(oFiler: TAdvFiler);
Begin { Define }
  Inherited;

  oFiler['Name'].DefineString(FName);
  oFiler['Description'].DefineString(FDescription);
End; { Define }

{----------------------------------------------------------------------------------------}
Procedure TAction.Process(oRecord: TcxCustomGridRecord);
Begin { Process }
  FFilename := StringUtils.Trim(oRecord.Values[frmMain.fmeFilenameTags.colPath.Index]) +
               StringUtils.Trim(oRecord.Values[frmMain.fmeFilenameTags.colFilename.Index]) +
               StringUtils.Trim(oRecord.Values[frmMain.fmeFilenameTags.colFileExt.Index]);
End; { Process }

{----------------------------------------------------------------------------------------}
Function TAction.Preview: String;
Begin { Preview }
  Result := '';
End; { Preview }

{ TActionList }

{----------------------------------------------------------------------------------------}
Function TActionList.Add(Value: TAction): Integer;
Begin { Add }
  Result := Inherited Add(Value);
End; { Add }

{----------------------------------------------------------------------------------------}
Function TActionList.GetItems(iIndex: Integer): TAction;
Begin { GetItems }
  Result := TAction(Objects[iIndex]);
End; { GetItems }

{ TActions }

{----------------------------------------------------------------------------------------}
Constructor TActions.Create;
Begin { Create }
  Inherited;

  FList := TActionList.Create;
End; { Create }

{----------------------------------------------------------------------------------------}
Destructor TActions.Destroy;
Begin { Destroy }
  FList.Free;
  FList := Nil;

  Inherited;
End; { Destroy }

{----------------------------------------------------------------------------------------}
Procedure TActions.Initialise;
Var
  i : Integer;
Begin { Initialise }
  Inherited;

  For i := 0 To FList.Count-1 Do
    FList[i].Initialise;
End; { Initialise }

{----------------------------------------------------------------------------------------}
Procedure TActions.Finalise;
Var
  i : Integer;
Begin { Finalise }
  Inherited;

  For i := 0 To FList.Count-1 Do
    FList[i].Finalise;
End; { Finalise }

{----------------------------------------------------------------------------------------}
Procedure TActions.Assign(oObject: TAdvObject);
Begin { Assign }
  Inherited;

  FList.Assign(TActions(oObject).FList);
End; { Assign }

{----------------------------------------------------------------------------------------}
Procedure TActions.Define(oFiler: TAdvFiler);
Begin { Define }
  Inherited;

  oFiler['List'].DefineObject(FList);
End; { Define }

{----------------------------------------------------------------------------------------}
Function TActions.GetItem(iIndex: Integer): TAction;
Begin { GetItem }
  Result := FList[iIndex];
End; { GetItem }

{----------------------------------------------------------------------------------------}
Procedure TActions.Process(oRecord: TcxCustomGridRecord);
Var
  i : Integer;
Begin { Process }
  For i := 0 To FList.Count-1 Do
    FList[i].Process(oRecord);
End; { Process }


{----------------------------------------------------------------------------------------}
Function TActions.Preview: String;
Begin { Preview }
  Result := ToString(FList.Count)+' items';
End; { Preview }

{----------------------------------------------------------------------------------------}
Function TActions.Count: Integer;
Begin { Count }
  Result := FList.Count;
End; { Count }

{----------------------------------------------------------------------------------------}
Function TActions.Add(Value: TAction): Integer;
Begin { Add }
  Result := FList.Add(Value)
End; { Add }

procedure TActions.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

Initialization
  lActionFactory := Nil;

Finalization
  If Assigned(lActionFactory) Then
  Begin { If }
    lActionFactory.Free;
    lActionFactory := Nil;
  End; { If }

End.

