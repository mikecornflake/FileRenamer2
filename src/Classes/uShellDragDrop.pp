Unit uShellDragDrop;

(* Source: http://forum.lazarus.freepascal.org/index.php/topic,42920.msg299974.html
   Contributed by forum user @ASerge
*)

{$MODE OBJFPC}
{$LONGSTRINGS ON}

Interface

Uses Classes;

Procedure CopyFilesToClipboard(Files: TStrings); // By Ctrl+C
Function DragDropCopyComplete(Files: TStrings): Boolean; // By DragDrop

Implementation

Uses ShlObj, Windows, ActiveX, SysUtils;

Type
  TDropSource = Class(TInterfacedObject, IDropSource)
  Strict Private // IDropSource
    Function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HRESULT; Stdcall;
    Function GiveFeedback(dwEffect: DWORD): HRESULT; Stdcall;
  End;

  TDataObject = Class(TInterfacedObject, IDataObject)
  Private
    FList: TStringList;
  Strict Private  // IDataObject
    Function GetData(Const formatetcIn: TFormatEtc; out medium: TStgMedium): HRESULT; Stdcall;
    Function GetDataHere(Const formatetc: TFormatEtc; out medium: TStgMedium): HRESULT;
      Stdcall;
    Function QueryGetData(Const formatetc: TFormatEtc): HRESULT; Stdcall;
    Function GetCanonicalFormatEtc(Const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HRESULT; Stdcall;
    Function SetData(Const pformatetc : FORMATETC; var medium:STGMEDIUM; fRelease: BOOL): HRESULT; Stdcall;
    Function EnumFormatEtc(dwDirection: DWORD; out AenumFormatEtc: IEnumFormatEtc): HRESULT;
      Stdcall;
    Function DAdvise(Const formatetc: TFormatEtc; advf: DWORD;
      Const advSink: IAdviseSink; out dwConnection: DWORD): HRESULT; Stdcall;
    Function DUnadvise(dwConnection: DWORD): HRESULT; Stdcall;
    Function EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT; Stdcall;
  Public
    Constructor Create(AList: TStrings);
    Destructor Destroy; Override;
  End;

  TEnumFormatETC = Class(TInterfacedObject, IEnumFORMATETC)
  Private
    FEof: Boolean;
  Strict Private // IEnumFORMATETC
    Function Next(celt: ULONG; out elt: FormatEtc; pceltFetched: PULONG): HRESULT; Stdcall;
    Function Skip(celt: ULONG): HRESULT; Stdcall;
    Function Reset: HRESULT; Stdcall;
    Function Clone(out Enum: IEnumFormatEtc): HRESULT; Stdcall;
  End;

Const
  CFileDropFormat: TFormatEtc = (cfFormat: CF_HDROP; ptd: nil;
    dwAspect: DVASPECT_CONTENT; lindex: -1; tymed: TYMED_HGLOBAL);

Function FilesToHDrop(Files: TStrings): HGLOBAL;
Var
  DropInfo: PDropFiles;
  Size: SizeInt;
  Data: PChar;
  FileName: String;
Begin
  Result := 0;
  If (Files = nil) Or (Files.Count = 0) Then
    Exit;
  Size := SizeOf(TDropFiles) + 1; // + Last #0
  For FileName In Files Do
    Inc(Size, Length(FileName) + 1); // + #0
  Result := GlobalAlloc(GMEM_MOVEABLE, Size);
  DropInfo := GlobalLock(Result);
  ZeroMemory(DropInfo, SizeOf(TDropFiles));
  DropInfo^.pFiles := SizeOf(TDropFiles); // After last
  Data := Pointer(DropInfo);
  Inc(Data, SizeOf(TDropFiles));
  For FileName In Files Do
  Begin
    StrPCopy(Data, FileName);
    Inc(Data, Length(FileName) + 1);
  End;
  Data^ := #0;
  GlobalUnlock(Result);
End;

Function DragDropCopyComplete(Files: TStrings): Boolean;
Var
  Unused: DWORD;
Begin
  OleInitialize(nil);
  Try
    Result := DoDragDrop(TDataObject.Create(Files) As IDataObject,
      TDropSource.Create As IDropSource, DROPEFFECT_COPY, @Unused) = DRAGDROP_S_DROP;
  Finally
    OleUninitialize;
  End;
End;

Procedure CopyFilesToClipboard(Files: TStrings);
Begin
  OpenClipboard(0);
  Try
    EmptyClipboard;
    SetClipboardData(CFileDropFormat.CfFormat, FilesToHDrop(Files));
  Finally
    CloseClipboard;
  End;
End;

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
Procedure Unused(Const medium: TStgMedium); Inline;
Begin {No code}
End;

Procedure Unused(Const formatetc: TFormatEtc); Inline;
Begin {No code}
End;

Procedure Unused(Const P: DWORD); Inline;
Begin {No code}
End;

Procedure Unused(Const P: Pointer); Inline;
Begin {No code}
End;

Procedure Unused(Const P: BOOL); Inline;
Begin {No code}
End;

{$POP}

Function TEnumFormatETC.Clone(out Enum: IEnumFormatEtc): HRESULT; Stdcall;
Var
  LClone: TEnumFORMATETC;
Begin
  Try
    LClone := TEnumFORMATETC.Create;
    LClone.FEof := FEof;
    Enum := LClone As IEnumFORMATETC;
    Result := S_OK;
  Except
    Result := E_FAIL;
  End;
End;

Function TEnumFormatETC.Next(celt: ULONG; out elt: FormatEtc;
  pceltFetched: PULONG): HRESULT; Stdcall;
Begin
  If (celt = 1) And (@elt <> nil) And Not FEof Then
  Begin
    elt := CFileDropFormat;
    FEof := True;
    If Assigned(pceltFetched) Then
      pceltFetched^ := 1;
    Result := S_OK;
  End
  Else
    Result := S_FALSE;
End;

Function TEnumFormatETC.Reset: HRESULT; Stdcall;
Begin
  FEof := False;
  Result := S_OK;
End;

Function TEnumFormatETC.Skip(celt: ULONG): HRESULT; Stdcall;
Begin
  If (celt = 0) Or ((celt = 1) And Not FEof) Then
  Begin
    If celt > 0 Then
      FEof := True;
    Result := S_OK;
  End
  Else
    Result := S_FALSE;
End;

Constructor TDataObject.Create(AList: TStrings);
Begin
  Inherited Create;
  FList := TStringList.Create;
  FList.Capacity := AList.Count;
  FList.AddStrings(AList);
End;

Function TDataObject.DAdvise(Const formatetc: TFormatEtc; advf: DWORD;
  Const advSink: IAdviseSink; out dwConnection: DWORD): HRESULT; Stdcall;
Begin
  Unused(formatetc);
  Unused(advf);
  Unused(advSink);
  dwConnection := 0;
  Result := OLE_E_ADVISENOTSUPPORTED;
End;

Destructor TDataObject.Destroy;
Begin
  FList.Free;
End;

Function TDataObject.DUnadvise(dwConnection: DWORD): HRESULT; Stdcall;
Begin
  Unused(dwConnection);
  Result := OLE_E_ADVISENOTSUPPORTED;
End;

Function TDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT; Stdcall;
Begin
  enumAdvise := nil;
  Result := OLE_E_ADVISENOTSUPPORTED;
End;

Function TDataObject.EnumFormatEtc(dwDirection: DWORD;
  out AenumFormatEtc: IEnumFormatEtc): HRESULT; Stdcall;
Begin
  If dwDirection = DATADIR_GET Then
  Begin
    AenumFormatEtc := TEnumFORMATETC.Create As IEnumFORMATETC;
    Result := S_OK;
  End
  Else
    Result := E_NOTIMPL;
End;

Function TDataObject.GetCanonicalFormatEtc(Const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HRESULT; Stdcall;
Begin
  formatetcOut := formatetc;
  formatetcOut.ptd := nil;
  Result := DATA_S_SAMEFORMATETC;
End;

Function TDataObject.GetData(Const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HRESULT; Stdcall;
Begin
  ZeroMemory(@medium, SizeOf(medium));
  Result := QueryGetData(formatetcIn);
  If Result <> S_OK Then
    Exit;
  medium.tymed := formatetcIn.tymed;
  If formatetcIn.cfFormat = CFileDropFormat.cfFormat Then
    medium.hGlobal := FilesToHDrop(FList)
  Else
    Result := DV_E_FORMATETC;
End;

Function TDataObject.GetDataHere(Const formatetc: TFormatEtc;
  out medium: TStgMedium): HRESULT; Stdcall;
Begin
  Unused(formatetc);
  ZeroMemory(@medium, SizeOf(medium));
  Result := E_NOTIMPL;
End;

Function TDataObject.QueryGetData(Const formatetc: TFormatEtc): HRESULT; Stdcall;
Begin
  If formatetc.cfFormat = CFileDropFormat.cfFormat Then
    Result := S_OK
  Else
    Result := DV_E_FORMATETC;
End;

Function TDataObject.SetData(Const pformatetc : FORMATETC; var medium:STGMEDIUM; fRelease: BOOL): HRESULT; Stdcall;
Begin
  Unused(pformatetc);
  Unused(medium);
  Unused(fRelease);
  Result := E_NOTIMPL;
End;

Function TDropSource.GiveFeedback(dwEffect: DWORD): HRESULT; Stdcall;
Begin
  Unused(dwEffect);
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
End;

Function TDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: DWORD): HRESULT; Stdcall;
Var
  LeftPressed, RightPressed: Boolean;
Begin
  LeftPressed := (grfKeyState And MK_LBUTTON) <> 0;
  RightPressed := (grfKeyState And MK_RBUTTON) <> 0;
  If (LeftPressed And RightPressed) Or fEscapePressed Then
    Result := DRAGDROP_S_CANCEL
  Else
  If Not LeftPressed And Not RightPressed Then
    Result := DRAGDROP_S_DROP
  Else
    Result := S_OK;  // Continue
End;

End.
