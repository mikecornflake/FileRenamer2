Unit TagStorages;

Interface

Uses
  AdvObjects, TagSupport,
  ActiveX, ComObj,
  Windows;

Type
  TTagStorage = Class(TFileTag)
  Protected
    Function EnumerateReadProperties(oStorage: IStorage; oFMT: TGuid): Boolean;
  End;

  TTagNTFSStorage = Class(TTagStorage)
  Public
    Function Name : String; Override;

    Function ParseFile(sFilename : String) : Boolean; Override;
    Function Write : Boolean; Override;
  End;

  TTagCompoundStorage = Class(TTagStorage)
  Public
    Function Name : String; Override;

    Function ParseFile(sFilename : String) : Boolean; Override;
    Function Write : Boolean; Override;
  End;

Implementation


Uses
  SysUtils,
  StringSupport, MathSupport, DateSupport,
  Dialogs;

{----------------------------------------------------------------------------------------}
Function StgOpenStorageEx (
 Const pwcsName : POleStr;  //Pointer to the path of the
                            //file containing storage object
 grfMode : LongInt;         //Specifies the access mode for the object
 stgfmt : DWORD;            //Specifies the storage file format
 grfAttrs : DWORD;          //Reserved; must be zero
 pStgOptions : Pointer;     //Address of STGOPTIONS pointer
 reserved2 : Pointer;       //Reserved; must be zero
 riid : PGUID;              //Specifies the GUID of the interface pointer
 Out stgOpen :              //Address of an interface pointer
 IStorage ) : HResult; StdCall; External 'ole32.dll';

Const
  FMTID_SummaryInformation: TGUID =     '{F29F85E0-4FF9-1068-AB91-08002B27B3D9}';
  FMTID_DocSummaryInformation : TGUID = '{D5CDD502-2E9C-101B-9397-08002B2CF9AE}';
  FMTID_UserDefinedProperties : TGUID = '{D5CDD505-2E9C-101B-9397-08002B2CF9AE}';
  IID_IPropertySetStorage : TGUID =     '{0000013A-0000-0000-C000-000000000046}';

  STGFMT_FILE = 3; //Indicates that the file must not be a compound file.
                   //This element is only valid when using the StgCreateStorageEx
                   //or StgOpenStorageEx functions to access the NTFS file system
                   //implementation of the IPropertySetStorage interface.
                   //Therefore, these functions return an error if the riid
                   //parameter does not specify the IPropertySetStorage interface,
                   //or if the specified file is not located on an NTFS file system volume.

  STGFMT_ANY = 4; //Indicates that the system will determine the file type and
                  //use the appropriate structured storage or property set
                  //implementation.
                  //This value cannot be used with the StgCreateStorageEx function.


  // Summary Information
  PID_TITLE        = 2;
  PID_SUBJECT      = 3;
  PID_AUTHOR       = 4;
  PID_KEYWORDS     = 5;
  PID_COMMENTS     = 6;
  PID_TEMPLATE     = 7;
  PID_LASTAUTHOR   = 8;
  PID_REVNUMBER    = 9;
  PID_EDITTIME     = 10;
  PID_LASTPRINTED  = 11;
  PID_CREATE_DTM   = 12;
  PID_LASTSAVE_DTM = 13;
  PID_PAGECOUNT    = 14;
  PID_WORDCOUNT    = 15;
  PID_CHARCOUNT    = 16;
  PID_THUMBNAIL    = 17;
  PID_APPNAME      = 18;
  PID_SECURITY     = 19;

  // Document Summary Information
  PID_CATEGORY     = 2;
  PID_PRESFORMAT   = 3;
  PID_BYTECOUNT    = 4;
  PID_LINECOUNT    = 5;
  PID_PARCOUNT     = 6;
  PID_SLIDECOUNT   = 7;
  PID_NOTECOUNT    = 8;
  PID_HIDDENCOUNT  = 9;
  PID_MMCLIPCOUNT  = 10;
  PID_SCALE        = 11;
  PID_HEADINGPAIR  = 12;
  PID_DOCPARTS     = 13;
  PID_MANAGER      = 14;
  PID_COMPANY      = 15;
  PID_LINKSDIRTY   = 16;
  PID_CHARCOUNT2   = 17;

{----------------------------------------------------------------------------------------}
Function PropVariantIsString(Const Prop: TPropVariant): Boolean;
Begin { PropVariantIsString }
  Result := (Prop.vt=VT_BSTR) Or (Prop.vt=VT_LPSTR) Or (Prop.vt=VT_LPWSTR);
End; { PropVariantIsString }
{----------------------------------------------------------------------------------------}
Function PropVariantToString(Const Prop: TPropVariant): String;
Var
  TheDate: TFileTime;
  SysDate: TSystemTime;
Begin { PropVariantToString }
  Case Prop.vt Of
    VT_EMPTY, VT_NULL, VT_VOID: Result := '';

    VT_I2: Result := IntToStr(Prop.iVal);
    VT_I4, VT_INT: Result := IntToStr(Prop.lVal);
    VT_R4: Result := FloatToStr(Prop.fltVal);
    VT_R8: Result := FloatToStr(Prop.dblVal);
    VT_BSTR: Result := Prop.bstrVal;
    VT_ERROR: Result := 'Error code: ' + IntToStr(Prop.sCode);
    VT_BOOL: If Prop.boolVal = False Then
               Result := 'False'
             Else
               Result := 'True';
    VT_UI1: Result := IntToStr(Prop.bVal);
    VT_UI2: Result := IntToStr(Prop.uiVal);
    VT_UI4, VT_UINT: Result := IntToStr(Prop.ulVal);
    VT_I8: Result := IntToStr(Int64(Prop.hVal));
    VT_UI8: Result := IntToStr(Int64(Prop.uhVal));
    VT_LPSTR: Result := Prop.pszVal ;
    VT_LPWSTR: Result := Prop.pwszVal;
    VT_FILETIME:
      Begin { Else }
        FileTimeToLocalFileTime(TFileTime(Prop.date), TheDate);
        FileTimeToSystemTime(TheDate, SysDate);
        Result := DateTimeToStr(SystemTimeToDateTime(SysDate));
      End; { Else }
    VT_CLSID: Result := GuidToString(Prop.puuid^);
    Else
      Result := '';
  End; { Case }
End; { PropVariantToString }
{----------------------------------------------------------------------------------------}
Function SummaryPIDToCaption(Const ePID: Cardinal): String;
Begin { SummaryPIDToCaption }
  Case ePID Of
    PID_TITLE        : Result := 'Title';
    PID_SUBJECT      : Result := 'Subject';
    PID_AUTHOR       : Result := 'Author';
    PID_KEYWORDS     : Result := 'Keywords';
    PID_COMMENTS     : Result := 'Comments';
    PID_TEMPLATE     : Result := 'Template';
    PID_LASTAUTHOR   : Result := 'Last Saved By';
    PID_REVNUMBER    : Result := 'Revision Number';
    PID_EDITTIME     : Result := 'Total Editing Time';
    PID_LASTPRINTED  : Result := 'Last Printed';
    PID_CREATE_DTM   : Result := 'Create Time/Date';
    PID_LASTSAVE_DTM : Result := 'Last Saved Time/Date';
    PID_PAGECOUNT    : Result := 'Number of Pages';
    PID_WORDCOUNT    : Result := 'Number of Words';
    PID_CHARCOUNT    : Result := 'Number of Characters';
    PID_THUMBNAIL    : Result := 'Thumbnail';
    PID_APPNAME      : Result := 'Application';
    PID_SECURITY     : Result := 'Security';
  Else
    Result := '$' + IntToHex(ePID, 8);
  End; { Case }

  Result := '%' + Result + '%';
End; { SummaryPIDToCaption }
{----------------------------------------------------------------------------------------}
Function DocSummaryPIDToCaption(Const ePID: Cardinal): String;
Begin { DocSummaryPIDToCaption }
  Case ePID Of
    PID_CATEGORY    : Result := 'Category';
    PID_PRESFORMAT  : Result := 'PRESFORMAT';
    PID_BYTECOUNT   : Result := 'Byte Count';
    PID_LINECOUNT   : Result := 'Line Count';
    PID_PARCOUNT    : Result := 'Paragraph Count';
    PID_SLIDECOUNT  : Result := 'Slide Count';
    PID_NOTECOUNT   : Result := 'Note count';
    PID_HIDDENCOUNT : Result := 'Hidden Count';
    PID_MMCLIPCOUNT : Result := 'MM Clip Count';
    PID_SCALE       : Result := 'Scale';
    PID_HEADINGPAIR : Result := 'Heading Pair';
    PID_DOCPARTS    : Result := 'Doc Parts';
    PID_MANAGER     : Result := 'Manager';
    PID_COMPANY     : Result := 'Company';
    PID_LINKSDIRTY  : Result := 'Links Dirty?';
    PID_CHARCOUNT2  : Result := 'Character count';
  Else
    Result := '$' + IntToHex(ePID, 8);
  End; { Case }

  Result := '%' + Result + '%';
End; { DocSummaryPIDToCaption }


{ TTagStorage }

{----------------------------------------------------------------------------------------}
Function TTagStorage.EnumerateReadProperties(oStorage: IStorage; oFMT: TGuid): Boolean;
Var
  hr: HResult;
  oPropEnum: IEnumSTATPROPSTG;
  oPropertyStorage: IPropertyStorage;
  oPropSpec: TPropSpec;
  PropStg: STATPROPSTG;
  vProp: TPropVariant;
  sTag : String;
  sValue : String;
  bTemp : Boolean;

Begin { EnumerateReadProperties }
  hr := (oStorage As IPropertySetStorage).Open(oFMT, STGM_READ Or STGM_SHARE_EXCLUSIVE, oPropertyStorage);
  Result := Assigned(oPropertyStorage) And Succeeded(hr);
  If Result Then
  Begin { If }
    OleCheck(oPropertyStorage.Enum(oPropEnum));

    hr := oPropEnum.Next(1, PropStg, Nil);
    While hr = S_OK Do
    Begin { While }
      oPropSpec.ulKind := PRSPEC_PROPID;
      oPropSpec.propid := PropStg.propid;
      OleCheck(oPropertyStorage.ReadMultiple(1, @oPropSpec, @vProp));

      sValue := PropVariantToString(vProp);
      If IsEqualGUID(oFMT, FMTID_SummaryInformation) Then
        sTag := SummaryPIDToCaption(oPropSpec.propid)
      Else If IsEqualGUID(oFMT, FMTID_DocSummaryInformation) Then
        sTag := DocSummaryPIDToCaption(oPropSpec.propid)
      Else If IsEqualGUID(oFMT, FMTID_UserDefinedProperties) Then
        sTag := '%'+IntToHex(oPropSpec.propid, 8)+'%';

      bTemp := Not PropVariantIsString(vProp);
      AddValue(sTag, sValue, bTemp, Not bTemp);

      hr := oPropEnum.Next(1, PropStg, Nil);
    End; { While }
  End; { If }
End; { EnumerateReadProperties }

{ TTagCompoundStorage }

{----------------------------------------------------------------------------------------}
Function TTagCompoundStorage.Name: String;
Begin { Name }
  Result := 'Compound File';
End; { Name }

{----------------------------------------------------------------------------------------}
Function TTagCompoundStorage.ParseFile(sFilename: String): Boolean;
Var
  hr: HResult;
  oStorage: IStorage;
  pFilename : Array[0..255] Of WideChar;
Begin { ParseFile }
  Result := Inherited ParseFile(sFilename);

  If FileExists(sFilename) Then
  Begin { If }
    StringToWideChar(sFilename, pFilename, length(sFilename)+1);

    hr := StgOpenStorage(pFilename, Nil, STGM_READ Or STGM_SHARE_EXCLUSIVE, Nil, 0, oStorage);
    If Assigned(oStorage) And Succeeded(hr) Then
    Begin { If }
      Result := True;

      FHasTag := EnumerateReadProperties(oStorage, FMTID_SummaryInformation);
      FHasTag := EnumerateReadProperties(oStorage, FMTID_DocSummaryInformation) Or FHasTag;
      FHasTag := EnumerateReadProperties(oStorage, FMTID_UserDefinedProperties) Or FHasTag;
    End; { If }
  End; { If }
End; { parseFile }

{----------------------------------------------------------------------------------------}
Function TTagCompoundStorage.Write: Boolean;
Begin { Write }

End; { Write }

{ TTagNTFSStorage }

{----------------------------------------------------------------------------------------}
Function TTagNTFSStorage.Name: String;
Begin { Name }
  Result := 'NTFS Stream';
End; { Name }

{----------------------------------------------------------------------------------------}
Function TTagNTFSStorage.ParseFile(sFilename: String): Boolean;
Var
  hr: HResult;
  oStorage: IStorage;
  pFilename : Array[0..255] Of WideChar;
Begin { ParseFile }
  Result := Inherited ParseFile(sFilename);

  If FileExists(sFilename) Then
  Begin { If }
    StringToWideChar(sFilename, pFilename, length(sFilename)+1);

    hr := StgOpenStorageEx(pFileName, STGM_READ Or STGM_SHARE_DENY_WRITE, STGFMT_FILE, 0, Nil,  Nil, @IID_IPropertySetStorage, oStorage);
    If Assigned(oStorage) And Succeeded(hr) Then
    Begin { If }
      Result := True;

      FHasTag := EnumerateReadProperties(oStorage, FMTID_SummaryInformation);
      FHasTag := EnumerateReadProperties(oStorage, FMTID_DocSummaryInformation) Or FHasTag;
      FHasTag := EnumerateReadProperties(oStorage, FMTID_UserDefinedProperties) Or FHasTag;
    End; { If }
  End; { If }
End; { ParseFile }

{----------------------------------------------------------------------------------------}
Function TTagNTFSStorage.Write: Boolean;
Begin { Write }

End; { Write }

Initialization
  TagManager.Register(TTagCompoundStorage, ['.*']);
  TagManager.Register(TTagNTFSStorage,     ['.*']);

End.
