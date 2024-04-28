Unit DialogScanFolder;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls, Menus, Buttons;

Type

  { TdlgScanFolder }

  TdlgScanFolder = Class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbProcessMetaData: TCheckBox;
    edtFolder: TDirectoryEdit;
    edtFilter: TEditButton;
    Label1: TLabel;
    Label2: TLabel;
    mnuFilter: TPopupMenu;
    Procedure DoSelectFilter(ASender: TObject);
    Procedure edtFilterButtonClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
  Private
    FProcessMeta: Boolean;
    FActivated: Boolean;
    Function GetFilter: String;
    Function GetFolder: String;
    Function GetProcessMeta: Boolean;
    Procedure SetFilter(AValue: String);
    Procedure SetFolder(AValue: String);

  Public
    Constructor Create(TheOwner: TComponent); Override;

    Property Folder: String read GetFolder write SetFolder;
    Property Filter: String read GetFilter write SetFilter;

    Property ProcessMeta: Boolean read GetProcessMeta write FProcessMeta;
  End;

Implementation

Uses
  Tags, DBSupport, StringSupport;

{$R *.lfm}

Constructor TdlgScanFolder.Create(TheOwner: TComponent);
Var
  oMenuItem: TMenuItem;
  oFileTagger: TMetaFileHandler;
Begin
  Inherited Create(TheOwner);

  oMenuItem := TMenuItem.Create(mnuFilter);
  oMenuItem.Caption := 'All files (*.*)';
  oMenuItem.OnClick := @DoSelectFilter;

  mnuFilter.Items.Add(oMenuItem);

  For oFileTagger In TagManager.FileTaggers Do
  Begin
    oMenuItem := TMenuItem.Create(mnuFilter);
    oMenuItem.Caption := Format('%s (%s)', [oFileTagger.Name, oFileTagger.Filter]);
    oMenuItem.OnClick := @DoSelectFilter;

    mnuFilter.Items.Add(oMenuItem);
  End;

  FProcessMeta := True;
  FActivated := False;
End;

Procedure TdlgScanFolder.DoSelectFilter(ASender: TObject);
Begin
  If ASender Is TMenuItem Then
    edtFilter.Text := TextBetween(TMenuItem(ASender).Caption, '(', ')');
End;

Procedure TdlgScanFolder.edtFilterButtonClick(Sender: TObject);
Var
  oPoint: TPoint;
Begin
  oPoint := Point(edtFilter.Left + edtFilter.Width - edtFilter.ButtonWidth,
    edtFilter.Top + edtFilter.Height);
  oPoint := ClientToScreen(oPoint);
  mnuFilter.PopUp(oPoint.X, oPoint.Y);
End;

Procedure TdlgScanFolder.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    cbProcessMetaData.Checked := FProcessMeta;
    FActivated := True;
  End;
End;

Function TdlgScanFolder.GetFilter: String;
Begin
  Result := edtFilter.Text;
End;

Function TdlgScanFolder.GetFolder: String;
Begin
  Result := edtFolder.Text;
End;

Function TdlgScanFolder.GetProcessMeta: Boolean;
Begin
  Result := cbProcessMetaData.Checked;
End;

Procedure TdlgScanFolder.SetFilter(AValue: String);
Begin
  edtFilter.Text := AValue;
End;

Procedure TdlgScanFolder.SetFolder(AValue: String);
Begin
  edtFolder.Text := AValue;
End;

End.
