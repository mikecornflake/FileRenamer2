Unit DialogRenameOptions;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TdlgRenameOptions }

  TRenameOption = (roFiles, roFolders);
  TRenameOptions = Set Of TRenameOption;

  TdlgRenameOptions = Class(TForm)
    btnFileOnly: TButton;
    Button2: TButton;
    Button3: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Procedure btnOptionsClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  Private
    FOptions: TRenameOptions;
  Public
    Property Options: TRenameOptions read FOptions;
  End;

Var
  dlgRenameOptions: TdlgRenameOptions;

Implementation

{$R *.lfm}

{ TdlgRenameOptions }

Procedure TdlgRenameOptions.FormCreate(Sender: TObject);
Begin
  FOptions := [];
End;

Procedure TdlgRenameOptions.btnOptionsClick(Sender: TObject);
Begin
  Case (TButton(Sender).Tag) Of
    0: FOptions := [roFiles];
    1: FOptions := [roFolders];
    2: FOptions := [roFiles, roFolders];
  End;

  If FOptions = [] Then
    ModalResult := mrCancel
  Else
    ModalResult := mrOk;
End;

End.
