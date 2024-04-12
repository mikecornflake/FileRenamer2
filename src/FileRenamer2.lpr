Program FileRenamer2;

{$mode objfpc}{$H+}
{$DEFINE UseCThreads}

Uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  lazcontrols, SysUtils, bufdataset_parser,
  FormFileRenamer2,
  Tags,
  TagMultimedia,
  TagEXIF, DialogScanFolder, ThreadFolderParser, TagCommon, DialogConvertVideo, DialogRenameOptions;

{$R *.res}

Begin
  SetHeapTraceOutput(ChangeFileExt(Application.Exename, '.trc'));
  Application.Scaled:=True;
  Application.Title:='File Renamer 2';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmRenamer, frmRenamer);
  Application.Run;
End.
