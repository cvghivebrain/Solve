program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  ExplodeFunc in 'ExplodeFunc.pas',
  CRCFunc in 'CRCFunc.pas',
  FileFunc in 'FileFunc.pas',
  SolveFunc in 'SolveFunc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
