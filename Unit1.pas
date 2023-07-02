unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StrUtils, StdCtrls, ExplodeFunc, CRCFunc, FileFunc, SolveFunc;

type
  TForm1 = class(TForm)
    editInput: TEdit;
    btnGo: TButton;
    memOutput: TMemo;
    procedure btnGoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnGoClick(Sender: TObject);
begin
  SetLength(filearray,1);
  filearray[0] := 69;
  memOutput.Clear;
  val := 999;
  memOutput.Lines.Add(SolveStr(editInput.Text));
end;

end.
