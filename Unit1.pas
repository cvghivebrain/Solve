unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StrUtils, StdCtrls, SolveFunc;

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
  SetLength(filearray,3);
  filearray[0] := Ord(' ');
  filearray[1] := Ord('5');
  filearray[2] := Ord('5');
  filearray[3] := 0;
  fs := 4;
  memOutput.Clear;
  val := 999;
  memOutput.Lines.Add(SolveStr(editInput.Text));
end;

end.
