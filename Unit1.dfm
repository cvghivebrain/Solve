object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 309
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object editInput: TEdit
    Left = 8
    Top = 12
    Width = 434
    Height = 21
    TabOrder = 0
  end
  object btnGo: TButton
    Left = 448
    Top = 12
    Width = 73
    Height = 32
    Caption = 'Go'
    TabOrder = 1
    OnClick = btnGoClick
  end
  object memOutput: TMemo
    Left = 8
    Top = 56
    Width = 513
    Height = 235
    TabOrder = 2
  end
end
