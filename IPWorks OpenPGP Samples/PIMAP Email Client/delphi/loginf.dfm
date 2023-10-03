object FormLogin: TFormLogin
  Left = 389
  Top = 269
  BorderStyle = bsSingle
  Caption = 'Login'
  ClientHeight = 143
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 24
    Width = 63
    Height = 13
    Caption = 'IMAP Server:'
  end
  object Label2: TLabel
    Left = 12
    Top = 56
    Width = 28
    Height = 13
    Caption = 'User: '
  end
  object Label3: TLabel
    Left = 12
    Top = 88
    Width = 52
    Height = 13
    Caption = 'Password: '
  end
  object EditUser: TEdit
    Left = 88
    Top = 52
    Width = 175
    Height = 21
    TabOrder = 1
  end
  object EditIMAPServer: TEdit
    Left = 88
    Top = 20
    Width = 175
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 44
    Top = 112
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 148
    Top = 112
    Width = 75
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object EditPassword: TEdit
    Left = 88
    Top = 84
    Width = 175
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
end
