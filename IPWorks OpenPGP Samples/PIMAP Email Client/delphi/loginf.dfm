object FormLogin: TFormLogin
  Left = 389
  Top = 269
  BorderStyle = bsSingle
  Caption = 'Login'
  ClientHeight = 158
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 23
    Width = 63
    Height = 13
    Caption = 'IMAP Server:'
  end
  object Label2: TLabel
    Left = 8
    Top = 77
    Width = 28
    Height = 13
    Caption = 'User: '
  end
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 52
    Height = 13
    Caption = 'Password: '
  end
  object Label4: TLabel
    Left = 8
    Top = 50
    Width = 51
    Height = 13
    Caption = 'IMAP Port:'
  end
  object EditUser: TEdit
    Left = 87
    Top = 74
    Width = 175
    Height = 21
    TabOrder = 2
  end
  object EditIMAPServer: TEdit
    Left = 88
    Top = 20
    Width = 175
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 43
    Top = 128
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object Button2: TButton
    Left = 147
    Top = 128
    Width = 75
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object EditPassword: TEdit
    Left = 87
    Top = 101
    Width = 175
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object EditIMAPPort: TEdit
    Left = 87
    Top = 47
    Width = 175
    Height = 21
    TabOrder = 1
  end
end
