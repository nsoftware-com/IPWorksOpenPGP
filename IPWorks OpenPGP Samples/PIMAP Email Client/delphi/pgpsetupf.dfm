object FormPGPSetup: TFormPGPSetup
  Left = 0
  Top = 0
  Caption = 'OpenPGP Setup'
  ClientHeight = 224
  ClientWidth = 661
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 413
    Height = 13
    Caption = 
      'Select a keyring which holds keys to be used for decryption and ' +
      'signature verification.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 35
    Width = 641
    Height = 150
    Caption = 'Keys'
    TabOrder = 0
    object Label3: TLabel
      Left = 14
      Top = 50
      Width = 240
      Height = 13
      Caption = 'Private Key (Used to decrypt incoming messages) '
    end
    object Label5: TLabel
      Left = 14
      Top = 101
      Width = 183
      Height = 13
      Caption = 'Signer Key (Used to verify signatures)'
    end
    object Label4: TLabel
      Left = 394
      Top = 123
      Width = 59
      Height = 13
      Caption = 'Passphrase:'
    end
    object Label2: TLabel
      Left = 14
      Top = 26
      Width = 86
      Height = 13
      Caption = 'Keyring directory:'
    end
    object cboSignerKeys: TComboBox
      Left = 106
      Top = 120
      Width = 271
      Height = 21
      TabOrder = 0
    end
    object cboPrivateKeys: TComboBox
      Left = 106
      Top = 69
      Width = 271
      Height = 21
      TabOrder = 1
    end
    object txtPassphrase: TEdit
      Left = 459
      Top = 120
      Width = 140
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
    object btnBrowse: TButton
      Left = 605
      Top = 21
      Width = 25
      Height = 25
      Caption = '...'
      TabOrder = 3
      OnClick = btnBrowseClick
    end
    object txtKeyringDir: TEdit
      Left = 106
      Top = 23
      Width = 493
      Height = 21
      TabOrder = 4
    end
  end
  object Button1: TButton
    Left = 248
    Top = 191
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 329
    Top = 191
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object dlgKeyring: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 544
    Top = 8
  end
  object KeyMgr1: TipgKeyMgr
    OnKeyList = KeyMgr1KeyList
    Left = 584
    Top = 8
  end
end
