object FormOpenPGP: TFormOpenPGP
  Left = 200
  Top = 120
  Caption = 'OpenPGP Demo'
  ClientHeight = 543
  ClientWidth = 943
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 622
    Height = 13
    Caption = 
      'This demo shows how to use the OpenPGP component to encrypt, sig' +
      'n, decrypt, and verify signatures. To begin select a keyring.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 11
    Top = 30
    Width = 924
    Height = 211
    Caption = 'Keys'
    TabOrder = 0
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 86
      Height = 13
      Caption = 'Keyring directory:'
    end
    object Label3: TLabel
      Left = 16
      Top = 52
      Width = 378
      Height = 13
      Caption = 
        'Private Key (Used to sign outgoing messages and decrypt incoming' +
        ' messages) '
    end
    object Label4: TLabel
      Left = 551
      Top = 77
      Width = 59
      Height = 13
      Caption = 'Passphrase:'
    end
    object Label5: TLabel
      Left = 16
      Top = 104
      Width = 248
      Height = 13
      Caption = 'Recipient Key (Used to encrypt outgoing messages)'
    end
    object Label6: TLabel
      Left = 16
      Top = 156
      Width = 314
      Height = 13
      Caption = 'Signer Key (Used to verify the signature of an incoming message)'
    end
    object txtKeyringDir: TEdit
      Left = 137
      Top = 21
      Width = 642
      Height = 21
      TabOrder = 0
    end
    object btnCreate: TButton
      Left = 816
      Top = 19
      Width = 94
      Height = 25
      Caption = 'Create New'
      TabOrder = 1
      OnClick = btnCreateClick
    end
    object cboPrivateKeys: TComboBox
      Left = 137
      Top = 74
      Width = 408
      Height = 21
      TabOrder = 2
    end
    object txtPassphrase: TEdit
      Left = 639
      Top = 74
      Width = 140
      Height = 21
      TabOrder = 3
    end
    object cboRecipientKeys: TComboBox
      Left = 137
      Top = 126
      Width = 408
      Height = 21
      TabOrder = 4
    end
    object cboSignerKeys: TComboBox
      Left = 137
      Top = 178
      Width = 408
      Height = 21
      TabOrder = 5
    end
    object btnBrowse: TButton
      Left = 785
      Top = 19
      Width = 25
      Height = 25
      Caption = '...'
      TabOrder = 6
      OnClick = btnBrowseClick
    end
  end
  object ProgressBar1: TProgressBar
    Left = 11
    Top = 511
    Width = 924
    Height = 25
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 247
    Width = 927
    Height = 258
    ActivePage = tsFile
    TabOrder = 2
    object tsFile: TTabSheet
      Caption = 'File to File'
      object Label7: TLabel
        Left = 15
        Top = 95
        Width = 49
        Height = 13
        Caption = 'Input File:'
      end
      object Label8: TLabel
        Left = 15
        Top = 144
        Width = 57
        Height = 13
        Caption = 'Output File:'
      end
      object Label9: TLabel
        Left = 480
        Top = 200
        Width = 3
        Height = 13
      end
      object lblStatus: TLabel
        Left = 472
        Top = 200
        Width = 35
        Height = 13
        Caption = 'Status:'
      end
      object GroupBox2: TGroupBox
        Left = 15
        Top = 16
        Width = 409
        Height = 73
        Caption = 'Action'
        TabOrder = 0
        object rbEncrypt: TRadioButton
          Left = 16
          Top = 24
          Width = 73
          Height = 17
          Caption = 'Encrypt'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rbDecrypt: TRadioButton
          Left = 16
          Top = 47
          Width = 73
          Height = 17
          Caption = 'Decrypt'
          TabOrder = 1
        end
        object rbSign: TRadioButton
          Left = 121
          Top = 24
          Width = 73
          Height = 17
          Caption = 'Sign'
          TabOrder = 2
        end
        object rbVerify: TRadioButton
          Left = 121
          Top = 47
          Width = 73
          Height = 17
          Caption = 'Verify'
          TabOrder = 3
        end
        object rbSignAndEncrypt: TRadioButton
          Left = 240
          Top = 24
          Width = 137
          Height = 17
          Caption = 'Sign and Encrypt'
          TabOrder = 4
        end
        object rbDecryptAndVerify: TRadioButton
          Left = 240
          Top = 47
          Width = 146
          Height = 17
          Caption = 'Decrypt and Verify'
          TabOrder = 5
        end
      end
      object txtInputFile: TEdit
        Left = 15
        Top = 114
        Width = 794
        Height = 21
        TabOrder = 1
      end
      object txtOutputFile: TEdit
        Left = 15
        Top = 163
        Width = 794
        Height = 21
        TabOrder = 2
      end
      object chbOverwrite: TCheckBox
        Left = 15
        Top = 193
        Width = 177
        Height = 17
        Caption = 'Overwrite Existing File'
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 3
      end
      object btnGo: TButton
        Left = 384
        Top = 193
        Width = 75
        Height = 25
        Caption = 'Go!'
        TabOrder = 4
        OnClick = btnGoClick
      end
      object btnBrowseInput: TButton
        Left = 815
        Top = 112
        Width = 94
        Height = 25
        Caption = 'Browse...'
        TabOrder = 5
        OnClick = btnBrowseInputClick
      end
      object btnBrowseOutput: TButton
        Left = 815
        Top = 160
        Width = 94
        Height = 25
        Caption = 'Browse...'
        TabOrder = 6
        OnClick = btnBrowseOutputClick
      end
    end
    object tsString: TTabSheet
      Caption = 'String to String'
      ImageIndex = 1
      object txtDecryptedString: TMemo
        Left = 3
        Top = 10
        Width = 390
        Height = 217
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object txtEncryptedString: TMemo
        Left = 515
        Top = 10
        Width = 390
        Height = 217
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
      object btnEncrypt: TButton
        Left = 399
        Top = 24
        Width = 110
        Height = 25
        Caption = 'Encrypt->'
        TabOrder = 2
        OnClick = btnEncryptClick
      end
      object btnDecrypt: TButton
        Left = 399
        Top = 55
        Width = 110
        Height = 25
        Caption = '<-Decrypt'
        TabOrder = 3
        OnClick = btnDecryptClick
      end
      object btnSign: TButton
        Left = 399
        Top = 86
        Width = 110
        Height = 25
        Caption = 'Sign->'
        TabOrder = 4
        OnClick = btnSignClick
      end
      object btnVerify: TButton
        Left = 399
        Top = 117
        Width = 110
        Height = 25
        Caption = '<-Verify Sig'
        TabOrder = 5
        OnClick = btnVerifyClick
      end
      object btnSignAndEncrypt: TButton
        Left = 399
        Top = 148
        Width = 110
        Height = 25
        Caption = 'Sign and Encrypt->'
        TabOrder = 6
        OnClick = btnSignAndEncryptClick
      end
      object btnDecryptAndVerify: TButton
        Left = 399
        Top = 179
        Width = 110
        Height = 25
        Caption = '<-Decrypt and Verify'
        TabOrder = 7
        OnClick = btnDecryptAndVerifyClick
      end
    end
  end
  object dlgInputFile: TOpenDialog
    InitialDir = 'C:\'
    Left = 736
    Top = 232
  end
  object dlgOutputFile: TSaveDialog
    InitialDir = 'C:\'
    Left = 808
    Top = 232
  end
  object dlgKeyring: TFileOpenDialog
    DefaultFolder = 'C:\'
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 672
    Top = 232
  end
  object keymgr: TipgKeyMgr
    OnKeyList = keymgrKeyList
    Left = 704
  end
  object pgp: TipgOpenPGP
    CompressionMethod = 'zip'
    EncryptingAlgorithm = 'AES128'
    SigningAlgorithm = 'SHA1'
    OnProgress = pgpProgress
    Left = 760
  end
end


