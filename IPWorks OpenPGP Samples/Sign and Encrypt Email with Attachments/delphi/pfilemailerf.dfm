object FormPfilemailer: TFormPfilemailer
  Left = 0
  Top = 0
  Caption = 'PFileMailer Demo'
  ClientHeight = 616
  ClientWidth = 605
  Color = clBtnFace
  Constraints.MaxWidth = 621
  Constraints.MinHeight = 609
  Constraints.MinWidth = 621
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    605
    616)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 552
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'The PFileMailer component can be used to send signed and encrypt' +
      'ed emails with file attachments. It will handle all necessary en' +
      'coding automatically.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label5: TLabel
    AlignWithMargins = True
    Left = 22
    Top = 263
    Width = 36
    Height = 13
    Caption = 'Subject'
    Layout = tlCenter
  end
  object Attachments: TLabel
    AlignWithMargins = True
    Left = 22
    Top = 518
    Width = 61
    Height = 13
    Caption = 'Attachments'
  end
  object txtSubject: TEdit
    AlignWithMargins = True
    Left = 89
    Top = 263
    Width = 505
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object lbAttachments: TListBox
    AlignWithMargins = True
    Left = 89
    Top = 518
    Width = 302
    Height = 87
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnAdd: TButton
    AlignWithMargins = True
    Left = 397
    Top = 549
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add'
    TabOrder = 5
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    AlignWithMargins = True
    Left = 397
    Top = 580
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Remove'
    TabOrder = 6
    OnClick = btnRemoveClick
  end
  object btnSend: TButton
    AlignWithMargins = True
    Left = 478
    Top = 549
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Send'
    TabOrder = 7
    OnClick = btnSendClick
  end
  object txtMessage: TMemo
    AlignWithMargins = True
    Left = 8
    Top = 290
    Width = 586
    Height = 222
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      
        'This is a test message sent by the /n software PFileMailer compo' +
        'nent.'
      'Our website is http://www.nsoftware.com.'
      
        'If you have technical questions, you can email our Support Team ' +
        'at support@nsoftware.com.')
    TabOrder = 4
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 40
    Width = 589
    Height = 73
    Caption = 'Connection Settings'
    TabOrder = 1
    DesignSize = (
      589
      73)
    object Label2: TLabel
      AlignWithMargins = True
      Left = 14
      Top = 44
      Width = 53
      Height = 13
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = 'Mail Server'
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 14
      Top = 17
      Width = 39
      Height = 13
      Alignment = taCenter
      Caption = 'Send To'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Label4: TLabel
      AlignWithMargins = True
      Left = 326
      Top = 17
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'From'
      Layout = tlCenter
    end
    object txtFrom: TEdit
      AlignWithMargins = True
      Left = 356
      Top = 17
      Width = 230
      Height = 21
      Anchors = [akTop, akRight]
      AutoSize = False
      TabOrder = 1
    end
    object txtMailServer: TEdit
      AlignWithMargins = True
      Left = 81
      Top = 44
      Width = 505
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object txtSendTo: TEdit
      AlignWithMargins = True
      Left = 81
      Top = 17
      Width = 230
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 119
    Width = 589
    Height = 138
    Caption = 'PGP Settings'
    TabOrder = 2
    object Label6: TLabel
      AlignWithMargins = True
      Left = 14
      Top = 14
      Width = 36
      Height = 13
      Alignment = taRightJustify
      Caption = 'Keyring'
      Layout = tlCenter
    end
    object Label7: TLabel
      AlignWithMargins = True
      Left = 14
      Top = 41
      Width = 223
      Height = 13
      Alignment = taRightJustify
      Caption = 'Private Key (used to sign outgoing messages):'
      Layout = tlCenter
    end
    object Label8: TLabel
      AlignWithMargins = True
      Left = 14
      Top = 87
      Width = 252
      Height = 13
      Alignment = taRightJustify
      Caption = 'Recipient Key (Used to encrypt outgoing messages):'
      Layout = tlCenter
    end
    object Label9: TLabel
      Left = 324
      Top = 64
      Width = 59
      Height = 13
      Caption = 'Passphrase:'
    end
    object cboPrivateKeys: TComboBox
      AlignWithMargins = True
      Left = 81
      Top = 60
      Width = 230
      Height = 21
      TabOrder = 6
      TabStop = False
    end
    object cboRecipientKeys: TComboBox
      AlignWithMargins = True
      Left = 81
      Top = 106
      Width = 230
      Height = 21
      TabOrder = 0
      TabStop = False
    end
    object txtKeyring: TEdit
      AlignWithMargins = True
      Left = 81
      Top = 14
      Width = 473
      Height = 21
      TabStop = False
      TabOrder = 2
    end
    object btnLoadKeyring: TButton
      Left = 560
      Top = 14
      Width = 26
      Height = 25
      Caption = '...'
      TabOrder = 1
      OnClick = btnLoadKeyringClick
    end
    object chkEncrypt: TCheckBox
      Left = 389
      Top = 108
      Width = 97
      Height = 17
      Caption = 'Encrypt'
      TabOrder = 4
    end
    object chkSign: TCheckBox
      Left = 457
      Top = 108
      Width = 97
      Height = 17
      Caption = 'Sign'
      TabOrder = 5
    end
    object txtPassphrase: TEdit
      Left = 389
      Top = 60
      Width = 197
      Height = 21
      TabOrder = 3
    end
  end
  object odFileBrowse: TOpenDialog
    Options = []
    Left = 552
    Top = 568
  end
  object ipgPFileMailer1: TipgPFileMailer
    CompressionMethod = 'zip'
    EncryptingAlgorithm = 'AES128'
    SigningAlgorithm = 'SHA1'
    SSLCertStore = 'MY'
    Left = 552
    Top = 504
  end
  object ipgKeyMgr1: TipgKeyMgr
    OnKeyList = ipgKeyMgr1KeyList
    Left = 488
    Top = 503
  end
  object dlgKeyring: TFileOpenDialog
    DefaultFolder = 'C:\'
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 480
    Top = 568
  end
  object FileOpenDialog2: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 416
    Top = 504
  end
end


