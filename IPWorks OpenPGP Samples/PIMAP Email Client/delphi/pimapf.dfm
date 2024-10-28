object FormPimap: TFormPimap
  Left = 281
  Top = 159
  BorderStyle = bsSingle
  Caption = 'PIMAP Demo'
  ClientHeight = 642
  ClientWidth = 764
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    764
    642)
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 708
    Height = 26
    Caption = 
      'Use this demo to connect to an IMAP server and check email.  Cli' +
      'ck the '#39'Login'#39' button to download the folder list from the IMAP ' +
      'server.  Then select a mailbox from the Tree and the List Box wi' +
      'll be populated.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 244
    Top = 276
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 244
    Top = 260
    Width = 3
    Height = 13
  end
  object Label1: TLabel
    Left = 170
    Top = 261
    Width = 32
    Height = 13
    Caption = 'From:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 170
    Top = 276
    Width = 48
    Height = 13
    Caption = 'Subject:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ListViewMessages: TListView
    Left = 170
    Top = 88
    Width = 586
    Height = 169
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = '#'
        Width = 45
      end
      item
        Caption = 'From'
        Width = 100
      end
      item
        Caption = 'Subject'
        Width = 100
      end
      item
        Caption = 'Date'
        Width = 100
      end
      item
        Caption = 'Size'
        Width = 100
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = ListViewMessagesClick
    ExplicitWidth = 576
  end
  object ListBoxMessage: TListBox
    Left = 170
    Top = 326
    Width = 585
    Height = 308
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    ExplicitWidth = 575
    ExplicitHeight = 298
  end
  object TreeViewMailboxes: TTreeView
    Left = 8
    Top = 88
    Width = 157
    Height = 546
    Anchors = [akLeft, akTop, akBottom]
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    OnClick = TreeViewMailboxesClick
    ExplicitHeight = 536
  end
  object ButtonLogin: TButton
    Left = 8
    Top = 58
    Width = 75
    Height = 25
    Caption = '&Login'
    TabOrder = 3
    OnClick = ButtonLoginClick
  end
  object btnPGPSetup: TButton
    Left = 89
    Top = 58
    Width = 75
    Height = 25
    Caption = 'PGP Setup'
    TabOrder = 4
    OnClick = btnPGPSetupClick
  end
  object btnDecrypt: TButton
    Left = 170
    Top = 295
    Width = 75
    Height = 25
    Caption = '&Decypt'
    TabOrder = 5
    OnClick = btnDecryptClick
  end
  object btnVerify: TButton
    Left = 251
    Top = 295
    Width = 75
    Height = 25
    Caption = '&Verify'
    TabOrder = 6
    OnClick = btnVerifyClick
  end
  object btnDecryptAndVerify: TButton
    Left = 332
    Top = 295
    Width = 102
    Height = 25
    Caption = 'Decrypt &and Verify'
    TabOrder = 7
    OnClick = btnDecryptAndVerifyClick
  end
  object PIMAP1: TipgPIMAP
    Mailbox = 'Inbox'
    SSLCertStore = 'MY'
    OnMailboxList = PIMAP1MailboxList
    OnMessageInfo = PIMAP1MessageInfo
    Left = 600
    Top = 40
  end
end


