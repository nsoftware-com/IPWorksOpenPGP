object FormCreateKeyring: TFormCreateKeyring
  Left = 0
  Top = 0
  Caption = 'Create a new Keyring'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 619
    Height = 39
    Caption = 
      'To create a new keyring first select a directory to which the ke' +
      'yring will be saved. Next add new keys below. Note that this for' +
      'm provides a simple way to create a keyring for testing. For ful' +
      'l key management refer to the KeyMgr component documentation whi' +
      'ch provides much more functionality than what is shown here.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 60
    Width = 87
    Height = 13
    Caption = 'Keyring Directory:'
  end
  object txtKeyringDir: TEdit
    Left = 101
    Top = 55
    Width = 428
    Height = 21
    TabOrder = 0
  end
  object btnChoose: TButton
    Left = 535
    Top = 55
    Width = 82
    Height = 25
    Caption = 'Choose'
    TabOrder = 1
    OnClick = btnChooseClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 79
    Width = 619
    Height = 186
    Caption = 'Keys'
    TabOrder = 2
    object Label3: TLabel
      Left = 16
      Top = 131
      Width = 246
      Height = 13
      Caption = 'User Id in the form "First Last <user@email.com>":'
    end
    object Label4: TLabel
      Left = 275
      Top = 131
      Width = 59
      Height = 13
      Caption = 'Passphrase:'
    end
    object btnDelete: TButton
      Left = 527
      Top = 101
      Width = 82
      Height = 25
      Caption = 'Delete'
      TabOrder = 0
      OnClick = btnDeleteClick
    end
    object txtUserId: TEdit
      Left = 16
      Top = 150
      Width = 246
      Height = 21
      TabOrder = 1
      Text = 'John Doe <test@email.com>'
    end
    object txtPassphrase: TEdit
      Left = 275
      Top = 150
      Width = 246
      Height = 21
      TabOrder = 2
      Text = 'password'
    end
    object btnCreate: TButton
      Left = 527
      Top = 150
      Width = 82
      Height = 25
      Caption = 'Create'
      TabOrder = 3
      OnClick = btnCreateClick
    end
    object lvwKeys: TListView
      Left = 16
      Top = 16
      Width = 505
      Height = 109
      Columns = <
        item
          Caption = 'User ID'
          Width = 380
        end
        item
          Caption = 'Key ID'
          Width = 120
        end>
      RowSelect = True
      TabOrder = 4
      ViewStyle = vsReport
    end
  end
  object btnSave: TButton
    Left = 8
    Top = 271
    Width = 75
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object btnCancel: TButton
    Left = 101
    Top = 271
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object dlgKeyring: TFileOpenDialog
    DefaultFolder = 'C:\'
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 304
    Top = 256
  end
  object keymgr: TipgKeyMgr
    OnKeyList = keymgrKeyList
    Left = 392
    Top = 256
  end
end
