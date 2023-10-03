object FormSDA: TFormSDA
  Left = 200
  Top = 120
  Caption = 'SDA Demo'
  ClientHeight = 286
  ClientWidth = 507
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
    Top = 6
    Width = 441
    Height = 26
    Caption = 
      'This demo shows how to use the SDA component to create your own ' +
      'native self-decrypting archive executable.'
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
    Top = 40
    Width = 87
    Height = 13
    Caption = 'Source Directory: '
  end
  object Label3: TLabel
    Left = 8
    Top = 65
    Width = 91
    Height = 13
    Caption = 'Output exe name: '
  end
  object Label4: TLabel
    Left = 8
    Top = 90
    Width = 73
    Height = 13
    Caption = 'Extractor title: '
  end
  object Label5: TLabel
    Left = 10
    Top = 148
    Width = 154
    Height = 13
    Caption = 'Extractor banner text (optional)'
  end
  object Label6: TLabel
    Left = 8
    Top = 115
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object txtSourceDir: TEdit
    Left = 117
    Top = 37
    Width = 340
    Height = 21
    TabOrder = 0
    Text = 'C:\test'
  end
  object txtOutputName: TEdit
    Left = 117
    Top = 63
    Width = 340
    Height = 21
    TabOrder = 1
    Text = 'C:\myExtractor.exe'
  end
  object txtExtractorTitle: TEdit
    Left = 117
    Top = 87
    Width = 380
    Height = 21
    TabOrder = 2
    Text = 'My Software Extractor'
  end
  object btnBrowse: TButton
    Left = 466
    Top = 35
    Width = 31
    Height = 25
    Caption = '...'
    TabOrder = 3
    OnClick = btnBrowseClick
  end
  object btnCreate: TButton
    Left = 424
    Top = 137
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 4
    OnClick = btnCreateClick
  end
  object memBannerText: TMemo
    Left = 8
    Top = 168
    Width = 489
    Height = 90
    Lines.Strings = (
      
        'This banner text will display before the start of the extraction' +
        ', when the self-extractor exe first '
      
        'gets run.  Here you can optionally display important information' +
        ' to users of your software.  If this '
      'is left blank, no banner text will be shown at all..')
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 264
    Width = 489
    Height = 17
    TabOrder = 6
  end
  object btnSaveOutput: TButton
    Left = 466
    Top = 61
    Width = 31
    Height = 25
    Caption = '...'
    TabOrder = 7
    OnClick = btnSaveOutputClick
  end
  object txtPassword: TEdit
    Left = 117
    Top = 112
    Width = 204
    Height = 21
    PasswordChar = '*'
    TabOrder = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 456
  end
  object ipgSDA1: TipgSDA
    FileToExecute = '.'
    OnProgress = ipgSDA1Progress
    Left = 360
    Top = 112
  end
end


