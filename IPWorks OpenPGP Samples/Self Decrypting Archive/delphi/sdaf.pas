(*
 * IPWorks OpenPGP 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks OpenPGP in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksopenpgp
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit sdaf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipgcore, ipgtypes, ipgsda;

type
  TFormSDA = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    txtSourceDir: TEdit;
    txtOutputName: TEdit;
    txtExtractorTitle: TEdit;
    btnBrowse: TButton;
    Label5: TLabel;
    btnCreate: TButton;
    memBannerText: TMemo;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    btnSaveOutput: TButton;
    ipgSDA1: TipgSDA;
    Label6: TLabel;
    txtPassword: TEdit;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnSaveOutputClick(Sender: TObject);
    procedure ipgSDA1Progress(Sender: TObject; const Filename: string;
      BytesProcessed: Int64; PercentProcessed: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSDA: TFormSDA;

implementation

{$R *.dfm}

procedure TFormSDA.btnBrowseClick(Sender: TObject);
var
  DirSelected: UnicodeString;
  options: TSelectDirOpts;
begin
  if SelectDirectory(DirSelected, options, 0) then
  begin
    ChDir(DirSelected);
    txtSourceDir.Text := DirSelected;
  end;
end;

procedure TFormSDA.btnSaveOutputClick(Sender: TObject);
begin
  SaveDialog1.Title := 'Save your EXE file.';
  SaveDialog1.Filter := 'Exe Files (*.exe)|*.exe';
  if SaveDialog1.Execute then
  begin
    txtOutputName.Text := SaveDialog1.Filename;
  end;
end;

procedure TFormSDA.ipgSDA1Progress(Sender: TObject; const Filename: string;
  BytesProcessed: Int64; PercentProcessed: Integer);
begin
  ProgressBar1.Position := PercentProcessed;
end;

procedure TFormSDA.btnCreateClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    ipgSDA1.SourceDirectory := txtSourceDir.Text;
    ipgSDA1.ArchiveFile := txtOutputName.Text;
    ipgSDA1.CaptionText := txtExtractorTitle.Text;
    ipgSDA1.BannerText := memBannerText.Text;

    ipgSDA1.RecurseSubdirectories := true;

    ipgSDA1.Password := txtPassword.Text;

    ipgSDA1.CreateSDA();

    ShowMessage('Complete!');
  except
    on ex: EIPWorksOpenPGP do
      ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

end.

