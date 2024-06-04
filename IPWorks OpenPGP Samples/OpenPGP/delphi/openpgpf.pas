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
unit openpgpf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, FileCtrl, ipgcore, ipgtypes,
  ipgopenpgp, ipgkeymgr, createkeyringf;

type
  TFormOpenPGP = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtKeyringDir: TEdit;
    btnCreate: TButton;
    cboPrivateKeys: TComboBox;
    txtPassphrase: TEdit;
    cboRecipientKeys: TComboBox;
    cboSignerKeys: TComboBox;
    btnBrowse: TButton;
    ProgressBar1: TProgressBar;
    PageControl1: TPageControl;
    tsFile: TTabSheet;
    Label7: TLabel;
    Label8: TLabel;
    GroupBox2: TGroupBox;
    rbEncrypt: TRadioButton;
    rbDecrypt: TRadioButton;
    rbSign: TRadioButton;
    rbVerify: TRadioButton;
    rbSignAndEncrypt: TRadioButton;
    rbDecryptAndVerify: TRadioButton;
    txtInputFile: TEdit;
    txtOutputFile: TEdit;
    chbOverwrite: TCheckBox;
    btnGo: TButton;
    btnBrowseInput: TButton;
    btnBrowseOutput: TButton;
    tsString: TTabSheet;
    txtDecryptedString: TMemo;
    txtEncryptedString: TMemo;
    btnEncrypt: TButton;
    btnDecrypt: TButton;
    btnSign: TButton;
    btnVerify: TButton;
    btnSignAndEncrypt: TButton;
    btnDecryptAndVerify: TButton;
    dlgInputFile: TOpenDialog;
    dlgOutputFile: TSaveDialog;
    dlgKeyring: TFileOpenDialog;
    keymgr: TipgKeyMgr;
    pgp: TipgOpenPGP;
    lblStatus: TLabel;
    procedure btnCreateClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnBrowseInputClick(Sender: TObject);
    procedure btnBrowseOutputClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure keymgrKeyList(Sender: TObject; const UserId, KeyId,
      Fingerprint: string; HasSecretKey: Boolean;
      const PublicKeyAlgorithm: string; PublicKeyLength: Integer);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnSignAndEncryptClick(Sender: TObject);
    procedure btnDecryptAndVerifyClick(Sender: TObject);
    procedure pgpProgress(Sender: TObject; BytesProcessed: Int64;
    PercentProcessed, Operation: Integer; var IsEOF: Boolean);
  private
    procedure listKeys;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOpenPGP: TFormOpenPGP;

implementation

{$R *.dfm}



procedure TFormOpenPGP.btnBrowseClick(Sender: TObject);
begin
  if(txtKeyringDir.Text <> '') then
    dlgKeyring.DefaultFolder := txtKeyringDir.Text;
  if (dlgKeyring.Execute()) then
  begin
	txtKeyringDir.Text := dlgKeyring.FileName;
	listKeys();
  end;
end;

procedure TFormOpenPGP.btnBrowseInputClick(Sender: TObject);
begin
  if(dlgInputFile.Execute()) then
  begin
	txtInputFile.Text := dlgInputFile.FileName;
	dlgInputFile.InitialDir := dlgInputFile.GetNamePath();
  end;
end;

procedure TFormOpenPGP.btnBrowseOutputClick(Sender: TObject);
begin
  if(dlgOutputFile.Execute()) then
  begin
	txtOutputFile.Text := dlgOutputFile.FileName;
	dlgOutputFile.InitialDir := dlgOutputFile.GetNamePath();
  end;
end;

procedure TFormOpenPGP.btnCreateClick(Sender: TObject);
begin
  FormCreateKeyring.txtKeyringDir.Text := txtKeyringDir.Text;
  if(FormCreateKeyring.ShowModal() = mrOk) then
  begin
    txtKeyringDir.Text := FormCreateKeyring.txtKeyringDir.Text;
    listKeys();
  end;
end;



procedure TFormOpenPGP.btnGoClick(Sender: TObject);
begin
  lblStatus.Caption := 'Status:';
  Progressbar1.Position := 0;
  if (Length(txtInputFile.Text) <= 0) then
  begin
    ShowMessage('Please select an input file.');
    Exit;
  end;
  if (Length(txtOutputFile.Text) <= 0) then
  begin
    ShowMessage('Please select an output file.');
    Exit;
  end;

  try
    Screen.Cursor := crHourGlass;
    pgp.Reset();
    pgp.InputFile := txtInputFile.Text;
    pgp.OutputFile := txtOutputFile.Text;
    pgp.Overwrite := chbOverwrite.Checked;
    pgp.ASCIIArmor := true;

    if (rbEncrypt.Checked) then
    begin
      if (Length(cboRecipientKeys.Text) <= 0) then
      begin
        ShowMessage('Please select a recipient key.');
        Exit;
      end;
      pgp.RecipientKeyCount := 1;
      pgp.RecipientKeyKeyring[0] := txtKeyringDir.Text;
      pgp.RecipientKeyUserId[0] := cboRecipientKeys.Text;

      pgp.Encrypt();
    end
    else if (rbDecrypt.Checked) then
    begin
      if (Length(cboPrivateKeys.Text) <= 0) then
      begin
        ShowMessage('Please select a private key.');
        Exit;
      end;
      pgp.KeyCount := 1;
      pgp.KeyKeyring[0] := txtKeyringDir.Text;
      pgp.KeyUserId[0] := cboPrivateKeys.Text;
      pgp.KeyPassphrase[0] := txtPassphrase.Text;

      pgp.Decrypt();
    end
    else if (rbSign.Checked) then
    begin
      if (Length(cboPrivateKeys.Text) <= 0) then
      begin
        ShowMessage('Please select a private key.');
        Exit;
      end;
      pgp.KeyCount := 1;
      pgp.KeyKeyring[0] := txtKeyringDir.Text;
      pgp.KeyUserId[0] := cboPrivateKeys.Text;
      pgp.KeyPassphrase[0] := txtPassphrase.Text;

      pgp.Sign();
    end
    else if (rbVerify.Checked) then
    begin
      if (Length(cboSignerKeys.Text) <= 0) then
      begin
        ShowMessage('Please select a signer key.');
        Exit;
      end;
      pgp.SignerKeyCount := 1;
      pgp.SignerKeyKeyring[0] := txtKeyringDir.Text;
      pgp.SignerKeyUserId[0] := cboPrivateKeys.Text;

      pgp.VerifySignature();
    end
    else if (rbSignAndEncrypt.Checked) then
    begin
      if (Length(cboPrivateKeys.Text) <= 0) then
      begin
        ShowMessage('Please select a private key.');
        Exit;
      end;
      if (Length(cboRecipientKeys.Text) <= 0) then
      begin
        ShowMessage('Please select a recipient key.');
        Exit;
      end;
      pgp.KeyCount := 1;
      pgp.KeyKeyring[0] := txtKeyringDir.Text;
      pgp.KeyUserId[0] := cboPrivateKeys.Text;
      pgp.KeyPassphrase[0] := txtPassphrase.Text;
      pgp.RecipientKeyCount := 1;
      pgp.RecipientKeyKeyring[0] := txtKeyringDir.Text;
      pgp.RecipientKeyUserId[0] := cboRecipientKeys.Text;

      pgp.SignAndEncrypt();
    end
    else if (rbDecryptAndVerify.Checked) then
    begin
      if (Length(cboPrivateKeys.Text) <= 0) then
      begin
        ShowMessage('Please select a private key.');
        Exit;
      end;
      if (Length(cboSignerKeys.Text) <= 0) then
      begin
        ShowMessage('Please select a signer key.');
        Exit;
      end;
      pgp.KeyCount := 1;
      pgp.KeyKeyring[0] := txtKeyringDir.Text;
      pgp.KeyUserId[0] := cboPrivateKeys.Text;
      pgp.KeyPassphrase[0] := txtPassphrase.Text;
      pgp.SignerKeyCount := 1;
      pgp.SignerKeyKeyring[0] := txtKeyringDir.Text;
      pgp.SignerKeyUserId[0] := cboPrivateKeys.Text;

      pgp.DecryptAndVerifySignature();
    end;

    lblStatus.Caption := 'Status: Operation Complete!';
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormOpenPGP.btnDecryptAndVerifyClick(Sender: TObject);
begin
  if (Length(cboSignerKeys.Text) <= 0) then
  begin
    ShowMessage('Please select a signer key.');
    Exit;
  end;
  if (Length(cboPrivateKeys.Text) <= 0) then
  begin
    ShowMessage('Please select a private key.');
    Exit;
  end;
  pgp.InputMessage := txtEncryptedString.Text;

  pgp.KeyCount := 1;
  pgp.KeyKeyring[0] := txtKeyringDir.Text;
  pgp.KeyUserId[0] := cboPrivateKeys.Text;
  pgp.KeyPassphrase[0] := txtPassphrase.Text;
  pgp.SignerKeyCount := 1;
  pgp.SignerKeyKeyring[0] := txtKeyringDir.Text;
  pgp.SignerKeyUserId[0] := cboPrivateKeys.Text;

  pgp.DecryptAndVerifySignature();

  txtDecryptedString.Text := pgp.OutputMessage;
end;

procedure TFormOpenPGP.btnDecryptClick(Sender: TObject);
begin
  if (Length(cboPrivateKeys.Text) <= 0) then
  begin
    ShowMessage('Please select a private key.');
    Exit;
  end;
  pgp.InputMessage := txtEncryptedString.Text;

  pgp.KeyCount := 1;
  pgp.KeyKeyring[0] := txtKeyringDir.Text;
  pgp.KeyUserId[0] := cboPrivateKeys.Text;
  pgp.KeyPassphrase[0] := txtPassphrase.Text;

  pgp.Decrypt();

  txtDecryptedString.Text := pgp.OutputMessage;
end;

procedure TFormOpenPGP.btnEncryptClick(Sender: TObject);
begin
  if (Length(cboRecipientKeys.Text) <= 0) then
  begin
    ShowMessage('Please select a recipient key.');
    Exit;
  end;
  pgp.InputMessage := txtDecryptedString.Text;

  pgp.RecipientKeyCount := 1;
  pgp.RecipientKeyKeyring[0] := txtKeyringDir.Text;
  pgp.RecipientKeyUserId[0] := cboRecipientKeys.Text;
  pgp.ASCIIArmor := true;

  pgp.Encrypt();

  txtEncryptedString.Text := pgp.OutputMessage;
end;


procedure TFormOpenPGP.btnSignAndEncryptClick(Sender: TObject);
begin
  if (Length(cboRecipientKeys.Text) <= 0) then
  begin
    ShowMessage('Please select a recipient key.');
    Exit;
  end;
  pgp.InputMessage := txtDecryptedString.Text;

  pgp.RecipientKeyCount := 1;
  pgp.RecipientKeyKeyring[0] := txtKeyringDir.Text;
  pgp.RecipientKeyUserId[0] := cboRecipientKeys.Text;
  pgp.KeyCount := 1;
  pgp.KeyKeyring[0] := txtKeyringDir.Text;
  pgp.KeyUserId[0] := cboPrivateKeys.Text;
  pgp.KeyPassphrase[0] := txtPassphrase.Text;
  pgp.ASCIIArmor := true;

  pgp.SignAndEncrypt();

  txtEncryptedString.Text := pgp.OutputMessage;
end;

procedure TFormOpenPGP.btnSignClick(Sender: TObject);
begin
  if (Length(cboPrivateKeys.Text) <= 0) then
  begin
    ShowMessage('Please select a private key.');
    Exit;
  end;
  pgp.InputMessage := txtDecryptedString.Text;

  pgp.KeyCount := 1;
  pgp.KeyKeyring[0] := txtKeyringDir.Text;
  pgp.KeyUserId[0] := cboPrivateKeys.Text;
  pgp.KeyPassphrase[0] := txtPassphrase.Text;
  pgp.ASCIIArmor := true;

  pgp.Sign();

  txtEncryptedString.Text := pgp.OutputMessage;
end;

procedure TFormOpenPGP.btnVerifyClick(Sender: TObject);
begin
  if (Length(cboSignerKeys.Text) <= 0) then
  begin
    ShowMessage('Please select a signer key.');
    Exit;
  end;
  pgp.InputMessage := txtEncryptedString.Text;

  pgp.SignerKeyCount := 1;
  pgp.SignerKeyKeyring[0] := txtKeyringDir.Text;
  pgp.SignerKeyUserId[0] := cboPrivateKeys.Text;

  pgp.VerifySignature();

  txtDecryptedString.Text := pgp.OutputMessage;
end;

procedure TFormOpenPGP.keymgrKeyList(Sender: TObject; const UserId, KeyId,
  Fingerprint: string; HasSecretKey: Boolean; const PublicKeyAlgorithm: string;
  PublicKeyLength: Integer);
begin
 if (HasSecretKey) then
  cboPrivateKeys.Items.Add(UserId);

  cboRecipientKeys.Items.Add(UserId);
  cboSignerKeys.Items.Add(UserId);
end;

procedure TFormOpenPGP.listKeys;
begin
  cboPrivateKeys.Clear();
  cboRecipientKeys.Clear();
  cboSignerKeys.Clear();
  cboPrivateKeys.Text := '';
  cboRecipientKeys.Text := '';
  cboSignerKeys.Text := '';
  keymgr.LoadKeyring(txtKeyringDir.Text);
  keymgr.ListKeys();

  if (cboPrivateKeys.Items.Count > 0) then
  begin
  	cboPrivateKeys.ItemIndex := 0;
  end;

  if (cboRecipientKeys.Items.Count > 0) then
  begin
  	cboRecipientKeys.ItemIndex := 0;
  	cboSignerKeys.ItemIndex := 0;
  end;

end;



procedure TFormOpenPGP.pgpProgress(Sender: TObject; BytesProcessed: Int64;
  PercentProcessed, Operation: Integer; var IsEOF: Boolean);
begin
Progressbar1.Position :=  PercentProcessed;
end;

end.

