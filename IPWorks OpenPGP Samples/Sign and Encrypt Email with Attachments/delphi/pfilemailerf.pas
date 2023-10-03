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
unit pfilemailerf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Math,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, FileCtrl,
  StrUtils, ipgcore, ipgtypes, ipgpfilemailer, ipgkeymgr;

type
  TFormPfilemailer = class(TForm)
    Label1: TLabel;
    txtMailServer: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    txtSendTo: TEdit;
    Label4: TLabel;
    txtFrom: TEdit;
    txtSubject: TEdit;
    Label5: TLabel;
    lbAttachments: TListBox;
    Attachments: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    btnSend: TButton;
    txtMessage: TMemo;
    txtKeyring: TEdit;
    Label6: TLabel;
    cboPrivateKeys: TComboBox;
    Label7: TLabel;
    cboRecipientKeys: TComboBox;
    Label8: TLabel;
    chkEncrypt: TCheckBox;
    chkSign: TCheckBox;
    btnLoadKeyring: TButton;
    odFileBrowse: TOpenDialog;
    ipgPFileMailer1: TipgPFileMailer;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ipgKeyMgr1: TipgKeyMgr;
    Label9: TLabel;
    txtPassphrase: TEdit;
    dlgKeyring: TFileOpenDialog;
    FileOpenDialog2: TFileOpenDialog;
    procedure btnLoadKeyringClick(Sender: TObject);
    procedure ipgKeyMgr1KeyList(Sender: TObject; const UserId, KeyId,
      Fingerprint: string; HasSecretKey: Boolean;
      const PublicKeyAlgorithm: string; PublicKeyLength: Integer);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    { Private declarations }
    procedure SignMsg;
    procedure EncryptMsg;
    procedure SignAndEncryptMsg;
  public
    { Public declarations }
    procedure ListKeys;
  end;

var
  FormPfilemailer: TFormPfilemailer;

implementation

{$R *.dfm}

procedure TFormPfilemailer.btnAddClick(Sender: TObject);
begin
  if(odFileBrowse.Execute()) then
    lbAttachments.AddItem(odFileBrowse.FileName, nil);
end;

procedure TFormPfilemailer.btnLoadKeyringClick(Sender: TObject);
begin
  if(txtKeyring.Text <> '') then
    dlgKeyring.DefaultFolder := txtKeyring.Text;
  if(dlgKeyring.Execute()) then
  begin
    txtKeyring.Text := dlgKeyring.FileName;
    ListKeys();
  end;
end;

procedure TFormPfilemailer.btnRemoveClick(Sender: TObject);
begin
  if(lbAttachments.ItemIndex > -1) then
    lbAttachments.Items.Delete(lbAttachments.ItemIndex)
  else
    ShowMessage('You must select an attachment to remove.');
end;

procedure TFormPfilemailer.btnSendClick(Sender: TObject);
var
I: Integer;
begin
  btnSend.Enabled := false;

  ipgPFileMailer1.RecipientKeyCount := -1;
  ipgPFileMailer1.KeyCount := -1;
  ipgPFileMailer1.ResetHeaders();
  ipgPFileMailer1.AttachmentCount := -1;
  ipgPFileMailer1.Config('ProcessAttachments=true'); //Encrypt/Sign Attachments.

  if(Length(txtSendTo.Text) > 0) then
  begin
    ipgPFileMailer1.SendTo := txtSendTo.Text;
    if(Length(txtFrom.Text) > 0) then
    begin
      ipgPFileMailer1.From := txtFrom.Text;

      if(Length(txtMailServer.Text) > 0) then
      begin
        if(AnsiContainsText(txtMailServer.Text, ':')) then
        begin
          ipgPfileMailer1.MailServer := AnsiMidStr(txtMailServer.Text, 0, Pos(':', txtMailServer.Text)-1);
          ipgPFileMailer1.MailPort := StrToInt(AnsiMidStr(txtMailServer.Text, Pos(':', txtMailServer.Text) + 1,
                                      (Length(txtMailServer.Text)-Pos(':', txtMailServer.Text))));
        end
        else
          ipgPFileMailer1.MailServer := txtMailServer.Text;

        for I := 0 to lbAttachments.Items.Count-1 do
          ipgPFileMailer1.AddAttachment(lbAttachments.Items[I]);

        ipgPFileMailer1.MessageText := txtMessage.Text;
        ipgPFileMailer1.Subject := txtSubject.Text;

        try
          if(chkEncrypt.Checked and chkSign.Checked) then
            SignAndEncryptMsg()
          else if(chkEncrypt.Checked) then
            EncryptMsg()
          else if(chkSign.Checked) then
            SignMsg();
          ShowMessage('Message Sent');
        Except on
          E: Exception do
          begin
            ShowMessage('Error: ' + E.Message);
            btnSend.Enabled := true;
          end;
        end;
      end
      else
        ShowMessage('You must specify a "Mail Server".');
    end
    else
      ShowMessage('You must specify a "From" address.');
  end
  else
    ShowMessage('You must specify a "Send To" address.');
  btnSend.Enabled := true;
end;

procedure TFormPfilemailer.ipgKeyMgr1KeyList(Sender: TObject; const UserId, KeyId,
  Fingerprint: string; HasSecretKey: Boolean; const PublicKeyAlgorithm: string;
  PublicKeyLength: Integer);
begin
  if(HasSecretKey) then
    cboPrivateKeys.Items.Add(UserId);

  cboRecipientKeys.Items.Add(UserId);
end;

procedure TFormPfilemailer.ListKeys;
begin
  cboPrivateKeys.Clear();
  cboRecipientKeys.Clear();
  cboPrivateKeys.Text := '';
  cboRecipientKeys.Text := '';

  try
    ipgKeymgr1.LoadKeyring(txtKeyring.Text);
    ipgKeyMgr1.ListKeys();

    if(cboPrivateKeys.Items.Count > 0) then
    begin
      cboPrivateKeys.ItemIndex := 0;
      cboPrivateKeys.Text := cboPrivateKeys.Items[0];
    end;
    if(cboRecipientKeys.Items.Count > 0) then
    begin
      cboRecipientKeys.ItemIndex := 0;
      cboRecipientKeys.Text := cboRecipientKeys.Items[0];
    end;
  Except
    on E: Exception do
    ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TFormPfilemailer.SignAndEncryptMsg;
begin
  if(Length(cboPrivateKeys.Text) > 0) then
  begin
    if(Length(cboRecipientKeys.Text) > 0) then
    begin
      ipgPFileMailer1.KeyCount := 1;
      ipgPFileMailer1.KeyKeyring[0] := txtKeyring.Text;
      ipgPFileMailer1.KeyUserId[0] := cboPrivateKeys.Text;
      ipgPFileMailer1.KeyPassphrase[0] := txtPassphrase.Text;

      ipgPFileMailer1.RecipientKeyCount := 1;
      ipgPFileMailer1.RecipientKeyKeyring[0] := txtKeyring.Text;
      ipgPFileMailer1.RecipientKeyUserId[0] := cboRecipientKeys.Text;
      ipgPFileMailer1.SignAndEncrypt();
      ipgPFileMailer1.Send();
    end
    else
      ShowMessage('You must select a Recipient Key.');
  end
  else
    ShowMessage('You must select a Private Key.');
end;

procedure TFormPfilemailer.SignMsg;
begin
  if(Length(cboPrivateKeys.Text) > 0) then
  begin
    ipgPFileMailer1.KeyCount := 1;
    ipgPFileMailer1.KeyKeyring[0] := txtKeyring.Text;
    ipgPFileMailer1.KeyUserId[0] := cboPrivateKeys.Text;
    ipgPFileMailer1.KeyPassphrase[0] := txtPassphrase.Text;

    ipgPFileMailer1.Sign();

    ipgPFileMailer1.Send();
  end
  else
    ShowMessage('You must select a Private Key.');
end;

procedure TFormPfilemailer.EncryptMsg;
begin
  if(Length(cboRecipientKeys.Text) > 0) then
  begin
    ipgPFileMailer1.RecipientKeyCount := 1;
    ipgPFileMailer1.RecipientKeyKeyring[0] := txtKeyring.Text;
    ipgPFileMailer1.RecipientKeyUserId[0] := cboRecipientKeys.Text;

    ipgPFileMailer1.Encrypt();

    ipgPFileMailer1.Send();
  end;
end;
end.

