unit pgpsetupf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ipgcore, ipgtypes, ipgkeymgr, StdCtrls;

type
  TFormPGPSetup = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    cboSignerKeys: TComboBox;
    Label5: TLabel;
    cboPrivateKeys: TComboBox;
    Label4: TLabel;
    txtPassphrase: TEdit;
    btnBrowse: TButton;
    txtKeyringDir: TEdit;
    Label2: TLabel;
    dlgKeyring: TFileOpenDialog;
    KeyMgr1: TipgKeyMgr;
    Button1: TButton;
    Button2: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure KeyMgr1KeyList(Sender: TObject; const UserId, KeyId,
      Fingerprint: string; HasSecretKey: Boolean; const PublicKeyAlgorithm: string;
      PublicKeyLength: Integer; const Curve: string);
  private
    procedure listKeys;
  public
    { Public declarations }
  end;

var
  FormPGPSetup: TFormPGPSetup;

implementation

{$R *.dfm}

procedure TFormPGPSetup.btnBrowseClick(Sender: TObject);
begin
  try
    if(txtKeyringDir.Text <> '') then
      dlgKeyring.DefaultFolder := txtKeyringDir.Text;
    if (dlgKeyring.Execute()) then
    begin
    	txtKeyringDir.Text := dlgKeyring.FileName;
	    listKeys();
    end;
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
end;

procedure TFormPGPSetup.KeyMgr1KeyList(Sender: TObject; const UserId, KeyId,
  Fingerprint: string; HasSecretKey: Boolean; const PublicKeyAlgorithm: string;
  PublicKeyLength: Integer; const Curve: string);
begin
 if (HasSecretKey) then
  cboPrivateKeys.Items.Add(UserId);

  cboSignerKeys.Items.Add(UserId);
end;

procedure TFormPGPSetup.listKeys;
begin
  cboPrivateKeys.Clear();
  cboSignerKeys.Clear();
  cboPrivateKeys.Text := '';
  cboSignerKeys.Text := '';
  keymgr1.LoadKeyring(txtKeyringDir.Text);
  keymgr1.ListKeys();

  if (cboPrivateKeys.Items.Count > 0) then
  begin
  	cboPrivateKeys.ItemIndex := 0;
  end;

  if (cboSignerKeys.Items.Count > 0) then
  begin
  	cboSignerKeys.ItemIndex := 0;
  end;

end;

end.
