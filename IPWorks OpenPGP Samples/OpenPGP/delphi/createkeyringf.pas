unit createkeyringf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ipgcore, ipgtypes, ipgkeymgr, ComCtrls, StdCtrls;

type
  TFormCreateKeyring = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    txtKeyringDir: TEdit;
    btnChoose: TButton;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    btnDelete: TButton;
    txtUserId: TEdit;
    txtPassphrase: TEdit;
    btnCreate: TButton;
    lvwKeys: TListView;
    btnSave: TButton;
    btnCancel: TButton;
    dlgKeyring: TFileOpenDialog;
    keymgr: TipgKeyMgr;
    procedure btnChooseClick(Sender: TObject);
    procedure keymgrKeyList(Sender: TObject; const UserId, KeyId,
      Fingerprint: string; HasSecretKey: Boolean;
      const PublicKeyAlgorithm: string; PublicKeyLength: Integer);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure listKeys;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCreateKeyring: TFormCreateKeyring;

implementation

{$R *.dfm}

procedure TFormCreateKeyring.btnChooseClick(Sender: TObject);
begin
  if(txtKeyringDir.Text <> '') then
    dlgKeyring.DefaultFolder := txtKeyringDir.Text;
  if (dlgKeyring.Execute) then
  begin
    txtKeyringDir.Text := dlgKeyring.FileName;
    keymgr.LoadKeyring(txtKeyringDir.Text);
    listKeys();
  end;
end;

procedure TFormCreateKeyring.btnCreateClick(Sender: TObject);
begin
keymgr.CreateKey(txtuserid.Text,txtPassphrase.Text);
listKeys();
end;

procedure TFormCreateKeyring.btnDeleteClick(Sender: TObject);
begin
keymgr.DeleteKey(lvwKeys.Selected.Caption);
listKeys();
end;

procedure TFormCreateKeyring.btnSaveClick(Sender: TObject);
begin
keymgr.SaveKeyring(txtKeyringDir.Text);
end;

procedure TFormCreateKeyring.FormActivate(Sender: TObject);
begin
  if(txtKeyringDir.Text <> '') then
  begin
    listKeys();
  end;
end;

procedure TFormCreateKeyring.keymgrKeyList(Sender: TObject; const UserId, KeyId,
  Fingerprint: string; HasSecretKey: Boolean; const PublicKeyAlgorithm: string;
  PublicKeyLength: Integer);
begin
lvwKeys.Items.Add;
lvwKeys.Items.Item[lvwKeys.Items.Count - 1].Caption := UserId;
lvwKeys.Items.Item[lvwKeys.Items.Count - 1].SubItems.Add(KeyId);
end;

procedure TFormCreateKeyring.listKeys;
begin
  lvwKeys.Items.Clear();
  keymgr.ListKeys;

end;

end.
