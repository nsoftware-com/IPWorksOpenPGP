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
unit pimapf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ipgMIME, StdCtrls, ExtCtrls, ComCtrls, ipgcore, ipgtypes,
  ipgpimap;

type
  GetMode = ( HEADERS_, TEXT_ );
  TFormPimap = class(TForm)
    ListViewMessages: TListView;
    ListBoxMessage: TListBox;
    TreeViewMailboxes: TTreeView;
    ButtonLogin: TButton;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    btnPGPSetup: TButton;
    PIMAP1: TipgPIMAP;
    btnDecrypt: TButton;
    btnVerify: TButton;
    btnDecryptAndVerify: TButton;
    procedure ButtonLoginClick(Sender: TObject);
    procedure TreeViewMailboxesClick(Sender: TObject);
    procedure ListViewMessagesClick(Sender: TObject);
    procedure btnPGPSetupClick(Sender: TObject);
    procedure PIMAP1MailboxList(Sender: TObject; const Mailbox, Separator,
      Flags: string);
    procedure PIMAP1MessageInfo(Sender: TObject; const MessageId, Subject,
      MessageDate, From, Flags: string; Size: Int64);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure btnDecryptAndVerifyClick(Sender: TObject);

  private
    { Private declarations }
    head_: TTreeNode;
    sep: String;
    state: GetMode;
  public
    { Public declarations }
    constructor Create( Owner: TComponent );  override;
    function treesearch( Node: TTreeNode; text: String): TTreeNode;
    procedure buildtree( Node: TTreeNode; lstr: String; rstr: String);
    function mailboxname( Node: TTreeNode ): String;
  end;

var
  FormPimap: TFormPimap;

implementation

uses loginf, pgpsetupf;

{$R *.DFM}

constructor TFormPimap.Create( Owner: TComponent );
begin
   inherited Create(Owner);
   head_ := TreeViewMailboxes.Items.Add( nil, 'Folders');
end;


procedure TFormPimap.ButtonLoginClick(Sender: TObject);
begin
// Login / Logout
   Screen.Cursor := crHourGlass;
   if ButtonLogin.Caption = '&Login' then
   begin
      if FormLogin.ShowModal() = mrOk then
      begin
         PIMAP1.MailServer := FormLogin.EditIMAPServer.Text;
         PIMAP1.User := FormLogin.EditUser.Text;
         PIMAP1.Password := FormLogin.EditPassword.Text;
         try
            PIMAP1.Connect;
            PIMAP1.Mailbox := '*';
            PIMAP1.ListMailboxes;
         except on E: EipgPIMAP do
            ShowMessage(E.Message);
         end;
         ButtonLogin.Caption := '&Logout';
      end;
   end
   else
   begin
      try
         PIMAP1.disconnect();
      except on E: EipgPIMAP do
         ShowMessage(E.Message);
      end;
      TreeViewMailboxes.Items.Clear();
      ListViewMessages.Items.Clear();
      ListBoxMessage.Items.Clear();
      ButtonLogin.Caption := '&Login';
      head_ := TreeViewMailboxes.Items.Add( nil, 'Folders' );
   end;
   Screen.Cursor := crDefault;
end;


function TFormPimap.treesearch( Node: TTreeNode; text: String): TTreeNode;
var
   key: TTreeNode;
   i: integer;
begin
   key := nil;
   if Node.Text = text then
      Result := Node
   else if Node.Count = 0 then
      Result := nil
   else
   begin
      for i := 0 to Node.Count - 1 do
         if key = nil then
            key := treesearch(Node.Item[i], text);
      Result := key;
   end;
end;


procedure TFormPimap.btnDecryptAndVerifyClick(Sender: TObject);
begin
   try
      if(FormPGPSetup.cboPrivateKeys.Text = '') then begin
        ShowMessage('Please select a private key');
        exit;
      end;

      if(FormPGPSetup.cboSignerKeys.Text = '') then begin
        ShowMessage ('Please select a signer key');
        exit;
      end;

      begin
         PIMAP1.KeyCount := 1;
         PIMAP1.KeyKeyring[0] := FormPGPSetup.txtKeyringDir.Text;
         PIMAP1.KeyUserId[0] := FormPGPSetup.cboPrivateKeys.Text;
         PIMAP1.KeyPassphrase[0] := FormPGPSetup.txtPassphrase.Text;

         PIMAP1.SignerKeyCount := 1;
         PIMAP1.SignerKeyKeyring[0] := FormPGPSetup.txtKeyringDir.Text;
         PIMAP1.SignerKeyUserId[0] := FormPGPSetup.cboSignerKeys.Text;

         PIMAP1.DecryptAndVerifySignature();
         ListBoxMessage.Items.Text := PIMAP1.MessageText;

      end;
   except on E: EipgPIMAP do
      ShowMessage(E.Message);
   end;
end;

procedure TFormPimap.btnDecryptClick(Sender: TObject);
begin
   try
      if(FormPGPSetup.cboPrivateKeys.Text = '') then begin
        ShowMessage('Please select a private key');
        exit;
      end;

      begin
         PIMAP1.KeyCount := 1;
         PIMAP1.KeyKeyring[0] := FormPGPSetup.txtKeyringDir.Text;
         PIMAP1.KeyUserId[0] := FormPGPSetup.cboPrivateKeys.Text;
         PIMAP1.KeyPassphrase[0] := FormPGPSetup.txtPassphrase.Text;

         PIMAP1.Decrypt();
         ListBoxMessage.Items.Text := PIMAP1.MessageText;
      end;
   except on E: EipgPIMAP do
      ShowMessage(E.Message);
   end;
end;

procedure TFormPimap.btnVerifyClick(Sender: TObject);
begin
   try
      if(FormPGPSetup.cboSignerKeys.Text = '') then begin
        ShowMessage ('Please select a signer key');
        exit;
      end;

      begin
         PIMAP1.KeyCount := 1;

         PIMAP1.SignerKeyCount := 1;
         PIMAP1.SignerKeyKeyring[0] := FormPGPSetup.txtKeyringDir.Text;
         PIMAP1.SignerKeyUserId[0] := FormPGPSetup.cboSignerKeys.Text;

         PIMAP1.VerifySignature();
         ListBoxMessage.Items.Text := PIMAP1.MessageText;

      end;
   except on E: EipgPIMAP do
      ShowMessage(E.Message);
   end;
end;

procedure TFormPimap.btnPGPSetupClick(Sender: TObject);
begin
      FormPGPSetup.ShowModal()
end;





procedure TFormPimap.buildtree( Node: TTreeNode; lstr: String; rstr: String);
var
   key: TTreeNode;
begin
   key := treesearch(node, lstr);
   if key = nil then
      key := TreeViewMailboxes.Items.AddChild(node, lstr);

   lstr := rstr;
   if Pos(sep, lstr) = 0 then
      rstr := ''
   else
   begin
      rstr := lstr;
      Delete( lstr, Pos(sep, lstr), Length(lstr) );
      Delete( rstr, 1, Length(lstr) + Length(sep) );
   end;

   if Length(lstr) > 0 then
      buildtree(key, lstr, rstr);

end;


function TFormPimap.mailboxname( Node: TTreeNode ): String;
begin
  if node.Parent.Level <> 0 then
     Result := mailboxname(node.Parent) + sep + node.Text
  else
     Result := node.Text;
end;


procedure TFormPimap.PIMAP1MailboxList(Sender: TObject; const Mailbox,
  Separator, Flags: String);
var
   ltmp: String;
   rtmp: String;

begin

   sep := Separator;
   ltmp := Mailbox;

   if Pos(sep, ltmp) = 0 then
      rtmp := ''
   else
   begin
      rtmp := ltmp;
      Delete( ltmp, Pos(sep, ltmp), Length(ltmp) );
      Delete( rtmp, 1, Length(ltmp) + Length(sep) );
   end;

   buildtree(head_, ltmp, rtmp);

end;



procedure TFormPimap.PIMAP1MessageInfo(Sender: TObject; const MessageId,
  Subject, MessageDate, From, Flags: string; Size: Int64);
begin
   if state = HEADERS_ then
   begin
      ListViewMessages.Items.Add();
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].Caption := MessageId;
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(From);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(Subject);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(MessageDate);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(IntToStr(Size));
   end;
end;

procedure TFormPimap.TreeViewMailboxesClick(Sender: TObject);
begin
// Click the tree
   Screen.Cursor := crHourGlass;
   try
      if TreeViewMailboxes.Selected.Level > 0 then
      begin
         PIMAP1.Mailbox := '"' + mailboxname(TreeViewMailboxes.Selected) + '"';
         PIMAP1.SelectMailbox();
         ListViewMessages.Items.Clear();
         if PIMAP1.MessageCount > 0 then
         begin
            PIMAP1.MessageSet := '1:' + IntToStr(PIMAP1.MessageCount);
            state := HEADERS_;
            PIMAP1.FetchMessageInfo();
         end;
      end;
   except on E: EipgPIMAP do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;



procedure TFormPimap.ListViewMessagesClick(Sender: TObject);
begin
// Select a message
   Screen.Cursor := crHourGlass;
   try
      PIMAP1.MessageSet := ListViewMessages.Selected.Caption;
      ListBoxMessage.Items.Clear();
      state := TEXT_;
      PIMAP1.FetchMessageInfo();
      PIMAP1.FetchMessageHeaders();
      PIMAP1.FetchMessageText();
      ListBoxMessage.Items.Text := PIMAP1.MessageText;

      Label3.Caption := ListViewMessages.Selected.SubItems.Strings[0];
      Label4.Caption := ListViewMessages.Selected.SubItems.Strings[1];
   except on E: EipgPIMAP do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;

end.



//---------------------------------------------------------------------------




