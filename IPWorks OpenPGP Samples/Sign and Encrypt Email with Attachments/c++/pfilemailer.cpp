/*
 * IPWorks OpenPGP 2024 C++ Edition - Sample Project
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
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworksopenpgp.h"

#define LINE_LEN 80
#define MESSAGE_LEN 1024

class MyKeyMgr: public KeyMgr
{
  virtual int FireKeyList(KeyMgrKeyListEventParams *e)
  {
    printf("%-58s %-8s %-12s\n", e->UserId, e->KeyId, e->HasSecretKey ? "private" : ""); 
    return 0;
  }
};

int main(int argc, char **argv)
{
	PFileMailer pfilemailer;
  MyKeyMgr keymgr;

	char buffer[LINE_LEN];     // input text buffer
  char keyringDir[LINE_LEN];
  int ret_code;

  printf("***************************************************************\n");
  printf("* This demo shows how to use the PFileMailer and KeyMgr       *\n");
  printf("* components to sign and encrypt email messages.              *\n");
  printf("***************************************************************\n\n");

	printf("Mail Server: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer)-1] = '\0';
	pfilemailer.SetMailServer(buffer);

  printf("Mail Port: ");
  fgets(buffer, LINE_LEN, stdin);
  buffer[strlen(buffer) - 1] = '\0';
  pfilemailer.SetMailPort(atoi(buffer));

  pfilemailer.SetSSLStartMode(SSL_AUTOMATIC);

	printf("From: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer)-1] = '\0';
	pfilemailer.SetFrom(buffer);

	printf("To: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer)-1] = '\0';
	pfilemailer.SetSendTo(buffer);

  printf("User: ");
  fgets(buffer, LINE_LEN, stdin);
  buffer[strlen(buffer) - 1] = '\0';
  pfilemailer.SetUser(buffer);

  printf("Password: ");
  fgets(buffer, LINE_LEN, stdin);
  buffer[strlen(buffer) - 1] = '\0';
  pfilemailer.SetPassword(buffer);

	printf("Subject: ");
	fgets(buffer, LINE_LEN, stdin);
	buffer[strlen(buffer)-1] = '\0';
	pfilemailer.SetSubject(buffer);

  printf("Enter the message. To end the message, enter \".\" on a single line by itself.\n");
	printf("Message:\n");

	char message[MESSAGE_LEN];
	message[0] = '\0';
	while(fgets(buffer, LINE_LEN, stdin))
	{
		buffer[strlen(buffer)-1] = '\0';
		strcat(message, buffer);
		if (strcmp(buffer, ".") == 0)
			break;
	}

	pfilemailer.SetMessageText(message);

	// if you want to add attachments:
	printf("Enter file names to attach. (press enter to stop adding attachments) ...\n");
	printf("File Name: ");
	while (fgets(buffer, LINE_LEN, stdin))
	{
		buffer[strlen(buffer)-1] = '\0';
		if (strlen(buffer) > 0)
		{
			pfilemailer.AddAttachment(buffer);
			printf("File Name: ");
		}
		else
			break;
	}

  printf("Please select an option: (0: Cancel, 1: Sign, 2: Encrypt, 3: Sign and Encrypt)");
  fgets(buffer, LINE_LEN, stdin);
  buffer[strlen(buffer)-1] = '\0';
  int cmd = atoi(buffer);

  if (cmd < 1 || cmd > 3) goto done;
  
  printf("Keyring Dir: ");
  fgets(keyringDir, LINE_LEN, stdin);
  keyringDir[strlen(keyringDir)-1] = '\0';
  ret_code = keymgr.LoadKeyring(keyringDir);
  if (ret_code) goto done;

  printf("\nListing Keys...\n");
  printf("%-58s %-8s %-12s\n", "User ID", "Key ID", "Private Key"); 
  keymgr.ListKeys();

  if (cmd == 0)
  {
    goto done;
  }
  else if (cmd  == 1) // sign
  { 
    pfilemailer.SetKeyCount(1);
    pfilemailer.SetKeyKeyring(0, keyringDir);

    printf("\nPrivate Key User ID (Used to sign outgoing messages): ");
	  fgets(buffer, LINE_LEN, stdin);
	  buffer[strlen(buffer)-1] = '\0';
    pfilemailer.SetKeyUserId(0, buffer);

    printf("Passphrase: ");
	  fgets(buffer, LINE_LEN, stdin);
	  buffer[strlen(buffer)-1] = '\0';
    pfilemailer.SetKeyPassphrase(0, buffer);

    ret_code = pfilemailer.Sign();
    if (ret_code) goto done;
  }
  else if (cmd == 2) // encrypt
  {    
    pfilemailer.SetRecipientKeyCount(1);
    pfilemailer.SetRecipientKeyKeyring(0, keyringDir);

    printf("Recipient Key User ID (Used to encrypt outgoing messages): ");
	  fgets(buffer, LINE_LEN, stdin);
	  buffer[strlen(buffer)-1] = '\0';
    pfilemailer.SetRecipientKeyUserId(0, buffer);

    ret_code = pfilemailer.Encrypt();
    if (ret_code) goto done;
  }
  else if (cmd == 3) // sign and encrypt
  {
    pfilemailer.SetKeyCount(1);
    pfilemailer.SetKeyKeyring(0, keyringDir);

    printf("\nPrivate Key User ID (Used to sign outgoing messages): ");
	  fgets(buffer, LINE_LEN, stdin);
	  buffer[strlen(buffer)-1] = '\0';
    pfilemailer.SetKeyUserId(0, buffer);

    printf("Passphrase: ");
	  fgets(buffer, LINE_LEN, stdin);
	  buffer[strlen(buffer)-1] = '\0';
    pfilemailer.SetKeyPassphrase(0, buffer);

    pfilemailer.SetRecipientKeyCount(1);
    pfilemailer.SetRecipientKeyKeyring(0, keyringDir);

    printf("Recipient Key User ID (Used to encrypt outgoing messages): ");
	  fgets(buffer, LINE_LEN, stdin);
	  buffer[strlen(buffer)-1] = '\0';
    pfilemailer.SetRecipientKeyUserId(0, buffer);

    ret_code = pfilemailer.SignAndEncrypt();
    if (ret_code) goto done;
  }
  else
  {
    printf("Invalid Command.\n");
    goto done;
  }

	printf("Sending message ...\n");
	ret_code = pfilemailer.Send();

	if (ret_code)     // Got an error.  The user is done.
	{
		goto done;
	}
	else
	{
		printf("Message sent successfully\n");
	}

done:
  if (ret_code)    // Got an error.  The user is done.
  {
    printf("\nError: %d", ret_code);
    if (keymgr.GetLastError())
    {
      printf(" \"%s\"\n", keymgr.GetLastError());
    }
    if (pfilemailer.GetLastError())
    {
      printf(" \"%s\"\n", pfilemailer.GetLastError());
    }
  }
	fprintf(stderr, "Exiting... (press enter)\n");
	getchar();
	exit(ret_code);
	return 0;
}




