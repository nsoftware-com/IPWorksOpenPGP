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
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/ipworksopenpgp.h"

#define LINE_LEN 80

class MyKeyMgr: public KeyMgr
{
  virtual int FireKeyList(KeyMgrKeyListEventParams *e)
  {
    printf("User Id: %s\t\tKey Id: %s\n",e->UserId,e->KeyId);
    return 0;
  }
};

class MyPIMAP: public PIMAP
{

public:

  MyPIMAP()
  {
    lines = 0;
  }

  virtual int FireMailboxList(PIMAPMailboxListEventParams *e)
  {
    printf( "%s\n", e->Mailbox );
    lines++;
    if ( lines == 22 )
    {
      printf("Press enter to continue...");
      getchar();
      lines = 0;
    }
    return 0;
  }

  virtual int FireMessageInfo(PIMAPMessageInfoEventParams *e)
  {

    // Truncate fields
    printf( "%4s  ", e->MessageId );
    printf( "%-25.23s", e->Subject );
    printf( "%-18.16s", e->MessageDate );
    printf( "%-30.30s\n", e->From );

    lines++;
    if ( lines == 22 )
    {
      printf("Press enter to continue...");
      getchar();
      lines = 0;
    }
    return 0;

  }
  virtual int FireTransfer(PIMAPTransferEventParams *e)
  {
    printf( "%s\n", e->Text );
    lines++;
    if ( lines == 22 )
    {
      printf("Press enter to continue...");
      getchar();
      lines = 0;
    }
    return 0;
  }

  int lines;

};


int main(int argc, char * argv[])
{

  MyPIMAP imap;               
  MyKeyMgr keymgr;

  char command[LINE_LEN];     // user's command
  char buffer[LINE_LEN];      // text buffer
  char *argument;             // arguments following command

  int msgnum = 0;             // current message number for next command
  char msg_range[LINE_LEN];
  char msg_limit[10];
  int ret_code;

  printf("***************************************************************\n");
  printf("* This demo shows how to use the PIMAP and KeyMgr components  *\n");
  printf("* to decrypt and verify OpenPGP email messages.               *\n");
  printf("***************************************************************\n\n");

  printf( "IMAP Server  : " );
  fgets(buffer,LINE_LEN,stdin);
  buffer[strlen(buffer)-1] = '\0';
  imap.SetMailServer( buffer );

  printf("IMAP Port    : ");
  fgets(buffer, LINE_LEN, stdin);
  buffer[strlen(buffer) - 1] = '\0';
  imap.SetMailPort(atoi(buffer));

  imap.SetSSLStartMode(SSL_AUTOMATIC);

  printf( "Login        : " );
  fgets(buffer,LINE_LEN,stdin);
  buffer[strlen(buffer)-1] = '\0';
  imap.SetUser( buffer );

  printf( "Password     : " );
  fgets(buffer,LINE_LEN,stdin);
  buffer[strlen(buffer)-1] = '\0';
  imap.SetPassword( buffer );

  printf( "Keyring Dir  : " );
  fgets(buffer,LINE_LEN,stdin);
  buffer[strlen(buffer)-1] = '\0';

  ret_code = keymgr.LoadKeyring(buffer);
  if (ret_code) goto done;

  ret_code = imap.Connect();
  if (ret_code) goto done;

  printf("Type \"?\" for a list of commands.\n\n");
  while (1)
  {

    imap.lines = 0;
    printf( "> " );
    fgets(command,LINE_LEN,stdin);
    buffer[strlen(command)-1] = '\0';
    argument = strtok( command, " \n\t" );

    if ( ! strcmp(command, "?") )
    {

      printf( "IMAP Commands\n"
        "l                               list mailboxes\n"
        "s <mailbox>                     select mailbox\n"
        "t <message number>              type messages\n"
        "n                               goto and type next message\n"
        "f                               give head lines of messages\n"
        "q                               quit, saving unresolved messages in mbox\n"
        "h                               print out active message headers\n" 
        "lk                              list keys in the keyring\n"
        "d <message number>              decrypt the message\n"
        "v <message number>              verify the message\n"
        "dv <message number>             decrypt and verify the message\n");

    }

    else if ( ! strcmp(command, "s") )
    {

      argument = strtok( NULL, " \t\n" );
      imap.SetMailbox(argument);
      ret_code = imap.SelectMailbox();
    }

    else if ( ! strcmp(command, "f") )
    {

      if ( imap.GetMessageCount() )
      {
        strcpy(msg_range, "1:");
        sprintf(msg_limit,"%i",imap.GetMessageCount());
        strcat(msg_range, msg_limit);
        imap.SetMessageSet(msg_range);
        ret_code = imap.RetrieveMessageInfo();
      }
      else
      {
        printf( "No messages in this mailbox.\n" );
      }

    }

    else if ( ! strcmp(command, "h") )
    {

      if ( imap.GetMessageCount() )
      {
        strcpy(msg_range, "1:");
        sprintf(msg_limit,"%i", imap.GetMessageCount());
        strcat(msg_range, msg_limit);
        imap.SetMessageSet(msg_range);
        ret_code = imap.RetrieveMessageInfo();
      }
      else
      {
        printf( "No messages in this mailbox.\n" );
      }

    }

    else if ( ! strcmp(command, "l") )
    {

      if ( ! (argument = strtok( NULL, " \t\n" )) )
      {
        strcpy(buffer, "*");
        ret_code = imap.SetMailbox(buffer);
      }
      else
      {
        imap.SetMailbox(argument);
      }
      imap.ListMailboxes();

    }

    else if ( ! strcmp(command, "n") )
    {

      msgnum++;
      sprintf(buffer, "%i", msgnum);
      imap.SetMessageSet( buffer );
      ret_code = imap.RetrieveMessageText();

    }

    else if ( ! strcmp(command, "q") )
    {

      ret_code = imap.Disconnect();
      exit(0);

    }

    else if ( ! strcmp(command, "t") )
    {
      argument = strtok( NULL, " \t\n" );
      imap.SetMessageSet(argument);
      ret_code = imap.RetrieveMessageText();
    }

    else if ( ! strcmp(command, "lk") )
    {
      printf("\n\nAvailable Keys:\n");
      keymgr.ListKeys();
      printf("\n\n");
    }

    else if ( ! strcmp(command, "dv") )
    {
      argument = strtok( NULL, " \t\n" );
      imap.SetMessageSet(argument);
      ret_code = imap.RetrieveMessageHeaders();
      if(ret_code) goto done;
      ret_code = imap.RetrieveMessageText();
      if(ret_code) goto done;

      printf( "\n\nUserId (or email) of decryption key: " );
      fgets(buffer,LINE_LEN,stdin);
      buffer[strlen(buffer)-1] = '\0';

      imap.SetKeyCount(1);
      imap.SetKeyKeyring(0,keymgr.GetKeyring());
      imap.SetKeyUserId(0,buffer);

      printf( "Decryption key passphrase: " );
      fgets(buffer,LINE_LEN,stdin);
      buffer[strlen(buffer)-1] = '\0';

      imap.SetKeyPassphrase(0,buffer);

      printf( "UserId (or email) of signer key: " );
      fgets(buffer,LINE_LEN,stdin);
      buffer[strlen(buffer)-1] = '\0';

      imap.SetSignerKeyCount(1);
      imap.SetSignerKeyKeyring(0,keymgr.GetKeyring());
      imap.SetSignerKeyUserId(0,buffer);

      ret_code = imap.DecryptAndVerifySignature();

      printf("\n\nDecrypted and verified message:\n%s\n", imap.GetMessageText());
    }

    else if ( ! strcmp(command, "d") )
    {
      argument = strtok( NULL, " \t\n" );
      imap.SetMessageSet(argument);
      ret_code = imap.RetrieveMessageHeaders();
      if(ret_code) goto done;
      ret_code = imap.RetrieveMessageText();
      if(ret_code) goto done;

      printf( "\n\nUserId (or email) of decryption key: " );
      fgets(buffer,LINE_LEN,stdin);
      buffer[strlen(buffer)-1] = '\0';

      imap.SetKeyCount(1);
      imap.SetKeyKeyring(0,keymgr.GetKeyring());
      imap.SetKeyUserId(0,buffer);

      printf( "Decryption key passphrase: " );
      fgets(buffer,LINE_LEN,stdin);
      buffer[strlen(buffer)-1] = '\0';

      imap.SetKeyPassphrase(0,buffer);

      ret_code = imap.Decrypt();

      printf("\n\nDecrypted message:\n%s\n", imap.GetMessageText());
    }

    else if ( ! strcmp(command, "v") )
    {
      argument = strtok( NULL, " \t\n" );
      imap.SetMessageSet(argument);
      ret_code = imap.RetrieveMessageHeaders();
      if(ret_code) goto done;
      ret_code = imap.RetrieveMessageText();
      if(ret_code) goto done;

      printf( "\n\nUserId (or email) of signer key: " );
      fgets(buffer,LINE_LEN,stdin);
      buffer[strlen(buffer)-1] = '\0';

      imap.SetSignerKeyCount(1);
      imap.SetSignerKeyKeyring(0,keymgr.GetKeyring());
      imap.SetSignerKeyUserId(0,buffer);

      ret_code = imap.VerifySignature();

      printf("\n\nVerified message:\n%s\n", imap.GetMessageText());
    }

    else if (! strcmp(command, "") )
    {
      // do nothing
    }
    else
    {

      if ( isdigit( (int) command[0] ) )
      {
        // allow user to enter only the number
        //  of the message they want to view
        imap.SetMessageSet(command);
        msgnum = atoi(command);
        ret_code = imap.RetrieveMessageText();
      }
      else
      {
        printf( "Bad command / Not implemented in demo.\n" );
      }

    } // end of command checking

    if (ret_code)     // Got an error.  The user is done.
    {
      printf("\nError: %d", ret_code);
      if (imap.GetLastError())
      {
        printf( " \"%s\"\n", imap.GetLastError() );
      }
    }
    ret_code = 0;   // flush out error
  }  // end of main while loop

done:
  if (ret_code)     // Got an error.  The user is done.
  {
    printf( "\nError: %d", ret_code );
    if (imap.GetLastError())
    {
      printf( " \"%s\"\n", imap.GetLastError() );
    }
    if (keymgr.GetLastError())
    {
      printf( " \"%s\"\n", keymgr.GetLastError() );
    }
  }
  printf( "Exiting... (press enter)\n" );
  getchar();
  exit(ret_code);
  return 0;
}





