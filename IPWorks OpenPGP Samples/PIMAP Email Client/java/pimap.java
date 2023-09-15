/*
 * IPWorks OpenPGP 2022 Java Edition - Sample Project
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

import java.io.*;
import ipworksopenpgp.*;

public class pimap extends ConsoleDemo {
	private static int lines = 0;

	public enum MenuChoices
	{
		l,s,t,n,f,q,h,lk,d,v,dv;
	}
	
	public static void main(String[] args) {
		Pimap imap = new Pimap();
		Keymgr keymgr = new Keymgr();

		try {
			System.out.println("***************************************************************");
			System.out.println("* This demo shows how to use the PIMAP and KeyMgr components  *");
			System.out.println("* to decrypt and verify OpenPGP email messages.               *");
			System.out.println("***************************************************************\n");			
			
			imap.addPimapEventListener(new DefaultPimapEventListener() {
				public void mailboxList(PimapMailboxListEvent arg) {
					System.out.println(arg.mailbox);

					if (++lines == 22) {
						prompt("Press enter to continue", "...");
						lines = 0;
					}
				}

				public void messageInfo(PimapMessageInfoEvent arg) {
					System.out.print(arg.messageId + "  ");
					System.out.print(arg.subject + "  ");
					System.out.print(arg.messageDate + "  ");
					System.out.println(arg.from);

					if (++lines == 22) {
						prompt("Press enter to continue", "...");
						lines = 0;
					}
				}

				public void transfer(PimapTransferEvent arg) {
					System.out.print(arg.text);

					if (++lines == 22) {
						prompt("Press enter to continue", "...");
						lines = 0;
					}
				}
			});

			keymgr.addKeymgrEventListener(new DefaultKeymgrEventListener() {
				public void keyList(KeymgrKeyListEvent arg) {
					System.out.println("UserId: " + arg.userId + "\t\tKeyId: " + arg.keyId);
				}
			});

			imap.setMailServer(prompt("IMAP Server"));
			imap.setUser(prompt("User Name"));
			imap.setPassword(prompt("Password"));
			keymgr.loadKeyring(prompt("Keyring Dir"));

			imap.connect();

			int msgnum = 0; // current message number for next command

			System.out.println("Type \"?\" for a list of commands.\n");

			while (true) {
				lines = 0;

				String command = prompt("", ">");
				String[] argument = command.split("\\s");

				if (argument.length == 0 || argument[0].length() == 0) {
					continue;
				}

				if (command.equals("?")) {
					displayMenu();
					continue;
				}

				switch (MenuChoices.valueOf(argument[0])) {
				case s:
					imap.setMailbox(argument[1]);
					imap.selectMailbox();
					msgnum = 0;
					System.out.println("There are " + imap.getMessageCount() + " messages in this mailbox.");
					break;

				case f:
					if (imap.getMessageCount() > 0) {
						imap.setMessageSet("0:" + imap.getMessageCount());
						imap.fetchMessageInfo();
					} else {
						System.out.println("No messages in this mailbox.");
					}
					break;

				case h:
					if (imap.getMessageCount() > 0) {
						imap.setMessageSet("0:" + imap.getMessageCount());
						imap.fetchMessageInfo();
					} else {
						System.out.println("No messages in this mailbox.");
					}
					break;

				case l:
					if (argument.length < 2) {
						imap.setMailbox("*");
					} else {
						imap.setMailbox(argument[1]);
					}
					imap.listMailboxes();
					break;

				case n:
					if (msgnum < imap.getMessageCount()) {
						msgnum++;
						imap.setMessageSet(String.valueOf(msgnum));
						imap.fetchMessageInfo();
						imap.fetchMessageText();
					} else {
						System.out.println("No more messages in this mailbox.");
						msgnum = 0;
					}
					break;

				case q:
					imap.disconnect();
					return;

				case t:
					msgnum = Integer.parseInt(argument[1]);
					imap.setMessageSet(argument[1]);
					imap.fetchMessageText();
					break;

				case lk:
					System.out.println("\n\nAvailable keys:\n");
					keymgr.listKeys();
					System.out.println("\n");
					break;

				case d:
					imap.setMessageSet(argument[1]);
					imap.fetchMessageHeaders();
					imap.fetchMessageText();

					String decryptUserId = prompt("UserId (or email) of decryption key");

					imap.getKeys().clear();
					imap.getKeys().add(new Key(keymgr.getKeyring(), decryptUserId));
					imap.getKeys().item(0).setPassphrase(prompt("Decryption key passphrase"));

					imap.decrypt();

					System.out.println("\n\nDecrypted message: \n" + imap.getMessageText());

					break;	
					
				case v:
					imap.setMessageSet(argument[1]);
					imap.fetchMessageHeaders();
					imap.fetchMessageText();

					String signerUserId = prompt("UserId (or email) of signer key");

					imap.getSignerKeys().clear();
					imap.getSignerKeys().add(new Key(keymgr.getKeyring(), signerUserId));

					imap.verifySignature();

					System.out.println("\n\nVerified message: \n" + imap.getMessageText());

					break;					
					
				case dv:
					imap.setMessageSet(argument[1]);
					imap.fetchMessageHeaders();
					imap.fetchMessageText();

					decryptUserId = prompt("UserId (or email) of decryption key");

					imap.getKeys().clear();
					imap.getKeys().add(new Key(keymgr.getKeyring(), decryptUserId));
					imap.getKeys().item(0).setPassphrase(prompt("Decryption key passphrase"));

					signerUserId = prompt("UserId (or email) of signer key");

					imap.getSignerKeys().clear();
					imap.getSignerKeys().add(new Key(keymgr.getKeyring(), signerUserId));

					imap.decryptAndVerifySignature();

					System.out.println("\n\nDecrypt and verified message: \n" + imap.getMessageText());

					break;

				default: // allow user to enter only the number of the message
							// they want to view
					try {
						msgnum = Integer.parseInt(command);
						imap.setMessageSet(command);
						imap.fetchMessageText();
					} catch (NumberFormatException e) {
						System.out.println("Bad command / Not implemented in demo.");
					}

				}
			}
		} catch (IPWorksOpenPGPException ex) {
			System.out.println("IPWorksOpenPGP exception thrown: " + ex.getCode() + " [" + ex.getMessage() + "].");
			try {
				imap.disconnect();
			} catch (Exception e) {
			}
		} catch (Exception ex) {
			System.out.println(ex.getMessage());
		}
	}

	private static void displayMenu() {
		System.out.println("IMAP Commands");
		System.out.println("l                               list mailboxes");
		System.out.println("s <mailbox>                     select mailbox");
		System.out.println("t <message number>              type messages");
		System.out.println("n                               goto and type next message");
		System.out.println("f                               give head lines of messages");
		System.out.println("q                               quit, saving unresolved messages in mbox");
		System.out.println("h                               print out active message headers");
		System.out.println("lk                              list keys in the keyring");
		System.out.println("d <message number>              decrypt the message");
		System.out.println("v <message number>              verify the message");
		System.out.println("dv <message number>             decrypt and verify the message");
	}

}


class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksOpenPGPException) {
      System.out.print(" (" + ((IPWorksOpenPGPException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



