/*
 * IPWorks OpenPGP 2024 Java Edition - Sample Project
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

public class pfilemailer extends ConsoleDemo {
  PFileMailer mailer = new PFileMailer();
  KeyMgr keymgr = new KeyMgr();
  
  boolean encrypt = false;
  boolean sign = false;
  
  public pfilemailer() {
    String keyringDir = "";

    try {
      System.out.println("**************************************************************");
      System.out.println("IPWorks OpenPGP Java Edition - PFilemailer Demo Application.");
      System.out.println("This demo shows how to use the PFilemailer component to send  ");
      System.out.println("encrypted and signed emails.                                  ");
      System.out.println("**************************************************************\n");

      keymgr.addKeyMgrEventListener(new DefaultKeyMgrEventListener() {
        public void keyList(KeyMgrKeyListEvent e) {
          System.out.println(String.format("%-80s [%-8s] %-12s", e.userId, e.keyId, (e.hasSecretKey ? "private" : "")));
        }
      });        

      while(true) {        
        int command = Character.getNumericValue(ask("Please enter a command", ":", "(0: quit, 1: configure pgp settings, 2: send new email)"));
        if (command < 0 || command > 2) {
          System.out.println("Invalid Command.");
          continue;
        }
        if (command == 0) {
          break;
        } else if(command == 1) { //Configure PGP settings.
          while(true) {
            System.out.println("Current PGP Settings...");
            System.out.println("\tKeyring:                " + 
                      (keymgr.getKeyring().length() > 0 ? keymgr.getKeyring() : "Not set"));
            System.out.println("\tPrivate Key:            " + 
                      (mailer.getKeys().size() > 0 ? mailer.getKeys().get(0).getUserId() : "None selected"));
            System.out.println("\tPrivate Key Passphrase: " + (mailer.getKeys().size() > 0 ? 
                      (mailer.getKeys().get(0).getPassphrase().length() > 0 ? "Set" : "Not set") : "Not set"));
            System.out.println("\tRecipient Key:          " + 
                      (mailer.getRecipientKeys().size() > 0 ? mailer.getRecipientKeys().get(0).getUserId() : "Non selected"));
            System.out.println();
            int option = Character.getNumericValue(ask("Please select an operation", ":", "(0: back, 1: change keyring, " + 
                      "2: list keys, 3: change private key, 4: change recipient key)"));
            
            if(option == 0) {       //Go back to previous menu.
              break;
            } else if(option == 1) {   //Specify keyring
              boolean keyringLoaded = false;
              while(!keyringLoaded) {
                try {
                  String entry = prompt("Keyring Directory (Enter \":q\" to exit)");
                  if(entry.equalsIgnoreCase(":q"))
                    break;
                  keymgr.loadKeyring(entry);
                  keyringDir = entry;
                  keyringLoaded = true;
                } catch(Exception ex) {
                  displayError(ex);      
                }
              }

              System.out.println("\nListing keys...\n");
              System.out.println(String.format("%-80s %-10s %-12s", "User ID", "Key ID", "Private Key"));
              keymgr.listKeys();
              System.out.println();
            } else if(option == 2) {   //List keys
              System.out.println("\nListing keys...\n");
              System.out.println(String.format("%-80s %-10s %-12s", "User ID", "Key ID", "Private Key"));
              keymgr.listKeys();
              System.out.println();
            } else if(option == 3) {   //Specify private key
              boolean keyLoaded = false;
              while(!keyLoaded) {
                try {
                  String entry = prompt("UserId (Enter \":q\" to exit, \":c\" to clear entry)");
                  if(entry.equalsIgnoreCase(":q")) {
                    break;
                  } else if(entry.equalsIgnoreCase(":c")) {
                    mailer.getKeys().clear();
                  } else {
                    mailer.getKeys().clear();
                    mailer.getKeys().add(new Key(keyringDir, entry));
                    mailer.getKeys().get(0).setPassphrase(prompt("Passphrase"));
                  }
                  keyLoaded = true;
                } catch(Exception ex) {
                  displayError(ex);
                }
              }              
            } else if(option == 4) {  //Specify recipient key
              boolean keyLoaded = false;
              while(!keyLoaded) {
                try {
                  String entry = prompt("Recipient UserId (Enter \":q\" to exit, \":c\" to clear entry)");
                  if(entry.equalsIgnoreCase(":q")) {
                    break;
                  } else if(entry.equalsIgnoreCase(":c")) {
                    mailer.getRecipientKeys().clear();
                  } else {
                    mailer.getRecipientKeys().clear();
                    mailer.getRecipientKeys().add(new Key(keyringDir, entry));
                  }
                  keyLoaded = true;
                } catch(Exception ex) {
                  displayError(ex);
                }
              }
            } else {          //Invalid option specified
              System.out.println("Invalid command.");
            }
          }
        } else if(command == 2) {  //Send an email
          mailer.setMailServer(prompt("Enter mail server"));
          mailer.setMailPort(Integer.parseInt(prompt("Port")));
          mailer.setFrom(prompt("From"));
          mailer.setSendTo(prompt("To"));
          mailer.setSubject(prompt("Subject"));
          
          mailer.setSSLStartMode(0); // auto

          //Use these properties for client authentication.
          mailer.setUser(prompt("User"));
          mailer.setPassword(prompt("Password"));
          
          mailer.config("ProcessAttachments=true");
          
          System.out.println("Please enter the message. When finished enter \":q\" on a line by itself:");
                BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));
                String temp = "";
                String message = "";
                while (!(temp = bf.readLine()).equalsIgnoreCase(":q")) {
                  message += temp + "\n";
                }
                mailer.setMessageText(message);
          
                
                mailer.getAttachments().clear();
                System.out.println("Please enter the full path to attachments that should be sent, one per line." + 
                          "When finished, enter \":q\" on a line by itself:");
                while(!(temp = bf.readLine()).equalsIgnoreCase(":q")) {
                  mailer.getAttachments().add(new FileAttachment(temp));
                }
                
                if(mailer.getKeys().size() > 0 && mailer.getRecipientKeys().size() > 0) {
                  mailer.signAndEncrypt();
                } else if(mailer.getKeys().size() > 0) {
                  mailer.sign();
                } else if(mailer.getRecipientKeys().size() > 0) {
                  mailer.encrypt();
                }
                
                mailer.send();
                System.out.println("Message sent.");
        }
      }      
    } catch(Exception e) {
      displayError(e);
    }
  }

  public static void main(String[] args) {
    new pfilemailer();
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
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
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

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



