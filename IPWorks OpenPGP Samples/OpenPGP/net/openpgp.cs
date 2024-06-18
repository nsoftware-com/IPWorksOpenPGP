/*
 * IPWorks OpenPGP 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using nsoftware.IPWorksOpenPGP;

class openpgpDemo
{
  private static OpenPGP openpgp = new nsoftware.IPWorksOpenPGP.OpenPGP();

  static void Main(string[] args)
  {
    if (args.Length < 6)
    {
      Console.WriteLine("usage: openpgp /k keyringdir /p passphrase /a action /f inputfile /o outputfile [/w] /s inputstring\n");
      Console.WriteLine("  keyringdir   the path to the keyring, which must contain the files secring.gpg and pubring.gpg");
      Console.WriteLine("  passphrase   required to sign or decrypt");
      Console.WriteLine("  action       chosen from {sign, encrypt, signencrypt, decrypt, verify, decryptverify}");
      Console.WriteLine("  inputfile    the path to the input file (specify this or inputstring, but not both)");
      Console.WriteLine("  outputfile   the path to the output file (specify if inputfile is specified)");
      Console.WriteLine("  /w           whether to overwrite the output file (optional)");
      Console.WriteLine("  inputstring  the message to sign, encrypt, decrypt, or verify");
      Console.WriteLine("\nExample: openpgp /k c:\\pubring /p password /a signencrypt /f c:\\myfile.txt /o c:\\myencryptedfile.dat /w\n");
    }
    else
    {
      System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);
      string action = myArgs["a"].ToLower();

      KeyMgr keymgr = new KeyMgr();
      keymgr.LoadKeyring(myArgs["k"]);

      // Set up the action.
      if (myArgs.ContainsKey("f")) openpgp.InputFile = myArgs["f"];
      if (myArgs.ContainsKey("o")) openpgp.OutputFile = myArgs["o"];
      if (myArgs.ContainsKey("s")) openpgp.InputMessage = myArgs["s"];
      openpgp.Overwrite = myArgs.ContainsKey("w");
      openpgp.ASCIIArmor = true;

      // Perform the action.
      switch (action)
      {
        case "sign":
          // Need the private key to sign.
          keymgr.Key.Passphrase = myArgs["p"];
          openpgp.Keys.Add(keymgr.Key);

          openpgp.Sign();
          break;
        case "encrypt":
          // Need the recipient key to encrypt.
          openpgp.RecipientKeys.Add(keymgr.Key);

          openpgp.Encrypt();
          break;
        case "signencrypt":
          // Need the private key to sign.
          keymgr.Key.Passphrase = myArgs["p"];
          openpgp.Keys.Add(keymgr.Key);

          // Need the recipient key to encrypt.
          openpgp.RecipientKeys.Add(keymgr.Key);

          openpgp.SignAndEncrypt();
          break;
        case "decrypt":
          // Need the private key to decrypt.
          keymgr.Key.Passphrase = myArgs["p"];
          openpgp.Keys.Add(keymgr.Key);

          openpgp.Decrypt();
          break;
        case "verify":
          // Need the signer key to verify a signature.
          openpgp.SignerKeys.Add(keymgr.Key);

          openpgp.VerifySignature();
          break;
        case "decryptverify":
          // Need the private key to decrypt.
          keymgr.Key.Passphrase = myArgs["p"];
          openpgp.Keys.Add(keymgr.Key);

          // Need the signer key to verify a signature.
          openpgp.SignerKeys.Add(keymgr.Key);

          openpgp.DecryptAndVerifySignature();
          break;
        default:
          throw new Exception("Invalid action.\n");
      }

      Console.WriteLine("Completed " + action + " action.");
      if (myArgs.ContainsKey("o"))
      {
        Console.WriteLine("Output file: " + openpgp.OutputFile);
      }
      else
      {
        Console.WriteLine("Output message: " + openpgp.OutputMessage);
      }
    }
  }
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}