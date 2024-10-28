# 
# IPWorks OpenPGP 2024 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of IPWorks OpenPGP in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/ipworksopenpgp
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from ipworksopenpgp import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] is None:
    args[index] = input(prompt)


pgp = OpenPGP()
kmgr = KeyMgr()
keyringDir = ""

def createKey():
  if(keyringDir != ""):
    listKeys()
  kmgr.create_key(input("Enter a UserId: "), input("Enter a passphrase: "))
  if(input("Create another key? (y/n): ") == "y"):
    create_key()

def fireKeyList(e):
  print("%8s  %5s            %s" %(e.key_id, e.has_secret_key, e.user_id))

def listKeys():
  print("Listing keys...")
  print("KeyId     Has Secret Key?  UserId")
  print("---------------------------------")
  kmgr.load_keyring(keyringDir)
  kmgr.list_keys()

def selectKey():
  userId = input("Enter the UserId of the private key used for signing/decryption: ")
  pgp.set_key_count(1)
  pgp.set_key_keyring(0, keyringDir)
  pgp.set_key_user_id(0, userId)
  passphrase = input("Enter the passphrase for the selected key: ")
  pgp.set_key_passphrase(0, passphrase)

def selectRecipientKey():
  userId = input("Enter the UserId of the public key used for encryption: ")
  pgp.set_recipient_key_count(1)
  pgp.set_recipient_key_keyring(0, keyringDir)
  pgp.set_recipient_key_user_id(0, userId)

def selectSignerKey():
  userId = input("Enter the UserId of the public key used for verification: ")
  pgp.set_recipient_key_count(1)
  pgp.set_recipient_key_keyring(0, keyringDir)
  pgp.set_recipient_key_user_id(0, userId)

def promptForAsciiArmor():
  return True if(input("Use ASCII Armor? (y/n): ") == "y") else False

def doEncrypt():
  selectRecipientKey()
  pgp.set_ascii_armor(promptForAsciiArmor())
  pgp.encrypt()

def doDecrypt():
  selectKey()
  pgp.decrypt()

def doSign():
  selectKey()
  pgp.set_ascii_armor(promptForAsciiArmor())
  pgp.sign()

def doVerify():
  selectSignerKey()
  pgp.verify()

def doSignAndEncrypt():
  selectKey()
  selectRecipientKey()
  pgp.set_ascii_armor(promptForAsciiArmor())
  pgp.sign_and_encrypt

def doDecryptAndVerify():
  selectKey()
  selectSignerKey()
  pgp.decrypt_and_verify()

kmgr.on_key_list = fireKeyList

print("\nWelcome to the /n software OpenPGP demo.\n")
prompt = "What would you like to do?"
prompt += "\n  0: quit"
prompt += "\n  1: encrypt"
prompt += "\n  2: decrypt"
prompt += "\n  3: sign"
prompt += "\n  4: verify"
prompt += "\n  5: sign & encrypt"
prompt += "\n  6: decrypt & verify\n"
prompt += "Choice: "
op = input(prompt)

if(op == "0"):
  sys.exit(0)

inSrc = input("Read from a file or string? (f/s): ")
if(inSrc == "s"):
  pgp.set_input_message(input("Please enter the input string: "))
else:
  pgp.set_input_file(input("Please enter the input file path: "))

outSrc = input("Write to a file or string? (f/s): ")
if(outSrc == "f"):
  pgp.set_output_file(input("Please enter the output file path: "))

keyringDir = input("Provide a path to a keyring (or for one to be created at): ")
if(input("Create a new key? (y/n): ") == "y"):
  createKey()
  kmgr.save_keyring(keyringDir)
listKeys()

if(op == "1"):
  doEncrypt()
elif(op == "2"):
  doDecrypt()
elif(op == "3"):
  doSign()
elif(op == "4"):
  doVerify()
elif(op == "5"):
  doSignAndEncrypt
elif(op == "6"):
  doDecryptAndVerify

if(outSrc == "s"):
  print("OutputMessage: %s\n" %(pgp.get_output_message()))
print("\nThe demo has finished running.")

