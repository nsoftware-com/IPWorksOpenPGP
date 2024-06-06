<?php
/*
 * IPWorks OpenPGP 2024 PHP Edition - Sample Project
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
require_once('../include/ipworksopenpgp_openpgp.php');
require_once('../include/ipworksopenpgp_keymgr.php');
require_once('../include/ipworksopenpgp_const.php');
?>
<?php

class MyKeyMgr extends IPWorksOpenPGP_KeyMgr
{
  function FireKeyList($param) {
    echo $param['keyid'] . "\t" . $param['userid'] . "\tPrivate Key: " . $param['hassecretkey'] . "\n";
  }
}

try{
  if ($argc < 3) {
    echo "Usage: node openpgp.js [options]\n";
    echo "Commands: \n";
    echo "  -k, --list-keys    list keys in keyring (--homedir)\n";
    echo "  -s, --sign         sign\n";
    echo "  -e, --encrypt      encrypt data\n";
    echo "  -d, --decrypt      decrypt data\n";
    echo "  -v, --verify       verify a signature\n";
    echo "Options for key selection:\n";
    echo "  --homedir          keyring directory (containing secring.gpg and pubring.gpg)\n";
    echo "  -r, --recipient    encrypt for USER-ID\n";
    echo "  -u, --local-user   private key to use for sign or decrypt\n";
    echo "  -p,                passphrase for private key\n";
    echo "  -c,                create a test key to use\n";
    echo "Options to specify input/output:\n";
    echo "  -f                 the input file to process\n";
    echo "  -o, --output       the output file (by default, output writes to console)\n";
    echo "  -w                 whether to overwrite the output file\n";
    echo "Examples:\n";
    echo "  node openpgp.js --homedir C:/temp/keys -k                       // list keys\n";
    echo "  node openpgp.js -se --homedir C:/temp/keys -r Bob -f input.txt  // sign and encrypt for user Bob\n";
    return;
  }

  $pgp = new IPWorksOpenPGP_OpenPGP();
  $keymgr = new MyKeyMgr();
  $encrypt = $sign = $verify = $decrypt = $list = $createKey = False;
  $recipientUserId = $userId = $passphrase = $keyringDir = "";

  for ($i = 1; $i < $argc; $i++) {
    if (str_starts_with($argv[$i],"-")) {
      if ($argv[$i] == "-k" || $argv[$i] == "--list-keys") { $list = True; }
      if ($argv[$i] == "-s" || $argv[$i] == "--sign") { $sign = True; }
      if ($argv[$i] == "-e" || $argv[$i] == "--encrypt") { $encrypt = True; }
      if ($argv[$i] == "-se") { $sign = True; $encrypt = True; }
      if ($argv[$i] == "-d" || $argv[$i] == "--decrypt") { $decrypt = True; }
      if ($argv[$i] == "-v" || $argv[$i] == "--verify") { $verify = True; }
      if ($argv[$i] == "-dv") { $decrypt = True; $verify = True; }
      if ($argv[$i] == "-f" || $argv[$i] == "--file") { $pgp->setInputFile($argv[$i + 1]); }
      if ($argv[$i] == "-o" || $argv[$i] == "--output") { $pgp->setOutputFile($argv[$i + 1]); }
      if ($argv[$i] == "-w") { $pgp->setOverwrite(True); }
      if ($argv[$i] == "-r" || $argv[$i] == "--recipient") { $recipientUserId = $argv[$i + 1]; }
      if ($argv[$i] == "-u" || $argv[$i] == "--local-user") { $userId = $argv[$i + 1]; }
      if ($argv[$i] == "-p") { $passphrase = $argv[$i + 1]; }
      if ($argv[$i] == "--homedir") { $keyringDir = $argv[$i + 1]; }
      if ($argv[$i] == "-c") { $createKey = True; }
    }
  }

  if ($createKey) { // do this first, in case we need to use it
    echo "Creating a test key (test@nsoftware.com). This key will be used for all operations.\n";
    if (strlen($keyringDir) == 0) { $keyringDir = ""; }
    $keymgr->doLoadKeyring($keyringDir);
    $keymgr->doCreateKey("test@nsoftware.com", "test");
    $keymgr->doSaveKeyring($keyringDir);
    $userId = "test@nsoftware.com";
    $passphrase = "test";
    $recipientUserId = "test@nsoftware.com";
  }

  $pgp->setASCIIArmor(True);
  if ($list) {
    if (strlen($keyringDir) == 0) {
      echo "No keyring specified.\n";
      return;
    }
    echo "Listing keys in keyring: " . $keyringDir . "...\n";
    $keymgr->doLoadKeyring($keyringDir);
    $keymgr->doListKeys();
  } else {
    // check input file...
    if (strlen($pgp->getInputFile()) == 0) {
      echo "No input file specified.\n";
      return;
    }

    if ($sign && $encrypt) {
      echo "Sign and Encrypt\n";

      $pgp->setKeyCount(1);
      $pgp->setKeyKeyring(0, $keyringDir);
      $pgp->setKeyUserId(0, $userId);
      $pgp->setKeyPassphrase(0, $passphrase);

      $pgp->setRecipientKeyCount(1);
      $pgp->setRecipientKeyKeyring(0, $keyringDir);
      $pgp->setRecipientKeyUserId(0, $recipientUserId);

      $pgp->doSignAndEncrypt();
    } elseif ($sign) {
      echo "Sign\n";

      $pgp->setKeyCount(1);
      $pgp->setKeyKeyring(0, $keyringDir);
      $pgp->setKeyUserId(0, $userId);
      $pgp->setKeyPassphrase(0, $passphrase);

      $pgp->doSign();
    } elseif ($encrypt) {
      echo "Encrypt\n";

      $pgp->setRecipientKeyCount(1);
      $pgp->setRecipientKeyKeyring(0, $keyringDir);
      $pgp->setRecipientKeyUserId(0, $recipientUserId);

      $pgp->doEncrypt();
    } elseif ($decrypt && $verify) {
      echo "Decrypt and Verify\n";

      $pgp->setKeyCount(1);
      $pgp->setKeyKeyring(0, $keyringDir);
      $pgp->setKeyUserId(0, $userId);
      $pgp->setKeyPassphrase(0, $passphrase);

      $pgp->setSignerKeyCount(1);
      $pgp->setSignerKeyKeyring(0, $keyringDir);
      $pgp->setSignerKeyUserId(0, $recipientUserId);

      $pgp->doDecryptAndVerifySignature();
    } elseif ($decrypt) {
      echo "Decrypt\n";

      $pgp->setKeyCount(1);
      $pgp->setKeyKeyring(0, $keyringDir);
      $pgp->setKeyUserId(0, $userId);
      $pgp->setKeyPassphrase(0, $passphrase);

      $pgp->doDecrypt();
    } elseif ($verify) {
      echo "Verify\n";

      $pgp->setSignerKeyCount(1);
      $pgp->setSignerKeyKeyring(0, $keyringDir);
      $pgp->setSignerKeyUserId(0, $recipientUserId);

      $pgp->doVerifySignature();
    } else {
      echo "No known action specified.\n";
    }

    if ($pgp->getOutputFile() == "") {
      echo "Output Message:\n" . $pgp->getOutputMessage() . "\n";
    }

  }
} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>