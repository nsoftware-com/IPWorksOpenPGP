/*
 * IPWorks OpenPGP 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworksopenpgp = require("@nsoftware/ipworksopenpgp");

if(!ipworksopenpgp) {
  console.error("Cannot find ipworksopenpgp.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main();

async function main() {
  const argv = process.argv;
  if (argv.length < 3) {
    console.log("Usage: node openpgp.js  -k keyringdir [options]-u userid -p passphrase -a action -f inputfile -o outputfile [-w] -s inputstring");
    console.log("Commands: ");
    console.log("  -k, --list-keys    list keys in keyring (--homedir)");
    console.log("  -s, --sign         sign");
    console.log("  -e, --encrypt      encrypt data");
    console.log("  -d, --decrypt      decrypt data");
    console.log("  -v, --verify       verify a signature");
    console.log("Options for key selection:");
    console.log("  --homedir          keyring directory (containing secring.gpg and pubring.gpg)");
    console.log("  -r, --recipient    encrypt for USER-ID");
    console.log("  -u, --local-user   private key to use for sign or decrypt");
    console.log("  -p,                passphrase for private key");
    console.log("  -c,                create a test key to use");
    console.log("Options to specify input/output:");
    console.log("  -f                 the input file to process");
    console.log("  -o, --output       the output file (by default, output writes to console)");
    console.log("  -w                 whether to overwrite the output file");
    console.log("Examples:");
    console.log("  node openpgp.js --homedir C:/temp/keys -k                       // list keys");
    console.log("  node openpgp.js -se --homedir C:/temp/keys -r Bob -f input.txt  // sign and encrypt for user Bob");
    process.exit();
  }

  
  const pgp = new ipworksopenpgp.openpgp();
  const keymgr = new ipworksopenpgp.keymgr();
  keymgr.on('KeyList', function(e){
    console.log(e.keyId + "\t" + e.userId + "\tPrivate Key: " + e.hasSecretKey);
  });
  let encrypt, sign, verify, decrypt, list, createKey = false;
  let recipientUserId, userId, passphrase, keyringDir = "";
  for (i = 0; i < argv.length; ++i) {
    if (argv[i].startsWith("-")) {
      if (argv[i] === "-k" || argv[i] === "--list-keys") {  list = true; }
      if (argv[i] === "-s" || argv[i] === "--sign") {       sign = true; }
      if (argv[i] === "-e" || argv[i] === "--encrypt") {    encrypt = true; } 
      if (argv[i] === "-se") {                              sign = true; encrypt = true; }
      if (argv[i] === "-d" || argv[i] === "--decrypt") {    decrypt = true; }
      if (argv[i] === "-v" || argv[i] === "--verify") {     verify = true; }
      if (argv[i] === "-dv") {                              decrypt = true; verify = true; }
      if (argv[i] === "-f") {                               pgp.setInputFile(argv[i + 1]); }
      if (argv[i] === "-o" || argv[i] === "--output") {     pgp.setOutputFile(argv[i + 1]); }
      if (argv[i] === "-w") {                               pgp.setOverwrite(true); }
      if (argv[i] === "-r" || argv[i] === "--recipient") {  recipientUserId = argv[i + 1]; }
      if (argv[i] === "-u" || argv[i] === "--local-user") { userId = argv[i + 1]; }
      if (argv[i] === "-p") {                               passphrase = argv[i + 1]; }
      if (argv[i] === "--homedir") {                        keyringDir = argv[i + 1]; }
      if (argv[i] === "-c") {                               createKey = true; }
    }
  }

  if (createKey) { // do this first, in case we need to use it
    console.log("Creating a test key (test@nsoftware.com). This key will be used for all operations.");
    if (keyringDir.length == 0) { keyringDir = "./"; }
    await keymgr.loadKeyring(keyringDir).catch((e) => {
      console.error(e);
      process.exit();
    });
    await keymgr.createKey("test@nsoftware.com","test").catch((e) => {
      console.error(e);
      process.exit();
    });
    await keymgr.saveKeyring(keyringDir).catch((e) => {
      console.error(e);
      process.exit();
    });
    userid = "test@nsoftware.com";
    passphrase = "test";
    recipientUserId = "test@nsoftware.com";
  }

  pgp.setASCIIArmor(true);

  if (list) {
    if (keyringDir.length == 0) {
      console.log("No keyring specified.");
      process.exit();
    }
    console.log("Listing keys in keyring: " + keyringDir + "...");
    await keymgr.loadKeyring(keyringDir).catch((e) => {
      console.error(e);
      process.exit();
    });
    await keymgr.listKeys().catch((e) => {
      console.error(e);
      process.exit();
    });
  }
  else 
  {
    // check input file
    if (pgp.getInputFile().length == 0) {
      console.log("No input file specified.");
      process.exit();
    }

    try {
      if (sign && encrypt) {
        console.log("Sign and Encrypt");
        // todo: verify options fit action ??
        pgp.getKeys().add(await new ipworksopenpgp.Key(keyringDir, userId));
        pgp.getKeys().get(0).setPassphrase(passphrase);
        pgp.getRecipientKeys().add(await new ipworksopenpgp.Key(keyringDir, recipientUserId));
        await pgp.signAndEncrypt();
      } else if (sign) {
        console.log("Signing...");
        pgp.getKeys().add(await new ipworksopenpgp.Key(keyringDir, userId));
        pgp.getKeys().get(0).setPassphrase(passphrase);
        await pgp.sign();
      } else if (encrypt) {
        console.log("Encrypting...");
        pgp.getRecipientKeys().add(await new ipworksopenpgp.Key(keyringDir, recipientUserId));
        await pgp.encrypt();
      } else if (decrypt && verify) {
        console.log("Decrypt and Verify");
        pgp.getKeys().add(await new ipworksopenpgp.Key(keyringDir, userId));
        pgp.getKeys().get(0).setPassphrase(passphrase);
        pgp.getSignerKeys().add(await new ipworksopenpgp.Key(keyringDir, recipientUserId));
        await pgp.decryptAndVerifySignature();
      } else if (decrypt) {
        console.log("Decrypt");
        pgp.getKeys().add(await new ipworksopenpgp.Key(keyringDir, userId));
        pgp.getKeys().get(0).setPassphrase(passphrase);
        await pgp.decrypt();
      } else if (verify) {
        console.log("Verify");
        pgp.getSignerKeys().add(await new ipworksopenpgp.Key(keyringDir, recipientUserId));
        await pgp.verifySignature();
      } else {
        console.log("No known action specified.");
      }

      // output
      if (pgp.getOutputMessage().length > 0) {
        console.log("Output Message:\n" + pgp.getOutputMessage());
      }
    } catch (ex) {
      console.log("Error: " + ex.message);
      process.exit();
    }
  }
  process.exit();
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
