#!/usr/bin/env node

const fs = require('fs');
const process = require('process');
const crypto = require('crypto');
const path = require('path');

// things you may need to `npm install -g`.
const csv = require('csv-parse/sync');

let verbose = false;
function error_out(message) {
  console.error(message);
  process.exit(1);
}

function debug(message) {
  if (verbose) {
    console.error(message);
  }
}

// write a 128-bit unsigned integer to a buffer at a given offset, in big-endian order
function writeBigUInt128BE(buf, uint, offset) {
   const upper = uint >> 64n;
   const lower = uint & 0x0000000000000000ffffffffffffffffn;
   buf.writeBigUInt64BE(upper, offset);
   buf.writeBigUInt64BE(lower, offset + 8);
}

// make an NFT descriptor
function makeNFTDesc(dataHash, size, tickets) {
  // format
  // |--hash (32 bytes)--|--size (16 bytes)--|--tickets (16 bytes)--|

  const buf = Buffer.alloc(64);
  dataHash.copy(buf, 0);
  writeBigUInt128BE(buf, BigInt(size), 32);
  writeBigUInt128BE(buf, BigInt(tickets), 48);

  const descHasher = crypto.createHash('sha512-256');
  descHasher.update(buf);
  const descHash = descHasher.digest();

  const ret = {
    'nftDesc': buf,
    'nftDescHash': descHash,
    'dataHash': dataHash.toString('hex'),
    'size': size,
    'tickets': tickets
  };

  return ret;
}

// generate an NFT descriptor from a file path and its number of tickets
function NFTDescFromFile(path, tickets) {
  const fileBuff = fs.readFileSync(path);
  const fileInfo = fs.statSync(path);

  const hasher = crypto.createHash('sha512-256');
  hasher.update(fileBuff);
  const dataHash = hasher.digest();

  return makeNFTDesc(dataHash, fileInfo.size, tickets);
}

// load up the CSV of file paths <--> tickets
// return an object that maps the name of the NFT to its tickets
function loadNFTreeCSV(path) {
  try {
    const csv_data = fs.readFileSync(path)
    const nft_csv = csv.parse(csv_data, { columns: true });
    if (nft_csv === undefined) {
      error_out(`Malformed tickets CSV at ${path}: could not parse`);
    }

    // sanity check
    for (let i = 0; i < nft_csv.length; i++) {
      if (nft_csv[i]['name'] === undefined) {
        error_out(`Malformed tickets CSV at ${path}: missing 'name' column`);
      }
      if (nft_csv[i]['tickets'] === undefined) {
        error_out(`Malformed tickets CSV at ${path}: missing 'tickets' column`);
      }
    }

    // merge to object
    const tickets = {};
    for (let i = 0; i < nft_csv.length; i++) {
      const ticks = parseInt(nft_csv[i]['tickets']);
      if (isNaN(ticks)) {
        error_out(`Malformed tickets CSV at ${path}: invalid 'tickets' value for '${nft_csv[i]['name']}'`);
      }

      tickets[nft_csv[i]['name']] = { 'tickets': ticks };
    }
    return tickets;
  }
  catch (e) {
    error_out(`Failed to load ${path}`);
  }
}

// process one file.
// returns the nft desc and path to the nft itself.
// writes the nft data to `$dest_root/$hash`
// writes the nft desc to `$dest_root/$hash.desc`
function processOneFile(nft_csv, src_root, src_path, dest_root) {
  if (nft_csv[src_path] === undefined) {
    throw new Error(`No tickets defined for ${src_path} in ${src_root}`);
  }
  const tickets = nft_csv[src_path].tickets;
  const srcFullPath = path.join(src_root, src_path);
  const nftDesc = NFTDescFromFile(srcFullPath, tickets);

  const nftDestPath = path.join(dest_root, nftDesc.dataHash);
  const nftDescPath = `${nftDestPath}.desc`;

  try {
    const fileInfo = fs.statSync(nftDestPath);
    error_out(`NFT for ${srcFullPath} at ${nftDestPath} already exists`);
  }
  catch (e) {
  }
  
  fs.copyFileSync(srcFullPath, nftDestPath, fs.constants.COPYFILE_EXCL);
  fs.writeFileSync(nftDescPath, JSON.stringify(nftDesc.nftDesc.toString('hex')));

  const ret = {
    'nftDesc': nftDesc,
    'nftDestPath': nftDestPath
  };

  return ret;
}

// make a merkle tree out of a sequence of hashes.
// returns the merkle tree as a list of list of buffers.
// the first item is a list containing the leaf hashes.
// the last item is a 1-item list with the merkle root.
function makeMerkleTree(hashes) {
  const leaves = hashes.map(function(h) { return h });
  if (leaves.length % 2 === 1) {
    leaves.push(leaves[leaves.length - 1]);
  }
  const tree = [leaves];
  while (hashes.length > 1) {
    const next_layer = [];
    if (hashes.length % 2 === 1) {
      hashes.push(hashes[hashes.length - 1]);
    }
    for (let i = 0; i < hashes.length / 2; i++) {
      const h1 = hashes[2*i];
      const h2 = hashes[2*i + 1];

      const hasher = crypto.createHash('sha512-256');
      hasher.update(h1);
      hasher.update(h2);
      const h = hasher.digest();
      next_layer.push(h);
    }
    hashes = next_layer.map(function(h) { return h });
    tree.push(next_layer);
  }
  return tree;
}

// Make a merkle tree proof for the ith leaf (index)
function makeMerkleProof(merkle_tree, index) {
  const saveIndex = index;
  if (index >= merkle_tree[0].length) {
    throw new Error(`index out of bounds: ${index} >= ${merkle_tree[0].length}`);
  }
  const proof = [];
  for (let i = 0; i < merkle_tree.length - 1; i++) {
    let sibling_hash = undefined;
    if (index % 2 === 0) {
      sibling_hash = merkle_tree[i][index + 1];
    }
    else {
      sibling_hash = merkle_tree[i][index - 1];
    }
    proof.push(sibling_hash);
    index >>= 1;
  }
  const ret = {
    "hashes": proof.map(function(h) { return h.toString('hex') }),
    "index": saveIndex
  };
  return ret;
}

// Process a directory of NFTs
// * store each NFT in a file under dest_root named by its hash
// * store its NFT descriptor as $hash.desc
// * make a Merkle proof for the NFT descriotor, and store it as $hash.proof as a JSON string
// * store the Merkle tree root to $dest_root/root
function processOneDirectory(src_root, dest_root) {
  debug(`Process ${src_root} --> ${dest_root}`);

  // get tickets DB
  const tickets_path = path.join(src_root, 'tickets.csv');
  const nft_csv = loadNFTreeCSV(tickets_path);

  // process each file in lexicographic order
  const children = fs.readdirSync(src_root, { withFileTypes: true });
  children.sort(function(a, b) { return a.name < b.name ? -1 : a.name === b.name ? 0 : 1 });
  
  const nft_infos = [];
  const nft_desc_hashes = [];
  let total_tickets = 0;
  let total_size = 0;
  for (let i = 0; i < children.length; i++) {
    if (children[i].name === "tickets.csv") {
      continue;
    }

    let nft_info = undefined;
    if (children[i].isFile()) {
      try {
        const rec = processOneFile(nft_csv, src_root, children[i].name, dest_root);
        nft_info = rec.nftDesc;
      }
      catch (e) {
        console.error(`WARN: failed to process NFT file at ${path.join(src_root, children[i].name)}: ${e.message}`);
        continue;
      }
    }
    else if (children[i].isDirectory()) {
      try {
        const new_src_root = path.join(src_root, children[i].name);
        const new_dest_root = path.join(dest_root, children[i].name);
        const rec = processOneDirectory(new_src_root, dest_root);
        nft_info = rec.nftDesc;
      }
      catch (e) {
        console.error("WARN: failed to process NFT collection at ${new_src_root}: ${e.message}");
        continue;
      }
    }
    else {
      continue;
    }

    nft_infos.push(nft_info);
    total_tickets += nft_info.tickets;
    total_size += nft_info.size;
    nft_desc_hashes.push(nft_info.nftDescHash);
  }

  // override how much this NFTree costs?
  if (nft_csv['.'] !== undefined && nft_csv['.'].tickets !== undefined) {
    total_tickets = nft_csv['.'].tickets;
  }

  // empty hash
  let merkle_root = Buffer.from("c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a", "hex");
  if (nft_desc_hashes.length > 0) {
    // make and store proofs
    const merkle_tree = makeMerkleTree(nft_desc_hashes);
    for (let i = 0; i < nft_infos.length; i++) {
      const proof = makeMerkleProof(merkle_tree, i);
      const proof_str = JSON.stringify(proof);
      fs.writeFileSync(path.join(dest_root, `${nft_infos[i].dataHash}.proof`), proof_str);
    }
    merkle_root = merkle_tree[merkle_tree.length - 1][0];
  }

  // store Merkle root
  fs.writeFileSync(path.join(dest_root, "root"), JSON.stringify(merkle_root.toString('hex')));
  
  const dirDesc = makeNFTDesc(merkle_root, total_size, total_tickets);
  const dirDescPath = path.join(dest_root, `${dirDesc.dataHash}.desc`);
  fs.writeFileSync(dirDescPath, JSON.stringify(dirDesc.nftDesc.toString('hex')));

  const ret = {
    'nftDesc': dirDesc,
    'nftDestPath': dest_root
  };

  debug(`Merkle root of ${src_root} is in ${dirDescPath}`);
  return ret;
}

// verify a Merkle proof.
// root_hash and file_hash are buffers
// proof is loaded from disk
function verifyProof(root_hash, desc_hash, proof) {
  let idx = proof.index;
  let cur_hash = desc_hash;
  for (let i = 0; i < proof.hashes.length; i++) {
    const proof_hash = Buffer.from(proof.hashes[i], 'hex');
    const hasher = crypto.createHash('sha512-256');
    if (idx % 2 === 0) {
      hasher.update(cur_hash);
      hasher.update(proof_hash);
    }
    else {
      hasher.update(proof_hash);
      hasher.update(cur_hash);
    }
    cur_hash = hasher.digest();
  }
  return cur_hash.compare(root_hash) === 0;
}

// getopt(3)-like function.
// if opts is ab:c, then -a and -c are switches, and -b is a switch with an argument
function getOpts(argv, opts) {
  const optsTable = {};
  const remainingArgv = [];
  const argvBuff = argv.slice(0);

  for (let i = 0; i < opts.length; i++) {
    if (opts[i] == ':') {
      continue;
    }
    if (i + 1 < opts.length && opts[i + 1] == ':') {
      optsTable[opts[i]] = null;
    } else {
      optsTable[opts[i]] = false;
    }
  }

  for (const opt of Object.keys(optsTable)) {
    for (let i = 0; i < argvBuff.length; i++) {
      if (argvBuff[i] === null) {
        break;
      }
      if (argvBuff[i] === '--') {
        break;
      }

      const argvOpt = `-${opt}`;
      if (argvOpt === argvBuff[i]) {
        if (optsTable[opt] === false) {
          // boolean switch
          optsTable[opt] = true;
          argvBuff[i] = '';
        } else {
          // argument
          optsTable[opt] = argvBuff[i + 1];
          argvBuff[i] = '';
          argvBuff[i + 1] = '';
        }
      }
    }
  }

  for (let i = 0; i < argvBuff.length; i++) {
    if (argvBuff[i].length > 0) {
      if (argvBuff[i] === '--') {
        continue;
      }
      remainingArgv.push(argvBuff[i]);
    }
  }

  optsTable['_'] = remainingArgv;
  return optsTable;
}

function usage(progname, command) {
  console.error(`Usage: ${progname} [opts] command ARGS`);
  
  if (command === "build") {
    console.error("");
    console.error(`Command usage: ${progname} build SRC_DIR DEST_DIR`);
    console.error("Where:");
    console.error("   SRC_DIR: the path to the directory that contains all the original NFTs");
    console.error("   DEST_DIR: the path to the directory in which to store all the NFTree data to upload");
    console.error("");
  }
  else if (command === "verify") {
    console.error("");
    console.error(`Command usage: ${progname} verify ROOT_PATH DESC_PATH PROOF_PATH`);
    console.error("Where:");
    console.error("   ROOT_PATH: the path to the `root` file in the NFTree directory, or the parent NFT desc file for the collection that contains this NFT");
    console.error("   DESC_PATH: the path to the NFTree descriptor file (ends in .desc)");
    console.error("   PROOF_PATH: the path to the NFTree Merkle proof file (ends in .proof)");
    console.error("");
  }
  else {
    console.error("");
    console.error("Options:");
    console.error("   -v       Verbose debug output");
    console.error("");
    console.error("Commands:");
    console.error("   build SRC_DIR DEST_DIR");
    console.error("   verify ROOT_PATH DESC_PATH PROOF_PATH");
    console.error("");
    console.error(`Run \`${progname} COMMAND\` for command-specific help`);
    console.error("");
  }

  process.exit(1);
}

function main(argv) {
  const opts = getOpts(argv.slice(1), 'v');
  const args = opts['_'];
  if (args.length < 2) {
    usage(argv[1], undefined);
  }

  if (opts['v']) {
    verbose = true;
  }

  const command = args[1];
  if (command === "build") {
    if (args.length < 4) {
      usage(argv[1], command);
    }
    const src_dir = args[2];
    const dest_dir = args[3];

    if (fs.statSync(dest_dir, { throwIfNoEntry: false }) === undefined) {
      fs.mkdirSync(dest_dir, { recursive: true });
    }
    else {
      error_out(`The path ${dest_dir} already exists; aborting`);
    }

    const collection = processOneDirectory(src_dir, dest_dir);
    console.log(JSON.stringify(collection.nftDesc.nftDesc.toString('hex')));
    process.exit(0);
  }
  else if (command === "verify") {
    if (args.length < 5) {
      usage(argv[1], command);
    }
    let root_hash = Buffer.from(JSON.parse(fs.readFileSync(args[2])), 'hex');
    if (root_hash.length == 64) {
      // this is an NFT descriptor, so extract the hash
      root_hash = root_hash.slice(0,32);
    }
    const desc = Buffer.from(JSON.parse(fs.readFileSync(args[3])), 'hex');
    const proof = JSON.parse(fs.readFileSync(args[4]));

    const hasher = crypto.createHash('sha512-256');
    hasher.update(desc);
    const desc_hash = hasher.digest();

    console.log(JSON.stringify(verifyProof(root_hash, desc_hash, proof)));
    process.exit(0);
  }

  usage(argv[1], undefined);
}

main(process.argv);
