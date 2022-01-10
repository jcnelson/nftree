# NFTrees

A scalable way to mint arbitrarily large collections of NFTs that
earn their owners a yield in STX.

## TL;DR: Features

* Mint an *arbitrarily large* collection of NFTs with a *single* transaction.
* The buyer, not the creator, pays the transaction fee for minting the NFT.
* Minted but NFTs generate *passive income* for their owner in STX if the owner
  locks them up to prevent them from being sold or accessed on-chain.
* No pre-sale moral hazards.  NFTs are instantiated on-chain via the smart
  contract on buyer demand.  The NFT creator instantiates the set of NFTs, but the
buyer pays to instantiate an individual NFTs from the set when they take
possession.
* Minting process rewards early NFT community members of successful NFT projects
  built as NFTrees.

## Background

An NFT is a representation of a binding between a principal and data on a blockchain.  The principal
is identified by a set of one or more public keys or smart contracts, and the
data is a public arbitrary string of bytes (such as, but not limited to, an
image).  The binding is maintained by a blockchain, which provides a reasonable
and programmatically-enforced assurance that only the bound principal can
_transfer_ the data to another principal.

NFTs are a fundamental building block for a user-owned Internet.  Providing a
way to prove that a particular piece of data is bound to a particular
principal enables users to treat their data as _captial assets_ within the
context of NFT-aware applications.  For example, an NFT can act as an API key
to an application's features: only people who own a particular kind of NFT may
access them.  If the features are valuable -- as in, someone might be willing to
pay for them -- then the NFT itself is valuable.  As another example, an NFT can
act as a row in an equity capitalization table of a company: the owner of the
NFT is entitled to revenue from the company proportional to the amount of equity
the NFT represents.  As a third example, an NFT can represent user-generated
data, such as artworks, songs, movies, novels, and so on, whereby the act of
producing and selling NFTs that represent this data is the act of funding the
user's creative process.

The act of storing the NFT's binding on a blockchain is the act of empowering
individuals across the world to seamlessly treat their data as capital assets.
This is what differentiates NFTs from similar monetizable principal/data binding
systems like domain names and online game items.  The open-access nature of NFTs not only enables any
user anywhere to acquire, use, and transfer NFTs, but also produce whole new
sets of NFTs with as-of-yet-unknown use-cases.

### NFT Production and Valuation

Regardless of how NFTs are used, they are usually produced in the same way:

* They are produced as a collection.  The creator instantiates NFTs in batches,
  such as to raise funds for a creative endeavor or promote the applications that use them.

* Collections are produced in a series.  An NFT creator may produce multiple
  batches over time, such as when they produce more data to sell or add more
features to applications that use them.

This also applies to NFTs that are programmatically generated by a smart
contract: depending on the mechanism, the NFTs minted this way can be treated as
a single batch of NFTs minted over a long time period, or a series of 1-item
NFTs minted over the same time period.

Once an NFT is produced, it can be traded via the blockchain to other principals
for other crypto assets.  This enables a market for NFTs from a particular
collection or series to form, which in turn determines the spot price for an NFT
in a particular collection in a particular series over time.

### Problems with the State-of-the-Art

The state-of-the-art way to sell a collection of NFTs is for the NFT creator -- be it a
person or smart contract -- to instantiate the bindings for each NFT in one of
two ways: all up-front, or upon request from a buyer.
Once instantiated, they can be transferred to other users at the creator's whim,
such as by selling them.  While straightforward, this introduces several problems:

* It can clog the blockchain.  If the NFT creator instantiates an NFT before
  there is a buyer, it needlessly consumes blockchain compute capacity.

* It costs the creator tokens.  In both approaches, the NFT
creator ends up paying for the transaction fee in the underlying blockchain's
tokens.  While the creator can price the transaction fee into the sale cost, the
current sale mechanism creates a recurring token cost that the seller must be
willing to pay.

* It creates a moral hazard for the NFT creator.  The collection creator initially
  owns all NFTs, and effectively controls the market for them.  The NFT creator
could devalue their collection by dumping their holdings.  Also, because the NFT
creator receives payment for their sold NFTs up-front, they are in a position to
exit-scam their users:  they could promise to use the raised funds to build out an
application for the NFTs, and then just take the money and leave.

* It underutilizes and under-rewards the user community.  Selling NFTs
to would-be users this way only encourages them flip NFTs, instead of trying
to make the project itself better.  This is unsustainable, because
nothing appreciates forever.  If the NFT is sold as a way to raise funds to
build out some good or service that the NFT will eventually be used to access,
then this introduces a window of time where users would rather buy and flip NFTs
instead of commit to the project long-term (distorting the market and creating a
moral hazard for the NFT creators).

These problems are addressed by NFTrees.

## What is an NFTree?

An NFTree is an NFT that represents an arbitrarily-large collection of other NFTs and
NFTrees, while behaving as a single NFT on the blockchain.  A user instantiates an NFT
off of an NFTree by submitting a proof that the NFT is represented in the NFTree,
which then instantiates that NFT as a separate binding owned by that user.  Once
"plucked" from the NFTree, the NFT cannot be instantiated again -- it is
exclusively owned by that user.

An NFT creator would mint a single NFTree for each collection they make.  An NFT
buyer would pay to instantiate an NFT off of the NFTree.  This way, NFTs are
only instantiated once there is demand for them, which prevents the blockchain
from getting clogged.

Two of the problems with the state-of-the-art way of minting NFTs have to
do with creating the right incentives for a successful NFT project.  NFTrees
address these two problems by introducing a _mining_ protocol, modeled after how
an arcade makes money.

### The NFT Arcade

The astute reader will have deduced that an NFTree is probably a Merkle tree of
NFTs (and they would be right), and might be wondering how the NFT pricing
mechanism works.  The answer to this is that the NFTree commits to not only the
NFTs, but also a number of _tickets_ that they are worth.

An NFTree smart contract contains an internal "tickets" fungible token that
users must first acquire and then burn in order to instantiate the NFT.  This is
analogous to how an arcade works: in order to win prizes (the NFTs) whose value
is denominated in tickets, the user must pay to play arcade games that produce
tickets.  When they have enough tickets, they bring them to the prize counter
and exchange them for the prize.

With NFTrees, users earn tickets by participating in a proof-of-transfer-lite
([PoX-lite](https://github.com/jcnelson/poxl)) protocol to mine tickets, much like how
the [CityCoins](https://github.com/citycoins) project works.  They commit STX to
the NFTree contract, and in each Stacks block, they win a fixed amount of
tickets with probability proportional to how much STX they committed relative to
everyone else.  Once the user has earned enough tickets this way (or bought some
from someone else), they can mint an NFT off of the NFTree.

The reason for introducing tickets for buying NFTs is that it gives the NFT
creator a way to specify the _relative_ worth of NFTs without specifying an
absolute price.  The ticket mining protocol ensures that the tickets/STX ratio
is known at all Stacks blocks; from there, the price of each NFT in STX can be
calculated.  This partially removes the NFT creator's moral hazard: the NFT
creator does not control the initial valuation of the NFTs; the *users* do.
Moreover, that valuation changes over time based on *user* mining activity.
Additionally, if the NFT creator wants to own any of the NFTs they produce via
the NFTree, they must participate in ticket-mining as well.

To be clear, not all users need to participate in mining tickets.  They can just
buy them from someone who does.  But even then, as will be explained below, the
NFTree smart contract includes a set of marketplace functions that let users
submit buy-offers for NFTs in STX (including ones that have not yet been plucked off of
the NFTree), which can be fulfilled by other users who do have the requisite
tickets.

### Stacking NFTs

The other reason for pricing NFTs in tickets is that it introduces a way for
NFTs to generate _passive income_ for the users that hold them, but do not sell
them.  Like the STX token, an NFT in an NFTree can be stacked -- it can be
locked up so that it does not resolve on-chain and cannot be transferred for a
time, but during this time, the user receives a fraction of the STX spent by
ticket miners proportional to the ticket-denominated value of the NFT.  For
example, if there are 90 tickets-worth of NFTs stacked, and Alice stacks an NFT
worth 10 tickets, then she would receive 10% of the STX paid into the smart
contract for mining tickets over the duration of her NFT lock-up period.

The amount of STX users would receive depends on not only the ticket-denominated
valuation of their NFTs, but also how much STX other users spend mining tickets.
Since producing tickets is the act of mining them, the amount of STX flowing
into the NFTree contract would equal the value of the tickets produced.  Since
the worth of the tickets is underpinned by the worth of the NFTs, the amount of
STX a stacking user can expect to receive is determined by the current
STX-denominated valuation of the NFTs yet to be claimed.  In other words, users
who stack NFTs are incentivized to expand the market for NFTs in the NFTree --
both by growing the userbase and making the NFTs more valuable -- since this is
what earns them the most passive income.

Making it possible to stack NFTs addresses the third problem in the
state-of-the-art: by giving the user a choice between using the NFT, selling it,
or _stacking_ it and earing passive income, the user has more of a reason to
hold onto an NFT even if they can't use it for its intended purpose yet.  If they 
stack it, they don't need to engage in a speculative market for buying/selling
NFTs; they just need to get more people involved in the NFT project.  This is
ultimately achieved by making the NFT useful, which aligns the user's behavior
with the long-term success of the NFT project.

This is not to say that speculation will not happen (it will); this is to say that
NFTrees give users an alternative that offers positive cash-flow over time while
the NFT project is being built out.  This invests users in the long-term success
of the NFT project.

As a nice side-effect of stacking, the market price of an NFT would include its
discounted cash-flow on top of whatever fundamental value the NFT project
offers.  NFTs created from NFTrees would only become worthless if no one mines
tickets for them.

## User Roles

There are five user rules in an NFTree project:

* Creators, who mint whole collections of NFTs as NFTrees.  They receive a
  commission each time an NFT is minted off of an NFTree that they created.

* Ticket miners, who send STX to the NFTree contract to mine NFTree tickets
  which will be burnt to instantiate NFTs.  Ticket miners can claim the NFTs for
themselves, they can sell their tickets to other users, and they can atomically
instantiate and then sell an NFT for STX.

* NFT buyers, who spend STX to acquire NFTs by submitting buy orders to the
  contract.  The contract holds their STX in escrow for the duration of the buy
order's lifetime, which in turn provides a global insight into how much demand
(in STX) there is for NFTs in the contract's NFTrees.

* NFT sellers, who fulfill buy orders by either selling NFTs they own, or
  burning tickets to mint uninstantiated NFTs for buyers.  In the latter case, a
commission fee will be claimed by the NFTree creator for the sold NFT.

* NFT stackers, who lock up their NFTs to accumulate a STX yield from ticket
  mining.  The act of locking the NFT renders it unsellable, and unresolvable
via other smart contracts.

Each type of user has a different set of APIs to use to carry out their roles.

# How it Works

There are two parts to this project: the NFTree smart contract, and the
`nftree.js` command-line tool for making NFTrees.  The smart contract is a
proof-of-concept, and should be tailored to your project needs.

## NFT Descriptors

Because NFTrees commit to both the NFT data and tickets, they are represented in
the contract as a `(buff 64)`, which encodes three pieces of data:

* The NFT SHA512/256 hash
* The NFT's number of tickets
* The NFT data's size

The last field is included as an anti-DDoS measure for Gaia hubs and other data
hosts that hold onto the NFT data, so they'll know how big each NFT is before
trying to store it.

The encoding is as follows:

```
|--hash (32 bytes)--|--size (16 bytes)--|--tickets (16 bytes)--|
```

The `size` and `tickets` fields are big-endian.

## NFTree Construction

Building an NFTree is the act of building an NFT descriptor that commits to
all of the NFTs in the collection.  This is achieved by building a Merkle tree
out of the NFTs, and making the `hash` field of the NFTree's NFT descriptor the
Merkle root.

Building the Merkle tree, the Merkle proofs, and NFT descriptors from a set of
NFT data is handled by the `nftree.js` program.

```
$ cd ./src
$ ./nftree.js build /path/to/your/NFTs /path/to/NFTree/output
```

* The `/path/to/your/NFTs` argument is a directory with all of your NFTs as
  files.  In addition, there must be a `tickets.csv` file that has two columns:
`name` and `tickets`.  The `name` column is the file name, and `tickets` is the
number of tickets the NFT is worth.

* The `/path/to/NFTree/output` argument is a directory into which the NFTs will
  be copied, and into which NFT descriptors and NFT Merkle proofs will be
written.  Each NFT will be named after its SHA512/256 hash; each NFT descriptor
will be named after its hash and a `.desc` suffix; each Merkle proof will be
named after its hash and a `.proof` suffix.

The `/path/to/your/NFTs` argument can contain nested subdirectories.  Each
subdirectory represents NFTrees within the NFTree, and must have its own
`tickets.csv` file for its files.  An NFT descriptor will be created for each
subdirectory NFTree, such that once the NFTree is minted, the inner NFTs can
then be minted.

This program prints out the hex-encoded NFT descriptor for the NFTree as a JSON
string.

For example:

```
$ ./nftree.js build /tmp/nftree-input/ /tmp/nftree-test
"8a480f3f87b03dc1d6b8270cd65fc0e77f7640d49f97d4cd3b789c9de2d280080000000000000000000000000000001f00000000000000000000000000000684"
```

In more detail:

```
$ find /tmp/nftree-input/
/tmp/nftree-input/
/tmp/nftree-input/foo
/tmp/nftree-input/foo/foo
/tmp/nftree-input/foo/tickets.csv
/tmp/nftree-input/foo/bar
/tmp/nftree-input/tickets.csv
/tmp/nftree-input/hello
/tmp/nftree-input/snarf
/tmp/nftree-input/boop
$
$ cat /tmp/nftree-input/tickets.csv
name,tickets
hello,123
boop,456
snarf,789
$
$ cat /tmp/nftree-input/foo/tickets.csv
name,tickets
foo,100
bar,200
$
$ ./nftree.js build /tmp/nftree-input/ /tmp/nftree-test
"8a480f3f87b03dc1d6b8270cd65fc0e77f7640d49f97d4cd3b789c9de2d280080000000000000000000000000000001f00000000000000000000000000000684"
$
$ find /tmp/nftree-test
/tmp/nftree-test/
/tmp/nftree-test/6ff3e7040fc45301764d3b5be9a01814a64f756545d869f4a44d9689e854bf1f.proof
/tmp/nftree-test/23000de7d22826a683fac628c926427ddd986913dba5332d6227085e746b577f.proof
/tmp/nftree-test/04c2f43e067d637611958ae92a54e90848dedeb5908bb3d3363cc57c573332b4
/tmp/nftree-test/23000de7d22826a683fac628c926427ddd986913dba5332d6227085e746b577f.desc
/tmp/nftree-test/d3dbb6686010b4b74742c52dfa8564f2e1501219568452a01b5a69d295d7d8c2.proof
/tmp/nftree-test/8a480f3f87b03dc1d6b8270cd65fc0e77f7640d49f97d4cd3b789c9de2d28008.desc
/tmp/nftree-test/332bc3d727592cdc62f40526cc2476d2627441656352b33da1eb53556c69cd17
/tmp/nftree-test/d3dbb6686010b4b74742c52dfa8564f2e1501219568452a01b5a69d295d7d8c2
/tmp/nftree-test/d3dbb6686010b4b74742c52dfa8564f2e1501219568452a01b5a69d295d7d8c2.desc
/tmp/nftree-test/6ff3e7040fc45301764d3b5be9a01814a64f756545d869f4a44d9689e854bf1f
/tmp/nftree-test/root
/tmp/nftree-test/bf73ee1fb7e8bf8fcdd5da06dd547052cd0f929a88f4aead68b218d3bde91134.proof
/tmp/nftree-test/6ff3e7040fc45301764d3b5be9a01814a64f756545d869f4a44d9689e854bf1f.desc
/tmp/nftree-test/332bc3d727592cdc62f40526cc2476d2627441656352b33da1eb53556c69cd17.proof
/tmp/nftree-test/bf73ee1fb7e8bf8fcdd5da06dd547052cd0f929a88f4aead68b218d3bde91134.desc
/tmp/nftree-test/04c2f43e067d637611958ae92a54e90848dedeb5908bb3d3363cc57c573332b4.proof
/tmp/nftree-test/bf73ee1fb7e8bf8fcdd5da06dd547052cd0f929a88f4aead68b218d3bde91134
/tmp/nftree-test/04c2f43e067d637611958ae92a54e90848dedeb5908bb3d3363cc57c573332b4.desc
/tmp/nftree-test/332bc3d727592cdc62f40526cc2476d2627441656352b33da1eb53556c69cd17.desc
```

The NFTree creator needs to upload the contents of `/path/to/NFTree/output` to a
Web server, so they'll be available for viewing.  The URL to each NFT will be
constructed by the NFTree smart contract by appending the SHA512/256 hash of the
NFT to a knwon URL prefix.  So for example, if the NFTs are going to be
available at `https://nftree-example.com/nft-data`, the NFT creator would put
the contents of this directory onto the Web server such that
`https://nftree-example.com/nft-data/6ff3e7040fc45301764d3b5be9a01814a64f756545d869f4a44d9689e854bf1f`
resolved to the file `6ff3e7040fc45301764d3b5be9a01814a64f756545d869f4a44d9689e854bf1f`.
The NFTree project creator can change the URL prefix by setting the
`NFT_URL_PREFIX` constant in the `nftree.clar` smart contract.

Once the NFT creator has the outputted NFT descriptor, then can call one of the
top-level functions to instantiate it on-chain -- `(register-nftree)` and
`(register-contract-nftree)`:

```
(define-public (register-nftree (nft-rec { tickets: uint, data-hash: (buff 32), size: uint }))
(define-public (register-contract-nftree (nft-rec { tickets: uint, data-hash: (buff 32), size: uint }))
```

The only difference between them is that the contract will own the root NFTree
in the latter case, so no commission will be paid out for the root.
Once the NFT creator has the outputted NFT descriptor, they can call `(instantiate-nftree)` with it:

Both of these functions are used to mint the whole collection in `/path/to/your/NFTs`.  The NFT project leader(s)
would call this to instantiate new collections are they are made.  All NFTs
represented in `/path/to/your/NFTs` can then be instantiated by this smart
contract by interested users; they'd use the corresponding `.proof` and `.desc`
files to construct the relevant contract-calls.

These functions return an integer NFT ID, which must be passed into functions
related to instantiating NFTs off of the NFTree (this argument is called
`parent-nft-id`).

**WARNING**: If you make an NFT project, you will want to update these functions
to limit who can produce NFTrees in your project this way.  Usually, this will
only be a designated admin, but any authentication rules you can write in
Clarity are permitted.  For example, you could have a DAO contract decide who
can call this function.

## Ticket Mining

To mine NFTree tickets, a miner can either commit STX in a single block, or over a
range of blocks.  The functions to do so are:

```
(define-public (mine-tickets (amount-ustx uint))
```

and 

```
(define-public (mine-tickets-multi (amount-ustx-per-block uint) (num-blocks uint))
```

There will be one ticket winner per Stacks block, and the number of tickets
granted to the winner is fixed regardless of how many or few STX are committed.

The winning ticket miner will not be known for 100 blocks, to ensure that the
commitments are sufficiently confirmed.  At or after the 101st subsequent block,
the winner can claim their tickets by calling `(claim-tickets)`:

```
(define-public (claim-tickets (miner principal) (blk uint))
```

Here, `blk` is the Stacks block height at which the winner mined.
Miners can check if they were the winner in a block with `(check-winner)`:

```
(define-read-only (check-winner (miner principal) (blk uint))
```

## Buying an NFT

Users are not required to acquire and burn tickets in order to buy NFTs; it's
only necessary that _someone_ does this.  Instead, users can simply offer STX
for an NFT in the NFTree.  Importantly, the user can offer STX for an NFT that
is not yet instantiated, but is represented by the NFTree.

To submit a buy offer for a particular NFT, the user calls the `(submit-buy-offer)` function:

```
(define-public (submit-buy-offer (nft-desc (buff 64)) (amount-ustx uint) (expires uint))
```

* The `nft-desc` is the NFT descriptor for the NFT to buy

* The `amount-ustx` argument is the number of uSTX this buyer is willing to
  spend to get it.

* The `expires` argument is a future Stacks block height at which the buy offer
  expires.

When the user submits the buy offer, the contract escrows their STX so that the
NFT owner can later sell them the NFT.  If the buy offer expires, then the buyer
can call `(reclaim-buy-offer)` to get their STX back:

```
(define-public (reclaim-buy-offer (nft-desc (buff 64)))
```

They would pass the same `nft-desc` as they did when the called
`(submit-buy-offer)`.

A user can out-bid an existing buy offer by calling `(submit-buy-offer)` with a
higher `amount-ustx` offer for the same `nft-desc`.  If so, then the previous
buyer's STX are returned to them and the new buyer's buy offer replaces it.

### Fulfilling a Buy Offer

The NFT in a buy offer does not need to exist in order to be fulfilled.  If the
NFT already exists, then the NFT owner can sell the NFT by calling
`(fulfill-buy-offer)`:

```
(define-public (fulfill-buy-offer (nft-desc (buff 64)))
```

They would pass the NFT descriptor for their NFT as the sole argument.  If the
call succeeds, then ownership of the NFT transfers to the buyer in the buy offer
for this NFT, and the seller gets the STX escrowed by the smart contract.

If the NFT does not exist, then someone with tickets can fulfill the buy offer
by instantiating the NFT and then selling it to the buyer for the STX in one go via
the `(fulfill-mine-order)` function:

```
(define-public (fulfill-mine-order
                    (nft-desc (buff 64))
                    (parent-nft-id uint)
                    (proof { hashes: (list 32 (buff 32)), index: uint })
               )
```

* The `nft-desc` argument is the NFT descriptor for the NFT to be instantiated
  and sold.  It is stored in a `.desc` file created by `./nftree.js build`.

* The `parent-nft-id` argument is the NFT identifier of the NFTree that contains
  this NFT.

* The `proof` argument is a Merkle proof that links the `nft-desc` to the NFTree
  identified by `parent-nft-id`.  Its JSON representation is created via a
`./nftree.js build` invocation, and is stored in a `.proof` file.

If this function succeeds, the caller's tickets are burnt, the NFT is
instantiated and then transferred to the buyer, and the caller gets the offered STX.
The function returns the NFT ID for the instantiated NFT.

## Stacking an NFT

Once the user owns an NFT, they can stack it via the `(stack-nft)` function:

```
(define-public (stack-nft (nft-id uint) (num-cycs uint))
```

* The `nft-id` argument is the numeric NFT identifier, which is returned once the NFT
  is instantiated.

* The `num-cycs` argument is the number of reward cycles for which the NFT is
  stacked.

NFTs are stacked in fixed-length reward cycles, much like how PoX works in the
Stacks blockchain.  While the NFT is stacked, it cannot be transferred and it
will not resolve via the SIP 009 interface.  However, the owner of the NFT will
receive a portion of the STX spent on mining tickets while it is locked up,
proportional to the number of tickets the NFT is worth.

If an NFT is stacked, and the NFT is really an NFTree, then buy orders that
instantiate NFTs off of it will fail.

To get the stacking rewards after the NFT unlocks, the user would call
`(claim-stacking-rewards)`:

```
(define-public (claim-stacking-rewards (start-cyc uint) (num-cycs uint))
```

* `start-cyc` is the first reward cycle in which the NFT was stacked

* `num-cycs` is the number of cycles in which the NFT was stacked

To determine what the `start-cyc` is, the user can call the function
`(blk-to-cyc)` to convert the Stacks block height at which they stacked their
NFT into the reward cycle in which it resides.  The `start-cyc` value is the
_next_ reward cycle -- as with STX in PoX, the start reward cycle is the next
whole reward cycle at the point in time when the NFT gets stacked.

## Nested NFTrees

Each NFTree can represent up to about 4.1 billion (2^32 - 1) NFTs.  If for some
reason this isn't enough, an NFT in an NFTree can be another NFTree.  In this
case, the NFT hash field of its NFT descriptor is the Merkle root of the NFTs it
represents.

If someone instantiates an NFT that happens to be an NFTree, anyone can then
proceed to instantiate the NFTs it represents by passing the NFTree's
`parent-nft-id` value to whatever function is doing the instantiation.

Nested NFTrees are an advanced feature.  They are meant to enable use-cases such
as, but not limited to, the following:

* Selling a collection of NFTs to a single buyer in one go.

* Blocking the release of subsequent NFTs until enough tickets have been mined.

* Implementing a semi-fungible token.

In all cases, the _owner_ of the NFTree receives the commission fees for minting
NFTs off of it.  What this means is that if Alice registers an NFTree that
contains another NFTree, and Bob buys the inner NFTree, then the commission 
fees instantiated off of Bob's NFTree will go to Bob, not Alice.

## Standards Conformance

The proof-of-concept `nftree.clar` contract implements SIP 009 faithfully, and
implements all but the `(transfer)` function of SIP 010 for tickets.  The reason for this
omission is because both SIPs have a `(transfer)` function.  In place of a
`(transfer)` function for tickets, this contract offers `(transfer-tickets)`
with the same semantics.

The URL of an NFT is calculated by appending the hash of the NFT from its NFT
descriptor to a URL prefix.

# Running tests

You will need `clarity-cli` in your `$PATH`.  You can get it from
https://github.com/blockstack/stacks-blockchain.

```bash
$ cd ./contracts/tests && ./run-tests.sh
```

# Contributing

This repository is meant to be a proof-of-concept.  In keeping with the Stacks
Foundation ethos, this project is meant to set up other Stacks ecosystem
participants to succeed.  I will not be monetizing or productizing it for as
long as I work for the Stacks Foundation.

As such, I encourage you to fork this repo and make your own changes instead of
trying to send PRs.
