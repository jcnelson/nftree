;; NFTree contract

(define-non-fungible-token nftree uint)
(define-data-var next-nft-id uint u0)

(define-fungible-token tickets)

;; error codes
(define-constant CONFIRMATIONS u100)
(define-constant ERR_ALREADY_MINED u1000)
(define-constant ERR_NOT_CONFIRMED u1001)
(define-constant ERR_INVALID_BLOCK u1002)
(define-constant ERR_ALREADY_CLAIMED u1003)
(define-constant ERR_NOT_WINNER u1004)
(define-constant ERR_INSUFFICIENT_BALANCE u1005)
(define-constant ERR_INVALID_PROOF u1006)
(define-constant ERR_ALREADY_MINTED u1007)
(define-constant ERR_NO_SUCH_NFT u1008)
(define-constant ERR_PERMISSION_DENIED u1009)
(define-constant ERR_NFT_LOCKED u1010)
(define-constant ERR_INVALID_CYCLES u1011)
(define-constant ERR_INVALID_BUY_EXPIRE u1012)
(define-constant ERR_BAD_OFFER u1013)
(define-constant ERR_NO_BUY_OFFER u1014)
(define-constant ERR_INVALID_NFT_DESC u1015)
(define-constant ERR_INVALID_NUM_BLOCKS u1016)
(define-constant ERR_INVALID_USTX u1017)

(define-constant MAX_CYCLES u12)
(define-constant MAX_MINE_BLOCKS u50)

;; adjust to your liking, but the unit-tests assume the following:
(define-constant START_BLOCK block-height)
(define-constant CYCLE_LENGTH u5)
(define-constant TICKETS_PER_BLOCK u10)
(define-constant NFT_URL_PREFIX "https://nftrees.com/nfts/")
(define-constant CREATOR_COMMISSION u30)

(define-map miners-at-block
    {
        block: uint,
        miner: principal
    }
    {
        range-start: uint,
        range-end: uint,
        claimed: bool
    }
)

(define-map mined-in-block
    uint    ;; block
    uint    ;; amount
)

(define-map claimed-nfts
    { tickets: uint, data-hash: (buff 32), size: uint }
    uint    ;; nft ID
)

(define-map nft-recs
    uint    ;; nft ID
    { tickets: uint, data-hash: (buff 32), size: uint }
)

(define-map nft-lockups
    uint    ;; nft ID
    uint    ;; unlock height
)

(define-map owner-ticket-lockups
    { owner: principal, cycle: uint }
    { tickets: uint, reward-claimed: bool}
)

(define-map cycle-lockups
    uint    ;; cycle
    {
        ;; total NFT value locked in this cycle, measured by tickets
        tickets: uint,
        ;; total uSTX committed for tickets this cycle
        ustx: uint
    }
)

(define-map nft-buy-offers
    (buff 64)       ;; serialized nft record
    { buyer: principal, amount-ustx: uint, expires: uint }
)


(define-private (inner-register-nftree (nft-rec { tickets: uint, data-hash: (buff 32), size: uint }) (owner principal))
    (let (
        (nft-id (var-get next-nft-id))
    )
        (unwrap-panic (nft-mint? nftree nft-id owner))
        (map-set nft-recs nft-id nft-rec)
        (var-set next-nft-id (+ u1 nft-id))
        (ok nft-id)
    )
)

;; Register an NFTree from which NFTs can be minted.
;; This creates a "root" NFTree from which NFTs can be claimed.
;; The NFTree will be owned by tx-sender.
;; Your implemetation may want to constrain who can call this, and when.
(define-public (register-nftree (nft-rec { tickets: uint, data-hash: (buff 32), size: uint }))
    ;; TODO: you will need to fill in authentication logic here
    (inner-register-nftree nft-rec tx-sender)
)

;; Register an NFTree that will be owned by the contract (so, no commission).
;; This creates a "root" NFTree from which NFTs can be claimed.
;; Your implementation may want to constrain who can call this, and when.
(define-public (register-contract-nftree (nft-rec { tickets: uint, data-hash: (buff 32), size: uint }))
    ;; TODO: you will need to fill in authentication logic here
    (inner-register-nftree nft-rec (as-contract tx-sender))
)

(define-private (add-amount-for-block (blk uint) (amount-ustx uint))
    (match (map-get? mined-in-block blk)
        amount
            (let (
                (new-amount (+ amount amount-ustx))
            )
                (map-set mined-in-block blk (+ amount amount-ustx))
                new-amount
            )
        (begin
            (map-set mined-in-block blk amount-ustx)
            amount-ustx
        )
    )
)

(define-private (add-miner-for-block (miner principal) (blk uint) (amount-ustx uint))
    (match (map-get? miners-at-block { block: blk, miner: miner })
        miner-rec
            (err ERR_ALREADY_MINED)
        (let (
            (range-end (add-amount-for-block blk amount-ustx))
            (range-start (- range-end amount-ustx))
            (cur-lockup (default-to { tickets: u0, ustx: u0 } (map-get? cycle-lockups (blk-to-cyc blk))))
            (new-cycle-lockup (merge cur-lockup { ustx: (+ (get ustx cur-lockup) amount-ustx) }))
        )
            (map-set miners-at-block
                { block: blk, miner: miner }
                { range-start: range-start, range-end: range-end, claimed: false }
            )
            (map-set cycle-lockups
                (blk-to-cyc blk)
                new-cycle-lockup
            )
            (ok {
                miners-at-block: { range-start: range-start, range-end: range-end, claimed: false },
                cur-lockup: new-cycle-lockup
            })
        )
    )
)

;; Mine tickets in this block.  Tx-sender is the miner.
;; This works like PoX-lite token mining -- miners commit uSTX at this block in
;; a bid to mine tickets, which can then be redeemed for NFTs.
(define-public (mine-tickets (amount-ustx uint))
    (begin
        (asserts! (> (stx-get-balance tx-sender) amount-ustx)
            (err ERR_INSUFFICIENT_BALANCE)
        )
        (asserts! (> amount-ustx u0)
            (err ERR_INVALID_USTX)
        )
        
        (match (add-miner-for-block tx-sender block-height amount-ustx)
            success
                (begin
                    (unwrap-panic (stx-transfer? amount-ustx tx-sender (as-contract tx-sender)))
                    (ok true)
                )
            error
                (err error)
        )
    )
)

(define-private (inner-mine-tickets-multi (idx uint) (state { miner: principal, start-blk: uint, num-blocks: uint, amount-ustx: uint, res: (response bool uint) }))
    (if (and (is-ok (get res state)) (<= idx (get num-blocks state)))
        (match (add-miner-for-block (get miner state) (+ idx (get start-blk state)) (get amount-ustx state))
            success
                state
            error
                (merge state { res: (err error) })
        )
        state
    )
)

;; Mine tickets across a range of blocks. 
;; This works like PoX-lite token mining -- miners commit uSTX at this block in
;; a bid to mine tickets, which can then be redeemed for NFTs.
;; Unlike mine-tickets, this commits the given amount-ustx in the next N blocks.
(define-private (inner-add-miner-to-block-multi (amount-ustx uint) (num-blocks uint) (miner principal) (start-blk uint))
    (begin
        (asserts! (> (stx-get-balance miner) (* num-blocks amount-ustx))
            (err ERR_INSUFFICIENT_BALANCE)
        )
        
        (asserts! (and (> num-blocks u0) (<= num-blocks MAX_MINE_BLOCKS))
            (err ERR_INVALID_NUM_BLOCKS)
        )

        (asserts! (> amount-ustx u0)
            (err ERR_INVALID_USTX)
        )
 
        (unwrap-panic
            (get res
                (fold inner-mine-tickets-multi
                  (list  u1  u2  u3  u4  u5  u6  u7  u8  u9 u10
                        u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
                        u21 u22 u23 u24 u25 u26 u27 u28 u29 u30
                        u31 u32 u33 u34 u35 u36 u37 u38 u39 u40
                        u41 u42 u43 u44 u45 u46 u47 u48 u49 u50)
                  { miner: miner, start-blk: start-blk, num-blocks: num-blocks, amount-ustx: amount-ustx, res: (ok true) }
                )
            )
        )

        (ok true)
    )
)

(define-public (mine-tickets-multi (amount-ustx-per-block uint) (num-blocks uint))
    (begin
        (try! (inner-add-miner-to-block-multi amount-ustx-per-block num-blocks tx-sender block-height))
        (unwrap-panic (stx-transfer? (* num-blocks amount-ustx-per-block) tx-sender (as-contract tx-sender)))
        (ok true)
    )
)

(define-read-only (check-winner (miner principal) (blk uint))
    (match (get-random-uint-at-block blk)
        ;; have a random number at this block
        rand-uint
            (match (map-get? mined-in-block blk)
                ;; someone mined!
                amount-in-block
                    (match (map-get? miners-at-block { miner: miner, block: blk })
                        ;; this miner mined in this block!  did it win?
                        miner-rec
                            (ok (and (>= (mod rand-uint amount-in-block) (get range-start miner-rec))
                                     (< (mod rand-uint amount-in-block) (get range-end miner-rec))))
                        ;; this miner did not even mine in this block
                        (ok false)
                    )
                ;; no one mined in this block
                (ok false)
            )
        ;; invalid block height -- no VRF data
        (err ERR_INVALID_BLOCK)
    )
)

(define-private (inner-claim-tickets-at-block (miner principal) (blk uint))
    (match (check-winner miner blk)
        winner
            ;; we know whether or not this miner won
            (if winner
                ;; this miner won
                (let (
                    (miner-rec (unwrap-panic (map-get? miners-at-block { miner: miner, block: blk })))
                )
                    (if (not (get claimed miner-rec))
                        ;; claim tokens
                        (let (
                            (new-miner-rec (merge miner-rec { claimed: true }))
                        )
                            (unwrap-panic (ft-mint? tickets TICKETS_PER_BLOCK miner))
                            (map-set miners-at-block { miner: miner, block: blk } new-miner-rec)
                            (ok new-miner-rec)
                        )
                        ;; already claimed
                        (err ERR_ALREADY_CLAIMED)
                    )
                )
                ;; this miner did not win
                (err ERR_NOT_WINNER)
            )
        error
            ;; could not determine winner, so propagate error
            (err error)
    )
)

(define-public (claim-tickets (miner principal) (blk uint))
    (if (>= (+ blk CONFIRMATIONS) block-height)
       ;; not enough confirmations
       (err ERR_NOT_CONFIRMED)
       ;; enough confirmations
       (inner-claim-tickets-at-block miner blk)
    )
)

(define-private (read-uint-closure (idx uint) (state { acc: uint, offset: uint, data: (buff 64) }))
    (let (
        (acc (get acc state))
        (data (get data state))
        (offset (get offset state))
        (byte (buff-to-u8 (unwrap-panic (element-at data (+ idx offset)))))
    )
        ;; acc = byte * (2**(8 * (15 - idx))) + acc
        (merge state { acc: (+ (* byte (pow u2 (* u8 (- u15 idx)))) acc) })
    )
)

(define-private (read-uint (data (buff 64)) (offset uint))
    (get acc
        (fold read-uint-closure (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15) { acc: u0, data: data, offset: offset })
    )
)

(define-private (read-buff32-closure (idx uint) (state { offset: uint, data: (buff 64), acc: (buff 32) }))
    (let (
        (byte-data (unwrap-panic (element-at (get data state) (+ idx (get offset state)))))
    )
        (merge state { acc: (unwrap-panic (as-max-len? (concat (get acc state) byte-data) u32)) })
    )
)

(define-private (read-buff32 (data (buff 64)) (offset uint))
    (get acc
        (fold read-buff32-closure
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31)
            { offset: offset, data: data, acc: 0x }
        )
    )
)

(define-private (merkle-proof-root-closure (idx uint) (state { acc: (buff 32), acc-index: uint, hashes: (list 32 (buff 32)) }))
    (if (< idx (len (get hashes state)))
        (let (
            (acc-index (get acc-index state))
            (ith-hash (unwrap-panic (element-at (get hashes state) idx)))
            (ith-bit (mod acc-index u2))
            (next-acc
                (if (is-eq ith-bit u0)
                    (sha512/256 (concat (get acc state) ith-hash))
                    (sha512/256 (concat ith-hash (get acc state))))
            )
            (next-acc-index (/ acc-index u2))
        )
            (merge state { acc: next-acc, acc-index: next-acc-index })
        )
        state
    )
)           

;; compute the merkle root of a merkle proof
(define-private (merkle-proof-root (nft-desc (buff 64)) (proof { hashes: (list 32 (buff 32)), index: uint }))
    (get acc
        (fold merkle-proof-root-closure
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31)
            { acc: (sha512/256 nft-desc), acc-index: (get index proof), hashes: (get hashes proof) }
        )
    )
)

(define-private (inner-instantiate-nft (nft-data { tickets: uint, data-hash: (buff 32), size: uint }) (owner principal))
    (let (
        (nft-id (var-get next-nft-id))
        (ticks (get tickets nft-data))
    )
        (if (> ticks u0)
            (begin
                (unwrap-panic (ft-burn? tickets ticks owner))
                true
            )
            true
        )
        (unwrap-panic (nft-mint? nftree nft-id owner))
        (map-set nft-recs nft-id nft-data)
        (map-set claimed-nfts nft-data nft-id)
        (var-set next-nft-id (+ u1 nft-id))
        (ok nft-id)
    )
)

(define-private (parse-nft-desc (nft-desc (buff 64)))
    (if (< (len nft-desc) u64)
        none
        (let (
            ;; nft-desc: |--hash (32 bytes)--|--size (16 bytes)--|--tickets (16 bytes)--|
            (data-hash (read-buff32 nft-desc u0))
            (size (read-uint nft-desc u32))
            (num-tickets (read-uint nft-desc u48))
            (nft-rec { tickets: num-tickets, size: size, data-hash: data-hash })
        )
            (some nft-rec)
        )
    )
)

(define-private (inner-can-claim-nft? (nft-desc (buff 64)) (owner principal) (parent-nft-id uint) (proof { hashes: (list 32 (buff 32)), index: uint }) (cur-blk uint))
    (let (
        (nft-rec (unwrap! (parse-nft-desc nft-desc) (err ERR_INVALID_NFT_DESC))) 

        ;; parent nft must exist
        (parent-nft (unwrap! (map-get? nft-recs parent-nft-id) (err ERR_NO_SUCH_NFT)))
    )
        ;; must own enough tickets
        (asserts! (>= (ft-get-balance tickets owner) (get tickets nft-rec))
            (err ERR_INSUFFICIENT_BALANCE))

        ;; nft must not have been claimed yet
        (asserts! (is-none (map-get? claimed-nfts nft-rec))
            (err ERR_ALREADY_MINTED))
        
        ;; parent nft must not be locked for stacking
        (asserts! (not (nft-stacked? parent-nft-id cur-blk))
            (err ERR_NFT_LOCKED))

        ;; given nft must be a descendant of the parent nft
        (asserts! (is-eq (get data-hash parent-nft) (merkle-proof-root nft-desc proof))
            (err ERR_INVALID_PROOF))

        (ok nft-rec)
    )
)

(define-public (can-claim-nft? (nft-desc (buff 64)) (owner principal) (parent-nft-id uint) (proof { hashes: (list 32 (buff 32)), index: uint }))
    (inner-can-claim-nft? nft-desc owner parent-nft-id proof block-height)
)

;; stacking

(define-read-only (blk-to-cyc (blk uint))
    (/ (- blk START_BLOCK) CYCLE_LENGTH)
)

(define-read-only (cyc-to-blk (cyc uint))
    (+ START_BLOCK (* cyc CYCLE_LENGTH))
)

(define-read-only (nft-stacked? (nft-id uint) (cur-blk uint))
    (match (map-get? nft-lockups nft-id)
        unlock-ht
            (> unlock-ht cur-blk)
        false
    )
)

;; can we stack an NFT?
(define-private (inner-can-stack-nft? (nft-id uint) (owner principal) (num-cycs uint) (cur-blk uint))
    (begin
       ;; nft must exist and be owned by this owner
       (asserts! (is-eq (nft-get-owner? nftree nft-id) (some owner))
           (err ERR_PERMISSION_DENIED))

       ;; nft can't be stacked already
       (asserts! (not (nft-stacked? nft-id cur-blk))
           (err ERR_NFT_LOCKED))

       ;; lockup cycles must be bound
       (asserts! (and (> num-cycs u0) (<= num-cycs MAX_CYCLES))
           (err ERR_INVALID_CYCLES))

       (ok true)
    )
)

(define-read-only (can-stack-nft? (nft-id uint) (owner principal) (num-cycs uint) (cur-blk uint))
    (inner-can-stack-nft? nft-id owner num-cycs block-height)
)

;; insert the owner's locked NFT into reward cycles, valuated by tickets
(define-private (put-owner-lockup-at-cycle (idx uint) (state { owner: principal, start-cyc: uint, num-cycs: uint, nft-tickets: uint }))
    (let (
        (this-cycle (+ idx (get start-cyc state)))
    )
        (if (< idx (get num-cycs state))
            ;; will stack in this cycle
            (let (
                (owner (get owner state))
                (nft-tickets (get nft-tickets state))
                (cur-lockup-tickets
                    (default-to { tickets: u0, reward-claimed: false }
                    (map-get? owner-ticket-lockups { owner: owner, cycle: this-cycle }))
                )
                (cur-lockup-cycle
                    (default-to { tickets: u0, ustx: u0 }
                    (map-get? cycle-lockups this-cycle))
                )
                (owner-locked-tickets (get tickets cur-lockup-tickets))
                (total-locked-tickets (get tickets cur-lockup-cycle))
            )
                (map-set owner-ticket-lockups
                    { owner: owner, cycle: this-cycle }
                    { tickets: (+ owner-locked-tickets nft-tickets), reward-claimed: (get reward-claimed cur-lockup-tickets) }
                )
                (map-set cycle-lockups
                    this-cycle
                    (merge cur-lockup-cycle { tickets: (+ total-locked-tickets nft-tickets) })
                )
                state
            )
            ;; will not stack in this cycle
            state
        )
    )
)

(define-private (inner-stack-nft-in-cycles (nft-id uint) (owner principal) (num-cycs uint) (cur-blk uint))
    (let (
        (start-cyc (+ u1 (blk-to-cyc cur-blk)))
        (unlock-ht (cyc-to-blk (+ start-cyc num-cycs)))
        (nft-rec (unwrap-panic (map-get? nft-recs nft-id)))
    )
        (map-set nft-lockups nft-id unlock-ht)
        (fold put-owner-lockup-at-cycle
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11)
            { owner: owner, start-cyc: start-cyc, num-cycs: num-cycs, nft-tickets: (get tickets nft-rec) }
        )
    )
)

(define-private (inner-stack-nft (nft-id uint) (owner principal) (num-cycs uint) (cur-blk uint))
    (begin
        (try! (inner-can-stack-nft? nft-id owner num-cycs cur-blk))
        (inner-stack-nft-in-cycles nft-id owner num-cycs cur-blk)
        (ok true)
    )
)

;; stack an NFT -- render it untransferable and unresolvable, but give the owner the right to claim a fraction of uSTX sent to this contract over its lockup period.
(define-public (stack-nft (nft-id uint) (num-cycs uint))
    (inner-stack-nft nft-id tx-sender num-cycs block-height)
)

(define-private (inner-stacking-totals (idx uint) (state { start-cyc: uint, end-cyc: uint, owner: principal, ustx-due: uint }))
    (let (
        (start-cyc (get start-cyc state))
        (cur-cyc (+ idx start-cyc))
        (total-locked (default-to { tickets: u0, ustx: u0 } (map-get? cycle-lockups cur-cyc)))
        (owner-locked (default-to { tickets: u0, reward-claimed: false } (map-get? owner-ticket-lockups { owner: (get owner state), cycle: cur-cyc })))

        (total-tickets (get tickets total-locked))
        (owner-tickets 
            (if (not (get reward-claimed owner-locked))
                (get tickets owner-locked)
                u0
            )
        )
    )
    (if (< cur-cyc (get end-cyc state))
        ;; in query range
        (merge state {
            ustx-due: (+ (get ustx-due state) 
                (if (> total-tickets u0)
                    (/ (* owner-tickets (get ustx total-locked)) total-tickets)
                    u0
                )
            )
        })
        ;; out of query range
        state
    ))
)

(define-read-only (get-stacking-totals (owner principal) (start-cyc uint) (num-cycs uint))
    (fold inner-stacking-totals
        (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11)
        { start-cyc: start-cyc, end-cyc: (+ start-cyc num-cycs), owner: owner, ustx-due: u0 }
    )
)

(define-private (inner-set-rewards-claimed-for-cycle (idx uint) (state { start-cyc: uint, owner: principal, end-cyc: uint }))
    (let (
        (cyc (+ (get start-cyc state) idx))
        (owner (get owner state))
        (owner-locked
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner, cycle: cyc })))
    )
        (if (and (< cyc (get end-cyc state)) (not (get reward-claimed owner-locked)))
            (map-set owner-ticket-lockups { owner: owner, cycle: cyc } (merge owner-locked { reward-claimed: true }))
            true
        )
        state
    )
)

(define-private (inner-claim-stacking-rewards (owner principal) (start-cyc uint) (num-cycs uint) (ustx-due uint))
    (begin
        ;; mark claimed
        (fold inner-set-rewards-claimed-for-cycle
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11)
            { owner: owner, start-cyc: start-cyc, end-cyc: (+ start-cyc num-cycs) }
        )

        ;; pay out
        (if (> ustx-due u0)
            (begin
                (unwrap-panic
                    (as-contract (stx-transfer? ustx-due tx-sender owner))
                )
                (ok true)
            )
            (ok true)
        )
    )
)

(define-read-only (inner-can-claim-stacking-rewards (owner principal) (start-cyc uint) (num-cycs uint) (cur-blk uint))
    (let (
        (cur-cyc (blk-to-cyc cur-blk))
        (ustx-due (get ustx-due (get-stacking-totals owner start-cyc num-cycs)))
    )
        ;; can claim at most 12 cycles
        (asserts! (<= num-cycs MAX_CYCLES)
            (err ERR_INVALID_CYCLES)
        )

        ;; start-cyc + num-cycs must be in the past
        (asserts! (< (+ start-cyc num-cycs) cur-cyc)
            (err ERR_NOT_CONFIRMED)
        )

        ;; contract should hold enough STX to pay out.
        (asserts! (>= (stx-get-balance (as-contract tx-sender)) ustx-due)
            (err ERR_INSUFFICIENT_BALANCE)
        )

        (ok ustx-due)
    )
)

(define-read-only (can-claim-stacking-rewards (owner principal) (start-cyc uint) (num-cycs uint))
    (inner-can-claim-stacking-rewards owner start-cyc num-cycs block-height)
)

;; Claim stacking rewards.
;; For each reward cycle from start-cyc to (+ start-cyc num-cycs), claim the fraction
;; of uSTX transferred to this contract that the tx-sender is due.  The fraction for a
;; cycle is calculated as the number of tickets the tx-sender's stacked NFTs are worth,
;; divided by the total number of tickets all stacked NFTs are worth in the cycle.
;;
;; A cycle's tokens can only be claimed once (obviously), and only if the entire range
;; of cycles from start-cyc to (+ start-cyc num-cycs) is in the past.
(define-public (claim-stacking-rewards (start-cyc uint) (num-cycs uint))
    (inner-claim-stacking-rewards tx-sender start-cyc num-cycs
        (try! (inner-can-claim-stacking-rewards tx-sender start-cyc num-cycs block-height))
    )
)

;; An NFT buy offer if the buyer has enough STX, and if the given offer expiry is in the
;; future relative to the given Stacks block height (`blk`).  The NFT may not exist yet;
;; the buy offer may be for an NFT that has yet to be minted (in which case, a miner can mint
;; and then sell the NFT with their tickets).  Or, the NFT might exist, in which case, the owner
;; can simply sell the NFT directly.
(define-private (inner-can-submit-buy-offer (nft-desc (buff 64)) (buyer principal) (amount-ustx uint) (expires uint) (blk uint))
    (let (
        (prev-offer-data
            (match (map-get? nft-buy-offers nft-desc)
                ;; case 1: there's an existing offer that expires.  The offer is better if it expired, or if the uSTX offer is higher.
                existing-offer
                    (if (or
                            ;; expired?
                            (<= (get expires existing-offer) blk)
                            ;; not expired, but buyer offers more?
                            (> amount-ustx (get amount-ustx existing-offer))
                        )
                        ;; can replace
                        { is-better: true, prev-offer: (some existing-offer) }
                        ;; cannot replace
                        { is-better: false, prev-offer: (some existing-offer) }
                    )
                ;; case 2: there's no existing offer, in which case this is automatically the best offer.
                { is-better: true, prev-offer: none }
            )
        )
    )
        ;; must be a better offer
        (asserts! (get is-better prev-offer-data)
            (err ERR_BAD_OFFER)
        )

        ;; can't back-date offer
        (asserts! (< blk expires)
            (err ERR_INVALID_BUY_EXPIRE)
        )

        ;; buyer must have STX
        (asserts! (>= (stx-get-balance buyer) amount-ustx)
            (err ERR_INSUFFICIENT_BALANCE)
        )

        (ok prev-offer-data)
    )
)

;; Submit an offer to buy a given NFT from a given buyer principal for a given number of uSTX, 
;; with a Stacks block height expiration.  Does not check for validity.
;; 
;; Takes possession of buyer's uSTX. The caller of this function must verify that the buyer is tx-sender.
;; If the offer supercedes a prior offer, then the prior offer is refunded to the previous buyer.
;; 
;; The buyer can get back the escrowed uSTX once the offer expires.
;;
;; buyer must be tx-sender, or this function panics.
(define-private (inner-submit-buy-offer
                    (nft-desc (buff 64))
                    (buyer principal)
                    (amount-ustx uint)
                    (expires uint)
                    (prev-offer-opt (optional { buyer: principal, amount-ustx: uint, expires: uint }))
                    (blk uint)
                )
    (begin
        ;; take possession of the uSTX into escrow.
        ;; NOTE: this will abort if buyer is not tx-sender.
        (unwrap-panic (stx-transfer? amount-ustx buyer (as-contract tx-sender)))
      
        ;; refund the previous offer if it exists
        (match prev-offer-opt
            prev-offer
                (unwrap-panic (as-contract (stx-transfer? (get amount-ustx prev-offer) tx-sender (get buyer prev-offer))))
            true
        )

        ;; store the new buy offer
        (map-set nft-buy-offers nft-desc { buyer: buyer, amount-ustx: amount-ustx, expires: expires })

        (ok true)
    )
)

;; Submit a buy offer for a given NFT record (which may not yet be materialized as an NFT) for a given amount of uSTX.
;; The offer stands up to the given block height `expires`.
(define-public (submit-buy-offer (nft-desc (buff 64)) (amount-ustx uint) (expires uint))
    (let (
        (prev-offer-data (try! (inner-can-submit-buy-offer nft-desc tx-sender amount-ustx expires block-height)))
    )
        (inner-submit-buy-offer
            nft-desc
            tx-sender
            amount-ustx
            expires
            (get prev-offer prev-offer-data)
            block-height
        )
    )
)

;; Check that the given buyer can reclaim the uSTX for a buy offer.
;; The offer must exist and be expired, and the buyer must match the buyer record
;; for the given NFT descriptor.
(define-private (inner-can-reclaim-buy-offer (nft-desc (buff 64)) (buyer principal) (blk uint))
    (let (
        ;; buy offer must exist -- i.e. it's unfulfilled 
        (buy-offer (unwrap! (map-get? nft-buy-offers nft-desc) (err ERR_NO_SUCH_NFT)))
    )
        ;; only the buyer can call this
        (asserts! (is-eq buyer (get buyer buy-offer))
            (err ERR_PERMISSION_DENIED)
        )

        ;; buy offer must have expired -- blk must be at or after the expiry height
        (asserts! (<= (get expires buy-offer) blk)
            (err ERR_INVALID_BUY_EXPIRE)
        )

        (ok buy-offer)
    )
)

;; Reclaim a buy offer
(define-private (inner-reclaim-buy-offer (nft-desc (buff 64)) (buy-offer { buyer: principal, amount-ustx: uint, expires: uint }))
    (begin
        ;; reclaim uSTX
        (unwrap-panic (as-contract (stx-transfer? (get amount-ustx buy-offer) tx-sender (get buyer buy-offer))))

        ;; delete buy offer, which must exist
        (unwrap-panic
            (if (map-delete nft-buy-offers nft-desc)
                (ok true)
                (err ERR_NO_BUY_OFFER)
            )
        )

        (ok true)
    )
)

;; Reclaim a buy offer that has expired and not been fulfilled.
;; The buyer would call this for a given NFT they previously submitted a buy offer to.
;; The only time a buyer needs to do this is if the offer expired.  If they were out-bid,
;; then their offer is automatically refunded.
(define-public (reclaim-buy-offer (nft-desc (buff 64)))
    (inner-reclaim-buy-offer
        nft-desc
        (try! (inner-can-reclaim-buy-offer nft-desc tx-sender block-height))
    )
)

;; Can an existing NFT be sold by an owner?
;; * The NFT must exist
;; * The NFT must be owned by the given seller
;; * The buy offer for this NFT must exist and must not be expired
;; Returns the buy offer and NFT ID.
(define-private (inner-can-fulfill-buy-offer? (nft-desc (buff 64)) (seller principal) (blk uint))
    (let (
        (buy-offer-opt (map-get? nft-buy-offers nft-desc))
        (nft-rec (unwrap! (parse-nft-desc nft-desc) (err ERR_INVALID_NFT_DESC)))
        (nft-id (unwrap! (map-get? claimed-nfts nft-rec) (err ERR_NO_SUCH_NFT)))
    )
        ;; buy-offer must exist and must not be expired
        (asserts!
            (match buy-offer-opt
                buy-offer
                    (< blk (get expires buy-offer))
                false
            )
            (err ERR_NO_BUY_OFFER)
        )

        ;; nft must exist, must be owned by this owner, and must not be stacked
        (asserts!
            (and
                (is-eq (some seller) (nft-get-owner? nftree nft-id))
                (not (nft-stacked? nft-id blk))
            )
            (err ERR_PERMISSION_DENIED)
        )

        (ok { buy-offer: (unwrap-panic buy-offer-opt), nft-id: nft-id })
    )
)

;; Fulfill a buy offer.
;; * send the NFT to the buyer in buy-offer
;; * send uSTX from escrow to the seller
;; No validity checking is done; do this in (inner-can-fulfill-buy-offer?)
(define-private (inner-fulfill-buy-offer (nft-id uint) (nft-desc (buff 64)) (buy-offer { buyer: principal, amount-ustx: uint, expires: uint }) (seller principal))
    (begin
        ;; transfer NFT to new owner
        (unwrap-panic (nft-transfer? nftree nft-id seller (get buyer buy-offer)))

        ;; transfer uSTX to seller
        (unwrap-panic (as-contract (stx-transfer? (get amount-ustx buy-offer) tx-sender seller)))

        ;; delete buy offer, which must exist
        (unwrap-panic
            (if (map-delete nft-buy-offers nft-desc)
                (ok true)
                (err ERR_NO_BUY_OFFER)
            )
        )

        (ok true)
    )
)

;; Fulfill a buy offer for a given NFT (tx-sender is the seller).
;; The NFT must already exist and be owned by the seller.
(define-public (fulfill-buy-offer (nft-desc (buff 64)))
    (let (
        (order-data (try! (inner-can-fulfill-buy-offer? nft-desc tx-sender block-height)))
    )
        (inner-fulfill-buy-offer (get nft-id order-data) nft-desc (get buy-offer order-data) tx-sender)
    )
)

;; Determine if a miner can fulfill a buy offer.
;; Return the parsed NFT record and buy offer the miner fulfills.
(define-private (inner-can-fulfill-mine-order
                    (nft-desc (buff 64))
                    (miner principal)
                    (parent-nft-id uint)
                    (proof { hashes: (list 32 (buff 32)), index: uint })
                    (cur-blk uint)
                )
    (let (
        (buy-offer (unwrap! (map-get? nft-buy-offers nft-desc) (err ERR_NO_BUY_OFFER)))
        (nft-rec (try! (inner-can-claim-nft? nft-desc miner parent-nft-id proof cur-blk)))
    )
        (ok { buy-offer: buy-offer, nft-rec: nft-rec })
    )
)

;; Fulfill a buy order by materializing the NFT it describes and giving it to the buyer.
;; The owner of the parent NFTree from which this NFT will be instantiated receives a commission.
(define-private (inner-fulfill-mine-order
                    (nft-desc (buff 64))
                    (nft-rec { tickets: uint, data-hash: (buff 32), size: uint })
                    (parent-nft-id uint)
                    (buy-offer { buyer: principal, amount-ustx: uint, expires: uint })
                    (miner principal)
                )
    (let (
        ;; instantiate the NFT and give it to the miner
        (nft-id (unwrap-panic (inner-instantiate-nft nft-rec miner)))
        (amount-ustx (get amount-ustx buy-offer))
        (parent-owner (unwrap-panic (nft-get-owner? nftree parent-nft-id)))
    )
        ;; miner claims the buyer's escrowed STX, minus the commission: (amount-ustx * (100 - CREATOR_COMMISSION)) / 100
        (unwrap-panic (as-contract (stx-transfer? (/ (* (- u100 CREATOR_COMMISSION) amount-ustx) u100)  tx-sender miner)))

        ;; parent NFT owner claims commission: (amount-ustx * CREATOR_COMMISSION) / 100
        (unwrap-panic (as-contract (stx-transfer? (/ (* CREATOR_COMMISSION amount-ustx) u100) tx-sender parent-owner)))

        ;; send the newly-created NFT to the buyer
        (unwrap-panic (nft-transfer? nftree nft-id miner (get buyer buy-offer)))

        ;; delete buy offer, which must exist
        (unwrap-panic
            (if (map-delete nft-buy-offers nft-desc)
                (ok true)
                (err ERR_NO_BUY_OFFER)
            )
        )
        (ok nft-id)
    )
)

;; Fulfill a buy offer for an NFT that does not exist yet.
;; Here, tx-sender must be a miner (i.e. someone with enough tickets).
;; Any miner (i.e. anyone with enough tickets) can do this.
(define-public (fulfill-mine-order
                    (nft-desc (buff 64))
                    (parent-nft-id uint)
                    (proof { hashes: (list 32 (buff 32)), index: uint })
               )
    (let (
        (order-data (try! (inner-can-fulfill-mine-order nft-desc tx-sender parent-nft-id proof block-height)))
    )
        (inner-fulfill-mine-order nft-desc (get nft-rec order-data) parent-nft-id (get buy-offer order-data) tx-sender)
    )
)
 
;; SIP 009
(define-read-only (get-last-token-id)
    (let (
        (next (var-get next-nft-id))
    )
        (if (> next u0)
            (ok (- next u1))
            (ok u0)
        )
    )
)

;; SIP 009
(define-read-only (get-token-url (nft-id uint))
    (if (nft-stacked? nft-id block-height)
        (ok none)
        (match (map-get? nft-recs nft-id)
            nft-rec
                (ok (some (concat NFT_URL_PREFIX (buff32-to-string-ascii (get data-hash nft-rec)))))
            (ok none)
        )
    )
)

;; SIP 009 
(define-read-only (get-owner (nft-id uint))
    (if (nft-stacked? nft-id block-height)
        (ok none)
        (ok (nft-get-owner? nftree nft-id))
    )
)

;; SIP 009 
(define-public (transfer (nft-id uint) (sender principal) (receiver principal))
    (if (nft-stacked? nft-id block-height)
        ;; if it's stacked, it doesn't exist
        (err u3)
        ;; not stacked, so can transfer
        (if (is-eq sender tx-sender)
            (nft-transfer? nftree nft-id sender receiver)
            (err ERR_PERMISSION_DENIED)
        )
    )
)

;; so, we can't implement both SIP 009 and SIP 010 in the same contract, but try to anyway,
;; so miners can sell their tickets.
(define-public (transfer-tickets (amount uint) (sender principal) (receiver principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-eq sender tx-sender)
            (err ERR_PERMISSION_DENIED)
        )
        (ft-transfer? tickets amount sender receiver)
    )
)

;; SIP 010
(define-public (get-name)
    (ok "nftree-tickets")
)

;; SIP 010
(define-public (get-symbol)
    (ok "nftx")
)

;; SIP 010
(define-public (get-decimals)
    (ok u0)
)

;; SIP 010
(define-public (get-total-supply)
    (ok (ft-get-supply tickets))
)

;; SIP 010
(define-public (get-token-uri)
    (ok none)
)

;; VRF
(define-read-only (get-random-uint-at-block (blk uint))
    (match (get-block-info? vrf-seed blk)
        vrf-seed
            (some (buff-to-uint-le (lower-16-le vrf-seed)))
        none
    )
)

(define-constant BUFF_TO_BYTE (list 
    0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f
    0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17 0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f
    0x20 0x21 0x22 0x23 0x24 0x25 0x26 0x27 0x28 0x29 0x2a 0x2b 0x2c 0x2d 0x2e 0x2f
    0x30 0x31 0x32 0x33 0x34 0x35 0x36 0x37 0x38 0x39 0x3a 0x3b 0x3c 0x3d 0x3e 0x3f
    0x40 0x41 0x42 0x43 0x44 0x45 0x46 0x47 0x48 0x49 0x4a 0x4b 0x4c 0x4d 0x4e 0x4f
    0x50 0x51 0x52 0x53 0x54 0x55 0x56 0x57 0x58 0x59 0x5a 0x5b 0x5c 0x5d 0x5e 0x5f
    0x60 0x61 0x62 0x63 0x64 0x65 0x66 0x67 0x68 0x69 0x6a 0x6b 0x6c 0x6d 0x6e 0x6f
    0x70 0x71 0x72 0x73 0x74 0x75 0x76 0x77 0x78 0x79 0x7a 0x7b 0x7c 0x7d 0x7e 0x7f
    0x80 0x81 0x82 0x83 0x84 0x85 0x86 0x87 0x88 0x89 0x8a 0x8b 0x8c 0x8d 0x8e 0x8f
    0x90 0x91 0x92 0x93 0x94 0x95 0x96 0x97 0x98 0x99 0x9a 0x9b 0x9c 0x9d 0x9e 0x9f
    0xa0 0xa1 0xa2 0xa3 0xa4 0xa5 0xa6 0xa7 0xa8 0xa9 0xaa 0xab 0xac 0xad 0xae 0xaf
    0xb0 0xb1 0xb2 0xb3 0xb4 0xb5 0xb6 0xb7 0xb8 0xb9 0xba 0xbb 0xbc 0xbd 0xbe 0xbf
    0xc0 0xc1 0xc2 0xc3 0xc4 0xc5 0xc6 0xc7 0xc8 0xc9 0xca 0xcb 0xcc 0xcd 0xce 0xcf
    0xd0 0xd1 0xd2 0xd3 0xd4 0xd5 0xd6 0xd7 0xd8 0xd9 0xda 0xdb 0xdc 0xdd 0xde 0xdf
    0xe0 0xe1 0xe2 0xe3 0xe4 0xe5 0xe6 0xe7 0xe8 0xe9 0xea 0xeb 0xec 0xed 0xee 0xef
    0xf0 0xf1 0xf2 0xf3 0xf4 0xf5 0xf6 0xf7 0xf8 0xf9 0xfa 0xfb 0xfc 0xfd 0xfe 0xff
))

(define-constant BYTE_TO_STRING (list
    "00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f"
    "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "1a" "1b" "1c" "1d" "1e" "1f"
    "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "2a" "2b" "2c" "2d" "2e" "2f"
    "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "3a" "3b" "3c" "3d" "3e" "3f"
    "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" "4a" "4b" "4c" "4d" "4e" "4f"
    "50" "51" "52" "53" "54" "55" "56" "57" "58" "59" "5a" "5b" "5c" "5d" "5e" "5f"
    "60" "61" "62" "63" "64" "65" "66" "67" "68" "69" "6a" "6b" "6c" "6d" "6e" "6f"
    "70" "71" "72" "73" "74" "75" "76" "77" "78" "79" "7a" "7b" "7c" "7d" "7e" "7f"
    "80" "81" "82" "83" "84" "85" "86" "87" "88" "89" "8a" "8b" "8c" "8d" "8e" "8f"
    "90" "91" "92" "93" "94" "95" "96" "97" "98" "99" "9a" "9b" "9c" "9d" "9e" "9f"
    "a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9" "aa" "ab" "ac" "ad" "ae" "af"
    "b0" "b1" "b2" "b3" "b4" "b5" "b6" "b7" "b8" "b9" "ba" "bb" "bc" "bd" "be" "bf"
    "c0" "c1" "c2" "c3" "c4" "c5" "c6" "c7" "c8" "c9" "ca" "cb" "cc" "cd" "ce" "cf"
    "d0" "d1" "d2" "d3" "d4" "d5" "d6" "d7" "d8" "d9" "da" "db" "dc" "dd" "de" "df"
    "e0" "e1" "e2" "e3" "e4" "e5" "e6" "e7" "e8" "e9" "ea" "eb" "ec" "ed" "ee" "ef"
    "f0" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "fa" "fb" "fc" "fd" "fe" "ff"
))
    

;; Convert a 1-byte buffer into its uint representation.
(define-private (buff-to-u8 (byte (buff 1)))
    (unwrap-panic (index-of BUFF_TO_BYTE byte))
)

;; Convert a little-endian 16-byte buff into a uint.
(define-private (buff-to-uint-le (word (buff 16)))
    (get acc
        (fold add-and-shift-uint-le (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15) { acc: u0, data: word })
    )
)

;; Inner fold function for converting a 16-byte buff into a uint.
(define-private (add-and-shift-uint-le (idx uint) (input { acc: uint, data: (buff 16) }))
    (let (
        (acc (get acc input))
        (data (get data input))
        (byte (buff-to-u8 (unwrap-panic (element-at data idx))))
    )
    {
        ;; acc = byte * (2**(8 * (15 - idx))) + acc
        acc: (+ (* byte (pow u2 (* u8 (- u15 idx)))) acc),
        data: data
    })
)

;; Convert the lower 16 bytes of a buff into a little-endian uint.
(define-private (lower-16-le (input (buff 32)))
    (get acc
        (fold lower-16-le-closure (list u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31) { acc: 0x, data: input })
    )
)

;; Inner closure for obtaining the lower 16 bytes of a 32-byte buff
(define-private (lower-16-le-closure (idx uint) (input { acc: (buff 16), data: (buff 32) }))
    (let (
        (acc (get acc input))
        (data (get data input))
        (byte (unwrap-panic (element-at data idx)))
    )
    {
        acc: (unwrap-panic (as-max-len? (concat acc byte) u16)),
        data: data
    })
)

(define-private (byte-to-string (byte (buff 1)))
    (unwrap-panic (element-at BYTE_TO_STRING (unwrap-panic (index-of BUFF_TO_BYTE byte))))
)

(define-private (buff32-to-string-ascii-closure (idx uint) (state { acc: (string-ascii 64), data: (buff 32) }))
    {
        acc: (unwrap-panic (as-max-len? (concat (get acc state) (byte-to-string (unwrap-panic (element-at (get data state) idx)))) u64)),
        data: (get data state)
    }
)

(define-private (buff32-to-string-ascii (data (buff 32)))
   (unwrap-panic
        (if (is-eq (len data) u32)
            (ok (get acc (fold buff32-to-string-ascii-closure (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31) { acc: "", data: data })))
            (err false)
        )
    )
)


