(define-public (list-tests)
    (begin
        (print "test: unit-tests")
        (print "test: unit-tests-2")
        (ok true)
    )
)

(define-private (test-add-amount-for-block)
    (begin
        (print "run test-add-amount-for-block")

        (asserts! (is-eq u100 (add-amount-for-block u1000000000 u100))
            (err u0))

        (asserts! (is-eq u111 (add-amount-for-block u1000000000 u11))
            (err u1))

        (asserts! (is-eq u12 (add-amount-for-block u10000000001 u12))
            (err u2))

        (ok u0)
    )
)

(define-private (test-add-miner-for-block)
    (begin
        (print "run test-add-miner-for-block")
        (asserts! (is-eq
            (add-miner-for-block 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S u2000000 u100)
            (ok {
                miners-at-block: { range-start: u0, range-end: u100, claimed: false },
                cur-lockup: { tickets: u0, ustx: u100 }
            }))
            (err u0)
        )

        (asserts! (is-eq
            (add-miner-for-block 'SP2FXK1EKMR9KDFJCMYD1M3P2ZSFJC0B2QGYD9746 u2000000 u11)
            (ok {
                miners-at-block: { range-start: u100, range-end: u111, claimed: false },
                cur-lockup: { tickets: u0, ustx: u111 }
            }))
            (err u1)
        )

        ;; same cycle
        (asserts! (is-eq
            (add-miner-for-block 'SP2QRYHBCW6PNMK3P9KN5FE82MK15X2ZFBPH0V2PS u2000001 u12)
            (ok {
                miners-at-block: { range-start: u0, range-end: u12, claimed: false },
                cur-lockup: { tickets: u0, ustx: u123 }
            }))
            (err u2)
        )

        ;; different cycle
        (asserts! (is-eq
            (add-miner-for-block 'SP1WPSYKBESFW1M5P0MYSB8V8S6AN33F9RCVN8ZX u3000001 u12)
            (ok {
                miners-at-block: { range-start: u0, range-end: u12, claimed: false },
                cur-lockup: { tickets: u0, ustx: u12 }
            }))
            (err u2)
        )

        ;; duplicate miner
        (asserts! (is-eq
            (add-miner-for-block 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S u2000000 u1)
            (err ERR_ALREADY_MINED))
            (err u4)
        )

        (ok u0)
    )
)

(define-private (test-check-winner)
    (begin
        (print "run test-check-winner")
        (asserts! (is-eq
            (check-winner 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S u4000000)
            (err ERR_INVALID_BLOCK))
            (err u0)
        )

        (unwrap-panic (add-miner-for-block 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S (- block-height u1) u1))
        (asserts! (is-eq
            (check-winner 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S (- block-height u1))
            (ok true))
            (err u1)
        )

        (unwrap-panic (add-miner-for-block 'SP1WPSYKBESFW1M5P0MYSB8V8S6AN33F9RCVN8ZX (- block-height u1) u10000000000))
        (asserts! (is-eq
            (check-winner 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S (- block-height u1))
            (ok false))
            (err u2)
        )
        (asserts! (is-eq
            (check-winner 'SP1WPSYKBESFW1M5P0MYSB8V8S6AN33F9RCVN8ZX (- block-height u1))
            (ok true))
            (err u3)
        )

        (ok u0)
    )
)

(define-private (test-inner-claim-tickets-at-block)
    (begin
        (print "run test-inner-claim-tickets-at-block")
       
        ;; claim tickets mined in test-check-winner.
        ;; to reproduce: 
        ;; (unwrap-panic (add-miner-for-block 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S (- block-height u1) u1))
        ;; (unwrap-panic (add-miner-for-block 'SP1WPSYKBESFW1M5P0MYSB8V8S6AN33F9RCVN8ZX (- block-height u1) u10000000000))

        (asserts! (is-eq
            (inner-claim-tickets-at-block 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S (- block-height u1))
            (err ERR_NOT_WINNER))
            (err u0)
        )

        (asserts! (is-eq
            (inner-claim-tickets-at-block 'SP1WPSYKBESFW1M5P0MYSB8V8S6AN33F9RCVN8ZX (- block-height u1))
            (ok { range-start: u1, range-end: u10000000001, claimed: true }))
            (err u1)
        )

        (asserts! (is-eq
            (inner-claim-tickets-at-block 'SP1WPSYKBESFW1M5P0MYSB8V8S6AN33F9RCVN8ZX (- block-height u1))
            (err ERR_ALREADY_CLAIMED))
            (err u2)
        )

        (ok u0)
    )
)

(define-private (test-read-uint)
    (begin
        (print "run test-read-uint")
        (asserts! (is-eq
            (read-uint 0x11111111111111111111111111111112000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 u0)
            u22685491128062564230891640495451214098)
            (err u0)
        )
        (asserts! (is-eq
            (read-uint 0x11111111111111111111111111111112000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 u16)
            u0)
            (err u1)
        )
        (ok u0)
    )
)

(define-private (test-read-buff32)
    (begin
        (print "run test-read-buff32")
        (asserts! (is-eq
            (read-buff32 0x11111111111111111111111111111112000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 u0)
            0x1111111111111111111111111111111200000000000000000000000000000000)
            (err u0)
        )
        (asserts! (is-eq
            (read-buff32 0x11111111111111111111111111111112000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 u15)
            0x1200000000000000000000000000000000000000000000000000000000000000)
            (err u1)
        )
        (ok u0)
    )
)

(define-private (make-merkle-tree-8 (prefix (buff 8)))
    (let (
        (nft-descs (list
            (concat prefix 0x0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
            (concat prefix 0x1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)
            (concat prefix 0x2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222)
            (concat prefix 0x3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333)
            (concat prefix 0x4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444)
            (concat prefix 0x5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555)
            (concat prefix 0x6666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666)
            (concat prefix 0x7777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777)
        ))
        (row-0 (map sha512/256 nft-descs))
        (row-1 (list
            (sha512/256 (concat (unwrap-panic (element-at row-0 u0)) (unwrap-panic (element-at row-0 u1))))
            (sha512/256 (concat (unwrap-panic (element-at row-0 u2)) (unwrap-panic (element-at row-0 u3))))
            (sha512/256 (concat (unwrap-panic (element-at row-0 u4)) (unwrap-panic (element-at row-0 u5))))
            (sha512/256 (concat (unwrap-panic (element-at row-0 u6)) (unwrap-panic (element-at row-0 u7))))
        ))
        (row-2 (list
            (sha512/256 (concat (unwrap-panic (element-at row-1 u0)) (unwrap-panic (element-at row-1 u1))))
            (sha512/256 (concat (unwrap-panic (element-at row-1 u2)) (unwrap-panic (element-at row-1 u3))))
        ))
        (root (sha512/256 (concat (unwrap-panic (element-at row-2 u0)) (unwrap-panic (element-at row-2 u1)))))
    )
        (unwrap-panic (if (is-eq (len prefix) u8) (ok true) (err u0)))
        { root: root, nft-descs: nft-descs, rows: (list row-0 row-1 row-2 (list root)) }
    )
)

(define-private (get-sibling-hash (rows (list 4 (list 8 (buff 32)))) (row uint) (col uint))
    (unwrap-panic (element-at (unwrap-panic (element-at rows row)) col))
)

(define-private (serialize-uint-byte (idx uint) (state { data: (buff 64), num: uint }))
    (let (
        (data (get data state))
        (num (get num state))
        (byte (mod (/ num (pow u2 (* u8 (- u15 idx)))) u256))
        (buff (unwrap-panic (element-at BUFF_TO_BYTE byte)))
    )
    {
        data: (unwrap-panic (as-max-len? (concat data buff) u64)),
        num: num
    })
)   

(define-private (nft-rec-to-desc (nft-desc { tickets: uint, data-hash: (buff 32), size: uint }))
    (let (
        (data (get data-hash nft-desc))
        (ticks (get tickets nft-desc))
        (size (get size nft-desc))
        (data-with-size
            (get data
                (fold serialize-uint-byte
                    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15)
                    { data: (unwrap-panic (as-max-len? data u64)), num: size })
            )
        )
        (data-with-size-and-tickets
            (get data
                (fold serialize-uint-byte
                    (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15)
                    { data: (unwrap-panic (as-max-len? data-with-size u64)), num: ticks })
            )
        )
    )
        data-with-size-and-tickets
    )
)

(define-private (test-merkle-proof-root)
    (let (
        (merkle-tree (make-merkle-tree-8 0x0000000000000000))
        (nft-descs (get nft-descs merkle-tree))
        (root (get root merkle-tree))
        (rows (get rows merkle-tree))
    )
        (print "run test-merkle-proof-root") 

        (asserts! (is-eq
            root
            (merkle-proof-root
                (unwrap-panic (element-at nft-descs u0))
                { hashes: (list
                    (get-sibling-hash rows u0 u1)
                    (get-sibling-hash rows u1 u1)
                    (get-sibling-hash rows u2 u1)
                  ),
                  index: u0
                }
            ))
            (err u0)
        )
        
        (asserts! (is-eq
            root
            (merkle-proof-root
                (unwrap-panic (element-at nft-descs u1))
                { hashes: (list
                    (get-sibling-hash rows u0 u0)
                    (get-sibling-hash rows u1 u1)
                    (get-sibling-hash rows u2 u1)
                  ),
                  index: u1
                }
            ))
            (err u1)
        )
        
        (asserts! (is-eq
            root
            (merkle-proof-root
                (unwrap-panic (element-at nft-descs u2))
                { hashes: (list
                    (get-sibling-hash rows u0 u3)
                    (get-sibling-hash rows u1 u0)
                    (get-sibling-hash rows u2 u1)
                  ),
                  index: u2
                }
            ))
            (err u2)
        )
        
        (asserts! (is-eq
            root
            (merkle-proof-root
                (unwrap-panic (element-at nft-descs u3))
                { hashes: (list
                    (get-sibling-hash rows u0 u2)
                    (get-sibling-hash rows u1 u0)
                    (get-sibling-hash rows u2 u1)
                  ),
                  index: u3
                }
            ))
            (err u3)
        )
        
        (asserts! (is-eq
            root
            (merkle-proof-root
                (unwrap-panic (element-at nft-descs u4))
                { hashes: (list
                    (get-sibling-hash rows u0 u5)
                    (get-sibling-hash rows u1 u3)
                    (get-sibling-hash rows u2 u0)
                  ),
                  index: u4
                }
            ))
            (err u4)
        )

        (asserts! (is-eq
            root
            (merkle-proof-root
                (unwrap-panic (element-at nft-descs u5))
                { hashes: (list
                    (get-sibling-hash rows u0 u4)
                    (get-sibling-hash rows u1 u3)
                    (get-sibling-hash rows u2 u0)
                  ),
                  index: u5
                }
            ))
            (err u5)
        )
        
        (asserts! (is-eq
            root
            (merkle-proof-root
                (unwrap-panic (element-at nft-descs u6))
                { hashes: (list
                    (get-sibling-hash rows u0 u7)
                    (get-sibling-hash rows u1 u2)
                    (get-sibling-hash rows u2 u0)
                  ),
                  index: u6
                }
            ))
            (err u6)
        )

        (asserts! (is-eq
            root
            (merkle-proof-root
                (unwrap-panic (element-at nft-descs u7))
                { hashes: (list
                    (get-sibling-hash rows u0 u6)
                    (get-sibling-hash rows u1 u2)
                    (get-sibling-hash rows u2 u0)
                  ),
                  index: u7
                }
            ))
            (err u7)
        )

        (ok u0)
    )
)

(define-private (test-nft-rec-to-desc)
    (begin
        (print "run test-nft-rec-to-desc")
        (asserts! (is-eq
            0x11111111111111111111111111111111111111111111111111111111111111130000000000000000000000000000000100000000000000000000000000000002
            (nft-rec-to-desc { data-hash: 0x1111111111111111111111111111111111111111111111111111111111111113, size: u1, tickets: u2 }))
            (err u0)
        )
        (asserts! (is-eq
            0x11111111111111111111111111111111111111111111111111111111111111130000000000000000000000000000040100000000000000000000000000000502
            (nft-rec-to-desc { data-hash: 0x1111111111111111111111111111111111111111111111111111111111111113, size: u1025, tickets: u1282 }))
            (err u0)
        )
        (ok u0)
    )
)

(define-private (test-inner-register-nftree)
    (let (
        (nft-id (var-get next-nft-id))
    )
        (print "run test-inner-register-nftree")
        (asserts! (is-none (map-get? nft-recs nft-id))
            (err u1))

        (unwrap-panic (inner-register-nftree { tickets: u21, data-hash: 0x1111111111111111111111111111111111111111111111111111111111111111, size: u21 } 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC))

        (asserts! (is-eq (var-get next-nft-id) (+ u1 nft-id))
            (err u10))
        (asserts! (is-eq (map-get? nft-recs nft-id) (some { tickets: u21, data-hash: 0x1111111111111111111111111111111111111111111111111111111111111111, size: u21 }))
            (err u11))
        (asserts! (is-eq (nft-get-owner? nftree nft-id) (some 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC))
            (err u12))

        (ok u0)
    )
)

(define-private (test-inner-instantiate-nft)
    (let (
        (nft-id (var-get next-nft-id))
    )
        (print "run test-inner-instantiate-nft")
        (asserts! (is-none (map-get? nft-recs nft-id))
            (err u1))

        (unwrap-panic (inner-instantiate-nft { tickets: u0, data-hash: 0x0000000000000000000000000000000000000000000000000000000000000001, size: u1 } 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S))

        (asserts! (is-eq (var-get next-nft-id) (+ u1 nft-id))
            (err u10))
        (asserts! (is-eq (map-get? nft-recs nft-id) (some { tickets: u0, data-hash: 0x0000000000000000000000000000000000000000000000000000000000000001, size: u1 }))
            (err u11))
        (asserts! (is-eq (nft-get-owner? nftree nft-id) (some 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S))
            (err u12))
        
        (ok u0)
    )
)

(define-private (test-inner-can-claim-nft)
    (let (
        (merkle-tree (make-merkle-tree-8 0x0000000000000000))
        (nft-descs (get nft-descs merkle-tree))
        (root (get root merkle-tree))
        (rows (get rows merkle-tree))

        (nft-desc-0 (unwrap-panic (element-at nft-descs u0)))
        (nft-proof-0
            { hashes: (list
                (get-sibling-hash rows u0 u1)
                (get-sibling-hash rows u1 u1)
                (get-sibling-hash rows u2 u1)
              ),
              index: u0
            }
        )

        (nft-desc-1 (unwrap-panic (element-at nft-descs u1)))
        (nft-proof-1
            { hashes: (list
                (get-sibling-hash rows u0 u0)
                (get-sibling-hash rows u1 u1)
                (get-sibling-hash rows u2 u1)
              ),
              index: u1
            }
        )
        
        (nft-desc-2 (unwrap-panic (element-at nft-descs u2)))
        (nft-proof-2
            { hashes: (list
                (get-sibling-hash rows u0 u3)
                (get-sibling-hash rows u1 u0)
                (get-sibling-hash rows u2 u1)
              ),
              index: u2
            }
        )

        ;; create parent
        (parent-nft-id (unwrap-panic (inner-register-nftree { tickets: u21, data-hash: root, size: u21 } 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)))
    )
        (print "run test-inner-can-claim-nft")

        ;; parent doesn't exist 
        (asserts! (is-eq
            (err ERR_NO_SUCH_NFT)
            (inner-can-claim-nft? nft-desc-0 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S (+ u1 parent-nft-id) nft-proof-0 block-height))
            (err u0)
        )

        ;; create parent
        (unwrap-panic (inner-register-nftree { tickets: u21, data-hash: root, size: u21 } 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC))
        
        ;; can we claim the free nft?
        (asserts! (is-eq
            (ok { tickets: u0, data-hash: 0x0000000000000000000000000000000000000000000000000000000000000000, size: u0 })
            (inner-can-claim-nft? nft-desc-0 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S parent-nft-id nft-proof-0 block-height))
            (err u1)
        )

        ;; mark free NFT as claimed
        (unwrap-panic (inner-instantiate-nft { tickets: u0, data-hash: 0x0000000000000000000000000000000000000000000000000000000000000000, size: u0 } 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S))

        ;; try to claim the free NFT again (should fail since already claimed)
        (asserts! (is-eq
            (err ERR_ALREADY_MINTED)
            (inner-can-claim-nft? nft-desc-0 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S parent-nft-id nft-proof-0 block-height))
            (err u2)
        )

        ;; need tickets, so can't claim non-free nft
        (asserts! (is-eq
            (err ERR_INSUFFICIENT_BALANCE)
            (inner-can-claim-nft? nft-desc-1 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S parent-nft-id nft-proof-1 block-height))
            (err u3)
        )

        ;; grant tickets (10 * 0x11111111111111111111111111111111 tickets)
        (unwrap-panic (ft-mint? tickets (* u10 u22685491128062564230891640495451214097) 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S))

        ;; should succeed now
        (asserts! (is-eq
            (ok { tickets: u22685491128062564230891640495451214097, data-hash: 0x0000000000000000111111111111111111111111111111111111111111111111, size: u22685491128062564230891640495451214097 })
            (inner-can-claim-nft? nft-desc-1 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S parent-nft-id nft-proof-1 block-height))
            (err u4)
        )

        ;; lock parent for stacking
        (map-set nft-lockups parent-nft-id (+ u1 block-height))

        ;; can't claim if parent is locked
        (asserts! (is-eq
            (err ERR_NFT_LOCKED)
            (inner-can-claim-nft? nft-desc-2 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S parent-nft-id nft-proof-2 block-height))
            (err u5)
        )

        ;; cleanup
        (map-delete nft-lockups parent-nft-id)
        (ok u0)
    )
)

(define-private (test-inner-can-stack-nft)
    (let (
        (nft-id (unwrap-panic (inner-register-nftree { tickets: u21, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222222, size: u21 } 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)))
    )
        (print "run test-inner-can-stack-nft")

        ;; someone else tries to stack this NFT and fails
        (asserts! (is-eq
            (err ERR_PERMISSION_DENIED)
            (inner-can-stack-nft? nft-id 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S u1 block-height))
            (err u0)
        )

        ;; invalid cycles
        (asserts! (is-eq
            (err ERR_INVALID_CYCLES)
            (inner-can-stack-nft? nft-id 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC u0 block-height))
            (err u1)
        )
        (asserts! (is-eq
            (err ERR_INVALID_CYCLES)
            (inner-can-stack-nft? nft-id 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC u13 block-height))
            (err u2)
        )

        ;; should succeed
        (asserts! (is-eq
            (ok true)
            (inner-can-stack-nft? nft-id 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC u1 block-height))
            (err u3)
        )
        
        ;; stack it
        (map-set nft-lockups nft-id (+ u1 block-height))

        ;; now should fail
        (asserts! (is-eq
            (err ERR_NFT_LOCKED)
            (inner-can-stack-nft? nft-id 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC u1 block-height))
            (err u4)
        )

        ;; clean up
        (map-delete nft-lockups nft-id)

        (ok u0)
    )
)

(define-private (test-put-owner-lockup-at-cycle)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)
        (owner-2 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S)

        (nft-id-1 (unwrap-panic (inner-register-nftree { tickets: u23, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222223, size: u23 } owner-1)))
        (nft-id-2 (unwrap-panic (inner-register-nftree { tickets: u24, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222224, size: u24 } owner-1)))
        (nft-id-3 (unwrap-panic (inner-register-nftree { tickets: u25, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222225, size: u25 } owner-2)))

        (cyc u1000)
        (locked-at-cyc
            (default-to { tickets: u0, ustx: u0 }
            (map-get? cycle-lockups cyc))
        )

        (locked-by-1
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: cyc }))
        )
        (locked-by-2
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-2, cycle: cyc }))
        )
    )

        (print "run test-put-owner-lockup-at-cycle")

        (put-owner-lockup-at-cycle u0 { owner: owner-1, start-cyc: cyc, num-cycs: u1, nft-tickets: u23 })

        (asserts! (is-eq
            (map-get? cycle-lockups cyc)
            (some { tickets: (+ (get tickets locked-at-cyc) u23), ustx: (get ustx locked-at-cyc) }))
            (err u0)
        )
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: cyc })
            (some { tickets: (+ (get tickets locked-by-1) u23), reward-claimed: false }))
            (err u10)
        )
        (asserts! (is-eq
            (default-to { tickets: u0, reward-claimed: false }
                (map-get? owner-ticket-lockups { owner: owner-2, cycle: cyc }))
            { tickets: (get tickets locked-by-2), reward-claimed: false })
            (err u20)
        )

        (put-owner-lockup-at-cycle u1 { owner: owner-1, start-cyc: cyc, num-cycs: u2, nft-tickets: u23 })

        (asserts! (is-eq
            (map-get? cycle-lockups (+ u1 cyc))
            (some { tickets: (+ (get tickets locked-at-cyc) u23), ustx: (get ustx locked-at-cyc) }))
            (err u1)
        )
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: cyc })
            (some { tickets: (+ (get tickets locked-by-1) u23), reward-claimed: false }))
            (err u11)
        )
        (asserts! (is-eq
            (default-to { tickets: u0, reward-claimed: false }
                (map-get? owner-ticket-lockups { owner: owner-2, cycle: cyc }))
            { tickets: (get tickets locked-by-2), reward-claimed: false })
            (err u21)
        )

        (put-owner-lockup-at-cycle u0 { owner: owner-2, start-cyc: cyc, num-cycs: u1, nft-tickets: u25 })
        
        (asserts! (is-eq
            (map-get? cycle-lockups cyc)
            (some { tickets: (+ (get tickets locked-at-cyc) u23 u25), ustx: (get ustx locked-at-cyc) }))
            (err u2)
        )
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: cyc })
            (some { tickets: (+ (get tickets locked-by-1) u23), reward-claimed: false }))
            (err u22)
        )
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-2, cycle: cyc })
            (some { tickets: (+ (get tickets locked-by-2) u25), reward-claimed: false }))
            (err u12)
        )

        (ok u0)
    )
)

(define-private (test-inner-stack-nft-in-cycles)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)

        (nft-id-1 (unwrap-panic (inner-register-nftree { tickets: u23, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222226, size: u23 } owner-1)))

        (cyc u2000)
        (locked-at-cyc
            (default-to { tickets: u0, ustx: u0 }
            (map-get? cycle-lockups cyc))
        )

        (locked-by-1-at-0
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: cyc }))
        )
        (locked-by-1-at-1
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u1 cyc) }))
        )
        (locked-by-1-at-2
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u2 cyc) }))
        )
        (locked-by-1-at-3
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u3 cyc) }))
        )
        (locked-by-1-at-4
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u4 cyc) }))
        )
    )
        (print "run test-inner-stack-nft-in-cycles")

        ;; owner 1 stacks nft 1 for 3 cycles
        (inner-stack-nft-in-cycles nft-id-1 owner-1 u3 (cyc-to-blk cyc))

        ;; check each cycle -- total tickets locked must have increased
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u1 cyc ) })
            (some { tickets: (+ (get tickets locked-by-1-at-1) u23), reward-claimed: false }))
            (err u1)
        )
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u2 cyc ) })
            (some { tickets: (+ (get tickets locked-by-1-at-2) u23), reward-claimed: false }))
            (err u2)
        )
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u3 cyc ) })
            (some { tickets: (+ (get tickets locked-by-1-at-3) u23), reward-claimed: false }))
            (err u3)
        )
        ;; cycle + 4 not affected
        (asserts! (is-eq
            (default-to { tickets: u0, reward-claimed: false }
                (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u4 cyc ) })
            )
            locked-by-1-at-4)
            (err u4)
        )
        ;; cycle + 0 not affected
        (asserts! (is-eq
            (default-to { tickets: u0, reward-claimed: false }
                (map-get? owner-ticket-lockups { owner: owner-1, cycle: cyc })
            )
            locked-by-1-at-0)
            (err u5)
        )

        ;; nft unlocks at the right block
        (asserts! (nft-stacked? nft-id-1 (cyc-to-blk cyc))
            (err u16))
        (asserts! (nft-stacked? nft-id-1 (cyc-to-blk (+ u1 cyc)))
            (err u17))
        (asserts! (nft-stacked? nft-id-1 (cyc-to-blk (+ u2 cyc)))
            (err u18))
        (asserts! (nft-stacked? nft-id-1 (cyc-to-blk (+ u3 cyc)))
            (err u19))
        (asserts! (not (nft-stacked? nft-id-1 (cyc-to-blk (+ u4 cyc))))
            (err u20))

        (ok u0)
    )
)

(define-private (test-inner-set-rewards-claimed-for-cycle)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)

        (cyc u3000)
        (nft-id-1 (unwrap-panic (inner-register-nftree { tickets: u23, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222230, size: u23 } owner-1)))

        (locked-by-1-at-0
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: cyc }))
        )
        (locked-by-1-at-1
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u1 cyc) }))
        )
        (locked-by-1-at-2
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u2 cyc) }))
        )
        (locked-by-1-at-3
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u3 cyc) }))
        )
        (locked-by-1-at-4
            (default-to { tickets: u0, reward-claimed: false }
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u4 cyc) }))
        )
    )
        (print "run test-inner-set-rewards-claimed-for-cycle")

        ;; stack this NFT in cycles 1-3
        (inner-stack-nft-in-cycles nft-id-1 owner-1 u3 (cyc-to-blk cyc))

        (inner-set-rewards-claimed-for-cycle u0 { start-cyc: (+ u1 cyc), owner: owner-1, end-cyc: (+ u12 cyc) })
        (inner-set-rewards-claimed-for-cycle u1 { start-cyc: (+ u1 cyc), owner: owner-1, end-cyc: (+ u12 cyc) })
        (inner-set-rewards-claimed-for-cycle u2 { start-cyc: (+ u1 cyc), owner: owner-1, end-cyc: (+ u12 cyc) })

        ;; check claim status for cycles 1-3
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u1 cyc) })
            (some { tickets: (+ (get tickets locked-by-1-at-1) u23), reward-claimed: true }))
            (err u1)
        )
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u2 cyc) })
            (some { tickets: (+ (get tickets locked-by-1-at-2) u23), reward-claimed: true }))
            (err u2)
        )
        (asserts! (is-eq
            (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u3 cyc) })
            (some { tickets: (+ (get tickets locked-by-1-at-3) u23), reward-claimed: true }))
            (err u3)
        )
        ;; cycle + 4 not affected
        (asserts! (is-eq
            (default-to { tickets: u0, reward-claimed: false }
                (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u4 cyc ) })
            )
            locked-by-1-at-4)
            (err u4)
        )
        ;; cycle + 0 not affected
        (asserts! (is-eq
            (default-to { tickets: u0, reward-claimed: false }
                (map-get? owner-ticket-lockups { owner: owner-1, cycle: cyc })
            )
            locked-by-1-at-0)
            (err u5)
        )

        (ok u0)
    )
)

(define-private (test-inner-stacking-totals)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)
        (miner-1 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S)

        (cyc u4000)
        (nft-id-1 (unwrap-panic (inner-register-nftree { tickets: u23, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222231, size: u23 } owner-1)))
        (nft-id-2 (unwrap-panic (inner-register-nftree { tickets: u24, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222232, size: u24 } owner-1)))
        (nft-id-3 (unwrap-panic (inner-register-nftree { tickets: u25, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222233, size: u24 } miner-1)))

        (stacking-total-start { start-cyc: (+ u1 cyc), end-cyc: (+ u12 cyc), ustx-due: u0, owner: owner-1 })
    )
        (print "run test-inner-stacking-totals")

        ;; lock these NFTs for 3 cycles
        (inner-stack-nft-in-cycles nft-id-1 owner-1 u3 (cyc-to-blk cyc))
        (inner-stack-nft-in-cycles nft-id-2 owner-1 u3 (cyc-to-blk cyc))
        (inner-stack-nft-in-cycles nft-id-3 miner-1 u3 (cyc-to-blk cyc))

        ;; mine 1,4,9 uSTX in each cycle
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u1 cyc)) u1))
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u2 cyc)) u4))
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u3 cyc)) u9))

        ;; verify that the totals are reflected correctly in each cycle
        (asserts! (is-eq
            (inner-stacking-totals u0 stacking-total-start)
            ;; ustx-due = ((23 + 24) * 1) / (23 + 24 + 25)
            (merge stacking-total-start { ustx-due: u0 }))
            (err u1)
        )
        (asserts! (is-eq
            (inner-stacking-totals u1 stacking-total-start)
            ;; ustx-due = ((23 + 24) * 4) / (23 + 24 + 25)
            (merge stacking-total-start { ustx-due: u2 }))
            (err u2)
        )
        (asserts! (is-eq
            (inner-stacking-totals u2 stacking-total-start)
            ;; ustx-due = ((23 + 24) * 9) / (23 + 24 + 25)
            (merge stacking-total-start { ustx-due: u5 }))
            (err u3)
        )
        ;; got nothing in cycle 4
        (asserts! (is-eq
            (inner-stacking-totals u3 stacking-total-start)
            stacking-total-start)
            (err u4)
        )
        ;; got nothing in cycle 0
        (asserts! (is-eq
            (inner-stacking-totals u0 { start-cyc: cyc, end-cyc: (+ u12 cyc), ustx-due: u0, owner: owner-1 })
            { start-cyc: cyc, end-cyc: (+ u12 cyc), owner: owner-1, ustx-due: u0 })
            (err u5)
        )

        (ok u0)
    )
)

(define-private (test-inner-can-claim-stacking-rewards)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)
        (miner-1 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S)

        (cyc u5000)
        (nft-id-1 (unwrap-panic (inner-register-nftree { tickets: u23, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222234, size: u23 } owner-1)))
        (nft-id-2 (unwrap-panic (inner-register-nftree { tickets: u24, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222235, size: u24 } owner-1)))
        (nft-id-3 (unwrap-panic (inner-register-nftree { tickets: u25, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222236, size: u24 } miner-1)))
    )
        (print "run test-inner-can-claim-stacking-rewards")

        ;; lock these NFTs for 3 cycles
        (inner-stack-nft-in-cycles nft-id-1 owner-1 u3 (cyc-to-blk cyc))
        (inner-stack-nft-in-cycles nft-id-2 owner-1 u3 (cyc-to-blk cyc))
        (inner-stack-nft-in-cycles nft-id-3 miner-1 u3 (cyc-to-blk cyc))

        ;; mine 1,4,9 uSTX in each cycle
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u1 cyc)) u1))
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u2 cyc)) u4))
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u3 cyc)) u9))

        ;; verify that we can claim stacking rewards.
        ;; invalid cycles?
        (asserts! (is-eq
            (inner-can-claim-stacking-rewards owner-1 (+ u1 cyc) u13 (cyc-to-blk cyc))
            (err ERR_INVALID_CYCLES))
            (err u0)
        )
        
        ;; must be present cycles
        (asserts! (is-eq
            (inner-can-claim-stacking-rewards owner-1 (+ u1 cyc) u1 (- (cyc-to-blk (+ u2 cyc)) u1))
            (err ERR_NOT_CONFIRMED))
            (err u1)
        )
        (asserts! (is-eq
            (inner-can-claim-stacking-rewards owner-1 (+ u1 cyc) u3 (- (cyc-to-blk (+ u5 cyc)) u1))
            (err ERR_NOT_CONFIRMED))
            (err u2)
        )
        
        ;; currently, the contract has no STX, so this should fail.
        (asserts! (is-eq
            (inner-can-claim-stacking-rewards owner-1 (+ u1 cyc) u3 (cyc-to-blk (+ u5 cyc)))
            (err ERR_INSUFFICIENT_BALANCE))
            (err u4)
        )

        ;; grant the uSTX to the contract so it can pass
        (unwrap-panic (stx-transfer? (+ u1 u4 u9) tx-sender (as-contract tx-sender)))

        ;; cycle +1: ustx-due = ((23 + 24) * 1) / (23 + 24 + 25) = 0
        ;; cycle +2: ustx-due = ((23 + 24) * 4) / (23 + 24 + 25) = 2
        ;; cycle +3: ustx-due = ((23 + 24) * 9) / (23 + 24 + 25) = 5
        ;; total: 7

        (asserts! (is-eq
            (inner-can-claim-stacking-rewards owner-1 (+ u1 cyc) u3 (cyc-to-blk (+ u5 cyc)))
            (ok u7))
            (err u5)
        )

        (ok u0)
    )
)

(define-private (test-inner-claim-stacking-rewards)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)
        (miner-1 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S)

        (cyc u6000)
        (nft-id-1 (unwrap-panic (inner-register-nftree { tickets: u23, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222234, size: u23 } owner-1)))
        (nft-id-2 (unwrap-panic (inner-register-nftree { tickets: u24, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222235, size: u24 } owner-1)))
        (nft-id-3 (unwrap-panic (inner-register-nftree { tickets: u25, data-hash: 0x2222222222222222222222222222222222222222222222222222222222222236, size: u24 } miner-1)))
    )
        (print "run test-inner-claim-stacking-rewards")

        ;; lock these NFTs for 3 cycles
        (inner-stack-nft-in-cycles nft-id-1 owner-1 u3 (cyc-to-blk cyc))
        (inner-stack-nft-in-cycles nft-id-2 owner-1 u3 (cyc-to-blk cyc))
        (inner-stack-nft-in-cycles nft-id-3 miner-1 u3 (cyc-to-blk cyc))

        ;; mine 1,4,9 uSTX in each cycle
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u1 cyc)) u1))
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u2 cyc)) u4))
        (unwrap-panic (add-miner-for-block miner-1 (cyc-to-blk (+ u3 cyc)) u9))

        ;; grant the uSTX to the contract so it can pass
        (unwrap-panic (stx-transfer? (+ u1 u4 u9) tx-sender (as-contract tx-sender)))
        
        ;; first cycle should NOT be claimed by owner-1
        (asserts! (is-eq
            (get reward-claimed
                (default-to { tickets: u0, reward-claimed: false }
                    (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u1 cyc) })
                )
            )
            false)
            (err u0)
        )

        ;; claim first cycle
        (unwrap-panic (inner-claim-stacking-rewards owner-1 (+ u1 cyc) u1 u0))

        ;; first cycle should be claimed owner-1
        (asserts! (is-eq
            (get reward-claimed
                (default-to { tickets: u0, reward-claimed: false }
                    (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u1 cyc) })
                )
            )
            true)
            (err u1)
        )

        ;; second cycle should NOT be claimed by owner-1
        (asserts! (is-eq
            (get reward-claimed
                (default-to { tickets: u0, reward-claimed: false }
                    (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u2 cyc) })
                )
            )
            false)
            (err u2)
        )
        ;; third cycle should NOT be claimed by owner-1
        (asserts! (is-eq
            (get reward-claimed
                (default-to { tickets: u0, reward-claimed: false }
                    (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u3 cyc) })
                )
            )
            false)
            (err u3)
        )

        ;; claim second and third cycles
        (unwrap-panic (inner-claim-stacking-rewards owner-1 (+ u2 cyc) u2 u7))
        
        ;; second cycle should be claimed by owner-1
        (asserts! (is-eq
            (get reward-claimed
                (default-to { tickets: u0, reward-claimed: false }
                    (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u2 cyc) })
                )
            )
            true)
            (err u4)
        )
        ;; third cycle should be claimed by owner-1
        (asserts! (is-eq
            (get reward-claimed
                (default-to { tickets: u0, reward-claimed: false }
                    (map-get? owner-ticket-lockups { owner: owner-1, cycle: (+ u3 cyc) })
                )
            )
            true)
            (err u5)
        )

        ;; owner-1 has nothing left to claim
        (asserts! (is-eq
            (inner-can-claim-stacking-rewards owner-1 (+ u1 cyc) u3 (cyc-to-blk (+ u5 cyc)))
            (ok u0))
            (err u6)
        )

        (ok u0)
    )
)

(define-private (test-inner-can-submit-buy-offer)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)
        (miner-1 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S)

        (nft-desc-1 0x11111111111111111111111111111111111111111111111111111111111111130000000000000000000000000000000100000000000000000000000000000002)
    )
        (print "run test-inner-can-submit-buy-offer")

        ;; can't back-date
        (asserts! (is-eq
                    (err ERR_INVALID_BUY_EXPIRE)
                    (inner-can-submit-buy-offer nft-desc-1 owner-1 u1 u100 u101)
                  )
            (err u1)
        )

        ;; buyer must have the uSTX 
        (asserts! (is-eq
                    (err ERR_INSUFFICIENT_BALANCE)
                    (inner-can-submit-buy-offer nft-desc-1 owner-1 u10000000 u101 u100)
                  )
            (err u2)
        )

        ;; offer accepted!
        (asserts! (is-ok
                    (inner-can-submit-buy-offer nft-desc-1 owner-1 u1 u101 u100)
                  )
            (err u3)
        )

        ;; can't submit the same amount -- must give a better offer
        (map-set nft-buy-offers nft-desc-1 { buyer: miner-1, amount-ustx: u1, expires: u101 })
        (asserts! (is-eq
                    (err ERR_BAD_OFFER)
                    (inner-can-submit-buy-offer nft-desc-1 owner-1 u1 u101 u100)
                  )
            (err u4)
        )

        ;; giving a better offer works
        (asserts! (is-ok
                    (inner-can-submit-buy-offer nft-desc-1 owner-1 u2 u101 u100)
                  )
            (err u5)
        )

        ;; can overwrite an expired offer
        (map-set nft-buy-offers nft-desc-1 { buyer: miner-1, amount-ustx: u1, expires: u100 })
        (asserts! (is-ok
                    (inner-can-submit-buy-offer nft-desc-1 owner-1 u1 u101 u100)
                  )
            (err u6)
        )

        ;; clean up
        (map-delete nft-buy-offers nft-desc-1)
        (ok true)
    )
)

(define-private (test-inner-submit-buy-offer)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)
        (miner-1 'SP1W2XXGVPYH4J1380KRRP631424VX8VCCRSJ3H6S)

        (nft-desc-1 0x11111111111111111111111111111111111111111111111111111111111111130000000000000000000000000000000100000000000000000000000000000004)
        (tx-sender-stx-before (stx-get-balance tx-sender))
        (contract-stx-before (stx-get-balance (as-contract tx-sender)))
        (saved-tx-sender tx-sender)
    )
        (print "run test-inner-submit-buy-offer")

        ;; submit the buy offer from owner-1.
        ;; there's no prior offer.
        (asserts! (is-none (map-get? nft-buy-offers nft-desc-1))
            (err u0)
        )
       
        (unwrap-panic (inner-submit-buy-offer nft-desc-1 tx-sender u10 u101 none u100))

        ;; took possession
        (asserts! (is-eq (- tx-sender-stx-before u10) (stx-get-balance tx-sender))
            (err u1)
        )
        (asserts! (is-eq (+ contract-stx-before u10) (stx-get-balance (as-contract tx-sender)))
            (err u2)
        )

        ;; new buy offer exists for tx-sender
        (asserts! (is-eq
                (some { buyer: tx-sender, amount-ustx: u10, expires: u101 })
                (map-get? nft-buy-offers nft-desc-1))
            (err u3)
        )

        ;; replace with a better buy offer
        (unwrap-panic (inner-submit-buy-offer nft-desc-1 tx-sender u11 u101 (some { buyer: tx-sender, amount-ustx: u10, expires: u101 }) u100))
        
        ;; new buy offer exists for tx-sender
        (asserts! (is-eq
                (some { buyer: tx-sender, amount-ustx: u11, expires: u101 })
                (map-get? nft-buy-offers nft-desc-1))
            (err u4)
        )

        ;; tx-sender gets their money back from their original offer -- they've only committed u11 uSTX, not u11 + u10 uSTX
        (asserts! (is-eq (- tx-sender-stx-before u11) (stx-get-balance tx-sender))
            (err u5)
        )

        ;; revert
        (map-delete nft-buy-offers nft-desc-1)
        (unwrap-panic (as-contract (stx-transfer? u11 tx-sender saved-tx-sender)))
        (asserts! (is-eq tx-sender-stx-before (stx-get-balance tx-sender))
            (err u70)
        )
        (asserts! (is-eq contract-stx-before (stx-get-balance (as-contract tx-sender)))
            (err u7)
        )

        (ok true)
    )
)

(define-private (test-inner-can-reclaim-buy-offer)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)

        (nft-desc-1 0x11111111111111111111111111111111111111111111111111111111111111130000000000000000000000000000000100000000000000000000000000000005)
        (tx-sender-stx-before (stx-get-balance tx-sender))
        (contract-stx-before (stx-get-balance (as-contract tx-sender)))
        (saved-tx-sender tx-sender)
    )
        (print "run test-inner-can-reclaim-buy-offer")

        ;; can't reclaim a nonexistant buy offer
        (asserts! (is-eq
                    (err ERR_NO_SUCH_NFT)
                    (inner-can-reclaim-buy-offer nft-desc-1 tx-sender u101)
                  )
            (err u0)
        )

        ;; add a buy offer that expires in block 101 (while we're in block 100)
        (unwrap-panic (inner-submit-buy-offer nft-desc-1 tx-sender u10 u101 none u100))

        ;; only the buyer can try to reclaim
        (asserts! (is-eq
                    (err ERR_PERMISSION_DENIED)
                    (inner-can-reclaim-buy-offer nft-desc-1 owner-1 u101)
                  )
            (err u1)
        )

        ;; buy offer must have expired
        (asserts! (is-eq
                    (err ERR_INVALID_BUY_EXPIRE)
                    (inner-can-reclaim-buy-offer nft-desc-1 tx-sender u100)
                  )
            (err u2)
        )

        ;; check edge case
        (asserts! (is-ok
                    (inner-can-reclaim-buy-offer nft-desc-1 tx-sender u101)
                  )
            (err u3)
        )

        ;; clean up
        (map-delete nft-buy-offers nft-desc-1)
        (unwrap-panic (as-contract (stx-transfer? u10 tx-sender saved-tx-sender)))
        (asserts! (is-eq tx-sender-stx-before (stx-get-balance tx-sender))
            (err u70)
        )
        (asserts! (is-eq contract-stx-before (stx-get-balance (as-contract tx-sender)))
            (err u7)
        )

        (ok true)
    )
)

(define-private (test-inner-reclaim-buy-offer)
    (let (
        (owner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)

        (nft-desc-1 0x11111111111111111111111111111111111111111111111111111111111111140000000000000000000000000000000100000000000000000000000000000005)
        (tx-sender-stx-before (stx-get-balance tx-sender))
        (contract-stx-before (stx-get-balance (as-contract tx-sender)))
        (saved-tx-sender tx-sender)
    )
        (print "run test-inner-reclaim-buy-offer")
    
        ;; add a buy offer that expires in block 101 (while we're in block 100)
        (unwrap-panic (inner-submit-buy-offer nft-desc-1 tx-sender u10 u101 none u100))

        ;; reclaim it 
        (unwrap-panic (inner-reclaim-buy-offer nft-desc-1 { buyer: tx-sender, amount-ustx: u10, expires: u100 }))

        ;; verify that it cleaned up:
        ;; no more buy offer
        (asserts! (is-none (map-get? nft-buy-offers nft-desc-1))
            (err u0)
        )

        ;; ustx restored
        (asserts! (is-eq tx-sender-stx-before (stx-get-balance tx-sender))
            (err u70)
        )
        (asserts! (is-eq contract-stx-before (stx-get-balance (as-contract tx-sender)))
            (err u7)
        )

        (ok true)
    )
)

(define-private (test-inner-can-fulfill-buy-offer)
    (let (
        (buyer-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)

        ;; NOTE: this is 0 tickets, because for some reason, we trigger a runtime panic in the `stacker` crate on musl libc
        (nft-desc-1 0x11111111111111111111111111111111111111111111111111111111111111130000000000000000000000000000000100000000000000000000000000000000)
        (nft-rec-1 (unwrap-panic (parse-nft-desc nft-desc-1)))
        (tx-sender-stx-before (stx-get-balance tx-sender))
        (contract-stx-before (stx-get-balance (as-contract tx-sender)))
        (saved-tx-sender tx-sender)
    )
        (print "run test-inner-can-fulfill-buy-offer")

        ;; NFT in the nft-desc-1 doesn't exist yet, so this should fail
        (asserts! (is-eq
                    (err ERR_NO_SUCH_NFT)
                    (inner-can-fulfill-buy-offer? nft-desc-1 tx-sender u100)
                  )
            (err u0)
        )

        ;; make that NFT, and give it to tx-sender
        (unwrap-panic (inner-instantiate-nft nft-rec-1 tx-sender))

        ;; no buy offer yet, so this must fail
        (asserts! (is-eq
                    (err ERR_NO_BUY_OFFER)
                    (inner-can-fulfill-buy-offer? nft-desc-1 tx-sender u100)
                  )
            (err u1)
        )
    
        ;; add a buy offer from tx-sender that expires in block 102 (while we're in block 100)
        (unwrap-panic (inner-submit-buy-offer nft-desc-1 tx-sender u10 u102 none u100))

        ;; we can't fulfill an expired buy offer
        (asserts! (is-eq
                    (err ERR_NO_BUY_OFFER)
                    (inner-can-fulfill-buy-offer? nft-desc-1 tx-sender u102)
                  )
            (err u2)
        )

        ;; only the owner can fulfill this
        (asserts! (is-eq
                    (err ERR_PERMISSION_DENIED)
                    (inner-can-fulfill-buy-offer? nft-desc-1 buyer-1 u101)
                  )
            (err u3)
        )

        ;; the buy offer can't be fulfilled if the NFT is stacked
        ;; simulate an NFT lockup
        (map-set nft-lockups (unwrap-panic (map-get? claimed-nfts nft-rec-1)) u101)
        (asserts! (is-eq
                    (err ERR_PERMISSION_DENIED)
                    (inner-can-fulfill-buy-offer? nft-desc-1 tx-sender u100) 
                  )
            (err u4)
        )

        ;; would work in a later block though
        (asserts! (is-ok (inner-can-fulfill-buy-offer? nft-desc-1 tx-sender u101))
            (err u5)
        )

        ;; clean up
        (map-delete nft-lockups (unwrap-panic (map-get? claimed-nfts nft-rec-1)))
        (unwrap-panic (inner-reclaim-buy-offer nft-desc-1 { buyer: tx-sender, amount-ustx: u10, expires: u102 }))

        ;; no more buy offer
        (asserts! (is-none (map-get? nft-buy-offers nft-desc-1))
            (err u71)
        )

        ;; ustx restored
        (asserts! (is-eq tx-sender-stx-before (stx-get-balance tx-sender))
            (err u70)
        )
        (asserts! (is-eq contract-stx-before (stx-get-balance (as-contract tx-sender)))
            (err u7)
        )
        
        (ok true)
    )
)

(define-private (test-inner-fulfill-buy-offer)
    (let (
        (seller-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)

        ;; NOTE: this is 0 tickets, because for some reason, we trigger a runtime panic in the `stacker` crate on musl libc
        (nft-desc-1 0x11111111111111111111111111111111111111111111111111111111111111150000000000000000000000000000000100000000000000000000000000000000)
        (nft-rec-1 (unwrap-panic (parse-nft-desc nft-desc-1)))
        (tx-sender-stx-before (stx-get-balance tx-sender))
        (contract-stx-before (stx-get-balance (as-contract tx-sender)))
        (saved-tx-sender tx-sender)
        (seller-1-stx-before (stx-get-balance seller-1))
    )
        (print "run test-inner-fulfill-buy-offer")

        ;; make that NFT, and give it to seller-1
        (unwrap-panic (inner-instantiate-nft nft-rec-1 seller-1))
        
        ;; seller owns the NFT
        (asserts! (is-eq (nft-get-owner? nftree (unwrap-panic (map-get? claimed-nfts nft-rec-1))) (some seller-1))
            (err u10)
        )

        ;; add a buy offer from tx-sender that expires in block 102 (while we're in block 100)
        (unwrap-panic (inner-submit-buy-offer nft-desc-1 tx-sender u10 u102 none u100))

        (unwrap-panic
            (inner-fulfill-buy-offer
                (unwrap-panic (map-get? claimed-nfts nft-rec-1))
                nft-desc-1
                (unwrap-panic (map-get? nft-buy-offers nft-desc-1))
                seller-1
            )
        )

        ;; seller gets u10 ustx
        (asserts! (is-eq (+ u10 seller-1-stx-before) (stx-get-balance seller-1))
            (err u0)
        )

        ;; seller does not own the NFT; tx-sender does
        (asserts! (is-eq (nft-get-owner? nftree (unwrap-panic (map-get? claimed-nfts nft-rec-1))) (some tx-sender))
            (err u1)
        )

        ;; no more buy offer
        (asserts! (is-none (map-get? nft-buy-offers nft-desc-1))
            (err u71)
        )

        ;; tx-sender is out u10 ustx
        (asserts! (is-eq (- tx-sender-stx-before u10) (stx-get-balance tx-sender))
            (err u72)
        )

        ;; ustx quantity preserved in contract
        (asserts! (is-eq contract-stx-before (stx-get-balance (as-contract tx-sender)))
            (err u7)
        )

        (ok true)
    )
)

(define-private (test-inner-can-fulfill-mine-order)
    (let (
        (miner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)
        (merkle-tree (make-merkle-tree-8 0x0000000000000002))
        (nft-descs (get nft-descs merkle-tree))
        (nft-desc-1 (unwrap-panic (element-at nft-descs u0)))
        (nft-rec-1 (unwrap-panic (parse-nft-desc nft-desc-1)))
        (nft-id-1 (unwrap-panic (inner-register-nftree nft-rec-1 miner-1)))

        (root (get root merkle-tree))
        (rows (get rows merkle-tree))

        (nft-proof-1
            { hashes: (list
                (get-sibling-hash rows u0 u1)
                (get-sibling-hash rows u1 u1)
                (get-sibling-hash rows u2 u1)
              ),
              index: u0
            }
        )

        (tx-sender-stx-before (stx-get-balance tx-sender))
        (contract-stx-before (stx-get-balance (as-contract tx-sender)))
        (saved-tx-sender tx-sender)
        (miner-1-stx-before (stx-get-balance miner-1))

        ;; create parent to tx-sender
        (parent-nft-id (unwrap-panic (inner-register-nftree { tickets: u21, data-hash: root, size: u21 } tx-sender)))
    )
        (print "run test-inner-can-fulfill-mine-order")

        ;; fails without a buy offer
        (asserts! (is-eq
                    (err ERR_NO_BUY_OFFER)
                    (inner-can-fulfill-mine-order nft-desc-1 miner-1 parent-nft-id nft-proof-1 u100)
                  )
            (err u0)
        )

        ;; make a buy offer for the first NFT from tx-sender
        (unwrap-panic (inner-submit-buy-offer nft-desc-1 tx-sender u10 u101 none u100))

        ;; should succeed with correct data
        ;; (note: the implementation uses inner-can-claim-nft?, which is already tested above)
        (asserts! (is-eq
                    (ok { buy-offer: { buyer: tx-sender, amount-ustx: u10, expires: u101 }, nft-rec: nft-rec-1 })
                    (inner-can-fulfill-mine-order nft-desc-1 miner-1 parent-nft-id nft-proof-1 u100)
                  )
            (err u1)
        )

        (ok true)
    )
)

(define-private (test-inner-fulfill-mine-order)
    (let ( 
        (miner-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)
        (seller-1 'SP2FXK1EKMR9KDFJCMYD1M3P2ZSFJC0B2QGYD9746)
        (merkle-tree (make-merkle-tree-8 0x0000000000000003))
        (nft-descs (get nft-descs merkle-tree))
        (nft-desc-1 (unwrap-panic (element-at nft-descs u0)))
        (nft-rec-1 (unwrap-panic (parse-nft-desc nft-desc-1)))
        (nft-id-1 (unwrap-panic (inner-register-nftree nft-rec-1 miner-1)))

        (root (get root merkle-tree))
        (rows (get rows merkle-tree))

        (nft-proof-1
            { hashes: (list
                (get-sibling-hash rows u0 u1)
                (get-sibling-hash rows u1 u1)
                (get-sibling-hash rows u2 u1)
              ),
              index: u0
            }
        )

        (tx-sender-stx-before (stx-get-balance tx-sender))
        (contract-stx-before (stx-get-balance (as-contract tx-sender)))
        (saved-tx-sender tx-sender)
        (miner-1-stx-before (stx-get-balance miner-1))

        ;; create parent to tx-sender, and give it to the seller
        (parent-nft-id (let (
            (parent-nft-id (unwrap-panic (inner-register-nftree { tickets: u21, data-hash: root, size: u21 } tx-sender)))
        )
            (unwrap-panic (nft-transfer? nftree parent-nft-id tx-sender seller-1))
            parent-nft-id
        ))
        (parent-owner (unwrap-panic (nft-get-owner? nftree parent-nft-id)))
        (parent-balance-before (stx-get-balance parent-owner))
    )
        (print "run test-inner-fulfill-mine-order")

        ;; make a buy offer for the first NFT from tx-sender
        (unwrap-panic (inner-submit-buy-offer nft-desc-1 tx-sender u10 u101 none u100))

        (let (
            (nft-id (unwrap-panic (inner-fulfill-mine-order nft-desc-1 nft-rec-1 parent-nft-id (unwrap-panic (map-get? nft-buy-offers nft-desc-1)) miner-1)))
        )
            ;; NFT exists now, and it belongs to tx-sender
            (asserts! (is-eq (nft-get-owner? nftree nft-id) (some tx-sender))
                (err u0)
            )
        )

        ;; tx-sender spent the cost of the NFT
        (asserts! (is-eq (stx-get-balance tx-sender) (- tx-sender-stx-before u10))
            (err u1)
        )

        ;; contract holds the same amount of ustx, or more (due to rounding)
        (asserts! (>= (stx-get-balance (as-contract tx-sender)) contract-stx-before)
            (err u2)
        )

        ;; no more buy offer
        (asserts! (is-none (map-get? nft-buy-offers nft-desc-1))
            (err u3)
        )

        ;; miner gets the `(100 - CREATOR_COMMISSION) / 100`-percent of the STX
        (asserts! (is-eq
                    (+ miner-1-stx-before (/ (* u10 (- u100 CREATOR_COMMISSION)) u100))
                    (stx-get-balance miner-1)
                  )
            (err u4)
        )

        ;; NFT parent creator gets the `(CREATOR_COMMISSION / 100)`-percent of the STX
        (asserts! (is-eq
                    (+ parent-balance-before (/ (* u10 CREATOR_COMMISSION) u100))
                    (stx-get-balance parent-owner)
                  )
            (err u5)
        )

        (ok true)
    )
)

(define-private (test-inner-add-miner-to-block-multi)
    (let (
        (tx-sender-stx-before (stx-get-balance tx-sender))
        (contract-stx-before (stx-get-balance (as-contract tx-sender)))
    )
        (print "run test-inner-add-miner-to-block-multi")

        ;; must have sufficient balance
        (asserts! (is-eq
                    (err ERR_INSUFFICIENT_BALANCE)
                    (inner-add-miner-to-block-multi tx-sender-stx-before u2 tx-sender u10000)
                  )
            (err u0)
        )

        ;; valid num-blocks
        (asserts! (is-eq
                    (err ERR_INVALID_NUM_BLOCKS)
                    (inner-add-miner-to-block-multi u1 u0 tx-sender u10000)
                  )
            (err u2)
        )
        (asserts! (is-eq
                    (err ERR_INVALID_NUM_BLOCKS)
                    (inner-add-miner-to-block-multi u1 (+ u1 MAX_MINE_BLOCKS) tx-sender u10000)
                  )
            (err u2)
        )

        ;; valid uSTX
        (asserts! (is-eq
                    (err ERR_INVALID_USTX)
                    (inner-add-miner-to-block-multi u0 u1 tx-sender u10000)
                  )
            (err u3)
        )

        ;; should work
        (unwrap-panic (inner-add-miner-to-block-multi u12 u10 tx-sender u10000))

        (asserts! (is-eq
                    none
                    (map-get? miners-at-block { block: u10000, miner: tx-sender })
                  )
            (err u40)
        )
        (asserts! (is-eq
                    (some { range-start: u0, range-end: u12, claimed: false })
                    (map-get? miners-at-block { block: u10001, miner: tx-sender })
                  )
            (err u4)
        )
        (asserts! (is-eq
                    (some { range-start: u0, range-end: u12, claimed: false })
                    (map-get? miners-at-block { block: u10005, miner: tx-sender })
                  )
            (err u5)
        )
        (asserts! (is-eq
                    (some { range-start: u0, range-end: u12, claimed: false })
                    (map-get? miners-at-block { block: u10010, miner: tx-sender })
                  )
            (err u6)
        )
        (asserts! (is-eq
                    none
                    (map-get? miners-at-block { block: u10011, miner: tx-sender })
                  )
            (err u7)
        )

        (asserts! (is-eq
                    (some { tickets: u0, ustx: u12 })
                    (map-get? cycle-lockups (blk-to-cyc u10000))
                  )
            (err u80)
        )
        (asserts! (is-eq
                    (some { tickets: u0, ustx: u60 })
                    (map-get? cycle-lockups (+ u1 (blk-to-cyc u10000)))
                  )
            (err u8)
        )
        (asserts! (is-eq
                    (some { tickets: u0, ustx: u48 })
                    (map-get? cycle-lockups (+ u2 (blk-to-cyc u10000)))
                  )
            (err u9)
        )
        (asserts! (is-eq
                    none
                    (map-get? cycle-lockups (+ u3 (blk-to-cyc u10000)))
                  )
            (err u10)
        )

        (ok true)
    )
) 

(define-private (test-buff32-to-string-ascii)
    (begin
        (print "run test-buff32-to-string-ascii")
        (asserts! (is-eq
                    "a948904f2f0f479b8f8197694b30184b0d2ed1c1cd2a1ec0fb85d299a192a447"
                    (buff32-to-string-ascii 0xa948904f2f0f479b8f8197694b30184b0d2ed1c1cd2a1ec0fb85d299a192a447)
                  )
            (err u0)
        )
        (ok true)
    )
)

(define-private (test-get-token-url)
    (let (
        (seller-1 'SP1GNGP7GE7D8EK20R0CZ6H82STXAVPW8916C0EVC)

        (nft-desc-1 0x11111111111111111111111111111111111111111111111111111111111111160000000000000000000000000000000100000000000000000000000000000000)
        (nft-rec-1 (unwrap-panic (parse-nft-desc nft-desc-1)))
        (nft-id-1 (unwrap-panic (inner-instantiate-nft nft-rec-1 seller-1)))
    )
        (print "run test-get-token-url")

        (asserts! (is-eq
                    (ok (some "https://nftrees.com/nfts/1111111111111111111111111111111111111111111111111111111111111116"))
                    (get-token-url nft-id-1)
                  )
            (err u0)
        )
        (asserts! (is-eq
                    (ok none)
                    (get-token-url (+ u1 nft-id-1))
                  )
            (err u1)
        )

        (ok true)
    )
)

(define-public (unit-tests)
    (begin
        (try! (test-add-amount-for-block))
        (try! (test-add-miner-for-block))
        (try! (test-check-winner))
        (try! (test-inner-claim-tickets-at-block))
        (try! (test-read-uint))
        (try! (test-read-buff32))
        (try! (test-merkle-proof-root))
        (try! (test-nft-rec-to-desc))
        (try! (test-inner-register-nftree))
        (try! (test-inner-instantiate-nft))
        (try! (test-inner-can-claim-nft))
        (try! (test-inner-can-stack-nft))
        (try! (test-put-owner-lockup-at-cycle))
        (try! (test-inner-stack-nft-in-cycles))
        (try! (test-inner-set-rewards-claimed-for-cycle))
        (try! (test-inner-stacking-totals))
        (try! (test-inner-can-claim-stacking-rewards))
        (try! (test-inner-claim-stacking-rewards))
        (try! (test-inner-can-submit-buy-offer))
        (try! (test-inner-submit-buy-offer))
        (try! (test-inner-can-reclaim-buy-offer))
        (try! (test-inner-reclaim-buy-offer))
        (try! (test-inner-can-fulfill-buy-offer))
        (try! (test-inner-fulfill-buy-offer))
        (print "all unit tests pass")
        (ok u0)
    )
)

;; out of execution space in unit-test, so overflow to unit-tests-2
(define-public (unit-tests-2)
    (begin
        (try! (test-inner-can-fulfill-mine-order))
        (try! (test-inner-fulfill-mine-order))
        (try! (test-inner-add-miner-to-block-multi))
        (try! (test-buff32-to-string-ascii))
        (try! (test-get-token-url))
        (print "all unit tests 2 pass")
        (ok u0)
    )
)
