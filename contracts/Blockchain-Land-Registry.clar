(define-non-fungible-token land-parcel uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-already-listed (err u101))
(define-constant err-not-found (err u102))
(define-constant err-invalid-params (err u103))

(define-map land-registry
    uint 
    {
        owner: principal,
        gps-lat: int,
        gps-long: int,
        area: uint,
        registration-date: uint,
        last-transfer: uint,
        legal-doc-hash: (string-ascii 64),
        status: (string-ascii 20)
    }
)

(define-map transfer-requests
    uint 
    {
        from: principal,
        to: principal,
        price: uint,
        request-date: uint,
        status: (string-ascii 20)
    }
)

(define-map dao-members principal bool)

(define-public (register-land-parcel (parcel-id uint) (gps-lat int) (gps-long int) (area uint) (legal-doc-hash (string-ascii 64)))
    (let ((sender tx-sender))
        (asserts! (is-eq sender contract-owner) err-not-authorized)
        (asserts! (is-none (nft-get-owner? land-parcel parcel-id)) err-already-listed)
        
        (try! (nft-mint? land-parcel parcel-id sender))
        (map-set land-registry parcel-id {
            owner: sender,
            gps-lat: gps-lat,
            gps-long: gps-long,
            area: area,
            registration-date: stacks-block-height,
            last-transfer: stacks-block-height,
            legal-doc-hash: legal-doc-hash,
            status: "active"
        })
        (ok true)
    )
)

(define-public (initiate-transfer (parcel-id uint) (to principal) (price uint))
    (let ((sender tx-sender)
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found)))
        (asserts! (is-eq (get owner parcel) sender) err-not-authorized)
        (map-set transfer-requests parcel-id {
            from: sender,
            to: to,
            price: price,
            request-date: stacks-block-height,
            status: "pending"
        })
        (ok true)
    )
)

(define-public (accept-transfer (parcel-id uint))
    (let ((sender tx-sender)
          (transfer-req (unwrap! (map-get? transfer-requests parcel-id) err-not-found))
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found)))
        (asserts! (is-eq sender (get to transfer-req)) err-not-authorized)
        (try! (nft-transfer? land-parcel parcel-id (get from transfer-req) sender))
        (map-set land-registry parcel-id 
            (merge parcel {
                owner: sender,
                last-transfer: stacks-block-height
            })
        )
        (map-set transfer-requests parcel-id 
            (merge transfer-req {
                status: "completed"
            })
        )
        (ok true)
    )
)

(define-public (add-dao-member (member principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set dao-members member true)
        (ok true)
    )
)

(define-public (remove-dao-member (member principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-delete dao-members member)
        (ok true)
    )
)

(define-read-only (get-land-details (parcel-id uint))
    (map-get? land-registry parcel-id)
)

(define-read-only (get-transfer-details (parcel-id uint))
    (map-get? transfer-requests parcel-id)
)

(define-read-only (is-dao-member (member principal))
    (default-to false (map-get? dao-members member))
)

(define-map disputes
    uint
    {
        parcel-id: uint,
        complainant: principal,
        defendant: principal,
        reason: (string-ascii 256),
        created-at: uint,
        status: (string-ascii 20),
        votes-for: uint,
        votes-against: uint,
        resolution-deadline: uint
    }
)

(define-map dispute-votes
    {dispute-id: uint, voter: principal}
    {vote: bool, voted-at: uint}
)

(define-data-var dispute-counter uint u0)
(define-constant dispute-duration u144)
(define-constant min-dao-votes u3)

(define-public (create-dispute (parcel-id uint) (defendant principal) (reason (string-ascii 256)))
    (let ((dispute-id (+ (var-get dispute-counter) u1))
          (sender tx-sender)
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found)))
        (asserts! (not (is-eq sender defendant)) err-invalid-params)
        (asserts! (> (len reason) u0) err-invalid-params)
        
        (var-set dispute-counter dispute-id)
        (map-set disputes dispute-id {
            parcel-id: parcel-id,
            complainant: sender,
            defendant: defendant,
            reason: reason,
            created-at: stacks-block-height,
            status: "active",
            votes-for: u0,
            votes-against: u0,
            resolution-deadline: (+ stacks-block-height dispute-duration)
        })
        (ok dispute-id)
    )
)

(define-public (vote-on-dispute (dispute-id uint) (vote-for bool))
    (let ((sender tx-sender)
          (dispute (unwrap! (map-get? disputes dispute-id) err-not-found))
          (vote-key {dispute-id: dispute-id, voter: sender}))
        (asserts! (is-dao-member sender) err-not-authorized)
        (asserts! (is-eq (get status dispute) "active") err-invalid-params)
        (asserts! (< stacks-block-height (get resolution-deadline dispute)) err-invalid-params)
        (asserts! (is-none (map-get? dispute-votes vote-key)) err-already-listed)
        
        (map-set dispute-votes vote-key {
            vote: vote-for,
            voted-at: stacks-block-height
        })
        
        (if vote-for
            (map-set disputes dispute-id 
                (merge dispute {votes-for: (+ (get votes-for dispute) u1)}))
            (map-set disputes dispute-id 
                (merge dispute {votes-against: (+ (get votes-against dispute) u1)}))
        )
        (ok true)
    )
)

(define-public (resolve-dispute (dispute-id uint))
    (let ((dispute (unwrap! (map-get? disputes dispute-id) err-not-found))
          (total-votes (+ (get votes-for dispute) (get votes-against dispute)))
          (parcel-id (get parcel-id dispute))
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found)))
        (asserts! (is-eq (get status dispute) "active") err-invalid-params)
        (asserts! (>= stacks-block-height (get resolution-deadline dispute)) err-invalid-params)
        (asserts! (>= total-votes min-dao-votes) err-invalid-params)
        
        (if (> (get votes-for dispute) (get votes-against dispute))
            (begin
                (try! (nft-transfer? land-parcel parcel-id (get defendant dispute) (get complainant dispute)))
                (map-set land-registry parcel-id 
                    (merge parcel {
                        owner: (get complainant dispute),
                        last-transfer: stacks-block-height
                    }))
                (map-set disputes dispute-id (merge dispute {status: "resolved-for"}))
            )
            (map-set disputes dispute-id (merge dispute {status: "resolved-against"}))
        )
        (ok true)
    )
)

(define-read-only (get-dispute-details (dispute-id uint))
    (map-get? disputes dispute-id)
)

(define-read-only (get-user-vote (dispute-id uint) (voter principal))
    (map-get? dispute-votes {dispute-id: dispute-id, voter: voter})
)

(define-map valuation-history
    {parcel-id: uint, entry-id: uint}
    {
        valuation: uint,
        valuator: principal,
        valuation-date: uint,
        transaction-based: bool,
        area-price-per-unit: uint
    }
)

(define-map parcel-valuation-counter uint uint)

(define-map certified-valuators principal bool)

(define-public (add-certified-valuator (valuator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set certified-valuators valuator true)
        (ok true)
    )
)

(define-public (remove-certified-valuator (valuator principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-delete certified-valuators valuator)
        (ok true)
    )
)

(define-public (add-valuation (parcel-id uint) (valuation uint))
    (let ((sender tx-sender)
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found))
          (current-counter (default-to u0 (map-get? parcel-valuation-counter parcel-id)))
          (new-counter (+ current-counter u1))
          (price-per-unit (/ valuation (get area parcel))))
        (asserts! (is-certified-valuator sender) err-not-authorized)
        (asserts! (> valuation u0) err-invalid-params)
        
        (map-set parcel-valuation-counter parcel-id new-counter)
        (map-set valuation-history {parcel-id: parcel-id, entry-id: new-counter} {
            valuation: valuation,
            valuator: sender,
            valuation-date: stacks-block-height,
            transaction-based: false,
            area-price-per-unit: price-per-unit
        })
        (ok true)
    )
)

(define-private (record-transaction-valuation (parcel-id uint) (price uint))
    (let ((parcel (unwrap! (map-get? land-registry parcel-id) err-not-found))
          (current-counter (default-to u0 (map-get? parcel-valuation-counter parcel-id)))
          (new-counter (+ current-counter u1))
          (price-per-unit (/ price (get area parcel))))
        (map-set parcel-valuation-counter parcel-id new-counter)
        (map-set valuation-history {parcel-id: parcel-id, entry-id: new-counter} {
            valuation: price,
            valuator: tx-sender,
            valuation-date: stacks-block-height,
            transaction-based: true,
            area-price-per-unit: price-per-unit
        })
        (ok true)
    )
)

(define-public (accept-transfer-with-valuation (parcel-id uint))
    (let ((sender tx-sender)
          (transfer-req (unwrap! (map-get? transfer-requests parcel-id) err-not-found))
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found)))
        (asserts! (is-eq sender (get to transfer-req)) err-not-authorized)
        (try! (nft-transfer? land-parcel parcel-id (get from transfer-req) sender))
        (try! (record-transaction-valuation parcel-id (get price transfer-req)))
        (map-set land-registry parcel-id 
            (merge parcel {
                owner: sender,
                last-transfer: stacks-block-height
            })
        )
        (map-set transfer-requests parcel-id 
            (merge transfer-req {
                status: "completed"
            })
        )
        (ok true)
    )
)

(define-read-only (get-latest-valuation (parcel-id uint))
    (let ((counter (default-to u0 (map-get? parcel-valuation-counter parcel-id))))
        (if (> counter u0)
            (map-get? valuation-history {parcel-id: parcel-id, entry-id: counter})
            none
        )
    )
)

(define-read-only (get-valuation-history (parcel-id uint) (entry-id uint))
    (map-get? valuation-history {parcel-id: parcel-id, entry-id: entry-id})
)

(define-read-only (get-valuation-count (parcel-id uint))
    (default-to u0 (map-get? parcel-valuation-counter parcel-id))
)

(define-read-only (is-certified-valuator (valuator principal))
    (default-to false (map-get? certified-valuators valuator))
)

(define-read-only (estimate-land-value (area uint))
    (ok (/ (* area u1000) u1))
)