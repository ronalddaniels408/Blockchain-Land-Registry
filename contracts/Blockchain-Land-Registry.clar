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
