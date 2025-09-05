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

(define-map zoning-authorities principal bool)

(define-map land-zoning
    uint
    {
        zone-type: (string-ascii 30),
        max-building-height: uint,
        max-floor-area-ratio: uint,
        allowed-uses: (list 10 (string-ascii 20)),
        density-limit: uint,
        setback-requirements: uint,
        set-by: principal,
        effective-date: uint,
        expiry-date: (optional uint)
    }
)

(define-map development-permissions
    {parcel-id: uint, permit-id: uint}
    {
        applicant: principal,
        permit-type: (string-ascii 30),
        proposed-use: (string-ascii 20),
        building-height: uint,
        floor-area: uint,
        application-date: uint,
        status: (string-ascii 20),
        approved-by: (optional principal),
        approval-date: (optional uint),
        conditions: (string-ascii 256)
    }
)

(define-map parcel-permit-counter uint uint)

(define-data-var permit-counter uint u0)

(define-constant err-zoning-violation (err u104))
(define-constant err-permit-expired (err u105))
(define-constant err-invalid-authority (err u106))

(define-public (add-zoning-authority (authority principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set zoning-authorities authority true)
        (ok true)
    )
)

(define-public (remove-zoning-authority (authority principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-delete zoning-authorities authority)
        (ok true)
    )
)

(define-public (set-land-zoning (parcel-id uint) (zone-type (string-ascii 30)) (max-building-height uint) (max-floor-area-ratio uint) (allowed-uses (list 10 (string-ascii 20))) (density-limit uint) (setback-requirements uint) (expiry-date (optional uint)))
    (let ((sender tx-sender)
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found)))
        (asserts! (is-zoning-authority sender) err-not-authorized)
        (asserts! (> (len zone-type) u0) err-invalid-params)
        (asserts! (> (len allowed-uses) u0) err-invalid-params)
        
        (map-set land-zoning parcel-id {
            zone-type: zone-type,
            max-building-height: max-building-height,
            max-floor-area-ratio: max-floor-area-ratio,
            allowed-uses: allowed-uses,
            density-limit: density-limit,
            setback-requirements: setback-requirements,
            set-by: sender,
            effective-date: stacks-block-height,
            expiry-date: expiry-date
        })
        (ok true)
    )
)

(define-public (apply-for-development-permit (parcel-id uint) (permit-type (string-ascii 30)) (proposed-use (string-ascii 20)) (building-height uint) (floor-area uint) (conditions (string-ascii 256)))
    (let ((sender tx-sender)
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found))
          (current-counter (default-to u0 (map-get? parcel-permit-counter parcel-id)))
          (new-counter (+ current-counter u1))
          (global-permit-id (+ (var-get permit-counter) u1)))
        (asserts! (is-eq sender (get owner parcel)) err-not-authorized)
        (asserts! (> (len permit-type) u0) err-invalid-params)
        (asserts! (> (len proposed-use) u0) err-invalid-params)
        
        (var-set permit-counter global-permit-id)
        (map-set parcel-permit-counter parcel-id new-counter)
        (map-set development-permissions {parcel-id: parcel-id, permit-id: new-counter} {
            applicant: sender,
            permit-type: permit-type,
            proposed-use: proposed-use,
            building-height: building-height,
            floor-area: floor-area,
            application-date: stacks-block-height,
            status: "pending",
            approved-by: none,
            approval-date: none,
            conditions: conditions
        })
        (ok new-counter)
    )
)

(define-public (approve-development-permit (parcel-id uint) (permit-id uint))
    (let ((sender tx-sender)
          (permit-key {parcel-id: parcel-id, permit-id: permit-id})
          (permit (unwrap! (map-get? development-permissions permit-key) err-not-found))
          (zoning (unwrap! (map-get? land-zoning parcel-id) err-not-found)))
        (asserts! (is-zoning-authority sender) err-not-authorized)
        (asserts! (is-eq (get status permit) "pending") err-invalid-params)
        (try! (validate-zoning-compliance parcel-id permit zoning))
        
        (map-set development-permissions permit-key 
            (merge permit {
                status: "approved",
                approved-by: (some sender),
                approval-date: (some stacks-block-height)
            })
        )
        (ok true)
    )
)

(define-public (reject-development-permit (parcel-id uint) (permit-id uint) (reason (string-ascii 256)))
    (let ((sender tx-sender)
          (permit-key {parcel-id: parcel-id, permit-id: permit-id})
          (permit (unwrap! (map-get? development-permissions permit-key) err-not-found)))
        (asserts! (is-zoning-authority sender) err-not-authorized)
        (asserts! (is-eq (get status permit) "pending") err-invalid-params)
        (asserts! (> (len reason) u0) err-invalid-params)
        
        (map-set development-permissions permit-key 
            (merge permit {
                status: "rejected",
                approved-by: (some sender),
                approval-date: (some stacks-block-height),
                conditions: reason
            })
        )
        (ok true)
    )
)

(define-private (validate-zoning-compliance (parcel-id uint) (permit {applicant: principal, permit-type: (string-ascii 30), proposed-use: (string-ascii 20), building-height: uint, floor-area: uint, application-date: uint, status: (string-ascii 20), approved-by: (optional principal), approval-date: (optional uint), conditions: (string-ascii 256)}) (zoning {zone-type: (string-ascii 30), max-building-height: uint, max-floor-area-ratio: uint, allowed-uses: (list 10 (string-ascii 20)), density-limit: uint, setback-requirements: uint, set-by: principal, effective-date: uint, expiry-date: (optional uint)}))
    (let ((proposed-use (get proposed-use permit))
          (building-height (get building-height permit))
          (floor-area (get floor-area permit))
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found))
          (parcel-area (get area parcel))
          (floor-area-ratio (/ floor-area parcel-area)))
        (asserts! (<= building-height (get max-building-height zoning)) err-zoning-violation)
        (asserts! (<= floor-area-ratio (get max-floor-area-ratio zoning)) err-zoning-violation)
        (asserts! (is-some (index-of (get allowed-uses zoning) proposed-use)) err-zoning-violation)
        (match (get expiry-date zoning)
            expiry (asserts! (< stacks-block-height expiry) err-permit-expired)
            true
        )
        (ok true)
    )
)

(define-read-only (get-land-zoning (parcel-id uint))
    (map-get? land-zoning parcel-id)
)

(define-read-only (get-development-permit (parcel-id uint) (permit-id uint))
    (map-get? development-permissions {parcel-id: parcel-id, permit-id: permit-id})
)

(define-read-only (get-permit-count (parcel-id uint))
    (default-to u0 (map-get? parcel-permit-counter parcel-id))
)

(define-read-only (is-zoning-authority (authority principal))
    (default-to false (map-get? zoning-authorities authority))
)

(define-read-only (check-development-compliance (parcel-id uint) (proposed-use (string-ascii 20)) (building-height uint) (floor-area uint))
    (match (map-get? land-zoning parcel-id)
        zoning (match (map-get? land-registry parcel-id)
                   parcel (let ((parcel-area (get area parcel))
                                (floor-area-ratio (/ floor-area parcel-area)))
                              (and 
                                  (<= building-height (get max-building-height zoning))
                                  (<= floor-area-ratio (get max-floor-area-ratio zoning))
                                  (is-some (index-of (get allowed-uses zoning) proposed-use))
                                  (match (get expiry-date zoning)
                                      expiry (< stacks-block-height expiry)
                                      true
                                  )
                              )
                          )
                   false
               )
        false
        )
)

(define-map mortgages
    uint
    {
        borrower: principal,
        lender: principal,
        parcel-id: uint,
        loan-amount: uint,
        interest-rate: uint,
        duration-blocks: uint,
        start-block: uint,
        monthly-payment: uint,
        payments-made: uint,
        total-payments: uint,
        status: (string-ascii 20),
        collateral-locked: bool
    }
)

(define-map mortgage-payments
    {mortgage-id: uint, payment-number: uint}
    {
        amount: uint,
        payment-date: uint,
        interest-portion: uint,
        principal-portion: uint
    }
)

(define-data-var mortgage-counter uint u0)
(define-constant payment-interval-blocks u144)
(define-constant err-insufficient-collateral (err u107))
(define-constant err-payment-overdue (err u108))
(define-constant err-mortgage-inactive (err u109))

(define-public (create-mortgage (parcel-id uint) (loan-amount uint) (interest-rate uint) (duration-blocks uint))
    (let ((sender tx-sender)
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found))
          (mortgage-id (+ (var-get mortgage-counter) u1))
          (total-payments (/ duration-blocks payment-interval-blocks))
          (monthly-payment (/ (+ loan-amount (/ (* loan-amount interest-rate) u100)) total-payments)))
        (asserts! (is-eq sender (get owner parcel)) err-not-authorized)
        (asserts! (> loan-amount u0) err-invalid-params)
        (asserts! (> interest-rate u0) err-invalid-params)
        (asserts! (> duration-blocks payment-interval-blocks) err-invalid-params)
        (asserts! (is-eq (get status parcel) "active") err-invalid-params)
        
        (var-set mortgage-counter mortgage-id)
        (map-set mortgages mortgage-id {
            borrower: sender,
            lender: tx-sender,
            parcel-id: parcel-id,
            loan-amount: loan-amount,
            interest-rate: interest-rate,
            duration-blocks: duration-blocks,
            start-block: u0,
            monthly-payment: monthly-payment,
            payments-made: u0,
            total-payments: total-payments,
            status: "pending",
            collateral-locked: false
        })
        (ok mortgage-id)
    )
)

(define-public (fund-mortgage (mortgage-id uint))
    (let ((sender tx-sender)
          (mortgage (unwrap! (map-get? mortgages mortgage-id) err-not-found))
          (parcel-id (get parcel-id mortgage))
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found)))
        (asserts! (is-eq (get status mortgage) "pending") err-invalid-params)
        (asserts! (>= (stx-get-balance sender) (get loan-amount mortgage)) err-insufficient-collateral)
        
        (try! (stx-transfer? (get loan-amount mortgage) sender (get borrower mortgage)))
        (try! (nft-transfer? land-parcel parcel-id (get borrower mortgage) (as-contract tx-sender)))
        (map-set land-registry parcel-id 
            (merge parcel {status: "mortgaged"}))
        (map-set mortgages mortgage-id 
            (merge mortgage {
                lender: sender,
                start-block: stacks-block-height,
                status: "active",
                collateral-locked: true
            }))
        (ok true)
    )
)

(define-public (make-mortgage-payment (mortgage-id uint))
    (let ((sender tx-sender)
          (mortgage (unwrap! (map-get? mortgages mortgage-id) err-not-found))
          (payment-number (+ (get payments-made mortgage) u1))
          (payment-amount (get monthly-payment mortgage))
          (interest-portion (/ (* (get loan-amount mortgage) (get interest-rate mortgage)) (* u100 (get total-payments mortgage))))
          (principal-portion (- payment-amount interest-portion)))
        (asserts! (is-eq sender (get borrower mortgage)) err-not-authorized)
        (asserts! (is-eq (get status mortgage) "active") err-mortgage-inactive)
        (asserts! (>= (stx-get-balance sender) payment-amount) err-insufficient-collateral)
        (asserts! (< (get payments-made mortgage) (get total-payments mortgage)) err-invalid-params)
        
        (try! (stx-transfer? payment-amount sender (get lender mortgage)))
        (map-set mortgage-payments {mortgage-id: mortgage-id, payment-number: payment-number} {
            amount: payment-amount,
            payment-date: stacks-block-height,
            interest-portion: interest-portion,
            principal-portion: principal-portion
        })
        (map-set mortgages mortgage-id 
            (merge mortgage {payments-made: payment-number}))
        
        (if (is-eq payment-number (get total-payments mortgage))
            (complete-mortgage mortgage-id)
            (ok true)
        )
    )
)

(define-public (foreclose-mortgage (mortgage-id uint))
    (let ((sender tx-sender)
          (mortgage (unwrap! (map-get? mortgages mortgage-id) err-not-found))
          (parcel-id (get parcel-id mortgage))
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found))
          (expected-payment-block (+ (get start-block mortgage) (* (+ (get payments-made mortgage) u1) payment-interval-blocks))))
        (asserts! (is-eq sender (get lender mortgage)) err-not-authorized)
        (asserts! (is-eq (get status mortgage) "active") err-mortgage-inactive)
        (asserts! (> stacks-block-height (+ expected-payment-block payment-interval-blocks)) err-payment-overdue)
        (asserts! (< (get payments-made mortgage) (get total-payments mortgage)) err-invalid-params)
        
        (try! (nft-transfer? land-parcel parcel-id (as-contract tx-sender) sender))
        (map-set land-registry parcel-id 
            (merge parcel {
                owner: sender,
                status: "active",
                last-transfer: stacks-block-height
            }))
        (map-set mortgages mortgage-id 
            (merge mortgage {status: "foreclosed"}))
        (ok true)
    )
)

(define-private (complete-mortgage (mortgage-id uint))
    (let ((mortgage (unwrap! (map-get? mortgages mortgage-id) err-not-found))
          (parcel-id (get parcel-id mortgage))
          (parcel (unwrap! (map-get? land-registry parcel-id) err-not-found)))
        (try! (nft-transfer? land-parcel parcel-id (as-contract tx-sender) (get borrower mortgage)))
        (map-set land-registry parcel-id 
            (merge parcel {status: "active"}))
        (map-set mortgages mortgage-id 
            (merge mortgage {status: "completed"}))
        (ok true)
    )
)

(define-read-only (get-mortgage-details (mortgage-id uint))
    (map-get? mortgages mortgage-id)
)

(define-read-only (get-payment-history (mortgage-id uint) (payment-number uint))
    (map-get? mortgage-payments {mortgage-id: mortgage-id, payment-number: payment-number})
)

(define-read-only (calculate-remaining-balance (mortgage-id uint))
    (match (map-get? mortgages mortgage-id)
        mortgage (let ((total-amount (+ (get loan-amount mortgage) (/ (* (get loan-amount mortgage) (get interest-rate mortgage)) u100)))
                       (paid-amount (* (get payments-made mortgage) (get monthly-payment mortgage))))
                    (ok (- total-amount paid-amount)))
        err-not-found
    )
)

(define-read-only (is-payment-overdue (mortgage-id uint))
    (match (map-get? mortgages mortgage-id)
        mortgage (if (is-eq (get status mortgage) "active")
                     (let ((expected-payment-block (+ (get start-block mortgage) (* (+ (get payments-made mortgage) u1) payment-interval-blocks))))
                         (> stacks-block-height expected-payment-block))
                     false)
        false
    )
)