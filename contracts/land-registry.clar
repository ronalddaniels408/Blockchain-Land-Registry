;; Land Registry Smart Contract - Clarity v3
;; Manages property ownership, transfers, and registry operations

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-PROPERTY-NOT-FOUND (err u101))
(define-constant ERR-PROPERTY-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-OWNER (err u103))
(define-constant ERR-TRANSFER-PENDING (err u104))
(define-constant ERR-NO-PENDING-TRANSFER (err u105))
(define-constant ERR-INVALID-PRICE (err u106))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u107))
(define-constant ERR-MORTGAGE-NOT-FOUND (err u108))
(define-constant ERR-MORTGAGE-ALREADY-EXISTS (err u109))
(define-constant ERR-MORTGAGE-ACTIVE (err u110))
(define-constant ERR-INVALID-AMOUNT (err u111))

;; Data structures
(define-map properties
    { property-id: uint }
    {
        owner: principal,
        location: (string-ascii 256),
        size: uint,
        property-type: (string-ascii 64),
        value: uint,
        registration-date: uint,
        last-updated: uint,
        is-active: bool
    }
)

(define-map pending-transfers
    { property-id: uint }
    {
        from: principal,
        to: principal,
        price: uint,
        initiated-at: uint,
        expires-at: uint
    }
)

(define-map property-history
    { property-id: uint, transaction-id: uint }
    {
        previous-owner: principal,
        new-owner: principal,
        transaction-type: (string-ascii 32),
        price: uint,
        timestamp: uint
    }
)

(define-map mortgages
    { property-id: uint }
    {
        owner: principal,
        lender: principal,
        principal-amount: uint,
        remaining-balance: uint,
        interest-rate: uint,
        term-months: uint,
        monthly-payment: uint,
        start-date: uint,
        next-payment-due: uint,
        is-active: bool,
        payments-made: uint
    }
)

;; Contract state
(define-data-var property-counter uint u0)
(define-data-var transaction-counter uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var registry-fee uint u1000) ;; Fee in microSTX

;; Read-only functions
(define-read-only (get-property (property-id uint))
    (map-get? properties { property-id: property-id })
)

(define-read-only (get-property-owner (property-id uint))
    (match (map-get? properties { property-id: property-id })
        property-data (some (get owner property-data))
        none
    )
)

(define-read-only (get-pending-transfer (property-id uint))
    (map-get? pending-transfers { property-id: property-id })
)

(define-read-only (get-property-history (property-id uint) (transaction-id uint))
    (map-get? property-history { property-id: property-id, transaction-id: transaction-id })
)

(define-read-only (get-total-properties)
    (var-get property-counter)
)

(define-read-only (get-registry-fee)
    (var-get registry-fee)
)

(define-read-only (is-property-owner (property-id uint) (user principal))
    (match (get-property-owner property-id)
        owner (is-eq owner user)
        false
    )
)

(define-read-only (get-mortgage (property-id uint))
    (map-get? mortgages { property-id: property-id })
)

(define-read-only (has-active-mortgage (property-id uint))
    (match (get-mortgage property-id)
        mortgage-data (get is-active mortgage-data)
        false
    )
)

;; Property registration
(define-public (register-property 
    (location (string-ascii 256))
    (size uint)
    (property-type (string-ascii 64))
    (value uint)
)
    (let ((property-id (+ (var-get property-counter) u1))
          (current-block stacks-block-height))
        
        ;; Validate inputs
        (asserts! (> size u0) ERR-INVALID-PRICE)
        (asserts! (> value u0) ERR-INVALID-PRICE)
        (asserts! (> (len location) u0) ERR-UNAUTHORIZED)
        (asserts! (> (len property-type) u0) ERR-UNAUTHORIZED)
        
        ;; Check if property already exists (basic duplicate check)
        (asserts! (is-none (map-get? properties { property-id: property-id })) ERR-PROPERTY-ALREADY-EXISTS)
        
        ;; Register property
        (map-set properties
            { property-id: property-id }
            {
                owner: tx-sender,
                location: location,
                size: size,
                property-type: property-type,
                value: value,
                registration-date: current-block,
                last-updated: current-block,
                is-active: true
            }
        )
        
        ;; Update counter
        (var-set property-counter property-id)
        
        ;; Record transaction history
        (let ((transaction-id (+ (var-get transaction-counter) u1)))
            (map-set property-history
                { property-id: property-id, transaction-id: transaction-id }
                {
                    previous-owner: tx-sender,
                    new-owner: tx-sender,
                    transaction-type: "registration",
                    price: value,
                    timestamp: current-block
                }
            )
            (var-set transaction-counter transaction-id)
        )
        
        (ok property-id)
    )
)

;; Property transfer initiation
(define-public (initiate-transfer (property-id uint) (to principal) (price uint))
    (let ((property-data (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
          (current-block stacks-block-height))
        
        ;; Validate caller is property owner
        (asserts! (is-eq (get owner property-data) tx-sender) ERR-UNAUTHORIZED)
        
        ;; Validate inputs
        (asserts! (not (is-eq to tx-sender)) ERR-INVALID-OWNER)
        (asserts! (> price u0) ERR-INVALID-PRICE)
        (asserts! (get is-active property-data) ERR-PROPERTY-NOT-FOUND)
        
        ;; Check no pending transfer exists
        (asserts! (is-none (get-pending-transfer property-id)) ERR-TRANSFER-PENDING)
        
        ;; Create pending transfer (expires in 144 blocks ~ 24 hours)
        (map-set pending-transfers
            { property-id: property-id }
            {
                from: tx-sender,
                to: to,
                price: price,
                initiated-at: current-block,
                expires-at: (+ current-block u144) ;; ~24 hours in blocks
            }
        )
        
        (ok true)
    )
)

;; Complete property transfer
(define-public (complete-transfer (property-id uint))
    (let ((property-data (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
          (transfer-data (unwrap! (get-pending-transfer property-id) ERR-NO-PENDING-TRANSFER))
          (current-block stacks-block-height))
        
        ;; Validate caller is the intended recipient
        (asserts! (is-eq (get to transfer-data) tx-sender) ERR-UNAUTHORIZED)
        
        ;; Check transfer hasn't expired
        (asserts! (< current-block (get expires-at transfer-data)) ERR-NO-PENDING-TRANSFER)
        
        ;; Update property ownership
        (map-set properties
            { property-id: property-id }
            (merge property-data {
                owner: tx-sender,
                last-updated: current-block,
                value: (get price transfer-data)
            })
        )
        
        ;; Remove pending transfer
        (map-delete pending-transfers { property-id: property-id })
        
        ;; Record transaction history
        (let ((transaction-id (+ (var-get transaction-counter) u1)))
            (map-set property-history
                { property-id: property-id, transaction-id: transaction-id }
                {
                    previous-owner: (get from transfer-data),
                    new-owner: tx-sender,
                    transaction-type: "transfer",
                    price: (get price transfer-data),
                    timestamp: current-block
                }
            )
            (var-set transaction-counter transaction-id)
        )
        
        (ok true)
    )
)

;; Cancel pending transfer
(define-public (cancel-transfer (property-id uint))
    (let ((transfer-data (unwrap! (get-pending-transfer property-id) ERR-NO-PENDING-TRANSFER)))
        
        ;; Only initiator can cancel
        (asserts! (is-eq (get from transfer-data) tx-sender) ERR-UNAUTHORIZED)
        
        ;; Remove pending transfer
        (map-delete pending-transfers { property-id: property-id })
        
        (ok true)
    )
)

;; Property value update (owner only)
(define-public (update-property-value (property-id uint) (new-value uint))
    (let ((property-data (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
          (current-block stacks-block-height))
        
        ;; Validate caller is property owner
        (asserts! (is-eq (get owner property-data) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (> new-value u0) ERR-INVALID-PRICE)
        (asserts! (get is-active property-data) ERR-PROPERTY-NOT-FOUND)
        
        ;; Update property value
        (map-set properties
            { property-id: property-id }
            (merge property-data {
                value: new-value,
                last-updated: current-block
            })
        )
        
        (ok true)
    )
)

;; Deactivate property (owner only)
(define-public (deactivate-property (property-id uint))
    (let ((property-data (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
          (current-block stacks-block-height))
        
        ;; Validate caller is property owner
        (asserts! (is-eq (get owner property-data) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get is-active property-data) ERR-PROPERTY-NOT-FOUND)
        
        ;; Cancel any pending transfers
        (match (get-pending-transfer property-id)
            pending-transfer (map-delete pending-transfers { property-id: property-id })
            true
        )
        
        ;; Deactivate property
        (map-set properties
            { property-id: property-id }
            (merge property-data {
                is-active: false,
                last-updated: current-block
            })
        )
        
        (ok true)
    )
)

;; Administrative functions (contract owner only)
(define-public (set-registry-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
        (asserts! (> new-fee u0) ERR-INVALID-PRICE)
        (var-set registry-fee new-fee)
        (ok true)
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
        (asserts! (not (is-eq new-owner tx-sender)) ERR-INVALID-OWNER)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

;; Create mortgage for property
(define-public (create-mortgage
    (property-id uint)
    (lender principal)
    (principal-amount uint)
    (interest-rate uint)
    (term-months uint)
    (monthly-payment uint)
)
    (let ((property-data (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
          (current-block stacks-block-height))
        
        (asserts! (is-eq (get owner property-data) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (is-none (get-mortgage property-id)) ERR-MORTGAGE-ALREADY-EXISTS)
        (asserts! (> principal-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> monthly-payment u0) ERR-INVALID-AMOUNT)
        (asserts! (> term-months u0) ERR-INVALID-AMOUNT)
        (asserts! (get is-active property-data) ERR-PROPERTY-NOT-FOUND)
        (asserts! (not (is-eq lender tx-sender)) ERR-INVALID-OWNER)
        
        (map-set mortgages
            { property-id: property-id }
            {
                owner: tx-sender,
                lender: lender,
                principal-amount: principal-amount,
                remaining-balance: principal-amount,
                interest-rate: interest-rate,
                term-months: term-months,
                monthly-payment: monthly-payment,
                start-date: current-block,
                next-payment-due: (+ current-block u144),
                is-active: false,
                payments-made: u0
            }
        )
        
        (ok true)
    )
)

;; Accept mortgage offer (lender confirms)
(define-public (accept-mortgage (property-id uint))
    (let ((mortgage-data (unwrap! (get-mortgage property-id) ERR-MORTGAGE-NOT-FOUND)))
        
        (asserts! (is-eq (get lender mortgage-data) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (not (get is-active mortgage-data)) ERR-MORTGAGE-ACTIVE)
        
        (map-set mortgages
            { property-id: property-id }
            (merge mortgage-data {
                is-active: true
            })
        )
        
        (ok true)
    )
)

;; Make mortgage payment
(define-public (make-mortgage-payment (property-id uint) (payment-amount uint))
    (let ((mortgage-data (unwrap! (get-mortgage property-id) ERR-MORTGAGE-NOT-FOUND))
          (current-block stacks-block-height))
        
        (asserts! (is-eq (get owner mortgage-data) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get is-active mortgage-data) ERR-MORTGAGE-NOT-FOUND)
        (asserts! (> payment-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (>= payment-amount (get monthly-payment mortgage-data)) ERR-INSUFFICIENT-PAYMENT)
        
        (let ((new-balance (if (>= (get remaining-balance mortgage-data) payment-amount)
                                (- (get remaining-balance mortgage-data) payment-amount)
                                u0))
              (payments-count (+ (get payments-made mortgage-data) u1))
              (is-paid-off (is-eq new-balance u0)))
            
            (map-set mortgages
                { property-id: property-id }
                (merge mortgage-data {
                    remaining-balance: new-balance,
                    next-payment-due: (if is-paid-off current-block (+ current-block u144)),
                    is-active: (not is-paid-off),
                    payments-made: payments-count
                })
            )
            
            (ok true)
        )
    )
)

;; Default on mortgage (lender takes action)
(define-public (default-mortgage (property-id uint))
    (let ((mortgage-data (unwrap! (get-mortgage property-id) ERR-MORTGAGE-NOT-FOUND))
          (property-data (unwrap! (get-property property-id) ERR-PROPERTY-NOT-FOUND))
          (current-block stacks-block-height))
        
        (asserts! (is-eq (get lender mortgage-data) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get is-active mortgage-data) ERR-MORTGAGE-NOT-FOUND)
        (asserts! (> current-block (+ (get next-payment-due mortgage-data) u144)) ERR-INVALID-OWNER)
        
        (map-delete mortgages { property-id: property-id })
        (map-set properties
            { property-id: property-id }
            (merge property-data {
                owner: tx-sender,
                last-updated: current-block
            })
        )
        
        (let ((transaction-id (+ (var-get transaction-counter) u1)))
            (map-set property-history
                { property-id: property-id, transaction-id: transaction-id }
                {
                    previous-owner: (get owner mortgage-data),
                    new-owner: tx-sender,
                    transaction-type: "foreclosure",
                    price: (get remaining-balance mortgage-data),
                    timestamp: current-block
                }
            )
            (var-set transaction-counter transaction-id)
        )
        
        (ok true)
    )
)
