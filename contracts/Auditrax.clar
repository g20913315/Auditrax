;; title: Auditrax
;; version: 1.0.0
;; summary: DAO Audit Protocol - Hire auditors through trustless contracts
;; description: A decentralized platform for connecting DAOs with auditors through smart contracts

;; traits

;; token definitions

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_STATUS (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_DEADLINE_PASSED (err u106))
(define-constant ERR_AUDIT_NOT_COMPLETE (err u107))
(define-constant ERR_ALREADY_SUBMITTED (err u108))

(define-constant AUDIT_STATUS_OPEN u0)
(define-constant AUDIT_STATUS_ASSIGNED u1)
(define-constant AUDIT_STATUS_IN_PROGRESS u2)
(define-constant AUDIT_STATUS_SUBMITTED u3)
(define-constant AUDIT_STATUS_APPROVED u4)
(define-constant AUDIT_STATUS_REJECTED u5)
(define-constant AUDIT_STATUS_COMPLETED u6)

(define-constant MIN_AUDIT_REWARD u1000000)
(define-constant PLATFORM_FEE_PERCENT u5)

;; data vars
(define-data-var next-audit-id uint u1)
(define-data-var platform-fee-collected uint u0)

;; data maps
(define-map audit-requests
  uint
  {
    dao: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    reward: uint,
    deadline: uint,
    status: uint,
    auditor: (optional principal),
    created-at: uint
  }
)

(define-map audit-submissions
  uint
  {
    auditor: principal,
    report-hash: (string-ascii 64),
    submitted-at: uint,
    approved: bool
  }
)

(define-map auditor-profiles
  principal
  {
    name: (string-ascii 50),
    reputation-score: uint,
    total-audits: uint,
    successful-audits: uint,
    registered-at: uint
  }
)

(define-map dao-profiles
  principal
  {
    name: (string-ascii 50),
    total-requests: uint,
    completed-audits: uint,
    registered-at: uint
  }
)

(define-map audit-escrow
  uint
  uint
)

(define-map auditor-applications
  { audit-id: uint, auditor: principal }
  {
    applied-at: uint,
    message: (string-ascii 200)
  }
)

;; public functions
(define-public (register-dao (name (string-ascii 50)))
  (let ((dao-data {
    name: name,
    total-requests: u0,
    completed-audits: u0,
    registered-at: stacks-block-height
  }))
    (map-set dao-profiles tx-sender dao-data)
    (ok true)
  )
)

(define-public (register-auditor (name (string-ascii 50)))
  (let ((auditor-data {
    name: name,
    reputation-score: u100,
    total-audits: u0,
    successful-audits: u0,
    registered-at: stacks-block-height
  }))
    (map-set auditor-profiles tx-sender auditor-data)
    (ok true)
  )
)

(define-public (create-audit-request 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (reward uint)
  (deadline uint))
  (let (
    (audit-id (var-get next-audit-id))
    (platform-fee (/ (* reward PLATFORM_FEE_PERCENT) u100))
    (total-amount (+ reward platform-fee))
  )
    (asserts! (>= reward MIN_AUDIT_REWARD) ERR_INVALID_AMOUNT)
    (asserts! (> deadline stacks-block-height) ERR_DEADLINE_PASSED)
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    (map-set audit-requests audit-id {
      dao: tx-sender,
      title: title,
      description: description,
      reward: reward,
      deadline: deadline,
      status: AUDIT_STATUS_OPEN,
      auditor: none,
      created-at: stacks-block-height
    })
    
    (map-set audit-escrow audit-id total-amount)
    (var-set next-audit-id (+ audit-id u1))
    
    ;; (match (map-get? dao-profiles tx-sender)
    ;;   dao-profile (begin
    ;;     (map-set dao-profiles tx-sender 
    ;;       (merge dao-profile { total-requests: (+ (get total-requests dao-profile) u1) }))
    ;;     true)
    ;;   _ (begin
    ;;     (register-dao "Unknown DAO")
    ;;     true)
    ;; )
    
    (ok audit-id)
  )
)

(define-public (apply-for-audit (audit-id uint) (message (string-ascii 200)))
  (let ((audit-request (unwrap! (map-get? audit-requests audit-id) ERR_NOT_FOUND)))
    (asserts! (is-some (map-get? auditor-profiles tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status audit-request) AUDIT_STATUS_OPEN) ERR_INVALID_STATUS)
    (asserts! (< stacks-block-height (get deadline audit-request)) ERR_DEADLINE_PASSED)
    
    (map-set auditor-applications 
      { audit-id: audit-id, auditor: tx-sender }
      { applied-at: stacks-block-height, message: message }
    )
    (ok true)
  )
)

(define-public (assign-auditor (audit-id uint) (auditor principal))
  (let ((audit-request (unwrap! (map-get? audit-requests audit-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get dao audit-request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status audit-request) AUDIT_STATUS_OPEN) ERR_INVALID_STATUS)
    (asserts! (is-some (map-get? auditor-profiles auditor)) ERR_NOT_FOUND)
    (asserts! (< stacks-block-height (get deadline audit-request)) ERR_DEADLINE_PASSED)
    
    (map-set audit-requests audit-id 
      (merge audit-request { 
        status: AUDIT_STATUS_ASSIGNED,
        auditor: (some auditor)
      })
    )
    (ok true)
  )
)

(define-public (start-audit (audit-id uint))
  (let ((audit-request (unwrap! (map-get? audit-requests audit-id) ERR_NOT_FOUND)))
    (asserts! (is-eq (some tx-sender) (get auditor audit-request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status audit-request) AUDIT_STATUS_ASSIGNED) ERR_INVALID_STATUS)
    
    (map-set audit-requests audit-id 
      (merge audit-request { status: AUDIT_STATUS_IN_PROGRESS })
    )
    (ok true)
  )
)

(define-public (submit-audit (audit-id uint) (report-hash (string-ascii 64)))
  (let ((audit-request (unwrap! (map-get? audit-requests audit-id) ERR_NOT_FOUND)))
    (asserts! (is-eq (some tx-sender) (get auditor audit-request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status audit-request) AUDIT_STATUS_IN_PROGRESS) ERR_INVALID_STATUS)
    (asserts! (is-none (map-get? audit-submissions audit-id)) ERR_ALREADY_SUBMITTED)
    
    (map-set audit-submissions audit-id {
      auditor: tx-sender,
      report-hash: report-hash,
      submitted-at: stacks-block-height,
      approved: false
    })
    
    (map-set audit-requests audit-id 
      (merge audit-request { status: AUDIT_STATUS_SUBMITTED })
    )
    (ok true)
  )
)

(define-public (approve-audit (audit-id uint))
  (let (
    (audit-request (unwrap! (map-get? audit-requests audit-id) ERR_NOT_FOUND))
    (submission (unwrap! (map-get? audit-submissions audit-id) ERR_NOT_FOUND))
    (escrow-amount (unwrap! (map-get? audit-escrow audit-id) ERR_NOT_FOUND))
    (reward (get reward audit-request))
    (platform-fee (- escrow-amount reward))
    (auditor (get auditor submission))
  )
    (asserts! (is-eq tx-sender (get dao audit-request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status audit-request) AUDIT_STATUS_SUBMITTED) ERR_INVALID_STATUS)
    
    (try! (as-contract (stx-transfer? reward tx-sender auditor)))
    (var-set platform-fee-collected (+ (var-get platform-fee-collected) platform-fee))
    
    (map-set audit-submissions audit-id 
      (merge submission { approved: true })
    )
    
    (map-set audit-requests audit-id 
      (merge audit-request { status: AUDIT_STATUS_COMPLETED })
    )
    
    (map-delete audit-escrow audit-id)
    
    (match (map-get? auditor-profiles auditor)
      auditor-profile (map-set auditor-profiles auditor
        (merge auditor-profile {
          total-audits: (+ (get total-audits auditor-profile) u1),
          successful-audits: (+ (get successful-audits auditor-profile) u1),
          reputation-score: (+ (get reputation-score auditor-profile) u10)
        }))
      false
    )
    
    (match (map-get? dao-profiles tx-sender)
      dao-profile (map-set dao-profiles tx-sender
        (merge dao-profile { completed-audits: (+ (get completed-audits dao-profile) u1) }))
      false
    )
    
    (ok true)
  )
)

(define-public (reject-audit (audit-id uint))
  (let ((audit-request (unwrap! (map-get? audit-requests audit-id) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get dao audit-request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status audit-request) AUDIT_STATUS_SUBMITTED) ERR_INVALID_STATUS)
    
    (map-set audit-requests audit-id 
      (merge audit-request { status: AUDIT_STATUS_REJECTED })
    )
    (ok true)
  )
)

(define-public (cancel-audit-request (audit-id uint))
  (let (
    (audit-request (unwrap! (map-get? audit-requests audit-id) ERR_NOT_FOUND))
    (escrow-amount (unwrap! (map-get? audit-escrow audit-id) ERR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get dao audit-request)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status audit-request) AUDIT_STATUS_OPEN) ERR_INVALID_STATUS)
    
    (try! (as-contract (stx-transfer? escrow-amount tx-sender (get dao audit-request))))
    (map-delete audit-escrow audit-id)
    (map-delete audit-requests audit-id)
    (ok true)
  )
)

(define-public (withdraw-platform-fees)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (let ((fees (var-get platform-fee-collected)))
      (var-set platform-fee-collected u0)
      (try! (as-contract (stx-transfer? fees tx-sender CONTRACT_OWNER)))
      (ok fees)
    )
  )
)

;; read only functions
(define-read-only (get-audit-request (audit-id uint))
  (map-get? audit-requests audit-id)
)

(define-read-only (get-audit-submission (audit-id uint))
  (map-get? audit-submissions audit-id)
)

(define-read-only (get-auditor-profile (auditor principal))
  (map-get? auditor-profiles auditor)
)

(define-read-only (get-dao-profile (dao principal))
  (map-get? dao-profiles dao)
)

(define-read-only (get-auditor-application (audit-id uint) (auditor principal))
  (map-get? auditor-applications { audit-id: audit-id, auditor: auditor })
)

(define-read-only (get-platform-fee-collected)
  (var-get platform-fee-collected)
)

(define-read-only (get-next-audit-id)
  (var-get next-audit-id)
)

(define-read-only (get-escrow-amount (audit-id uint))
  (map-get? audit-escrow audit-id)
)

;; private functions