;; Reputation & Performance Scoring System
;; Advanced scoring system for auditors and DAOs to maintain platform quality

(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-INVALID-SCORE (err u501))
(define-constant ERR-PROFILE-NOT-FOUND (err u502))
(define-constant ERR-ALREADY-REVIEWED (err u503))
(define-constant ERR-INVALID-RATING (err u504))
(define-constant ERR-INSUFFICIENT-HISTORY (err u505))
(define-constant ERR-INVALID-TIMEFRAME (err u506))

;; Performance metrics for auditors
(define-map auditor-performance
    principal
    {
        total-score: uint,
        audit-count: uint,
        avg-completion-time: uint,
        quality-rating: uint,
        communication-score: uint,
        technical-score: uint,
        reliability-score: uint,
        last-updated: uint
    }
)

;; Performance metrics for DAOs
(define-map dao-performance
    principal
    {
        total-score: uint,
        audit-requests-count: uint,
        avg-response-time: uint,
        payment-reliability: uint,
        cooperation-score: uint,
        clarity-score: uint,
        last-updated: uint
    }
)

;; Individual audit reviews and ratings
(define-map audit-reviews
    { audit-id: uint, reviewer: principal }
    {
        auditor: principal,
        dao: principal,
        technical-rating: uint,
        communication-rating: uint,
        timeliness-rating: uint,
        overall-rating: uint,
        review-comments: (string-utf8 500),
        review-date: uint
    }
)

;; Historical performance tracking
(define-map monthly-performance
    { entity: principal, month: uint, year: uint }
    {
        score: uint,
        audit-count: uint,
        avg-rating: uint,
        improvement-rate: uint
    }
)

;; Platform-wide reputation tiers
(define-map reputation-tiers
    uint
    {
        tier-name: (string-ascii 20),
        min-score: uint,
        max-score: uint,
        benefits: (string-ascii 200),
        requirements: (string-ascii 200)
    }
)

;; Achievement system for exceptional performance
(define-map achievements
    { entity: principal, achievement-id: uint }
    {
        achievement-type: (string-ascii 30),
        description: (string-ascii 100),
        earned-date: uint,
        score-bonus: uint
    }
)

;; Performance improvement suggestions
(define-map performance-feedback
    { entity: principal, feedback-id: uint }
    {
        feedback-type: (string-ascii 20),
        suggestion: (string-utf8 300),
        priority-level: uint,
        created-date: uint,
        addressed: bool
    }
)

(define-data-var next-feedback-id uint u1)

;; Initialize reputation tiers
(define-public (initialize-reputation-tiers)
    (begin
        (map-set reputation-tiers u1 {
            tier-name: "Bronze",
            min-score: u0,
            max-score: u499,
            benefits: "Basic platform access",
            requirements: "Complete 2+ audits"
        })
        (map-set reputation-tiers u2 {
            tier-name: "Silver",
            min-score: u500,
            max-score: u799,
            benefits: "Priority matching, 5% bonus",
            requirements: "Maintain 4.0+ rating"
        })
        (map-set reputation-tiers u3 {
            tier-name: "Gold",
            min-score: u800,
            max-score: u949,
            benefits: "Premium visibility, 10% bonus",
            requirements: "Maintain 4.5+ rating"
        })
        (map-set reputation-tiers u4 {
            tier-name: "Platinum",
            min-score: u950,
            max-score: u1000,
            benefits: "Elite status, 15% bonus, priority support",
            requirements: "Maintain 4.8+ rating, 20+ audits"
        })
        (ok true)
    )
)

;; Submit review after audit completion
(define-public (submit-audit-review 
    (audit-id uint) 
    (auditor principal) 
    (dao principal) 
    (technical-rating uint) 
    (communication-rating uint) 
    (timeliness-rating uint) 
    (review-comments (string-utf8 500)))
    (let (
        (overall-rating (/ (+ technical-rating communication-rating timeliness-rating) u3))
    )
        (asserts! (and (>= technical-rating u1) (<= technical-rating u5)) ERR-INVALID-RATING)
        (asserts! (and (>= communication-rating u1) (<= communication-rating u5)) ERR-INVALID-RATING)
        (asserts! (and (>= timeliness-rating u1) (<= timeliness-rating u5)) ERR-INVALID-RATING)
        (asserts! (is-none (map-get? audit-reviews { audit-id: audit-id, reviewer: tx-sender })) ERR-ALREADY-REVIEWED)
        
        (map-set audit-reviews 
            { audit-id: audit-id, reviewer: tx-sender }
            {
                auditor: auditor,
                dao: dao,
                technical-rating: technical-rating,
                communication-rating: communication-rating,
                timeliness-rating: timeliness-rating,
                overall-rating: overall-rating,
                review-comments: review-comments,
                review-date: stacks-block-height
            }
        )
        
        ;; Update performance scores
        (unwrap-panic (update-auditor-performance auditor technical-rating communication-rating timeliness-rating))
        (unwrap-panic (update-dao-performance dao overall-rating))
        
        (ok true)
    )
)

;; Update auditor performance metrics
(define-private (update-auditor-performance 
    (auditor principal) 
    (tech-score uint) 
    (comm-score uint) 
    (time-score uint))
    (let (
        (current-performance (default-to 
            { total-score: u0, audit-count: u0, avg-completion-time: u0, quality-rating: u0, communication-score: u0, technical-score: u0, reliability-score: u0, last-updated: u0 }
            (map-get? auditor-performance auditor)))
        (new-audit-count (+ (get audit-count current-performance) u1))
        (new-tech-score (/ (+ (* (get technical-score current-performance) (get audit-count current-performance)) tech-score) new-audit-count))
        (new-comm-score (/ (+ (* (get communication-score current-performance) (get audit-count current-performance)) comm-score) new-audit-count))
        (new-reliability (/ (+ (* (get reliability-score current-performance) (get audit-count current-performance)) time-score) new-audit-count))
        (new-total-score (calculate-auditor-total-score new-tech-score new-comm-score new-reliability new-audit-count))
    )
        (map-set auditor-performance auditor {
            total-score: new-total-score,
            audit-count: new-audit-count,
            avg-completion-time: (get avg-completion-time current-performance),
            quality-rating: new-tech-score,
            communication-score: new-comm-score,
            technical-score: new-tech-score,
            reliability-score: new-reliability,
            last-updated: stacks-block-height
        })
        (ok true)
    )
)

;; Update DAO performance metrics
(define-private (update-dao-performance (dao principal) (rating uint))
    (let (
        (current-performance (default-to 
            { total-score: u0, audit-requests-count: u0, avg-response-time: u0, payment-reliability: u0, cooperation-score: u0, clarity-score: u0, last-updated: u0 }
            (map-get? dao-performance dao)))
        (new-request-count (+ (get audit-requests-count current-performance) u1))
        (new-cooperation-score (/ (+ (* (get cooperation-score current-performance) (get audit-requests-count current-performance)) rating) new-request-count))
        (new-total-score (calculate-dao-total-score new-cooperation-score (get payment-reliability current-performance) (get clarity-score current-performance)))
    )
        (map-set dao-performance dao {
            total-score: new-total-score,
            audit-requests-count: new-request-count,
            avg-response-time: (get avg-response-time current-performance),
            payment-reliability: (get payment-reliability current-performance),
            cooperation-score: new-cooperation-score,
            clarity-score: (get clarity-score current-performance),
            last-updated: stacks-block-height
        })
        (ok true)
    )
)

;; Calculate comprehensive auditor score
(define-private (calculate-auditor-total-score 
    (tech-score uint) 
    (comm-score uint) 
    (reliability uint) 
    (audit-count uint))
    (let (
        (base-score (/ (+ (* tech-score u40) (* comm-score u30) (* reliability u30)) u100))
        (experience-bonus (if (>= audit-count u20) u50 (if (>= audit-count u10) u25 (if (>= audit-count u5) u10 u0))))
        (final-score (+ (* base-score u180) experience-bonus))
    )
        (if (> final-score u1000) u1000 final-score)
    )
)

;; Calculate comprehensive DAO score
(define-private (calculate-dao-total-score 
    (cooperation uint) 
    (payment-reliability uint) 
    (clarity uint))
    (let (
        (weighted-score (/ (+ (* cooperation u40) (* payment-reliability u40) (* clarity u20)) u100))
        (final-score (* weighted-score u200))
    )
        (if (> final-score u1000) u1000 final-score)
    )
)

;; Award achievement for exceptional performance
(define-public (award-achievement 
    (entity principal) 
    (achievement-type (string-ascii 30)) 
    (description (string-ascii 100)) 
    (score-bonus uint))
    (let (
        (achievement-id u1) ;; Simplified - in real implementation would track next ID
    )
        (map-set achievements 
            { entity: entity, achievement-id: achievement-id }
            {
                achievement-type: achievement-type,
                description: description,
                earned-date: stacks-block-height,
                score-bonus: score-bonus
            }
        )
        (ok achievement-id)
    )
)

;; Provide performance improvement feedback
(define-public (provide-feedback 
    (entity principal) 
    (feedback-type (string-ascii 20)) 
    (suggestion (string-utf8 300)) 
    (priority-level uint))
    (let (
        (feedback-id (var-get next-feedback-id))
    )
        (asserts! (and (>= priority-level u1) (<= priority-level u5)) ERR-INVALID-RATING)
        
        (map-set performance-feedback 
            { entity: entity, feedback-id: feedback-id }
            {
                feedback-type: feedback-type,
                suggestion: suggestion,
                priority-level: priority-level,
                created-date: stacks-block-height,
                addressed: false
            }
        )
        
        (var-set next-feedback-id (+ feedback-id u1))
        (ok feedback-id)
    )
)

;; Get current reputation tier for entity
(define-read-only (get-reputation-tier (score uint))
    (if (<= score u499)
        (map-get? reputation-tiers u1)
        (if (<= score u799)
            (map-get? reputation-tiers u2)
            (if (<= score u949)
                (map-get? reputation-tiers u3)
                (map-get? reputation-tiers u4)
            )
        )
    )
)

;; Read-only functions
(define-read-only (get-auditor-performance (auditor principal))
    (map-get? auditor-performance auditor)
)

(define-read-only (get-dao-performance (dao principal))
    (map-get? dao-performance dao)
)

(define-read-only (get-audit-review (audit-id uint) (reviewer principal))
    (map-get? audit-reviews { audit-id: audit-id, reviewer: reviewer })
)

(define-read-only (get-monthly-performance (entity principal) (month uint) (year uint))
    (map-get? monthly-performance { entity: entity, month: month, year: year })
)

(define-read-only (get-performance-feedback (entity principal) (feedback-id uint))
    (map-get? performance-feedback { entity: entity, feedback-id: feedback-id })
)

;; Helper function to get all achievements for an entity (simplified)
(define-read-only (get-entity-achievements (entity principal))
    (list
        (map-get? achievements { entity: entity, achievement-id: u1 })
        (map-get? achievements { entity: entity, achievement-id: u2 })
        (map-get? achievements { entity: entity, achievement-id: u3 })
    )
)

(define-read-only (calculate-performance-trend (entity principal) (months uint))
    (let (
        (current-month u12) ;; Simplified - would use actual current month
        (current-year u2024) ;; Simplified - would use actual current year
        (current-performance (map-get? monthly-performance { entity: entity, month: current-month, year: current-year }))
        (previous-performance (map-get? monthly-performance { entity: entity, month: (- current-month u1), year: current-year }))
    )
        (match current-performance
            current (match previous-performance
                previous (some {
                    trend-direction: (if (> (get score current) (get score previous)) "increasing" "decreasing"),
                    score-change: (if (> (get score current) (get score previous)) 
                        (- (get score current) (get score previous))
                        (- (get score previous) (get score current))),
                    performance-consistency: (get avg-rating current)
                })
                none
            )
            none
        )
    )
)
