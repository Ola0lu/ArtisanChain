;; ArtisanChain - Artisan craft verification and authentication network
;; Artisans earn tokens based on craft verification and quality endorsements

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_ALREADY_VERIFIED (err u104))
(define-constant ERR_ALREADY_RATED (err u105))
(define-constant ERR_SELF_RATING (err u106))
(define-constant ERR_EMPTY_STRING (err u107))
(define-constant ERR_INVALID_RATING (err u108))
(define-constant ERR_INVALID_CRAFT_ID (err u109))
(define-constant ERR_EMPTY_HASH (err u110))

;; Constants
(define-constant MAX_RATING u5)
(define-constant VALIDATION_REWARD u10)
(define-constant EXCELLENCE_REWARD u20)
(define-constant AUTHENTICATION_REWARD u50)

;; Data maps
(define-map artisans
  { artisan-id: principal }
  { name: (string-ascii 50), craft-type: (string-ascii 20), reputation: uint, tokens: uint, authenticated: bool }
)

(define-map craft-items
  { craft-id: uint }
  { 
    creator: principal, 
    description: (string-ascii 500), 
    craft-hash: (buff 32),
    timestamp: uint, 
    verified: bool,
    verification-count: uint,
    endorsement-count: uint,
    quality-rating: uint,
    rating-count: uint
  }
)

(define-map craft-verifications
  { craft-id: uint, verifier: principal }
  { verified: bool }
)

(define-map craft-endorsements
  { craft-id: uint, endorser: principal }
  { endorsement-level: uint, endorsement-date: uint }
)

(define-map quality-ratings
  { craft-id: uint, rater: principal }
  { rating: uint }
)

;; Variables
(define-data-var next-craft-id uint u1)
(define-data-var action-counter uint u0)

;; Helper functions
(define-private (is-valid-craft-id (craft-id uint))
  (< craft-id (var-get next-craft-id))
)

;; Artisan functions
(define-public (register-artisan (name (string-ascii 50)) (craft-type (string-ascii 20)))
  (let ((caller tx-sender))
    (asserts! (> (len name) u0) ERR_EMPTY_STRING)
    (asserts! (or (is-eq craft-type "textile") (is-eq craft-type "pottery") (is-eq craft-type "jewelry")) ERR_INVALID_INPUT)
    (asserts! (is-none (map-get? artisans {artisan-id: caller})) ERR_ALREADY_EXISTS)
    (ok (map-set artisans 
      {artisan-id: caller} 
      {name: name, craft-type: craft-type, reputation: u0, tokens: u100, authenticated: false}))
  )
)

(define-public (update-artisan (name (string-ascii 50)) (craft-type (string-ascii 20)))
  (let ((caller tx-sender))
    (asserts! (> (len name) u0) ERR_EMPTY_STRING)
    (asserts! (or (is-eq craft-type "textile") (is-eq craft-type "pottery") (is-eq craft-type "jewelry")) ERR_INVALID_INPUT)
    (asserts! (is-some (map-get? artisans {artisan-id: caller})) ERR_NOT_FOUND)
    (ok (map-set artisans 
      {artisan-id: caller} 
      (merge (unwrap! (map-get? artisans {artisan-id: caller}) ERR_NOT_FOUND)
             {name: name, craft-type: craft-type})))
  )
)

;; Craft functions
(define-public (register-craft (description (string-ascii 500)) (craft-hash (buff 32)))
  (let ((caller tx-sender)
        (craft-id (var-get next-craft-id)))
    (asserts! (> (len description) u0) ERR_EMPTY_STRING)
    (asserts! (> (len craft-hash) u0) ERR_EMPTY_HASH)
    (asserts! (is-some (map-get? artisans {artisan-id: caller})) ERR_NOT_FOUND)
    (var-set action-counter (+ (var-get action-counter) u1))
    
    (map-set craft-items 
      {craft-id: craft-id} 
      { 
        creator: caller, 
        description: description, 
        craft-hash: craft-hash,
        timestamp: (var-get action-counter), 
        verified: false,
        verification-count: u0,
        endorsement-count: u0,
        quality-rating: u0,
        rating-count: u0
      })
    (var-set next-craft-id (+ craft-id u1))
    (ok craft-id)
  )
)

(define-public (verify-craft (craft-id uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-craft-id craft-id) ERR_INVALID_CRAFT_ID)
    (asserts! (is-some (map-get? artisans {artisan-id: caller})) ERR_NOT_FOUND)
    (asserts! (is-some (map-get? craft-items {craft-id: craft-id})) ERR_NOT_FOUND)
    
    (let ((craft (unwrap! (map-get? craft-items {craft-id: craft-id}) ERR_NOT_FOUND)))
      (asserts! (not (is-eq caller (get creator craft))) ERR_SELF_RATING)
      (asserts! (is-none (map-get? craft-verifications {craft-id: craft-id, verifier: caller})) ERR_ALREADY_VERIFIED)
      
      (map-set craft-verifications 
        {craft-id: craft-id, verifier: caller} 
        {verified: true})
      
      (let ((new-verification-count (+ (get verification-count craft) u1))
            (craft-creator (unwrap! (map-get? artisans {artisan-id: (get creator craft)}) ERR_NOT_FOUND))
            (verifier-artisan (unwrap! (map-get? artisans {artisan-id: caller}) ERR_NOT_FOUND)))
        
        (map-set craft-items 
          {craft-id: craft-id} 
          (merge craft {
            verification-count: new-verification-count,
            verified: (>= new-verification-count u3)
          }))
        
        (map-set artisans 
          {artisan-id: caller} 
          (merge verifier-artisan {
            tokens: (+ (get tokens verifier-artisan) u5),
            reputation: (+ (get reputation verifier-artisan) u1)
          }))
        
        (if (and (>= new-verification-count u3) (not (get verified craft)))
          (map-set artisans 
            {artisan-id: (get creator craft)} 
            (merge craft-creator {
              tokens: (+ (get tokens craft-creator) AUTHENTICATION_REWARD),
              reputation: (+ (get reputation craft-creator) u10),
              authenticated: true
            }))
          true)
        
        (ok new-verification-count)
      )
    )
  )
)

(define-public (endorse-craft (craft-id uint) (endorsement-level uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-craft-id craft-id) ERR_INVALID_CRAFT_ID)
    (asserts! (> endorsement-level u0) ERR_INVALID_INPUT)
    (asserts! (is-some (map-get? artisans {artisan-id: caller})) ERR_NOT_FOUND)
    (asserts! (is-some (map-get? craft-items {craft-id: craft-id})) ERR_NOT_FOUND)
    
    (let ((craft (unwrap! (map-get? craft-items {craft-id: craft-id}) ERR_NOT_FOUND)))
      (asserts! (get verified craft) ERR_UNAUTHORIZED)
      
      (map-set craft-endorsements 
        {craft-id: craft-id, endorser: caller} 
        {endorsement-level: endorsement-level, endorsement-date: (var-get action-counter)})
      
      (let ((new-endorsement-count (+ (get endorsement-count craft) endorsement-level))
            (craft-creator (unwrap! (map-get? artisans {artisan-id: (get creator craft)}) ERR_NOT_FOUND)))
        
        (map-set craft-items 
          {craft-id: craft-id} 
          (merge craft {endorsement-count: new-endorsement-count}))
        
        (map-set artisans 
          {artisan-id: (get creator craft)} 
          (merge craft-creator {
            tokens: (+ (get tokens craft-creator) (* VALIDATION_REWARD endorsement-level))
          }))
        
        (ok new-endorsement-count)
      )
    )
  )
)

(define-public (rate-craft-quality (craft-id uint) (rating uint))
  (let ((caller tx-sender))
    (asserts! (is-valid-craft-id craft-id) ERR_INVALID_CRAFT_ID)
    (asserts! (and (>= rating u1) (<= rating MAX_RATING)) ERR_INVALID_RATING)
    (asserts! (is-some (map-get? artisans {artisan-id: caller})) ERR_NOT_FOUND)
    (asserts! (is-some (map-get? craft-items {craft-id: craft-id})) ERR_NOT_FOUND)
    
    (let ((craft (unwrap! (map-get? craft-items {craft-id: craft-id}) ERR_NOT_FOUND)))
      (asserts! (not (is-eq caller (get creator craft))) ERR_SELF_RATING)
      (asserts! (is-none (map-get? quality-ratings {craft-id: craft-id, rater: caller})) ERR_ALREADY_RATED)
      
      (map-set quality-ratings 
        {craft-id: craft-id, rater: caller} 
        {rating: rating})
      
      (let ((current-total-rating (* (get quality-rating craft) (get rating-count craft)))
            (new-rating-count (+ (get rating-count craft) u1))
            (new-total-rating (+ current-total-rating rating))
            (new-average-rating (/ new-total-rating new-rating-count))
            (craft-creator (unwrap! (map-get? artisans {artisan-id: (get creator craft)}) ERR_NOT_FOUND))
            (rater-artisan (unwrap! (map-get? artisans {artisan-id: caller}) ERR_NOT_FOUND)))
        
        (map-set craft-items 
          {craft-id: craft-id} 
          (merge craft {
            quality-rating: new-average-rating,
            rating-count: new-rating-count
          }))
        
        (map-set artisans 
          {artisan-id: caller} 
          (merge rater-artisan {
            tokens: (+ (get tokens rater-artisan) u2),
            reputation: (+ (get reputation rater-artisan) u1)
          }))
        
        (if (>= rating u4)
          (map-set artisans 
            {artisan-id: (get creator craft)} 
            (merge craft-creator {
              tokens: (+ (get tokens craft-creator) EXCELLENCE_REWARD),
              reputation: (+ (get reputation craft-creator) u5)
            }))
          true)
        
        (ok new-average-rating)
      )
    )
  )
)

;; Read-only functions
(define-read-only (get-artisan-info (artisan-id principal))
  (map-get? artisans {artisan-id: artisan-id})
)

(define-read-only (get-craft (craft-id uint))
  (map-get? craft-items {craft-id: craft-id})
)

(define-read-only (get-craft-verification (craft-id uint) (verifier principal))
  (map-get? craft-verifications {craft-id: craft-id, verifier: verifier})
)

(define-read-only (get-craft-endorsement (craft-id uint) (endorser principal))
  (map-get? craft-endorsements {craft-id: craft-id, endorser: endorser})
)

(define-read-only (get-quality-rating (craft-id uint) (rater principal))
  (map-get? quality-ratings {craft-id: craft-id, rater: rater})
)

(define-read-only (get-total-crafts)
  (- (var-get next-craft-id) u1)
)