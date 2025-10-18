;; Dynamic Pricing Optimizer
;; Intelligent real-time pricing strategies for hotel revenue optimization
;; Implements multi-variable pricing engine with demand forecasting and competitive intelligence

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-params (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-insufficient-balance (err u105))
(define-constant err-pricing-rules-violation (err u106))
(define-constant err-invalid-date-range (err u107))
(define-constant err-market-data-unavailable (err u108))
(define-constant err-pricing-strategy-conflict (err u109))
(define-constant err-forecasting-error (err u110))
(define-constant err-segment-config-invalid (err u111))

;; Data Variables
(define-data-var contract-active bool true)
(define-data-var total-properties uint u0)
(define-data-var pricing-update-count uint u0)
(define-data-var revenue-optimization-score uint u0)

;; Data Maps

;; Hotel Properties Registry
(define-map hotel-properties
  { property-id: uint }
  {
    name: (string-ascii 100),
    location: (string-ascii 100), 
    total-rooms: uint,
    property-type: (string-ascii 50),
    base-rate: uint,
    manager: principal,
    created-at: uint,
    active: bool
  }
)

;; Room Categories and Configuration
(define-map room-categories
  { property-id: uint, category-id: uint }
  {
    category-name: (string-ascii 50),
    room-count: uint,
    base-price: uint,
    amenities: (string-ascii 200),
    max-occupancy: uint,
    size-sqft: uint,
    pricing-tier: (string-ascii 20),
    active: bool
  }
)

;; Dynamic Pricing Rules Engine
(define-map pricing-rules
  { property-id: uint, rule-id: uint }
  {
    rule-name: (string-ascii 100),
    rule-type: (string-ascii 50), ;; demand-based, competitive, event-driven, seasonal
    trigger-condition: (string-ascii 200),
    price-adjustment-type: (string-ascii 20), ;; percentage, fixed-amount
    adjustment-value: uint,
    min-price: uint,
    max-price: uint,
    priority: uint,
    active: bool,
    created-at: uint
  }
)

;; Market Demand Data
(define-map demand-data
  { property-id: uint, date: uint }
  {
    occupancy-rate: uint, ;; percentage * 100
    booking-pace: uint,
    search-volume: uint,
    cancellation-rate: uint,
    no-show-rate: uint,
    advance-booking-days: uint,
    market-demand-score: uint,
    seasonal-factor: uint,
    updated-at: uint
  }
)

;; Competitive Intelligence
(define-map competitor-data
  { property-id: uint, competitor-id: uint, date: uint }
  {
    competitor-name: (string-ascii 100),
    competitor-rate: uint,
    competitor-occupancy: uint,
    rate-position: (string-ascii 20), ;; above, below, equal
    market-share: uint,
    service-level: (string-ascii 50),
    last-updated: uint,
    data-confidence: uint
  }
)

;; Customer Segments and Pricing
(define-map customer-segments
  { property-id: uint, segment-id: uint }
  {
    segment-name: (string-ascii 50), ;; corporate, leisure, group, loyalty
    base-discount: uint,
    volume-threshold: uint,
    booking-window-preference: uint,
    price-sensitivity: uint, ;; 1-100 scale
    loyalty-tier: (string-ascii 20),
    segment-value-score: uint,
    active: bool
  }
)

;; Revenue Performance Analytics
(define-map revenue-performance
  { property-id: uint, date: uint }
  {
    total-revenue: uint,
    revpar: uint, ;; Revenue per Available Room * 100
    adr: uint,    ;; Average Daily Rate * 100
    occupancy: uint,
    room-nights-sold: uint,
    total-room-nights: uint,
    goppar: uint, ;; Gross Operating Profit per Available Room * 100
    market-penetration: uint,
    yield-percentage: uint
  }
)

;; Pricing Optimization History
(define-map pricing-history
  { property-id: uint, category-id: uint, timestamp: uint }
  {
    old-price: uint,
    new-price: uint,
    adjustment-reason: (string-ascii 200),
    rule-applied: uint,
    demand-factor: uint,
    competitive-factor: uint,
    revenue-impact-forecast: uint,
    approved-by: principal,
    effective-date: uint
  }
)

;; Private Functions

(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-property-manager (property-id uint))
  (match (map-get? hotel-properties { property-id: property-id })
    property (is-eq tx-sender (get manager property))
    false
  )
)

(define-private (calculate-demand-score (occupancy-rate uint) (booking-pace uint) (search-volume uint))
  (let (
    (weighted-occupancy (* occupancy-rate u40))
    (weighted-booking-pace (* booking-pace u35))
    (weighted-search-volume (* search-volume u25))
  )
    (/ (+ weighted-occupancy weighted-booking-pace weighted-search-volume) u100)
  )
)

(define-private (apply-pricing-constraints (base-price uint) (adjustment uint) (min-price uint) (max-price uint))
  (let (
    (new-price (+ base-price adjustment))
  )
    (if (< new-price min-price)
        min-price
        (if (> new-price max-price)
            max-price
            new-price
        )
    )
  )
)

(define-private (calculate-competitive-adjustment (property-id uint) (current-price uint) (date uint))
  (let (
    (market-position u0) ;; Simplified - would analyze competitor data
  )
    (if (> market-position u50)
        (* current-price u10) ;; 10% increase if above market
        (- current-price (* current-price u5)) ;; 5% decrease if below market
    )
  )
)

(define-private (validate-pricing-rules (property-id uint) (new-price uint) (category-id uint))
  (let (
    (room-category (unwrap! (map-get? room-categories { property-id: property-id, category-id: category-id }) false))
    (base-price (get base-price room-category))
  )
    (and 
      (> new-price u0)
      (< new-price (* base-price u300)) ;; Max 300% of base price
      (> new-price (/ base-price u2))   ;; Min 50% of base price
    )
  )
)

;; Public Functions

(define-public (register-hotel-property 
    (property-id uint) 
    (name (string-ascii 100)) 
    (location (string-ascii 100))
    (total-rooms uint)
    (property-type (string-ascii 50))
    (base-rate uint)
    (manager principal)
  )
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (asserts! (is-none (map-get? hotel-properties { property-id: property-id })) err-already-exists)
    (asserts! (and (> total-rooms u0) (> base-rate u0)) err-invalid-params)
    
    (map-set hotel-properties
      { property-id: property-id }
      {
        name: name,
        location: location,
        total-rooms: total-rooms,
        property-type: property-type,
        base-rate: base-rate,
        manager: manager,
        created-at: stacks-block-height,
        active: true
      }
    )
    
    (var-set total-properties (+ (var-get total-properties) u1))
    (ok property-id)
  )
)

(define-public (create-room-category
    (property-id uint)
    (category-id uint)
    (category-name (string-ascii 50))
    (room-count uint)
    (base-price uint)
    (amenities (string-ascii 200))
    (max-occupancy uint)
    (size-sqft uint)
    (pricing-tier (string-ascii 20))
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-manager property-id)) err-unauthorized)
    (asserts! (is-some (map-get? hotel-properties { property-id: property-id })) err-not-found)
    (asserts! (is-none (map-get? room-categories { property-id: property-id, category-id: category-id })) err-already-exists)
    (asserts! (and (> room-count u0) (> base-price u0) (> max-occupancy u0)) err-invalid-params)
    
    (map-set room-categories
      { property-id: property-id, category-id: category-id }
      {
        category-name: category-name,
        room-count: room-count,
        base-price: base-price,
        amenities: amenities,
        max-occupancy: max-occupancy,
        size-sqft: size-sqft,
        pricing-tier: pricing-tier,
        active: true
      }
    )
    
    (ok category-id)
  )
)

(define-public (create-pricing-rule
    (property-id uint)
    (rule-id uint)
    (rule-name (string-ascii 100))
    (rule-type (string-ascii 50))
    (trigger-condition (string-ascii 200))
    (price-adjustment-type (string-ascii 20))
    (adjustment-value uint)
    (min-price uint)
    (max-price uint)
    (priority uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-manager property-id)) err-unauthorized)
    (asserts! (is-some (map-get? hotel-properties { property-id: property-id })) err-not-found)
    (asserts! (is-none (map-get? pricing-rules { property-id: property-id, rule-id: rule-id })) err-already-exists)
    (asserts! (and (> adjustment-value u0) (< min-price max-price)) err-invalid-params)
    
    (map-set pricing-rules
      { property-id: property-id, rule-id: rule-id }
      {
        rule-name: rule-name,
        rule-type: rule-type,
        trigger-condition: trigger-condition,
        price-adjustment-type: price-adjustment-type,
        adjustment-value: adjustment-value,
        min-price: min-price,
        max-price: max-price,
        priority: priority,
        active: true,
        created-at: stacks-block-height
      }
    )
    
    (ok rule-id)
  )
)

(define-public (update-demand-data
    (property-id uint)
    (date uint)
    (occupancy-rate uint)
    (booking-pace uint)
    (search-volume uint)
    (cancellation-rate uint)
    (no-show-rate uint)
    (advance-booking-days uint)
    (seasonal-factor uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-manager property-id)) err-unauthorized)
    (asserts! (is-some (map-get? hotel-properties { property-id: property-id })) err-not-found)
    (asserts! (and (<= occupancy-rate u10000) (<= cancellation-rate u10000)) err-invalid-params)
    
    (let (
      (demand-score (calculate-demand-score occupancy-rate booking-pace search-volume))
    )
      (map-set demand-data
        { property-id: property-id, date: date }
        {
          occupancy-rate: occupancy-rate,
          booking-pace: booking-pace,
          search-volume: search-volume,
          cancellation-rate: cancellation-rate,
          no-show-rate: no-show-rate,
          advance-booking-days: advance-booking-days,
          market-demand-score: demand-score,
          seasonal-factor: seasonal-factor,
          updated-at: stacks-block-height
        }
      )
    )
    
    (ok true)
  )
)

(define-public (update-competitor-intelligence
    (property-id uint)
    (competitor-id uint)
    (date uint)
    (competitor-name (string-ascii 100))
    (competitor-rate uint)
    (competitor-occupancy uint)
    (rate-position (string-ascii 20))
    (market-share uint)
    (service-level (string-ascii 50))
    (data-confidence uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-manager property-id)) err-unauthorized)
    (asserts! (is-some (map-get? hotel-properties { property-id: property-id })) err-not-found)
    (asserts! (and (> competitor-rate u0) (<= data-confidence u100)) err-invalid-params)
    
    (map-set competitor-data
      { property-id: property-id, competitor-id: competitor-id, date: date }
      {
        competitor-name: competitor-name,
        competitor-rate: competitor-rate,
        competitor-occupancy: competitor-occupancy,
        rate-position: rate-position,
        market-share: market-share,
        service-level: service-level,
        last-updated: stacks-block-height,
        data-confidence: data-confidence
      }
    )
    
    (ok true)
  )
)

(define-public (optimize-room-pricing
    (property-id uint)
    (category-id uint)
    (target-date uint)
    (demand-factor uint)
    (competitive-factor uint)
    (segment-id uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-manager property-id)) err-unauthorized)
    
    (let (
      (room-category (unwrap! (map-get? room-categories { property-id: property-id, category-id: category-id }) err-not-found))
      (base-price (get base-price room-category))
      (demand-adjustment (* base-price (/ demand-factor u100)))
      (competitive-adjustment (* base-price (/ competitive-factor u100)))
      (total-adjustment (+ demand-adjustment competitive-adjustment))
      (new-price (+ base-price total-adjustment))
    )
      (asserts! (validate-pricing-rules property-id new-price category-id) err-pricing-rules-violation)
      
      ;; Record pricing history
      (map-set pricing-history
        { property-id: property-id, category-id: category-id, timestamp: stacks-block-height }
        {
          old-price: base-price,
          new-price: new-price,
          adjustment-reason: "Dynamic optimization based on demand and competition",
          rule-applied: u0,
          demand-factor: demand-factor,
          competitive-factor: competitive-factor,
          revenue-impact-forecast: u0, ;; Would be calculated
          approved-by: tx-sender,
          effective-date: target-date
        }
      )
      
      ;; Update room category base price
      (map-set room-categories
        { property-id: property-id, category-id: category-id }
        (merge room-category { base-price: new-price })
      )
      
      (var-set pricing-update-count (+ (var-get pricing-update-count) u1))
      (ok new-price)
    )
  )
)

(define-public (create-customer-segment
    (property-id uint)
    (segment-id uint)
    (segment-name (string-ascii 50))
    (base-discount uint)
    (volume-threshold uint)
    (booking-window-preference uint)
    (price-sensitivity uint)
    (loyalty-tier (string-ascii 20))
    (segment-value-score uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-manager property-id)) err-unauthorized)
    (asserts! (is-some (map-get? hotel-properties { property-id: property-id })) err-not-found)
    (asserts! (is-none (map-get? customer-segments { property-id: property-id, segment-id: segment-id })) err-already-exists)
    (asserts! (and (<= base-discount u100) (<= price-sensitivity u100)) err-invalid-params)
    
    (map-set customer-segments
      { property-id: property-id, segment-id: segment-id }
      {
        segment-name: segment-name,
        base-discount: base-discount,
        volume-threshold: volume-threshold,
        booking-window-preference: booking-window-preference,
        price-sensitivity: price-sensitivity,
        loyalty-tier: loyalty-tier,
        segment-value-score: segment-value-score,
        active: true
      }
    )
    
    (ok segment-id)
  )
)

(define-public (record-revenue-performance
    (property-id uint)
    (date uint)
    (total-revenue uint)
    (revpar uint)
    (adr uint)
    (occupancy uint)
    (room-nights-sold uint)
    (total-room-nights uint)
    (goppar uint)
    (market-penetration uint)
    (yield-percentage uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-manager property-id)) err-unauthorized)
    (asserts! (is-some (map-get? hotel-properties { property-id: property-id })) err-not-found)
    (asserts! (and (> total-revenue u0) (<= occupancy u10000)) err-invalid-params)
    
    (map-set revenue-performance
      { property-id: property-id, date: date }
      {
        total-revenue: total-revenue,
        revpar: revpar,
        adr: adr,
        occupancy: occupancy,
        room-nights-sold: room-nights-sold,
        total-room-nights: total-room-nights,
        goppar: goppar,
        market-penetration: market-penetration,
        yield-percentage: yield-percentage
      }
    )
    
    ;; Update overall revenue optimization score
    (var-set revenue-optimization-score (+ (var-get revenue-optimization-score) yield-percentage))
    (ok true)
  )
)

;; Read-Only Functions

(define-read-only (get-hotel-property (property-id uint))
  (map-get? hotel-properties { property-id: property-id })
)

(define-read-only (get-room-category (property-id uint) (category-id uint))
  (map-get? room-categories { property-id: property-id, category-id: category-id })
)

(define-read-only (get-pricing-rule (property-id uint) (rule-id uint))
  (map-get? pricing-rules { property-id: property-id, rule-id: rule-id })
)

(define-read-only (get-demand-data (property-id uint) (date uint))
  (map-get? demand-data { property-id: property-id, date: date })
)

(define-read-only (get-competitor-data (property-id uint) (competitor-id uint) (date uint))
  (map-get? competitor-data { property-id: property-id, competitor-id: competitor-id, date: date })
)

(define-read-only (get-customer-segment (property-id uint) (segment-id uint))
  (map-get? customer-segments { property-id: property-id, segment-id: segment-id })
)

(define-read-only (get-revenue-performance (property-id uint) (date uint))
  (map-get? revenue-performance { property-id: property-id, date: date })
)

(define-read-only (get-pricing-history (property-id uint) (category-id uint) (timestamp uint))
  (map-get? pricing-history { property-id: property-id, category-id: category-id, timestamp: timestamp })
)

(define-read-only (get-contract-stats)
  {
    total-properties: (var-get total-properties),
    pricing-updates: (var-get pricing-update-count),
    revenue-optimization-score: (var-get revenue-optimization-score),
    contract-active: (var-get contract-active)
  }
)

(define-read-only (calculate-optimal-price 
    (property-id uint) 
    (category-id uint) 
    (demand-score uint) 
    (competitor-avg-rate uint)
    (segment-id uint)
  )
  (let (
    (room-category (unwrap! (map-get? room-categories { property-id: property-id, category-id: category-id }) (err "Room category not found")))
    (customer-segment (map-get? customer-segments { property-id: property-id, segment-id: segment-id }))
    (base-price (get base-price room-category))
    (demand-multiplier (/ (+ demand-score u50) u100)) ;; Scale demand score
    (competitive-adjustment (if (> competitor-avg-rate u0) (/ competitor-avg-rate base-price) u100))
    (segment-discount (match customer-segment 
                        segment (get base-discount segment)
                        u0))
  )
    (let (
      (price-before-segment (* base-price (/ (* demand-multiplier competitive-adjustment) u100)))
      (final-price (- price-before-segment (/ (* price-before-segment segment-discount) u100)))
    )
      (ok final-price)
    )
  )
)
