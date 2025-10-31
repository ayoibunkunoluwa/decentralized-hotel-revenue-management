;; Booking Channel Coordinator
;; Orchestrates inventory distribution and pricing synchronization across all booking channels
;; Maintains rate parity and maximizes channel-specific revenue optimization

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-already-exists (err u202))
(define-constant err-invalid-params (err u203))
(define-constant err-unauthorized (err u204))
(define-constant err-insufficient-inventory (err u205))
(define-constant err-channel-unavailable (err u206))
(define-constant err-rate-parity-violation (err u207))
(define-constant err-booking-conflict (err u208))
(define-constant err-channel-capacity-exceeded (err u209))
(define-constant err-invalid-allocation (err u210))
(define-constant err-synchronization-failed (err u211))

;; Data Variables
(define-data-var contract-active bool true)
(define-data-var total-channels uint u0)
(define-data-var total-bookings uint u0)
(define-data-var synchronization-count uint u0)
(define-data-var rate-parity-violations uint u0)

;; Data Maps

;; Distribution Channels Registry
(define-map distribution-channels
  { channel-id: uint }
  {
    channel-name: (string-ascii 100),
    channel-type: (string-ascii 50), ;; OTA, GDS, direct, metasearch, wholesale
    commission-rate: uint, ;; percentage * 100
    booking-window-days: uint,
    market-segment: (string-ascii 50),
    priority-level: uint, ;; 1-10 scale
    active: bool,
    created-at: uint,
    last-sync: uint
  }
)

;; Property-Channel Configurations
(define-map property-channel-config
  { property-id: uint, channel-id: uint }
  {
    allocation-percentage: uint, ;; percentage * 100
    minimum-inventory: uint,
    maximum-inventory: uint,
    rate-modifier: uint, ;; percentage * 100 (e.g., 105 = 5% markup)
    booking-restrictions: (string-ascii 200),
    channel-manager: principal,
    auto-sync-enabled: bool,
    last-updated: uint
  }
)

;; Inventory Allocation Management
(define-map inventory-allocation
  { property-id: uint, category-id: uint, date: uint }
  {
    total-inventory: uint,
    allocated-inventory: uint,
    available-inventory: uint,
    blocked-inventory: uint,
    overbooking-limit: uint,
    last-allocation-update: uint,
    allocation-strategy: (string-ascii 50)
  }
)

;; Channel-Specific Inventory
(define-map channel-inventory
  { property-id: uint, category-id: uint, channel-id: uint, date: uint }
  {
    allocated-rooms: uint,
    booked-rooms: uint,
    available-rooms: uint,
    channel-rate: uint,
    rate-last-updated: uint,
    booking-pace: uint,
    conversion-rate: uint,
    channel-performance-score: uint
  }
)

;; Rate Parity Monitoring
(define-map rate-parity-data
  { property-id: uint, category-id: uint, date: uint }
  {
    base-rate: uint,
    lowest-channel-rate: uint,
    highest-channel-rate: uint,
    rate-variance: uint,
    parity-compliant: bool,
    violation-count: uint,
    last-parity-check: uint,
    corrective-action-required: bool
  }
)

;; Booking Transactions
(define-map booking-transactions
  { booking-id: uint }
  {
    property-id: uint,
    category-id: uint,
    channel-id: uint,
    guest-name: (string-ascii 100),
    check-in-date: uint,
    check-out-date: uint,
    room-nights: uint,
    total-amount: uint,
    commission-amount: uint,
    booking-status: (string-ascii 20), ;; confirmed, cancelled, modified, no-show
    created-at: uint,
    last-modified: uint
  }
)

;; Channel Performance Analytics
(define-map channel-performance
  { property-id: uint, channel-id: uint, date: uint }
  {
    bookings-count: uint,
    room-nights-sold: uint,
    gross-revenue: uint,
    net-revenue: uint,
    commission-paid: uint,
    conversion-rate: uint,
    average-booking-value: uint,
    cancellation-rate: uint,
    roi-score: uint
  }
)

;; Synchronization Log
(define-map sync-log
  { property-id: uint, channel-id: uint, timestamp: uint }
  {
    sync-type: (string-ascii 50), ;; inventory, rates, availability, restrictions
    sync-status: (string-ascii 20), ;; success, failed, partial
    records-updated: uint,
    error-message: (string-ascii 200),
    initiated-by: principal,
    completion-time: uint,
    next-sync-scheduled: uint
  }
)

;; Private Functions

(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-property-channel-manager (property-id uint) (channel-id uint))
  (match (map-get? property-channel-config { property-id: property-id, channel-id: channel-id })
    config (is-eq tx-sender (get channel-manager config))
    false
  )
)

(define-private (calculate-channel-allocation (total-inventory uint) (allocation-percentage uint))
  (/ (* total-inventory allocation-percentage) u10000)
)

(define-private (validate-rate-parity (property-id uint) (category-id uint) (new-rate uint) (date uint))
  (let (
    (parity-data (map-get? rate-parity-data { property-id: property-id, category-id: category-id, date: date }))
  )
    (match parity-data
      data (let (
        (base-rate (get base-rate data))
        (variance-threshold (/ base-rate u20)) ;; 5% tolerance
      )
        (and 
          (>= new-rate (- base-rate variance-threshold))
          (<= new-rate (+ base-rate variance-threshold))
        )
      )
      true ;; No existing parity data, allow rate
    )
  )
)

(define-private (calculate-commission (booking-amount uint) (commission-rate uint))
  (/ (* booking-amount commission-rate) u10000)
)

(define-private (update-channel-performance-score 
    (property-id uint) 
    (channel-id uint) 
    (conversion-rate uint) 
    (roi-score uint)
  )
  (let (
    (weighted-conversion (* conversion-rate u60))
    (weighted-roi (* roi-score u40))
    (performance-score (/ (+ weighted-conversion weighted-roi) u100))
  )
    performance-score
  )
)

(define-private (check-inventory-availability 
    (property-id uint) 
    (category-id uint) 
    (channel-id uint) 
    (date uint) 
    (requested-rooms uint)
  )
  (match (map-get? channel-inventory { property-id: property-id, category-id: category-id, channel-id: channel-id, date: date })
    inventory (>= (get available-rooms inventory) requested-rooms)
    false
  )
)

;; Public Functions

(define-public (register-distribution-channel
    (channel-id uint)
    (channel-name (string-ascii 100))
    (channel-type (string-ascii 50))
    (commission-rate uint)
    (booking-window-days uint)
    (market-segment (string-ascii 50))
    (priority-level uint)
  )
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (asserts! (is-none (map-get? distribution-channels { channel-id: channel-id })) err-already-exists)
    (asserts! (and (> commission-rate u0) (<= commission-rate u5000) (<= priority-level u10)) err-invalid-params)
    
    (map-set distribution-channels
      { channel-id: channel-id }
      {
        channel-name: channel-name,
        channel-type: channel-type,
        commission-rate: commission-rate,
        booking-window-days: booking-window-days,
        market-segment: market-segment,
        priority-level: priority-level,
        active: true,
        created-at: stacks-block-height,
        last-sync: u0
      }
    )
    
    (var-set total-channels (+ (var-get total-channels) u1))
    (ok channel-id)
  )
)

(define-public (configure-property-channel
    (property-id uint)
    (channel-id uint)
    (allocation-percentage uint)
    (minimum-inventory uint)
    (maximum-inventory uint)
    (rate-modifier uint)
    (booking-restrictions (string-ascii 200))
    (channel-manager principal)
    (auto-sync-enabled bool)
  )
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (asserts! (is-some (map-get? distribution-channels { channel-id: channel-id })) err-not-found)
    (asserts! (and (<= allocation-percentage u10000) (< minimum-inventory maximum-inventory)) err-invalid-params)
    
    (map-set property-channel-config
      { property-id: property-id, channel-id: channel-id }
      {
        allocation-percentage: allocation-percentage,
        minimum-inventory: minimum-inventory,
        maximum-inventory: maximum-inventory,
        rate-modifier: rate-modifier,
        booking-restrictions: booking-restrictions,
        channel-manager: channel-manager,
        auto-sync-enabled: auto-sync-enabled,
        last-updated: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (allocate-inventory
    (property-id uint)
    (category-id uint)
    (date uint)
    (total-inventory uint)
    (overbooking-limit uint)
    (allocation-strategy (string-ascii 50))
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-channel-manager property-id u0)) err-unauthorized)
    (asserts! (and (> total-inventory u0) (<= overbooking-limit u50)) err-invalid-params)
    
    (map-set inventory-allocation
      { property-id: property-id, category-id: category-id, date: date }
      {
        total-inventory: total-inventory,
        allocated-inventory: u0, ;; Will be calculated when channels allocate
        available-inventory: total-inventory,
        blocked-inventory: u0,
        overbooking-limit: overbooking-limit,
        last-allocation-update: stacks-block-height,
        allocation-strategy: allocation-strategy
      }
    )
    
    (ok true)
  )
)

(define-public (allocate-channel-inventory
    (property-id uint)
    (category-id uint)
    (channel-id uint)
    (date uint)
    (allocated-rooms uint)
    (channel-rate uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-channel-manager property-id channel-id)) err-unauthorized)
    (asserts! (is-some (map-get? property-channel-config { property-id: property-id, channel-id: channel-id })) err-not-found)
    
    (let (
      (inventory (unwrap! (map-get? inventory-allocation { property-id: property-id, category-id: category-id, date: date }) err-not-found))
      (available-inventory (get available-inventory inventory))
    )
      (asserts! (>= available-inventory allocated-rooms) err-insufficient-inventory)
      (asserts! (validate-rate-parity property-id category-id channel-rate date) err-rate-parity-violation)
      
      ;; Update channel inventory
      (map-set channel-inventory
        { property-id: property-id, category-id: category-id, channel-id: channel-id, date: date }
        {
          allocated-rooms: allocated-rooms,
          booked-rooms: u0,
          available-rooms: allocated-rooms,
          channel-rate: channel-rate,
          rate-last-updated: stacks-block-height,
          booking-pace: u0,
          conversion-rate: u0,
          channel-performance-score: u0
        }
      )
      
      ;; Update overall inventory allocation
      (map-set inventory-allocation
        { property-id: property-id, category-id: category-id, date: date }
        (merge inventory {
          allocated-inventory: (+ (get allocated-inventory inventory) allocated-rooms),
          available-inventory: (- available-inventory allocated-rooms)
        })
      )
      
      (ok allocated-rooms)
    )
  )
)

(define-public (create-booking
    (booking-id uint)
    (property-id uint)
    (category-id uint)
    (channel-id uint)
    (guest-name (string-ascii 100))
    (check-in-date uint)
    (check-out-date uint)
    (room-nights uint)
    (total-amount uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-channel-manager property-id channel-id)) err-unauthorized)
    (asserts! (is-none (map-get? booking-transactions { booking-id: booking-id })) err-already-exists)
    (asserts! (check-inventory-availability property-id category-id channel-id check-in-date room-nights) err-insufficient-inventory)
    
    (let (
      (channel (unwrap! (map-get? distribution-channels { channel-id: channel-id }) err-not-found))
      (commission-amount (calculate-commission total-amount (get commission-rate channel)))
    )
      (map-set booking-transactions
        { booking-id: booking-id }
        {
          property-id: property-id,
          category-id: category-id,
          channel-id: channel-id,
          guest-name: guest-name,
          check-in-date: check-in-date,
          check-out-date: check-out-date,
          room-nights: room-nights,
          total-amount: total-amount,
          commission-amount: commission-amount,
          booking-status: "confirmed",
          created-at: stacks-block-height,
          last-modified: stacks-block-height
        }
      )
      
      ;; Update channel inventory
      (let (
        (channel-inv (unwrap! (map-get? channel-inventory { property-id: property-id, category-id: category-id, channel-id: channel-id, date: check-in-date }) err-not-found))
      )
        (map-set channel-inventory
          { property-id: property-id, category-id: category-id, channel-id: channel-id, date: check-in-date }
          (merge channel-inv {
            booked-rooms: (+ (get booked-rooms channel-inv) room-nights),
            available-rooms: (- (get available-rooms channel-inv) room-nights)
          })
        )
      )
      
      (var-set total-bookings (+ (var-get total-bookings) u1))
      (ok booking-id)
    )
  )
)

(define-public (update-rate-parity
    (property-id uint)
    (category-id uint)
    (date uint)
    (base-rate uint)
    (lowest-channel-rate uint)
    (highest-channel-rate uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-channel-manager property-id u0)) err-unauthorized)
    (asserts! (and (> base-rate u0) (<= lowest-channel-rate highest-channel-rate)) err-invalid-params)
    
    (let (
      (rate-variance (- highest-channel-rate lowest-channel-rate))
      (variance-threshold (/ base-rate u20)) ;; 5% tolerance
      (parity-compliant (<= rate-variance variance-threshold))
    )
      (map-set rate-parity-data
        { property-id: property-id, category-id: category-id, date: date }
        {
          base-rate: base-rate,
          lowest-channel-rate: lowest-channel-rate,
          highest-channel-rate: highest-channel-rate,
          rate-variance: rate-variance,
          parity-compliant: parity-compliant,
          violation-count: (if parity-compliant u0 u1),
          last-parity-check: stacks-block-height,
          corrective-action-required: (not parity-compliant)
        }
      )
      
      (if (not parity-compliant)
        (var-set rate-parity-violations (+ (var-get rate-parity-violations) u1))
        true
      )
      
      (ok parity-compliant)
    )
  )
)

(define-public (synchronize-channel-data
    (property-id uint)
    (channel-id uint)
    (sync-type (string-ascii 50))
    (records-count uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-channel-manager property-id channel-id)) err-unauthorized)
    (asserts! (is-some (map-get? property-channel-config { property-id: property-id, channel-id: channel-id })) err-not-found)
    
    (map-set sync-log
      { property-id: property-id, channel-id: channel-id, timestamp: stacks-block-height }
      {
        sync-type: sync-type,
        sync-status: "success",
        records-updated: records-count,
        error-message: "",
        initiated-by: tx-sender,
        completion-time: stacks-block-height,
        next-sync-scheduled: (+ stacks-block-height u144) ;; Next sync in ~24 hours
      }
    )
    
    ;; Update channel last sync timestamp
    (let (
      (channel (unwrap! (map-get? distribution-channels { channel-id: channel-id }) err-not-found))
    )
      (map-set distribution-channels
        { channel-id: channel-id }
        (merge channel { last-sync: stacks-block-height })
      )
    )
    
    (var-set synchronization-count (+ (var-get synchronization-count) u1))
    (ok true)
  )
)

(define-public (record-channel-performance
    (property-id uint)
    (channel-id uint)
    (date uint)
    (bookings-count uint)
    (room-nights-sold uint)
    (gross-revenue uint)
    (commission-paid uint)
    (conversion-rate uint)
    (cancellation-rate uint)
  )
  (begin
    (asserts! (or (is-contract-owner) (is-property-channel-manager property-id channel-id)) err-unauthorized)
    (asserts! (and (> gross-revenue u0) (<= conversion-rate u10000)) err-invalid-params)
    
    (let (
      (net-revenue (- gross-revenue commission-paid))
      (average-booking-value (if (> bookings-count u0) (/ gross-revenue bookings-count) u0))
      (roi-score (if (> commission-paid u0) (/ (* net-revenue u100) commission-paid) u0))
    )
      (map-set channel-performance
        { property-id: property-id, channel-id: channel-id, date: date }
        {
          bookings-count: bookings-count,
          room-nights-sold: room-nights-sold,
          gross-revenue: gross-revenue,
          net-revenue: net-revenue,
          commission-paid: commission-paid,
          conversion-rate: conversion-rate,
          average-booking-value: average-booking-value,
          cancellation-rate: cancellation-rate,
          roi-score: roi-score
        }
      )
      
      (ok roi-score)
    )
  )
)

(define-public (cancel-booking
    (booking-id uint)
    (cancellation-reason (string-ascii 200))
  )
  (begin
    (let (
      (booking (unwrap! (map-get? booking-transactions { booking-id: booking-id }) err-not-found))
    )
      (asserts! (or (is-contract-owner) 
                   (is-property-channel-manager (get property-id booking) (get channel-id booking))) 
                err-unauthorized)
      
      ;; Update booking status
      (map-set booking-transactions
        { booking-id: booking-id }
        (merge booking {
          booking-status: "cancelled",
          last-modified: stacks-block-height
        })
      )
      
      ;; Return inventory to available pool
      (let (
        (channel-inv (unwrap! (map-get? channel-inventory { 
          property-id: (get property-id booking), 
          category-id: (get category-id booking), 
          channel-id: (get channel-id booking), 
          date: (get check-in-date booking) 
        }) err-not-found))
      )
        (map-set channel-inventory
          { property-id: (get property-id booking), 
            category-id: (get category-id booking), 
            channel-id: (get channel-id booking), 
            date: (get check-in-date booking) }
          (merge channel-inv {
            booked-rooms: (- (get booked-rooms channel-inv) (get room-nights booking)),
            available-rooms: (+ (get available-rooms channel-inv) (get room-nights booking))
          })
        )
      )
      
      (ok true)
    )
  )
)

;; Read-Only Functions

(define-read-only (get-distribution-channel (channel-id uint))
  (map-get? distribution-channels { channel-id: channel-id })
)

(define-read-only (get-property-channel-config (property-id uint) (channel-id uint))
  (map-get? property-channel-config { property-id: property-id, channel-id: channel-id })
)

(define-read-only (get-inventory-allocation (property-id uint) (category-id uint) (date uint))
  (map-get? inventory-allocation { property-id: property-id, category-id: category-id, date: date })
)

(define-read-only (get-channel-inventory (property-id uint) (category-id uint) (channel-id uint) (date uint))
  (map-get? channel-inventory { property-id: property-id, category-id: category-id, channel-id: channel-id, date: date })
)

(define-read-only (get-rate-parity-data (property-id uint) (category-id uint) (date uint))
  (map-get? rate-parity-data { property-id: property-id, category-id: category-id, date: date })
)

(define-read-only (get-booking-transaction (booking-id uint))
  (map-get? booking-transactions { booking-id: booking-id })
)

(define-read-only (get-channel-performance (property-id uint) (channel-id uint) (date uint))
  (map-get? channel-performance { property-id: property-id, channel-id: channel-id, date: date })
)

(define-read-only (get-sync-log (property-id uint) (channel-id uint) (timestamp uint))
  (map-get? sync-log { property-id: property-id, channel-id: channel-id, timestamp: timestamp })
)

(define-read-only (get-contract-stats)
  {
    total-channels: (var-get total-channels),
    total-bookings: (var-get total-bookings),
    synchronization-count: (var-get synchronization-count),
    rate-parity-violations: (var-get rate-parity-violations),
    contract-active: (var-get contract-active)
  }
)

(define-read-only (calculate-channel-roi 
    (property-id uint) 
    (channel-id uint) 
    (gross-revenue uint) 
    (commission-paid uint)
    (operational-cost uint)
  )
  (let (
    (net-revenue (- gross-revenue commission-paid))
    (total-cost (+ commission-paid operational-cost))
    (roi-percentage (if (> total-cost u0) (/ (* net-revenue u10000) total-cost) u0))
  )
    (ok roi-percentage)
  )
)

(define-read-only (get-channel-availability
    (property-id uint)
    (category-id uint)
    (channel-id uint)
    (date uint)
    (requested-rooms uint)
  )
  (match (map-get? channel-inventory { property-id: property-id, category-id: category-id, channel-id: channel-id, date: date })
    inventory {
      available: (get available-rooms inventory),
      can-accommodate: (>= (get available-rooms inventory) requested-rooms),
      channel-rate: (get channel-rate inventory),
      booking-pace: (get booking-pace inventory)
    }
    {
      available: u0,
      can-accommodate: false,
      channel-rate: u0,
      booking-pace: u0
    }
  )
)
