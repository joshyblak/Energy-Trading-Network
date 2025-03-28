;; Producer Registration Contract
;; Records details of energy generation sources

(define-data-var last-producer-id uint u0)

;; Define producer data structure
(define-map producers
  { producer-id: uint }
  {
    owner: principal,
    energy-type: (string-ascii 20),
    capacity: uint,
    location: (string-ascii 50),
    active: bool
  }
)

;; Register a new energy producer
(define-public (register-producer
    (energy-type (string-ascii 20))
    (capacity uint)
    (location (string-ascii 50)))
  (let
    (
      (new-id (+ (var-get last-producer-id) u1))
    )
    (asserts! (> capacity u0) (err u1)) ;; Capacity must be positive
    (var-set last-producer-id new-id)
    (map-set producers
      { producer-id: new-id }
      {
        owner: tx-sender,
        energy-type: energy-type,
        capacity: capacity,
        location: location,
        active: true
      }
    )
    (ok new-id)
  )
)

;; Update producer capacity
(define-public (update-capacity (producer-id uint) (new-capacity uint))
  (let
    (
      (producer (unwrap! (map-get? producers { producer-id: producer-id }) (err u404)))
    )
    (asserts! (is-eq (get owner producer) tx-sender) (err u403))
    (asserts! (> new-capacity u0) (err u1))

    (map-set producers
      { producer-id: producer-id }
      (merge producer { capacity: new-capacity })
    )
    (ok true)
  )
)

;; Deactivate a producer
(define-public (deactivate-producer (producer-id uint))
  (let
    (
      (producer (unwrap! (map-get? producers { producer-id: producer-id }) (err u404)))
    )
    (asserts! (is-eq (get owner producer) tx-sender) (err u403))

    (map-set producers
      { producer-id: producer-id }
      (merge producer { active: false })
    )
    (ok true)
  )
)

;; Reactivate a producer
(define-public (reactivate-producer (producer-id uint))
  (let
    (
      (producer (unwrap! (map-get? producers { producer-id: producer-id }) (err u404)))
    )
    (asserts! (is-eq (get owner producer) tx-sender) (err u403))

    (map-set producers
      { producer-id: producer-id }
      (merge producer { active: true })
    )
    (ok true)
  )
)

;; Read-only function to get producer details
(define-read-only (get-producer (producer-id uint))
  (map-get? producers { producer-id: producer-id })
)

;; Read-only function to check if producer is active
(define-read-only (is-producer-active (producer-id uint))
  (default-to
    false
    (get active (map-get? producers { producer-id: producer-id }))
  )
)

