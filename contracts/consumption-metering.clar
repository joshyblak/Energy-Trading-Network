;; Consumption Metering Contract
;; Tracks energy usage by consumers

(define-map consumer-meters
  { consumer: principal }
  {
    total-consumption: uint,
    last-reading: uint,
    last-update: uint
  }
)

;; Initialize a new consumer meter
(define-public (initialize-meter)
  (let
    (
      (consumer tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
    )
    (asserts! (is-none (map-get? consumer-meters { consumer: consumer })) (err u1))

    (map-set consumer-meters
      { consumer: consumer }
      {
        total-consumption: u0,
        last-reading: u0,
        last-update: current-time
      }
    )
    (ok true)
  )
)

;; Submit a new meter reading
(define-public (submit-reading (reading uint))
  (let
    (
      (consumer tx-sender)
      (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
      (meter-data (unwrap! (map-get? consumer-meters { consumer: consumer }) (err u404)))
      (last-reading (get last-reading meter-data))
      (consumption-delta (- reading last-reading))
    )
    (asserts! (>= reading last-reading) (err u2))

    (map-set consumer-meters
      { consumer: consumer }
      {
        total-consumption: (+ (get total-consumption meter-data) consumption-delta),
        last-reading: reading,
        last-update: current-time
      }
    )
    (ok consumption-delta)
  )
)

;; Get meter data for a consumer
(define-read-only (get-meter-data (consumer principal))
  (map-get? consumer-meters { consumer: consumer })
)

;; Get total consumption for a consumer
(define-read-only (get-total-consumption (consumer principal))
  (default-to
    u0
    (get total-consumption (map-get? consumer-meters { consumer: consumer }))
  )
)

;; Check if a consumer has an initialized meter
(define-read-only (has-meter (consumer principal))
  (is-some (map-get? consumer-meters { consumer: consumer }))
)

