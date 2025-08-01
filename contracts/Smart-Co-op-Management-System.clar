(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-already-member (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-proposal-not-found (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-proposal-expired (err u106))
(define-constant err-proposal-not-passed (err u107))
(define-constant err-invalid-amount (err u108))
(define-constant err-invalid-duration (err u109))
(define-constant reputation-proposal-creation u10)
(define-constant reputation-voting u5)
(define-constant reputation-execution u15)
(define-constant reputation-decay-rate u1)
(define-constant max-reputation u1000)

(define-constant escrow-fee-rate u50)
(define-constant err-escrow-not-found (err u200))
(define-constant err-escrow-already-funded (err u201))
(define-constant err-escrow-not-funded (err u202))
(define-constant err-unauthorized-escrow-action (err u203))
(define-constant err-escrow-already-released (err u204))

(define-data-var next-escrow-id uint u1)

(define-data-var next-proposal-id uint u1)
(define-data-var total-pool-funds uint u0)
(define-data-var member-count uint u0)

(define-map members principal 
  {
    contribution: uint,
    join-block: uint,
    active: bool
  })

(define-map proposals uint
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    proposal-type: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    created-at: uint,
    expires-at: uint
  })

(define-map votes {proposal-id: uint, voter: principal} bool)

(define-map profit-distributions uint
  {
    total-amount: uint,
    per-member-share: uint,
    distribution-block: uint,
    claimed-count: uint
  })

(define-map profit-claims {distribution-id: uint, member: principal} bool)

(define-public (join-coop (contribution uint))
  (let ((sender tx-sender))
    (asserts! (> contribution u0) err-invalid-amount)
    (asserts! (is-none (map-get? members sender)) err-already-member)
    (try! (stx-transfer? contribution sender (as-contract tx-sender)))
    (map-set members sender
      {
        contribution: contribution,
        join-block: stacks-block-height,
        active: true
      })
    (var-set total-pool-funds (+ (var-get total-pool-funds) contribution))
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (add-funds (amount uint))
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member)))
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get active member-data) err-not-member)
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (map-set members sender
      (merge member-data {contribution: (+ (get contribution member-data) amount)}))
    (var-set total-pool-funds (+ (var-get total-pool-funds) amount))
    (ok true)))

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (amount uint) (proposal-type (string-ascii 20)) (duration uint))
  (let ((sender tx-sender)
        (proposal-id (var-get next-proposal-id))
        (member-data (unwrap! (map-get? members sender) err-not-member)))
    (asserts! (get active member-data) err-not-member)
    (asserts! (> duration u0) err-invalid-duration)
    (asserts! (>= (var-get total-pool-funds) amount) err-insufficient-funds)
    (map-set proposals proposal-id
      {
        proposer: sender,
        title: title,
        description: description,
        amount: amount,
        proposal-type: proposal-type,
        votes-for: u0,
        votes-against: u0,
        executed: false,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration)
      })
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member))
        (proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found)))
    (asserts! (get active member-data) err-not-member)
    (asserts! (<= stacks-block-height (get expires-at proposal-data)) err-proposal-expired)
    (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: sender})) err-already-voted)
    (map-set votes {proposal-id: proposal-id, voter: sender} vote-for)
    (if vote-for
      (map-set proposals proposal-id
        (merge proposal-data {votes-for: (+ (get votes-for proposal-data) u1)}))
      (map-set proposals proposal-id
        (merge proposal-data {votes-against: (+ (get votes-against proposal-data) u1)})))
    (ok true)))

(define-public (execute-proposal (proposal-id uint))
  (let ((proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
        (total-votes (+ (get votes-for proposal-data) (get votes-against proposal-data)))
        (required-votes (/ (var-get member-count) u2)))
    (asserts! (not (get executed proposal-data)) err-proposal-not-passed)
    (asserts! (> stacks-block-height (get expires-at proposal-data)) err-proposal-expired)
    (asserts! (> (get votes-for proposal-data) (get votes-against proposal-data)) err-proposal-not-passed)
    (asserts! (>= total-votes required-votes) err-proposal-not-passed)
    (if (is-eq (get proposal-type proposal-data) "purchase")
      (begin
        (try! (as-contract (stx-transfer? (get amount proposal-data) tx-sender (get proposer proposal-data))))
        (var-set total-pool-funds (- (var-get total-pool-funds) (get amount proposal-data))))
      (if (is-eq (get proposal-type proposal-data) "investment")
        (begin
          (try! (as-contract (stx-transfer? (get amount proposal-data) tx-sender (get proposer proposal-data))))
          (var-set total-pool-funds (- (var-get total-pool-funds) (get amount proposal-data))))
        true))
    (map-set proposals proposal-id
      (merge proposal-data {executed: true}))
    (ok true)))

(define-public (distribute-profits (total-profit uint))
  (let ((sender tx-sender)
        (distribution-id (var-get next-proposal-id))
        (member-count-val (var-get member-count))
        (per-member-share (/ total-profit member-count-val)))
    (asserts! (is-eq sender contract-owner) err-owner-only)
    (asserts! (> total-profit u0) err-invalid-amount)
    (asserts! (> member-count-val u0) err-invalid-amount)
    (try! (stx-transfer? total-profit sender (as-contract tx-sender)))
    (map-set profit-distributions distribution-id
      {
        total-amount: total-profit,
        per-member-share: per-member-share,
        distribution-block: stacks-block-height,
        claimed-count: u0
      })
    (var-set next-proposal-id (+ distribution-id u1))
    (ok distribution-id)))

(define-public (claim-profit (distribution-id uint))
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member))
        (distribution-data (unwrap! (map-get? profit-distributions distribution-id) err-proposal-not-found)))
    (asserts! (get active member-data) err-not-member)
    (asserts! (is-none (map-get? profit-claims {distribution-id: distribution-id, member: sender})) err-already-voted)
    (try! (as-contract (stx-transfer? (get per-member-share distribution-data) tx-sender sender)))
    (map-set profit-claims {distribution-id: distribution-id, member: sender} true)
    (map-set profit-distributions distribution-id
      (merge distribution-data {claimed-count: (+ (get claimed-count distribution-data) u1)}))
    (ok true)))

(define-public (leave-coop)
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member)))
    (asserts! (get active member-data) err-not-member)
    (map-set members sender
      (merge member-data {active: false}))
    (var-set member-count (- (var-get member-count) u1))
    (ok true)))

(define-read-only (get-member-info (member principal))
  (map-get? members member))

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id))

(define-read-only (get-pool-funds)
  (var-get total-pool-funds))

(define-read-only (get-member-count)
  (var-get member-count))

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes {proposal-id: proposal-id, voter: voter}))

(define-read-only (get-profit-distribution (distribution-id uint))
  (map-get? profit-distributions distribution-id))

(define-read-only (has-claimed-profit (distribution-id uint) (member principal))
  (map-get? profit-claims {distribution-id: distribution-id, member: member}))

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id))


(define-data-var total-reputation-points uint u0)

(define-map member-reputation principal
  {
    points: uint,
    last-activity-block: uint,
    proposals-created: uint,
    votes-cast: uint,
    proposals-executed: uint
  })

(define-private (calculate-voting-weight (member principal))
  (let ((member-data (unwrap! (map-get? members member) u0))
        (reputation-data (default-to 
          {points: u0, last-activity-block: u0, proposals-created: u0, votes-cast: u0, proposals-executed: u0}
          (map-get? member-reputation member)))
        (contribution-weight (/ (get contribution member-data) u1000))
        (reputation-weight (/ (get points reputation-data) u10)))
    (+ contribution-weight reputation-weight)))

(define-private (update-member-reputation (member principal) (points-to-add uint) (activity-type (string-ascii 20)))
  (let ((current-reputation (default-to 
          {points: u0, last-activity-block: u0, proposals-created: u0, votes-cast: u0, proposals-executed: u0}
          (map-get? member-reputation member)))
        (decayed-points (decay-reputation-points (get points current-reputation) (get last-activity-block current-reputation)))
        (new-points (if (> (+ decayed-points points-to-add) max-reputation)
                       max-reputation
                       (+ decayed-points points-to-add))))
    (map-set member-reputation member
      (merge current-reputation 
        {
          points: new-points,
          last-activity-block: stacks-block-height,
          proposals-created: (if (is-eq activity-type "proposal") 
                               (+ (get proposals-created current-reputation) u1)
                               (get proposals-created current-reputation)),
          votes-cast: (if (is-eq activity-type "vote")
                        (+ (get votes-cast current-reputation) u1)
                        (get votes-cast current-reputation)),
          proposals-executed: (if (is-eq activity-type "execution")
                                (+ (get proposals-executed current-reputation) u1)
                                (get proposals-executed current-reputation))
        }))
    (var-set total-reputation-points (+ (- (var-get total-reputation-points) (get points current-reputation)) new-points))
    new-points))
(define-private (decay-reputation-points (current-points uint) (last-activity-block uint))
  (let ((blocks-since-activity (- stacks-block-height last-activity-block))
        (decay-amount (* (/ blocks-since-activity u1000) reputation-decay-rate)))
    (if (> decay-amount current-points) u0 (- current-points decay-amount))))

(define-public (vote-on-proposal-weighted (proposal-id uint) (vote-for bool))
  (let ((sender tx-sender)
        (member-data (unwrap! (map-get? members sender) err-not-member))
        (proposal-data (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
        (voting-weight (calculate-voting-weight sender)))
    (asserts! (get active member-data) err-not-member)
    (asserts! (<= stacks-block-height (get expires-at proposal-data)) err-proposal-expired)
    (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: sender})) err-already-voted)
    (map-set votes {proposal-id: proposal-id, voter: sender} vote-for)
    (update-member-reputation sender reputation-voting "vote")
    (if vote-for
      (map-set proposals proposal-id
        (merge proposal-data {votes-for: (+ (get votes-for proposal-data) voting-weight)}))
      (map-set proposals proposal-id
        (merge proposal-data {votes-against: (+ (get votes-against proposal-data) voting-weight)})))
    (ok voting-weight)))

(define-read-only (get-member-reputation (member principal))
  (map-get? member-reputation member))

(define-read-only (get-member-voting-weight (member principal))
  (calculate-voting-weight member))

(define-read-only (get-total-reputation-points)
  (var-get total-reputation-points))


(define-map escrows uint
  {
    payer: principal,
    payee: principal,
    amount: uint,
    funded: bool,
    released: bool,
    created-at: uint,
    expires-at: uint,
    description: (string-ascii 200)
  })

(define-public (create-escrow (payee principal) (amount uint) (duration uint) (description (string-ascii 200)))
  (let ((sender tx-sender)
        (escrow-id (var-get next-escrow-id)))
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-duration)
    (map-set escrows escrow-id
      {
        payer: sender,
        payee: payee,
        amount: amount,
        funded: false,
        released: false,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height duration),
        description: description
      })
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)))

(define-public (fund-escrow (escrow-id uint))
  (let ((sender tx-sender)
        (escrow-data (unwrap! (map-get? escrows escrow-id) err-escrow-not-found)))
    (asserts! (is-eq sender (get payer escrow-data)) err-unauthorized-escrow-action)
    (asserts! (not (get funded escrow-data)) err-escrow-already-funded)
    (asserts! (<= stacks-block-height (get expires-at escrow-data)) err-proposal-expired)
    (try! (stx-transfer? (get amount escrow-data) sender (as-contract tx-sender)))
    (map-set escrows escrow-id (merge escrow-data {funded: true}))
    (ok true)))

(define-public (release-escrow (escrow-id uint))
  (let ((sender tx-sender)
        (escrow-data (unwrap! (map-get? escrows escrow-id) err-escrow-not-found))
        (fee-amount (/ (* (get amount escrow-data) escrow-fee-rate) u10000))
        (payout-amount (- (get amount escrow-data) fee-amount)))
    (asserts! (or (is-eq sender (get payer escrow-data)) (is-eq sender (get payee escrow-data))) err-unauthorized-escrow-action)
    (asserts! (get funded escrow-data) err-escrow-not-funded)
    (asserts! (not (get released escrow-data)) err-escrow-already-released)
    (try! (as-contract (stx-transfer? payout-amount tx-sender (get payee escrow-data))))
    (var-set total-pool-funds (+ (var-get total-pool-funds) fee-amount))
    (map-set escrows escrow-id (merge escrow-data {released: true}))
    (ok payout-amount)))

(define-public (refund-escrow (escrow-id uint))
  (let ((sender tx-sender)
        (escrow-data (unwrap! (map-get? escrows escrow-id) err-escrow-not-found)))
    (asserts! (is-eq sender (get payer escrow-data)) err-unauthorized-escrow-action)
    (asserts! (get funded escrow-data) err-escrow-not-funded)
    (asserts! (not (get released escrow-data)) err-escrow-already-released)
    (asserts! (> stacks-block-height (get expires-at escrow-data)) err-proposal-expired)
    (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get payer escrow-data))))
    (map-set escrows escrow-id (merge escrow-data {released: true}))
    (ok (get amount escrow-data))))

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrows escrow-id))
