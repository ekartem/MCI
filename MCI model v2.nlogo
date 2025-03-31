; Enhanced Mass Casualty Incident Simulation Model
; Comprehensive revision with improved architecture and resource management
; Based on WHO Academy Mass Casualty Management Guide
globals [
  time-elapsed
  deceased-count
  discharged-count

  ; Hospital capacity parameters
  triage-capacity
  green-zone-capacity
  red-zone-capacity
  yellow-zone-capacity
  blue-zone-capacity
  icu-capacity
  or-capacity
  ward-capacity

  ; Resource parameters
  doctors-available
  nurses-available
  doctors-minimum-guarantee ; Minimum doctors needed for essential services
  nurses-minimum-guarantee  ; Minimum nurses needed for essential services

  ; Resource allocations by area
  doctors-in-or
  doctors-in-icu
  doctors-in-red
  doctors-in-yellow
  doctors-in-green
  doctors-in-triage
  doctors-in-blue
  doctors-in-ward

  nurses-in-or
  nurses-in-icu
  nurses-in-red
  nurses-in-yellow
  nurses-in-green
  nurses-in-triage
  nurses-in-blue
  nurses-in-ward

  ; Statistics
  total-casualties
  total-deaths
  total-treated
  avg-wait-time
  max-wait-time
  recorded-transitions  ; List to track transitions that have happened

  ; Visualization parameters
  incident-xcor
  incident-ycor
  hospital-xcor
  hospital-ycor
  triage-xcor
  triage-ycor
  red-zone-xcor
  red-zone-ycor
  yellow-zone-xcor
  yellow-zone-ycor
  green-zone-xcor
  green-zone-ycor
  blue-zone-xcor
  blue-zone-ycor
  icu-xcor
  icu-ycor
  or-xcor
  or-ycor
  ward-xcor
  ward-ycor
  morgue-xcor
  morgue-ycor
  discharge-xcor
  discharge-ycor

  ; Event type constants
  EVENT_CREATION            ; Casualty is created
  EVENT_MOVEMENT_START      ; Casualty begins moving
  EVENT_MOVEMENT_COMPLETE   ; Casualty arrives at destination
  EVENT_AMBULANCE_PICKUP    ; Casualty is loaded into ambulance
  EVENT_HOSPITAL_ARRIVAL    ; Casualty arrives at hospital
  EVENT_TRIAGE_START        ; Casualty begins triage process
  EVENT_TRIAGE_COMPLETE     ; Casualty is assigned a triage category
  EVENT_TRIAGE_CHANGE       ; Casualty's triage category changes
  EVENT_TREATMENT_START     ; Casualty begins treatment
  EVENT_TREATMENT_COMPLETE  ; Treatment is finished
  EVENT_TREATMENT_INTERRUPT ; Treatment is interrupted
  EVENT_ZONE_CHANGE         ; Casualty moves to a different zone
  EVENT_HEALTH_DETERIORATION ; Casualty's health worsens
  EVENT_HEALTH_IMPROVEMENT  ; Casualty's health improves
  EVENT_QUEUE_ENTRY         ; Casualty enters a queue
  EVENT_QUEUE_EXIT          ; Casualty exits a queue
  EVENT_DISCHARGE           ; Casualty is discharged
  EVENT_DEATH               ; Casualty dies

  ; Resource requirements per zone
  red-zone-doctors-required
  red-zone-nurses-required
  yellow-zone-doctors-required
  yellow-zone-nurses-required
  green-zone-doctors-required
  green-zone-nurses-required
  blue-zone-doctors-required
  blue-zone-nurses-required
  or-doctors-required
  or-nurses-required
  icu-doctors-required
  icu-nurses-required
  ward-doctors-required
  ward-nurses-required
  triage-doctors-required
  triage-nurses-required
]

; Define turtle breeds
breed [casualties casualty]
breed [ambulances ambulance]
breed [hospitals hospital]
breed [zone-labels zone-label]
breed [resource-indicators resource-indicator]
breed [progress-bars progress-bar]

casualties-own [
  severity             ; RPM score (1-12)
  triage-category      ; red, yellow, green, or blue (expectant)
  location             ; current location (scene, in-transit, triage-queue, red-zone, etc.)
  wait-time            ; time waiting for treatment
  deterioration-rate   ; how quickly condition worsens
  treatment-time       ; time needed for treatment
  treatment-started?   ; whether treatment has begun
  treatment-progress   ; progress of treatment (0-100%)
  survival-probability ; chance of survival based on RPM
  destination-xcor     ; x-coordinate of destination
  destination-ycor     ; y-coordinate of destination
  moving?              ; whether casualty is moving
  unique-id            ; unique identifier for this casualty
  scheduled-tasks      ; list of scheduled tasks for this casualty
  resources-allocated   ; list of resources this casualty is using [zone doctors nurses]

  event-log          ; List to store event records
]

ambulances-own [
  status               ; idle, en-route-to-scene, loading, en-route-to-hospital, returning
  capacity             ; how many casualties can be transported (normally 2)
  loaded-casualties    ; list of casualties being transported
  target               ; where the ambulance is headed
  destination-xcor     ; x-coordinate of destination
  destination-ycor     ; y-coordinate of destination
]

hospitals-own [
  triage-queue         ; list of casualties waiting for triage
  red-zone-queue       ; list of casualties in red zone
  yellow-zone-queue    ; list of casualties in yellow zone
  green-zone-queue     ; list of casualties in green zone
  blue-zone-queue      ; list of casualties in blue zone (expectant)
  icu-queue            ; list of casualties in ICU
  or-queue             ; list of casualties in operating room
  ward-queue           ; list of casualties in ward
]

zone-labels-own [
  zone-type            ; Type of zone this label represents
  count-display        ; The count of casualties to display
  doctors-display      ; Count of doctors in this zone
  nurses-display       ; Count of nurses in this zone
]

resource-indicators-own [
  resource-type        ; Type of resource this indicator represents
  count-display        ; The count of resources to display
]

; Setup procedure
to setup
  clear-all
  reset-ticks

  ; Set up visualization coordinates for each zone
  setup-visualization-coordinates

  set time-elapsed 0
  set deceased-count 0
  set discharged-count 0
  set recorded-transitions []

  ; Calculate required resources for each zone based on seats and staff-per-seat
  set red-zone-doctors-required red-zone-seats * red-zone-doctors-per-seat
  set red-zone-nurses-required red-zone-seats * red-zone-nurses-per-seat
  set yellow-zone-doctors-required yellow-zone-seats * yellow-zone-doctors-per-seat
  set yellow-zone-nurses-required yellow-zone-seats * yellow-zone-nurses-per-seat
  set green-zone-doctors-required green-zone-seats * green-zone-doctors-per-seat
  set green-zone-nurses-required green-zone-seats * green-zone-nurses-per-seat
  set blue-zone-doctors-required blue-zone-seats * blue-zone-doctors-per-seat
  set blue-zone-nurses-required blue-zone-seats * blue-zone-nurses-per-seat
  set or-doctors-required or-seats * or-doctors-per-seat
  set or-nurses-required or-seats * or-nurses-per-seat
  set icu-doctors-required icu-seats * icu-doctors-per-seat
  set icu-nurses-required icu-seats * icu-nurses-per-seat
  set ward-doctors-required ward-seats * ward-doctors-per-seat
  set ward-nurses-required ward-seats * ward-nurses-per-seat
  set triage-doctors-required triage-seats * triage-doctors-per-seat
  set triage-nurses-required triage-seats * triage-nurses-per-seat

  ; Set zone capacities based on interface values
  set red-zone-capacity red-zone-seats
  set yellow-zone-capacity yellow-zone-seats
  set green-zone-capacity green-zone-seats
  set blue-zone-capacity blue-zone-seats
  set or-capacity or-seats
  set icu-capacity icu-seats
  set ward-capacity ward-seats
  set triage-capacity 100000 ; Keep triage essentially unlimited

  ; Initialize resource allocation counts
  set doctors-in-or 0
  set doctors-in-icu 0
  set doctors-in-red 0
  set doctors-in-yellow 0
  set doctors-in-green 0
  set doctors-in-triage 0
  set doctors-in-blue 0
  set doctors-in-ward 0

  set nurses-in-or 0
  set nurses-in-icu 0
  set nurses-in-red 0
  set nurses-in-yellow 0
  set nurses-in-green 0
  set nurses-in-triage 0
  set nurses-in-blue 0
  set nurses-in-ward 0

  ; Initialize available resources
  set doctors-available number-of-doctors
  set nurses-available number-of-nurses

  ; Allocate resources at the beginning
  reallocate-all-resources

  ; Initialize statistics
  set total-casualties number-of-casualties
  set total-deaths 0
  set total-treated 0
  set avg-wait-time 0
  set max-wait-time 0

  ; Initialize event type constants
  set EVENT_CREATION "creation"
  set EVENT_MOVEMENT_START "movement-start"
  set EVENT_MOVEMENT_COMPLETE "movement-complete"
  set EVENT_AMBULANCE_PICKUP "ambulance-pickup"
  set EVENT_HOSPITAL_ARRIVAL "hospital-arrival"
  set EVENT_TRIAGE_START "triage-start"
  set EVENT_TRIAGE_COMPLETE "triage-complete"
  set EVENT_TRIAGE_CHANGE "triage-change"
  set EVENT_TREATMENT_START "treatment-start"
  set EVENT_TREATMENT_COMPLETE "treatment-complete"
  set EVENT_TREATMENT_INTERRUPT "treatment-interrupt"
  set EVENT_ZONE_CHANGE "zone-change"
  set EVENT_HEALTH_DETERIORATION "health-deterioration"
  set EVENT_HEALTH_IMPROVEMENT "health-improvement"
  set EVENT_QUEUE_ENTRY "queue-entry"
  set EVENT_QUEUE_EXIT "queue-exit"
  set EVENT_DISCHARGE "discharge"
  set EVENT_DEATH "death"

  ; Create the incident scene
  create-incident-scene

  ; Create hospital zones and labels
  create-hospital-zones

  ; Create the hospital
  create-hospitals 1 [
    set shape "house"
    set color white
    set size 3
    setxy hospital-xcor hospital-ycor
    set triage-queue []
    set red-zone-queue []
    set yellow-zone-queue []
    set green-zone-queue []
    set blue-zone-queue []
    set icu-queue []
    set or-queue []
    set ward-queue []
  ]

  ; Create ambulances
  create-ambulances number-of-ambulances [
    let position-offset who - count hospitals
    set shape "truck"
    set color yellow
    set size 1.2
    setxy hospital-xcor + 8 hospital-ycor + (position-offset * 2)
    set status "idle"
    set capacity 2
    set loaded-casualties []
    set destination-xcor xcor
    set destination-ycor ycor
  ]

  ; Create casualties based on severity distribution
  create-casualties-at-scene
end

; Main simulation procedure
to go
  ; Stop if all casualties have been processed
  if all? casualties [location = "discharged" or location = "deceased"] [stop]

  ; Increment time
  set time-elapsed time-elapsed + 1

  ; Update casualties (deterioration, movement, etc.)
  ask casualties [update-casualty]

  ; Update ambulances
  ask ambulances [update-ambulance]

  ; Allow some green casualties to self-transport
  self-transport-green-casualties

  ; Update hospital processing
  ask hospitals [update-hospital]

  ; Check and run any scheduled tasks for casualties
  ask casualties [run-scheduled-tasks]

  if (ticks mod 50 = 0) [reallocate-all-resources]

  ; Update zone labels with current casualty counts
  update-zone-labels

  ; Update resource indicators
  update-resource-indicators

  ; Update statistics
  update-statistics

  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PATIENT STATE MANAGEMENT SECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Main casualty update procedure - centralized
to update-casualty
  ; Skip processing for deceased or discharged casualties
  ;if location = "deceased" or location = "discharged" [
;    update-casualty-color
;    stop
;  ]

  ; Check if the casualty is dead by severity
  if severity = 0 [
    set location "deceased"
    set deceased-count deceased-count + 1
    set moving? true
    set destination-xcor morgue-xcor
    set destination-ycor morgue-ycor
    update-casualty-color
    stop
  ]

  ; Handle movement if casualty is moving
  if moving? [handle-movement]

  ; Handle deterioration for untreated casualties
  if (not treatment-started?) and (location != "discharged") and (location != "deceased") [
    handle-deterioration
  ]

  ; Handle treatment progress updates for those in treatment
  if treatment-started? and location != "discharged" and location != "deceased" [
    update-treatment-progress
  ]

  ; Update visual appearance
  update-casualty-color
end

; Handle casualty movement
to handle-movement
  ; Move toward destination
  face patch destination-xcor destination-ycor

  ; Calculate distance to move this tick
  let move-speed 0.5
  let dist-to-dest distancexy destination-xcor destination-ycor

  ; If we can reach destination this tick, just arrive there
  ifelse dist-to-dest <= move-speed [
    setxy destination-xcor destination-ycor
    set moving? false

    ; Record movement complete event
    record-event EVENT_MOVEMENT_COMPLETE (word "Arrived at " location)
  ][
    ; Otherwise move toward destination
    forward move-speed
  ]
end

; Handle casualty health deterioration
to handle-deterioration
  ; Store original severity
  let original-severity severity

  ; Increment wait time if not being treated
  set wait-time wait-time + 1

  ; Deteriorate condition (lower RPM score) over time
  ; Update condition based on deterioration rate
  if random-float 1 < (deterioration-rate / 60) [
    set severity max list 0 (severity - 1)
    calculate-survival-probability

    ; Log deterioration

    ; Check for condition-based zone transitions
    check-condition-based-transfers

    ; Check if deterioration occurred
    if severity < original-severity [
    record-event EVENT_HEALTH_DETERIORATION (word "RPM decreased from " original-severity " to " severity)
  ]
  ]
end

; Check if casualty needs to be transferred based on their deteriorated condition
to check-condition-based-transfers
  ; Check if green zone patient condition has worsened to critical (red) level
  if location = "green-zone" and severity <= 3 [
    ; Try to move to red zone if there's capacity
    if has-zone-capacity? "red-zone" [
      handle-zone-transition "red-zone"
    ]
  ]

  ; Check if yellow zone patient condition has worsened to critical level
  if location = "yellow-zone" and severity <= 3 [
    ; Try to move to red zone or OR if there's capacity
    ifelse has-zone-capacity? "icu" [
      handle-zone-transition "icu"
    ][
      if has-zone-capacity? "red-zone" [
        handle-zone-transition "red-zone"
      ]
    ]
  ]
end


; Run scheduled tasks for this casualty
to run-scheduled-tasks
  ; Skip if no tasks
  if scheduled-tasks = 0 or empty? scheduled-tasks [stop]

  ; Create a list to store tasks to execute
  let tasks-to-run []
  let remaining-tasks []

  ; Check each task to see if it's due
  foreach scheduled-tasks [ task-item ->
    let due-time item 0 task-item
    let task-command item 1 task-item

    ifelse due-time <= time-elapsed [
      ; Task is due, add to execute list
      set tasks-to-run lput task-command tasks-to-run
    ][
      ; Task not due yet, keep in the list
      set remaining-tasks lput task-item remaining-tasks
    ]
  ]

  ; Replace the task list with remaining tasks
  set scheduled-tasks remaining-tasks

  ; Execute all due tasks
  foreach tasks-to-run [ task-command ->
    run task-command
  ]
end

; Schedule a personal task for this casualty
to schedule-personal-task [delay task-command]
  ; Add [current-time + delay, task-command] to this casualty's task list
  set scheduled-tasks lput (list (time-elapsed + delay) task-command) scheduled-tasks
end

; Central procedure for transitioning a casualty between zones
to handle-zone-transition [target-zone]
  let current-zone location

  ; Skip if already at target
  if current-zone = target-zone [stop]

  ; Record zone change event
  record-event EVENT_ZONE_CHANGE (word current-zone " → " target-zone)

  ; Check if target zone has capacity
  if not has-zone-capacity? target-zone [
    let fallback-zone determine-fallback-zone current-zone target-zone
    ifelse fallback-zone != "none" [set target-zone fallback-zone] [stop]
  ]

 ; If patient was in treatment, stop it
if treatment-started? [
  set treatment-started? false

  ; Free up resources from current zone if in treatment
  release-resources-from-casualty
]

  ; Begin movement to new zone
  set location target-zone
  set moving? true

  ; Record movement start event
  record-event EVENT_MOVEMENT_START (word "Moving to " target-zone)

  set treatment-started? false
  set treatment-progress 0

  ; Set coordinates based on target zone
  set-destination-coordinates target-zone

  ; Update queues
  update-zone-queues current-zone target-zone

  ; Store transition record
  let transition-record (word "Time " time-elapsed ": Casualty " who " moved from " current-zone " to " target-zone)
  set recorded-transitions lput transition-record recorded-transitions

  ; Record terminal events
  if target-zone = "discharged" [record-event EVENT_DISCHARGE "Patient discharged"]

  if target-zone = "deceased" [record-event EVENT_DEATH "Patient deceased"]
end

; Determine appropriate fallback zone if preferred zone is full
to-report determine-fallback-zone [current-zone target-zone]
  ; Define fallback order for each zone
  if target-zone = "red-zone" [
    ; If red zone full, try yellow, then OR, then ICU
    if has-zone-capacity? "yellow-zone" [report "yellow-zone"]
    if has-zone-capacity? "or" [report "or"]
    if has-zone-capacity? "icu" [report "icu"]
  ]

  if target-zone = "or" [
    ; If OR full, try ICU, then red zone
    if has-zone-capacity? "icu" [report "icu"]
    if has-zone-capacity? "ward" [report "ward"]
  ]

  if target-zone = "icu" [
    ; If ICU full, try OR, then red zone
    if has-zone-capacity? "or" [report "or"]
    if has-zone-capacity? "ward" [report "ward"]
  ]

  if target-zone = "yellow-zone" [
    ; If yellow zone full, try green or red depending on severity
    ifelse severity <= 3 [
      if has-zone-capacity? "red-zone" [report "red-zone"]
    ][
      if has-zone-capacity? "green-zone" [report "green-zone"]
    ]
  ]

  if target-zone = "green-zone" [
    ; If green zone full, try yellow or just discharge
    if has-zone-capacity? "yellow-zone" [report "yellow-zone"]
    ; For minor cases, discharge is a fallback
    if severity > 8 [report "discharged"]
  ]

  if target-zone = "ward" [
    ; If ward full and patient stable, consider discharge
    if severity > 8 [report "discharged"]
  ]

  ; No fallback available
  report "none"
end

; Set destination coordinates based on zone
to set-destination-coordinates [zone]
  if zone = "red-zone" [
    set destination-xcor red-zone-xcor + random 2 - 1
    set destination-ycor red-zone-ycor + random 2 - 1
  ]

  if zone = "yellow-zone" [
    set destination-xcor yellow-zone-xcor + random 2 - 1
    set destination-ycor yellow-zone-ycor + random 2 - 1
  ]

  if zone = "green-zone" [
    set destination-xcor green-zone-xcor + random 2 - 1
    set destination-ycor green-zone-ycor + random 2 - 1
  ]

  if zone = "blue-zone" [
    set destination-xcor blue-zone-xcor + random 2 - 1
    set destination-ycor blue-zone-ycor + random 2 - 1
  ]

  if zone = "or" [
    set destination-xcor or-xcor + random 2 - 1
    set destination-ycor or-ycor + random 2 - 1
  ]

  if zone = "icu" [
    set destination-xcor icu-xcor + random 2 - 1
    set destination-ycor icu-ycor + random 2 - 1
  ]

  if zone = "ward" [
    set destination-xcor ward-xcor + random 2 - 1
    set destination-ycor ward-ycor + random 2 - 1
  ]

  if zone = "triage-queue" [
    set destination-xcor triage-xcor + random 2 - 1
    set destination-ycor triage-ycor + random 2 - 1
  ]

  if zone = "discharged" [
    set destination-xcor discharge-xcor + random 2 - 1
    set destination-ycor discharge-ycor + random 2 - 1
  ]

  if zone = "deceased" [
    set destination-xcor morgue-xcor + random 2 - 1
    set destination-ycor morgue-ycor + random 2 - 1
  ]
end

; Update hospital queues when a casualty changes zones
to update-zone-queues [from-zone to-zone]
  let current-casualty self

  ; Remove from old queue if applicable
  if from-zone = "triage-queue" [
    ask one-of hospitals [
      set triage-queue filter [c -> c != current-casualty] triage-queue
    ]
  ]

  if from-zone = "red-zone" [
    ask one-of hospitals [
      set red-zone-queue filter [c -> c != current-casualty] red-zone-queue
    ]
  ]

  if from-zone = "yellow-zone" [
    ask one-of hospitals [
      set yellow-zone-queue filter [c -> c != current-casualty] yellow-zone-queue
    ]
  ]

  if from-zone = "green-zone" [
    ask one-of hospitals [
      set green-zone-queue filter [c -> c != current-casualty] green-zone-queue
    ]
  ]

  if from-zone = "blue-zone" [
    ask one-of hospitals [
      set blue-zone-queue filter [c -> c != current-casualty] blue-zone-queue
    ]
  ]

  if from-zone = "or" [
    ask one-of hospitals [
      set or-queue filter [c -> c != current-casualty] or-queue
    ]
  ]

  if from-zone = "icu" [
    ask one-of hospitals [
      set icu-queue filter [c -> c != current-casualty] icu-queue
    ]
  ]

  if from-zone = "ward" [
    ask one-of hospitals [
      set ward-queue filter [c -> c != current-casualty] ward-queue
    ]
  ]

  ; Add to new queue if applicable
  if to-zone = "triage-queue" [
    ask one-of hospitals [
      set triage-queue lput current-casualty triage-queue
    ]
  ]

  if to-zone = "red-zone" [
    ask one-of hospitals [
      set red-zone-queue lput current-casualty red-zone-queue
    ]
  ]

  if to-zone = "yellow-zone" [
    ask one-of hospitals [
      set yellow-zone-queue lput current-casualty yellow-zone-queue
    ]
  ]

  if to-zone = "green-zone" [
    ask one-of hospitals [
      set green-zone-queue lput current-casualty green-zone-queue
    ]
  ]

  if to-zone = "blue-zone" [
    ask one-of hospitals [
      set blue-zone-queue lput current-casualty blue-zone-queue
    ]
  ]

  if to-zone = "or" [
    ask one-of hospitals [
      set or-queue lput current-casualty or-queue
    ]
  ]

  if to-zone = "icu" [
    ask one-of hospitals [
      set icu-queue lput current-casualty icu-queue
    ]
  ]

  if to-zone = "ward" [
    ask one-of hospitals [
      set ward-queue lput current-casualty ward-queue
    ]
  ]

  ; Update discharge/deceased counts
  if to-zone = "discharged" [
    set discharged-count discharged-count + 1
  ]

  if to-zone = "deceased" [
    set deceased-count deceased-count + 1
  ]
end

; Check if a zone has available capacity
to-report has-zone-capacity? [zone]
  if zone = "red-zone" [
    report count casualties with [location = "red-zone"] < red-zone-capacity
  ]

  if zone = "yellow-zone" [
    report count casualties with [location = "yellow-zone"] < yellow-zone-capacity
  ]

  if zone = "green-zone" [
    report count casualties with [location = "green-zone"] < green-zone-capacity
  ]

  if zone = "blue-zone" [
    report count casualties with [location = "blue-zone"] < blue-zone-capacity
  ]

  if zone = "or" [
    report count casualties with [location = "or"] < or-capacity
  ]

  if zone = "icu" [
    report count casualties with [location = "icu"] < icu-capacity
  ]

  if zone = "ward" [
    report count casualties with [location = "ward"] < ward-capacity
  ]

  if zone = "triage-queue" [
    report count casualties with [location = "triage-queue"] < triage-capacity
  ]

  ; Discharged and deceased always have capacity
  if zone = "discharged" or zone = "deceased" [
    report true
  ]

  ; Default
  report false
end

; Complete treatment for the current casualty
to complete-treatment

  ; Release resources for current zone if in treatment
  if treatment-started? [
    release-zone-resources location
  ]

  ; RED ZONE treatment completion
  if location = "red-zone" [
    ; Critical patients with RPM ≤ 3 need surgery
    if severity <= 3 [
      ; Route to OR or ICU based on availability
      ifelse has-zone-capacity? "or" and check-resource-availability "or" [
        handle-zone-transition "or"
      ][
        ifelse has-zone-capacity? "icu" and check-resource-availability "icu" [
          handle-zone-transition "icu"
        ][
          ; Keep in red zone if no capacity elsewhere
          set treatment-started? false
          set treatment-progress 0

          ; Add back to red zone queue for another attempt
          ask one-of hospitals [
            set red-zone-queue lput myself red-zone-queue
          ]
        ]
      ]
    ]

    ; Patients with RPM 4-8 (moderate) go to yellow zone
    if severity > 3 and severity <= 8 [
      ifelse has-zone-capacity? "yellow-zone" [
        handle-zone-transition "yellow-zone"
      ][
        ; Yellow zone not available, keep in red zone
        set treatment-started? false
        set treatment-progress 0

        ; Add back to red zone queue
        ask one-of hospitals [
          set red-zone-queue lput myself red-zone-queue
        ]
      ]
    ]

    ; Patients with RPM > 8 (minor) go to green zone or discharge
    if severity > 8 [
      ifelse has-zone-capacity? "green-zone" [
        handle-zone-transition "green-zone"
      ][
        ; Green zone not available, discharge directly
        handle-zone-transition "discharged"
      ]
    ]
  ]

  ; YELLOW ZONE treatment completion
  if location = "yellow-zone" [
    ; Determine outcome based on RPM
    ifelse severity <= 3 [
      ; Deteriorated patient needs higher level care
      ifelse has-zone-capacity? "or" and check-resource-availability "or" [
        handle-zone-transition "or"
      ][
        ifelse has-zone-capacity? "icu" and check-resource-availability "icu" [
          handle-zone-transition "icu"
        ][
          ifelse has-zone-capacity? "ward" [
            handle-zone-transition "ward"
          ][
            ; Keep in yellow as nothing else available
            set treatment-started? false
            set treatment-progress 0

            ; Add back to yellow zone queue
            ask one-of hospitals [
              set yellow-zone-queue lput myself yellow-zone-queue
            ]
          ]
        ]
      ]
    ][
      ifelse severity > 8 [
        ; Patient has improved to minor status
        handle-zone-transition "discharged"
      ][
        ; Standard moderate patient to ward if available
        ifelse has-zone-capacity? "ward" [
          handle-zone-transition "ward"
        ][
          ; Ward not available, keep in yellow zone
          set treatment-started? false
          set treatment-progress 0

          ; Add back to yellow zone queue
          ask one-of hospitals [
            set yellow-zone-queue lput myself yellow-zone-queue
          ]
        ]
      ]
    ]
  ]

  ; GREEN ZONE treatment completion
  if location = "green-zone" [
    ; Outcome depends on severity
    ifelse severity > 8 [
      ; Direct discharge for minor injuries
      handle-zone-transition "discharged"
    ][
      ; More severe cases to ward if available
      ifelse has-zone-capacity? "ward" [
        handle-zone-transition "ward"
      ][
        ; If ward not available, discharge anyway
        handle-zone-transition "discharged"
      ]
    ]
  ]

  ; BLUE ZONE treatment completion
  if location = "blue-zone" [
    ; Blue zone (expectant) patients have lower chance of survival
    ifelse random-float 1 < survival-probability [
      ; Check ward capacity for survivors
      ifelse has-zone-capacity? "ward" [
        handle-zone-transition "ward"
      ][
        ; Ward not available, discharge directly
        handle-zone-transition "discharged"
      ]
    ][
      ; Patient did not survive
      handle-zone-transition "deceased"
    ]
  ]

  ; WARD treatment completion
 if location = "ward" [
  ; Final health improvement from ward care
  set severity min list 12 (severity + 1)
  calculate-survival-probability

  ; Check survival with very high probability
  let ward-survival-probability min list 1 (survival-probability * 2.0)

  ifelse random-float 1 < ward-survival-probability [
    ; Discharge patient
    handle-zone-transition "discharged"
  ][
    ; Small chance of death in ward
    handle-zone-transition "deceased"
  ]
]

  ; Update appearance
  update-casualty-color
end

; Handle OR completion (called when OR treatment reaches maximum progress)
to handle-or-completion
  ; Improve condition from surgery
  set severity max list 12 (severity + 2)
  calculate-survival-probability

  ; Check if patient survived surgery
  ifelse random-float 1 < survival-probability [
    ; Patient survived - now either ICU or ward
    ifelse severity <= 4 [
      ; Critical patients go to ICU if available
      ifelse has-zone-capacity? "icu" and check-resource-availability "icu" [
        handle-zone-transition "icu"
      ][
        ; ICU full - try ward or discharge
        ifelse has-zone-capacity? "ward" [
          handle-zone-transition "ward"
        ][
          ; Direct discharge if no other options
          handle-zone-transition "discharged"
        ]
      ]
    ][
      ; Stable patients (RPM > 4) go to ward or discharge
      ifelse has-zone-capacity? "ward" [
        handle-zone-transition "ward"
      ][
        handle-zone-transition "discharged"
      ]
    ]
  ][
    ; Patient did not survive surgery
    handle-zone-transition "deceased"
  ]

  ; Release OR resources
  release-zone-resources "or"

end

; Handle ICU completion (called when ICU treatment reaches maximum progress)
to handle-icu-completion
  ; Improve condition from ICU care
  set severity min list 12 (severity + 2)
  calculate-survival-probability

   ifelse random-float 1 < survival-probability [
   ; Patient survived - now either ICU or ward

      ; Check if patient needs surgery vs discharge/ward
      ifelse severity <= 3 [
        ; Critical patients may need surgery
        ifelse has-zone-capacity? "or" and check-resource-availability "or" [
          handle-zone-transition "or"
        ][
          ; OR not available - try ward or discharge
          ifelse has-zone-capacity? "ward" [
            handle-zone-transition "ward"
          ][
            handle-zone-transition "discharged"
          ]
        ]
      ][
        ; Stable enough for ward or discharge
        ifelse has-zone-capacity? "ward" [
          handle-zone-transition "ward"
        ][
          handle-zone-transition "discharged"
        ]
      ]
  ][
    ; Patient did not survive surgery
    handle-zone-transition "deceased"
  ]

  ; Release ICU resources
  release-zone-resources "icu"


end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RESOURCE MANAGEMENT SECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Check if required resources are available for a zone
to-report check-resource-availability [zone]
  ; Always ensure minimum resources are maintained
  if (doctors-available < doctors-minimum-guarantee) or
     (nurses-available < nurses-minimum-guarantee) [
    report false
  ]

  ; Different resource requirements for each zone based on per-seat values
  if zone = "or" [
    report doctors-available >= or-doctors-per-seat and nurses-available >= or-nurses-per-seat
  ]

  if zone = "icu" [
    report doctors-available >= icu-doctors-per-seat and nurses-available >= icu-nurses-per-seat
  ]

  if zone = "red-zone" [
    report doctors-available >= red-zone-doctors-per-seat and nurses-available >= red-zone-nurses-per-seat
  ]

  if zone = "yellow-zone" [
    report doctors-available >= yellow-zone-doctors-per-seat and nurses-available >= yellow-zone-nurses-per-seat
  ]

  if zone = "green-zone" [
    report doctors-available >= green-zone-doctors-per-seat and nurses-available >= green-zone-nurses-per-seat
  ]

  if zone = "triage-queue" [
    report (doctors-available >= triage-doctors-per-seat) or (nurses-available >= triage-nurses-per-seat)
  ]

  if zone = "blue-zone" [
    report doctors-available >= blue-zone-doctors-per-seat and nurses-available >= blue-zone-nurses-per-seat
  ]

  if zone = "ward" [
    report doctors-available >= ward-doctors-per-seat and nurses-available >= ward-nurses-per-seat
  ]

  ; Default
  report false
end

; Allocate resources for treatment in a specific zone
to allocate-zone-resources [zone]
  ; First check if this casualty already has resources allocated
  if not empty? resources-allocated [
    ; Already has resources - this is an error condition
    release-resources-from-casualty
  ]

  ; Special case for when resources are extremely scarce but treatment must continue
  if zone = "red-zone" or zone = "yellow-zone" or zone = "green-zone" or zone = "blue-zone" [
    if doctors-available < 1 and nurses-available < 1 [
      ; Record the scarce resource situation but allow treatment to proceed
      ; We're allocating symbolic resources to allow treatment flow to continue
      set resources-allocated (list zone 0 0)
      stop  ; Use 'stop' instead of 'return'
    ]
  ]

  ; Different resource requirements per zone
  if zone = "or" [
    ; Check if we have the full required resources
    ifelse doctors-available >= or-doctors-per-seat and nurses-available >= or-nurses-per-seat [
      set doctors-available doctors-available - or-doctors-per-seat
      set nurses-available nurses-available - or-nurses-per-seat
      set doctors-in-or doctors-in-or + or-doctors-per-seat
      set nurses-in-or nurses-in-or + or-nurses-per-seat
      set resources-allocated (list zone or-doctors-per-seat or-nurses-per-seat)
    ][
      ; For OR, we must have minimum resources or procedures can't happen
      ; Allocate what we can, even if it's just 1 doctor and 1 nurse
      let doctors-to-allocate min list doctors-available or-doctors-per-seat
      let nurses-to-allocate min list nurses-available or-nurses-per-seat

      ; Only proceed if we have at least one of each
      if doctors-to-allocate >= 1 and nurses-to-allocate >= 1 [
        set doctors-available doctors-available - doctors-to-allocate
        set nurses-available nurses-available - nurses-to-allocate
        set doctors-in-or doctors-in-or + doctors-to-allocate
        set nurses-in-or nurses-in-or + nurses-to-allocate
        set resources-allocated (list zone doctors-to-allocate nurses-to-allocate)
      ]
    ]
  ]

  if zone = "icu" [
    ; Check if we have the full required resources
    ifelse doctors-available >= icu-doctors-per-seat and nurses-available >= icu-nurses-per-seat [
      set doctors-available doctors-available - icu-doctors-per-seat
      set nurses-available nurses-available - icu-nurses-per-seat
      set doctors-in-icu doctors-in-icu + icu-doctors-per-seat
      set nurses-in-icu nurses-in-icu + icu-nurses-per-seat
      set resources-allocated (list zone icu-doctors-per-seat icu-nurses-per-seat)
    ][
      ; For ICU, try to allocate at least minimum resources
      let doctors-to-allocate min list doctors-available icu-doctors-per-seat
      let nurses-to-allocate min list nurses-available icu-nurses-per-seat

      ; Only proceed if we have at least one of each
      if doctors-to-allocate >= 1 and nurses-to-allocate >= 1 [
        set doctors-available doctors-available - doctors-to-allocate
        set nurses-available nurses-available - nurses-to-allocate
        set doctors-in-icu doctors-in-icu + doctors-to-allocate
        set nurses-in-icu nurses-in-icu + nurses-to-allocate
        set resources-allocated (list zone doctors-to-allocate nurses-to-allocate)
      ]
    ]
  ]

  if zone = "red-zone" [
    ; Try to allocate full resources
    ifelse doctors-available >= red-zone-doctors-per-seat and nurses-available >= red-zone-nurses-per-seat [
      set doctors-available doctors-available - red-zone-doctors-per-seat
      set nurses-available nurses-available - red-zone-nurses-per-seat
      set doctors-in-red doctors-in-red + red-zone-doctors-per-seat
      set nurses-in-red nurses-in-red + red-zone-nurses-per-seat
      set resources-allocated (list zone red-zone-doctors-per-seat red-zone-nurses-per-seat)
    ][
      ; Allocate whatever we can, even partial resources
      let doctors-to-allocate min list doctors-available red-zone-doctors-per-seat
      let nurses-to-allocate min list nurses-available red-zone-nurses-per-seat

      ; Prefer at least one of each but can work with just one type
      ifelse doctors-to-allocate >= 1 or nurses-to-allocate >= 1 [
        ; Allocate available doctors
        if doctors-to-allocate > 0 [
          set doctors-available doctors-available - doctors-to-allocate
          set doctors-in-red doctors-in-red + doctors-to-allocate
        ]

        ; Allocate available nurses
        if nurses-to-allocate > 0 [
          set nurses-available nurses-available - nurses-to-allocate
          set nurses-in-red nurses-in-red + nurses-to-allocate
        ]

        set resources-allocated (list zone doctors-to-allocate nurses-to-allocate)
      ][
        ; No resources available, mark as allocated anyway to track treatment
        set resources-allocated (list zone 0 0)
      ]
    ]
  ]

  if zone = "yellow-zone" [
    ; Try to allocate full resources
    ifelse doctors-available >= yellow-zone-doctors-per-seat and nurses-available >= yellow-zone-nurses-per-seat [
      set doctors-available doctors-available - yellow-zone-doctors-per-seat
      set nurses-available nurses-available - yellow-zone-nurses-per-seat
      set doctors-in-yellow doctors-in-yellow + yellow-zone-doctors-per-seat
      set nurses-in-yellow nurses-in-yellow + yellow-zone-nurses-per-seat
      set resources-allocated (list zone yellow-zone-doctors-per-seat yellow-zone-nurses-per-seat)
    ][
      ; Allocate whatever we can, even partial resources
      let doctors-to-allocate min list doctors-available yellow-zone-doctors-per-seat
      let nurses-to-allocate min list nurses-available yellow-zone-nurses-per-seat

      ; Prefer at least one staff type
      ifelse doctors-to-allocate >= 1 or nurses-to-allocate >= 1 [
        ; Allocate available doctors
        if doctors-to-allocate > 0 [
          set doctors-available doctors-available - doctors-to-allocate
          set doctors-in-yellow doctors-in-yellow + doctors-to-allocate
        ]

        ; Allocate available nurses
        if nurses-to-allocate > 0 [
          set nurses-available nurses-available - nurses-to-allocate
          set nurses-in-yellow nurses-in-yellow + nurses-to-allocate
        ]

        set resources-allocated (list zone doctors-to-allocate nurses-to-allocate)
      ][
        ; No resources available, mark as allocated anyway to track treatment
        set resources-allocated (list zone 0 0)
      ]
    ]
  ]

  if zone = "green-zone" [
    ; Try to allocate full resources
    ifelse doctors-available >= green-zone-doctors-per-seat and nurses-available >= green-zone-nurses-per-seat [
      set doctors-available doctors-available - green-zone-doctors-per-seat
      set nurses-available nurses-available - green-zone-nurses-per-seat
      set doctors-in-green doctors-in-green + green-zone-doctors-per-seat
      set nurses-in-green nurses-in-green + green-zone-nurses-per-seat
      set resources-allocated (list zone green-zone-doctors-per-seat green-zone-nurses-per-seat)
    ][
      ; Green zone can operate with minimal staffing
      let doctors-to-allocate min list doctors-available green-zone-doctors-per-seat
      let nurses-to-allocate min list nurses-available green-zone-nurses-per-seat

      ; Green zone can function even with just nurses
      ifelse nurses-to-allocate >= 1 [
        ; Allocate available doctors
        if doctors-to-allocate > 0 [
          set doctors-available doctors-available - doctors-to-allocate
          set doctors-in-green doctors-in-green + doctors-to-allocate
        ]

        ; Allocate available nurses
        set nurses-available nurses-available - nurses-to-allocate
        set nurses-in-green nurses-in-green + nurses-to-allocate

        set resources-allocated (list zone doctors-to-allocate nurses-to-allocate)
      ][
        ; Even if no nurses, allow treatment in green zone in extreme cases
        set resources-allocated (list zone 0 0)
      ]
    ]
  ]

  if zone = "triage-queue" [
    ; Use doctor if available, otherwise nurse
    ifelse doctors-available >= triage-doctors-per-seat and triage-doctors-per-seat > 0 [
      set doctors-available doctors-available - triage-doctors-per-seat
      set doctors-in-triage doctors-in-triage + triage-doctors-per-seat
      set resources-allocated (list zone triage-doctors-per-seat 0)
    ][
      ifelse nurses-available >= triage-nurses-per-seat and triage-nurses-per-seat > 0 [
        set nurses-available nurses-available - triage-nurses-per-seat
        set nurses-in-triage nurses-in-triage + triage-nurses-per-seat
        set resources-allocated (list zone 0 triage-nurses-per-seat)
      ][
        ; In extreme cases, allow triage without resources
        set resources-allocated (list zone 0 0)
      ]
    ]
  ]

  if zone = "blue-zone" [
    ; Try to allocate full resources
    ifelse doctors-available >= blue-zone-doctors-per-seat and nurses-available >= blue-zone-nurses-per-seat [
      set doctors-available doctors-available - blue-zone-doctors-per-seat
      set nurses-available nurses-available - blue-zone-nurses-per-seat
      set doctors-in-blue doctors-in-blue + blue-zone-doctors-per-seat
      set nurses-in-blue nurses-in-blue + blue-zone-nurses-per-seat
      set resources-allocated (list zone blue-zone-doctors-per-seat blue-zone-nurses-per-seat)
    ][
      ; Blue zone is for palliative care, can function with minimal staffing
      let doctors-to-allocate min list doctors-available blue-zone-doctors-per-seat
      let nurses-to-allocate min list nurses-available blue-zone-nurses-per-seat

      ; Prefer nurses for palliative care
      ifelse nurses-to-allocate >= 1 [
        ; Allocate available doctors
        if doctors-to-allocate > 0 [
          set doctors-available doctors-available - doctors-to-allocate
          set doctors-in-blue doctors-in-blue + doctors-to-allocate
        ]

        ; Allocate available nurses
        set nurses-available nurses-available - nurses-to-allocate
        set nurses-in-blue nurses-in-blue + nurses-to-allocate

        set resources-allocated (list zone doctors-to-allocate nurses-to-allocate)
      ][
        ; Allow treatment even without resources in extreme cases
        set resources-allocated (list zone 0 0)
      ]
    ]
  ]

  if zone = "ward" [
    ; Try to allocate full resources
    ifelse doctors-available >= ward-doctors-per-seat and nurses-available >= ward-nurses-per-seat [
      set doctors-available doctors-available - ward-doctors-per-seat
      set nurses-available nurses-available - ward-nurses-per-seat
      set doctors-in-ward doctors-in-ward + ward-doctors-per-seat
      set nurses-in-ward nurses-in-ward + ward-nurses-per-seat
      set resources-allocated (list zone ward-doctors-per-seat ward-nurses-per-seat)
    ][
      ; Ward can operate with nurses primarily
      let doctors-to-allocate min list doctors-available ward-doctors-per-seat
      let nurses-to-allocate min list nurses-available ward-nurses-per-seat

      ifelse nurses-to-allocate >= 1 [
        ; Allocate available doctors
        if doctors-to-allocate > 0 [
          set doctors-available doctors-available - doctors-to-allocate
          set doctors-in-ward doctors-in-ward + doctors-to-allocate
        ]

        ; Allocate available nurses
        set nurses-available nurses-available - nurses-to-allocate
        set nurses-in-ward nurses-in-ward + nurses-to-allocate

        set resources-allocated (list zone doctors-to-allocate nurses-to-allocate)
      ][
        ; Allow treatment even without resources in extreme cases
        set resources-allocated (list zone 0 0)
      ]
    ]
  ]
end

to release-resources-from-casualty
  ; Skip if no resources allocated
  if empty? resources-allocated [stop]

  ; Extract zone and resource counts
  let zone item 0 resources-allocated
  let doctors-count item 1 resources-allocated
  let nurses-count item 2 resources-allocated

  ; Release doctors
  set doctors-available doctors-available + doctors-count

  ; Update zone counters for doctors
  if zone = "or" [set doctors-in-or doctors-in-or - doctors-count]
  if zone = "icu" [set doctors-in-icu doctors-in-icu - doctors-count]
  if zone = "red-zone" [set doctors-in-red doctors-in-red - doctors-count]
  if zone = "yellow-zone" [set doctors-in-yellow doctors-in-yellow - doctors-count]
  if zone = "triage-queue" [set doctors-in-triage doctors-in-triage - doctors-count]

  ; Release nurses
  set nurses-available nurses-available + nurses-count

  ; Update zone counters for nurses
  if zone = "or" [set nurses-in-or nurses-in-or - nurses-count]
  if zone = "icu" [set nurses-in-icu nurses-in-icu - nurses-count]
  if zone = "red-zone" [set nurses-in-red nurses-in-red - nurses-count]
  if zone = "yellow-zone" [set nurses-in-yellow nurses-in-yellow - nurses-count]
  if zone = "green-zone" [set nurses-in-green nurses-in-green - nurses-count]
  if zone = "triage-queue" [set nurses-in-triage nurses-in-triage - nurses-count]
  if zone = "blue-zone" [set nurses-in-blue nurses-in-blue - nurses-count]
  if zone = "ward" [set nurses-in-ward nurses-in-ward - nurses-count]

  ; Clear the resources-allocated list
  set resources-allocated []
end

; Release resources from a zone
to release-zone-resources [zone]
  ; This now simply calls release-resources-from-casualty
  release-resources-from-casualty
end

to validate-resource-allocation
  ; Calculate total allocated doctors and nurses
  let total-doctors-allocated (
    doctors-in-or + doctors-in-icu + doctors-in-red +
    doctors-in-yellow + doctors-in-green + doctors-in-triage +
    doctors-in-blue + doctors-in-ward
  )

  let total-nurses-allocated (
    nurses-in-or + nurses-in-icu + nurses-in-red +
    nurses-in-yellow + nurses-in-green + nurses-in-triage +
    nurses-in-blue + nurses-in-ward
  )

  ; Check for inconsistencies
  let doctors-expected (number-of-doctors - doctors-available)
  let nurses-expected (number-of-nurses - nurses-available)

  if doctors-expected != total-doctors-allocated [
    ; Fix the inconsistency
    let difference doctors-expected - total-doctors-allocated
    set doctors-available doctors-available - difference
  ]

  if nurses-expected != total-nurses-allocated [
    ; Fix the inconsistency
    let difference nurses-expected - total-nurses-allocated
    set nurses-available nurses-available - difference
  ]
end

; Main resource allocation procedure
to reallocate-all-resources
  ; Reset all allocated resources
  reset-allocated-resources

  ifelse prioritize-minimum-staffing [
    allocate-minimum-staffing
    allocate-remaining-resources-by-priority
  ][
    allocate-resources-by-priority
  ]

  ; Update the display
  update-resource-indicators
end

; Reset all allocated resources to zero
to reset-allocated-resources
  ; Reset counters
  set doctors-in-or 0
  set doctors-in-icu 0
  set doctors-in-red 0
  set doctors-in-yellow 0
  set doctors-in-green 0
  set doctors-in-triage 0
  set doctors-in-blue 0
  set doctors-in-ward 0

  set nurses-in-or 0
  set nurses-in-icu 0
  set nurses-in-red 0
  set nurses-in-yellow 0
  set nurses-in-green 0
  set nurses-in-triage 0
  set nurses-in-blue 0
  set nurses-in-ward 0

  ; Reset available resources
  set doctors-available number-of-doctors
  set nurses-available number-of-nurses
end

; Allocate at least one staff to each zone to ensure operation
to allocate-minimum-staffing
  ; Prioritize key departments first: OR, ICU, Red Zone

  ; Operating Room - highest priority
  if or-doctors-per-seat > 0 and doctors-available >= or-doctors-per-seat [
    set doctors-in-or or-doctors-per-seat
    set doctors-available doctors-available - or-doctors-per-seat
  ]
  if or-nurses-per-seat > 0 and nurses-available >= or-nurses-per-seat [
    set nurses-in-or or-nurses-per-seat
    set nurses-available nurses-available - or-nurses-per-seat
  ]

  ; ICU - second priority
  if icu-doctors-per-seat > 0 and doctors-available >= icu-doctors-per-seat [
    set doctors-in-icu icu-doctors-per-seat
    set doctors-available doctors-available - icu-doctors-per-seat
  ]
  if icu-nurses-per-seat > 0 and nurses-available >= icu-nurses-per-seat [
    set nurses-in-icu icu-nurses-per-seat
    set nurses-available nurses-available - icu-nurses-per-seat
  ]

  ; Red Zone - third priority
  if red-zone-doctors-per-seat > 0 and doctors-available >= red-zone-doctors-per-seat [
    set doctors-in-red red-zone-doctors-per-seat
    set doctors-available doctors-available - red-zone-doctors-per-seat
  ]
  if red-zone-nurses-per-seat > 0 and nurses-available >= red-zone-nurses-per-seat [
    set nurses-in-red red-zone-nurses-per-seat
    set nurses-available nurses-available - red-zone-nurses-per-seat
  ]

  ; Yellow Zone
  if yellow-zone-doctors-per-seat > 0 and doctors-available >= yellow-zone-doctors-per-seat [
    set doctors-in-yellow yellow-zone-doctors-per-seat
    set doctors-available doctors-available - yellow-zone-doctors-per-seat
  ]
  if yellow-zone-nurses-per-seat > 0 and nurses-available >= yellow-zone-nurses-per-seat [
    set nurses-in-yellow yellow-zone-nurses-per-seat
    set nurses-available nurses-available - yellow-zone-nurses-per-seat
  ]

  ; Triage
  if triage-doctors-per-seat > 0 and doctors-available >= triage-doctors-per-seat [
    set doctors-in-triage triage-doctors-per-seat
    set doctors-available doctors-available - triage-doctors-per-seat
  ]
  if triage-nurses-per-seat > 0 and nurses-available >= triage-nurses-per-seat [
    set nurses-in-triage triage-nurses-per-seat
    set nurses-available nurses-available - triage-nurses-per-seat
  ]

  ; Green Zone
  if green-zone-doctors-per-seat > 0 and doctors-available >= green-zone-doctors-per-seat [
    set doctors-in-green green-zone-doctors-per-seat
    set doctors-available doctors-available - green-zone-doctors-per-seat
  ]
  if green-zone-nurses-per-seat > 0 and nurses-available >= green-zone-nurses-per-seat [
    set nurses-in-green green-zone-nurses-per-seat
    set nurses-available nurses-available - green-zone-nurses-per-seat
  ]

  ; Blue Zone
  if blue-zone-doctors-per-seat > 0 and doctors-available >= blue-zone-doctors-per-seat [
    set doctors-in-blue blue-zone-doctors-per-seat
    set doctors-available doctors-available - blue-zone-doctors-per-seat
  ]
  if blue-zone-nurses-per-seat > 0 and nurses-available >= blue-zone-nurses-per-seat [
    set nurses-in-blue blue-zone-nurses-per-seat
    set nurses-available nurses-available - blue-zone-nurses-per-seat
  ]

  ; Ward
  if ward-doctors-per-seat > 0 and doctors-available >= ward-doctors-per-seat [
    set doctors-in-ward ward-doctors-per-seat
    set doctors-available doctors-available - ward-doctors-per-seat
  ]
  if ward-nurses-per-seat > 0 and nurses-available >= ward-nurses-per-seat [
    set nurses-in-ward ward-nurses-per-seat
    set nurses-available nurses-available - ward-nurses-per-seat
  ]
end

; Allocate resources prioritizing critical areas
to allocate-resources-by-priority
  ; Create lists of departments with their priority and requirement
  let doctor-departments (list
    (list "or" or-doctors-required 1)     ; Department, requirement, priority (1 = highest)
    (list "icu" icu-doctors-required 2)
    (list "red" red-zone-doctors-required 3)
    (list "yellow" yellow-zone-doctors-required 4)
    (list "triage" triage-doctors-required 5)
    (list "green" green-zone-doctors-required 6)
    (list "blue" blue-zone-doctors-required 7)
    (list "ward" ward-doctors-required 8)
  )

  let nurse-departments (list
    (list "or" or-nurses-required 1)
    (list "icu" icu-nurses-required 2)
    (list "red" red-zone-nurses-required 3)
    (list "yellow" yellow-zone-nurses-required 4)
    (list "triage" triage-nurses-required 5)
    (list "green" green-zone-nurses-required 6)
    (list "blue" blue-zone-nurses-required 7)
    (list "ward" ward-nurses-required 8)
  )

  ; Sort by priority
  set doctor-departments sort-by [[a b] -> item 2 a < item 2 b] doctor-departments
  set nurse-departments sort-by [[a b] -> item 2 a < item 2 b] nurse-departments

  ; Allocate doctors by priority
  foreach doctor-departments [ dept ->
    let department item 0 dept
    let required item 1 dept

    let to-allocate min (list required doctors-available)
    set doctors-available doctors-available - to-allocate

    ; Update the appropriate counter
    if department = "or" [set doctors-in-or to-allocate]
    if department = "icu" [set doctors-in-icu to-allocate]
    if department = "red" [set doctors-in-red to-allocate]
    if department = "yellow" [set doctors-in-yellow to-allocate]
    if department = "green" [set doctors-in-green to-allocate]
    if department = "triage" [set doctors-in-triage to-allocate]
    if department = "blue" [set doctors-in-blue to-allocate]
    if department = "ward" [set doctors-in-ward to-allocate]
  ]

  ; Allocate nurses by priority
  foreach nurse-departments [ dept ->
    let department item 0 dept
    let required item 1 dept

    let to-allocate min (list required nurses-available)
    set nurses-available nurses-available - to-allocate

    ; Update the appropriate counter
    if department = "or" [set nurses-in-or to-allocate]
    if department = "icu" [set nurses-in-icu to-allocate]
    if department = "red" [set nurses-in-red to-allocate]
    if department = "yellow" [set nurses-in-yellow to-allocate]
    if department = "green" [set nurses-in-green to-allocate]
    if department = "triage" [set nurses-in-triage to-allocate]
    if department = "blue" [set nurses-in-blue to-allocate]
    if department = "ward" [set nurses-in-ward to-allocate]
  ]
end

; Allocate remaining resources after minimum staffing
to allocate-remaining-resources-by-priority
  ; Similar to allocate-resources-by-priority but distributes remaining staff
  ; Create lists of departments with their priority and requirement
  let doctor-departments (list
    (list "or" (or-doctors-required - doctors-in-or) 1)     ; Department, remaining need, priority
    (list "icu" (icu-doctors-required - doctors-in-icu) 2)
    (list "red" (red-zone-doctors-required - doctors-in-red) 3)
    (list "yellow" (yellow-zone-doctors-required - doctors-in-yellow) 4)
    (list "triage" (triage-doctors-required - doctors-in-triage) 5)
    (list "green" (green-zone-doctors-required - doctors-in-green) 6)
    (list "blue" (blue-zone-doctors-required - doctors-in-blue) 7)
    (list "ward" (ward-doctors-required - doctors-in-ward) 8)
  )

  let nurse-departments (list
    (list "or" (or-nurses-required - nurses-in-or) 1)
    (list "icu" (icu-nurses-required - nurses-in-icu) 2)
    (list "red" (red-zone-nurses-required - nurses-in-red) 3)
    (list "yellow" (yellow-zone-nurses-required - nurses-in-yellow) 4)
    (list "triage" (triage-nurses-required - nurses-in-triage) 5)
    (list "green" (green-zone-nurses-required - nurses-in-green) 6)
    (list "blue" (blue-zone-nurses-required - nurses-in-blue) 7)
    (list "ward" (ward-nurses-required - nurses-in-ward) 8)
  )

  ; Filter out departments that don't need more staff
  set doctor-departments filter [dept -> item 1 dept > 0] doctor-departments
  set nurse-departments filter [dept -> item 1 dept > 0] nurse-departments

  ; Sort by priority
  set doctor-departments sort-by [[a b] -> item 2 a < item 2 b] doctor-departments
  set nurse-departments sort-by [[a b] -> item 2 a < item 2 b] nurse-departments

  ; Allocate remaining doctors by priority
  foreach doctor-departments [ dept ->
    let department item 0 dept
    let needed item 1 dept

    let to-allocate min (list needed doctors-available)
    set doctors-available doctors-available - to-allocate

    ; Update the appropriate counter
    if department = "or" [set doctors-in-or doctors-in-or + to-allocate]
    if department = "icu" [set doctors-in-icu doctors-in-icu + to-allocate]
    if department = "red" [set doctors-in-red doctors-in-red + to-allocate]
    if department = "yellow" [set doctors-in-yellow doctors-in-yellow + to-allocate]
    if department = "green" [set doctors-in-green doctors-in-green + to-allocate]
    if department = "triage" [set doctors-in-triage doctors-in-triage + to-allocate]
    if department = "blue" [set doctors-in-blue doctors-in-blue + to-allocate]
    if department = "ward" [set doctors-in-ward doctors-in-ward + to-allocate]
  ]

  ; Allocate remaining nurses by priority
  foreach nurse-departments [ dept ->
    let department item 0 dept
    let needed item 1 dept

    let to-allocate min (list needed nurses-available)
    set nurses-available nurses-available - to-allocate

    ; Update the appropriate counter
    if department = "or" [set nurses-in-or nurses-in-or + to-allocate]
    if department = "icu" [set nurses-in-icu nurses-in-icu + to-allocate]
    if department = "red" [set nurses-in-red nurses-in-red + to-allocate]
    if department = "yellow" [set nurses-in-yellow nurses-in-yellow + to-allocate]
    if department = "green" [set nurses-in-green nurses-in-green + to-allocate]
    if department = "triage" [set nurses-in-triage nurses-in-triage + to-allocate]
    if department = "blue" [set nurses-in-blue nurses-in-blue + to-allocate]
    if department = "ward" [set nurses-in-ward nurses-in-ward + to-allocate]
  ]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HOSPITAL PROCESSING SECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Main update procedure for hospital processing
to update-hospital
  ; Process zones based on priority
  process-or
  process-icu
  process-red-zone
  process-yellow-zone
  process-green-zone
  process-triage-queue
  process-blue-zone
  process-ward
end

; Process triage queue
to process-triage-queue
  ; Check if we have casualties in queue
  if length triage-queue > 0 [
    ; Get next casualty
    let next-casualty first triage-queue
    set triage-queue but-first triage-queue

    ; Ask the next-casualty to check if it's ready for processing
    ask next-casualty [
      ; Check if patient is ready to be processed (not still moving)
      ifelse not moving? [
        ; Log the triage process beginning
        record-event EVENT_TRIAGE_START "Beginning triage assessment"

        ; Allocate resources if available, but proceed with triage regardless
        if check-resource-availability "triage-queue" [
          allocate-zone-resources "triage-queue"
          ; Schedule resource release in 2 ticks
          schedule-personal-task 2 "release-zone-resources \"triage-queue\""
        ]

        ; Step 1: Initial sorting based on walking ability
        let walking? severity > 6  ; Simplified: casualties with higher RPM can walk

        ; Initial routing based on walking ability ONLY
        ifelse walking? [
          ; Walking casualties go to green zone
          ifelse has-zone-capacity? "green-zone" [
            handle-zone-transition "green-zone"
          ][
            ; If green zone is full, keep in triage queue
            set location "triage-queue"
            set moving? false

            ask one-of hospitals [
              set triage-queue lput myself triage-queue
            ]
          ]
        ][
          ; Non-walking casualties go to red zone
          ifelse has-zone-capacity? "red-zone" [
            handle-zone-transition "red-zone"
          ][
            ; If red zone is full, check yellow zone
            ifelse has-zone-capacity? "yellow-zone" [
              handle-zone-transition "yellow-zone"
            ][
              ; If red and yellow zones are full, keep in triage queue
              set location "triage-queue"
              set moving? false

              ask one-of hospitals [
                set triage-queue lput myself triage-queue
              ]
            ]
          ]
        ]
      ][
        ; Patient is still moving, add back to queue
        ask one-of hospitals [
          set triage-queue lput myself triage-queue
        ]
      ]
    ]
  ]
end

; Process red zone
to process-red-zone
  ; Only process patients who have physically arrived and waiting
  if length red-zone-queue > 0 and
     count casualties with [location = "red-zone" and treatment-started?] < red-zone-capacity [

    ; Get next casualty from queue
    let next-casualty first red-zone-queue
    set red-zone-queue but-first red-zone-queue

    ask next-casualty [
      ; Check if patient has physically arrived
      ifelse not moving? [
        ; PERFORM STEP 2 TRIAGE in any case
        step-2-triage

        ; After triage, check if routing to another zone is needed
        if severity > 8 [
          ; Patient is actually less critical, consider moving to green zone
          ifelse has-zone-capacity? "green-zone" [
            handle-zone-transition "green-zone"
            stop
          ][
            ; Green zone is full, try ward or keep in red
            ifelse has-zone-capacity? "ward" [
              handle-zone-transition "ward"
              stop
            ][]
          ]
        ]

        if severity > 3 and severity <= 8 [
          ; Moderate patient, try yellow zone
          ifelse has-zone-capacity? "yellow-zone" [
            handle-zone-transition "yellow-zone"
            stop
          ][]
        ]

        if triage-category = "blue" [
          ; Expectant patient, try blue zone
          ifelse has-zone-capacity? "blue-zone" [
            handle-zone-transition "blue-zone"
            stop
          ][]
        ]

        ; If we reach here, patient stays in red zone - start treatment
        set treatment-started? true

        ; Try to allocate red zone resources, but proceed with treatment regardless
        if check-resource-availability "red-zone" [
          allocate-zone-resources "red-zone"
        ]

        ; Red zone treatment improves condition slightly
        set severity severity + 1
        if severity > 12 [set severity 12]
        calculate-survival-probability

        ; Schedule treatment completion task (takes about 20 ticks)
        schedule-personal-task 20 "complete-treatment"
      ][
        ; If patient is still moving, put back in queue
        ask one-of hospitals [
          set red-zone-queue lput myself red-zone-queue
        ]
      ]
    ]
  ]
end

; Process yellow zone
to process-yellow-zone
  ; Only process patients who have physically arrived and waiting
  if length yellow-zone-queue > 0 and
     count casualties with [location = "yellow-zone" and treatment-started?] < yellow-zone-capacity [

    ; Get next casualty from queue
    let next-casualty first yellow-zone-queue
    set yellow-zone-queue but-first yellow-zone-queue

    ask next-casualty [
      ; Check if patient has physically arrived
      ifelse not moving? [
        ; PERFORM STEP 2 TRIAGE
        step-2-triage

        ; After triage, check routing needs
        if severity <= 3 [
          ; Critical patient needs OR or red zone
          ifelse has-zone-capacity? "or" and check-resource-availability "or" [
            handle-zone-transition "or"
            stop
          ][
            ifelse has-zone-capacity? "red-zone" [
              handle-zone-transition "red-zone"
              stop
            ][]
          ]
        ]

        if severity > 8 [
          ; Minor patient could go to green zone
          ifelse has-zone-capacity? "green-zone" [
            handle-zone-transition "green-zone"
            stop
          ][]
        ]

        ; Start treatment in yellow zone
        set treatment-started? true

        ; Try to allocate resources, but proceed with treatment regardless
        if check-resource-availability "yellow-zone" [
          allocate-zone-resources "yellow-zone"
        ]

        ; Yellow zone treatment improves condition slightly
        set severity severity + 1
        if severity > 12 [set severity 12]
        calculate-survival-probability

        ; Schedule treatment completion
        let treatment-duration treatment-time
        schedule-personal-task treatment-duration "complete-treatment"
      ][
        ; If patient is still moving, put back in queue
        ask one-of hospitals [
          set yellow-zone-queue lput next-casualty yellow-zone-queue
        ]
      ]
    ]
  ]
end

; Process green zone
to process-green-zone
  ; Only process patients who have physically arrived and waiting
  if length green-zone-queue > 0 and
     count casualties with [location = "green-zone" and treatment-started?] < green-zone-capacity [

    ; Get next casualty from queue
    let next-casualty first green-zone-queue
    set green-zone-queue but-first green-zone-queue

    ask next-casualty [
      ; Check if patient has physically arrived
      ifelse not moving? [
        ; PERFORM STEP 2 TRIAGE
        step-2-triage

        ; After triage, check routing needs
        if severity <= 3 [
          ; Critical patient should go to red zone
          ifelse has-zone-capacity? "red-zone" [
            handle-zone-transition "red-zone"
            stop
          ][
            ; If red zone full, try yellow
            ifelse has-zone-capacity? "yellow-zone" [
              handle-zone-transition "yellow-zone"
              stop
            ][]
          ]
        ]

        if severity > 3 and severity <= 8 [
          ; Moderate patient should go to yellow
          ifelse has-zone-capacity? "yellow-zone" [
            handle-zone-transition "yellow-zone"
            stop
          ][]
        ]

        ; Start treatment in green zone
        set treatment-started? true

        ; Try to allocate resources, but proceed with treatment regardless
        if check-resource-availability "green-zone" [
          allocate-zone-resources "green-zone"
        ]

        ; Green zone treatment improves condition
        set severity severity + 1
        if severity > 12 [set severity 12]
        calculate-survival-probability

        ; Schedule treatment completion (faster for green zone)
        let treatment-duration max list 10 (treatment-time / 2)
        schedule-personal-task treatment-duration "complete-treatment"
      ][
        ; If patient is still moving, put back in queue
        ask one-of hospitals [
          set green-zone-queue lput next-casualty green-zone-queue
        ]
      ]
    ]
  ]
end

; Process blue zone (expectant)
to process-blue-zone
  ; Only process patients who have physically arrived and waiting
  if length blue-zone-queue > 0 and
     count casualties with [location = "blue-zone" and treatment-started?] < blue-zone-capacity [

    ; Get next casualty from queue
    let next-casualty first blue-zone-queue
    set blue-zone-queue but-first blue-zone-queue

    ask next-casualty [
      ; Check if patient has physically arrived
      ifelse not moving? [
        ; Start palliative care
        set treatment-started? true

        ; Try to allocate resources, but proceed with treatment regardless
        if check-resource-availability "blue-zone" [
          allocate-zone-resources "blue-zone"
        ]

        ; Reduce treatment time for palliative care
        let treatment-duration treatment-time / 2

        ; Schedule completion
        schedule-personal-task treatment-duration "complete-treatment"
      ][
        ; If patient is still moving, put back in queue
        ask one-of hospitals [
          set blue-zone-queue lput next-casualty blue-zone-queue
        ]
      ]
    ]
  ]
end

; Process OR
to process-or
  ; Only process patients who have physically arrived and waiting
  if length or-queue > 0 and
     count casualties with [location = "or" and treatment-started?] < or-capacity [

    ; Get next casualty from queue
    let next-casualty first or-queue
    set or-queue but-first or-queue

    ask next-casualty [
      ; Check if patient has physically arrived
      ifelse not moving? [
        ; OR requires resources to function - check and allocate
        ifelse check-resource-availability "or" [
          ; Start surgery
          set treatment-started? true

          ; Allocate OR resources
          allocate-zone-resources "or"

          ; OR progress will be handled in update-treatment-progress
          ; This will eventually call handle-or-completion
        ][
          ; If resources not available, put back in queue
          ask one-of hospitals [
            set or-queue lput myself or-queue
          ]
        ]
      ][
        ; If patient is still moving, put back in queue
        ask one-of hospitals [
          set or-queue lput next-casualty or-queue
        ]
      ]
    ]
  ]
end

; Process ICU
to process-icu
  ; Only process patients who have physically arrived and waiting
  if length icu-queue > 0 and
     count casualties with [location = "icu" and treatment-started?] < icu-capacity [

    ; Get next casualty from queue
    let next-casualty first icu-queue
    set icu-queue but-first icu-queue

    ask next-casualty [
      ; Check if patient has physically arrived
      ifelse not moving? [
        ; ICU requires resources to function - check and allocate
        ifelse check-resource-availability "icu" [
          ; Start ICU care
          set treatment-started? true

          ; Allocate ICU resources
          allocate-zone-resources "icu"

          ; ICU progress will be handled in update-treatment-progress
          ; This will eventually call handle-icu-completion
        ][
          ; If resources not available, put back in queue
          ask one-of hospitals [
            set icu-queue lput myself icu-queue
          ]
        ]
      ][
        ; If patient is still moving, put back in queue
        ask one-of hospitals [
          set icu-queue lput next-casualty icu-queue
        ]
      ]
    ]
  ]
end

; Process ward
to process-ward
  ; Only process patients who have physically arrived and waiting
  if length ward-queue > 0 and
     count casualties with [location = "ward" and treatment-started?] < ward-capacity [

    ; Get next casualty from queue
    let next-casualty first ward-queue
    set ward-queue but-first ward-queue

    ask next-casualty [
      ; Check if patient has physically arrived
      ifelse not moving? [
        ; Start ward care
        set treatment-started? true

        ; Try to allocate resources, but proceed with treatment regardless
        if check-resource-availability "ward" [
          allocate-zone-resources "ward"
        ]

        ; Ward treatment is quick
        let treatment-duration max (list 15 (30 - (severity * 2)))

        ; Schedule completion
        schedule-personal-task treatment-duration "complete-treatment"
      ][
        ; If patient is still moving, put back in queue
        ask one-of hospitals [
          set ward-queue lput next-casualty ward-queue
        ]
      ]
    ]
  ]
end

; Update treatment progress for casualties receiving care
to update-treatment-progress
  ; Skip if not in treatment
  if not treatment-started? [stop]

  ; Record treatment start if this is the first update
  if treatment-progress = 0 [record-event EVENT_TREATMENT_START (word "Started in " location)]

  ; Calculate base progress increment
  ; The factor 3 makes progress roughly 3x faster than original
  let progress-increment (100 / treatment-time) * 3

  ; Different progress rates for different locations
  if location = "or" [
      ; Surgeries progress slower
      set progress-increment (100 / treatment-time) * 2

      ; OR patients never reach 100% while in OR (max 75%)
      let max-progress 75
      set treatment-progress min (list (treatment-progress + progress-increment) max-progress)

      ; If progress reaches max for OR, schedule transition
      if treatment-progress >= max-progress [
        schedule-personal-task 1 "handle-or-completion"
      ]
  ]

  if location = "icu" [
    ; ICU treatments progress at medium rate
      set progress-increment (100 / treatment-time) * 2.5

      ; ICU patients never reach 100% while in ICU (max 75%)
      let max-progress 75
      set treatment-progress min (list (treatment-progress + progress-increment) max-progress)

      ; If progress reaches max for ICU, schedule transition
      if treatment-progress >= max-progress [
        schedule-personal-task 1 "handle-icu-completion"
      ]
    ]

  ; For all other zones, update normally
  if location != "or" and location != "icu" [
    set treatment-progress treatment-progress + progress-increment

    ; Auto-complete treatment if it reaches 100%
    if treatment-progress >= 100 [
      set treatment-progress 100
      record-event EVENT_TREATMENT_COMPLETE (word "Completed in " location)
      complete-treatment
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AMBULANCE AND CASUALTY CREATION SECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ambulance update procedure
to update-ambulance
  ; Handle movement if ambulance is moving
  if status = "en-route-to-scene" or status = "en-route-to-hospital" or status = "returning" [
    ; Move toward destination
    face patch destination-xcor destination-ycor

    ; Calculate distance to move this tick (speed)
    let move-speed 0.8
    let dist-to-dest distancexy destination-xcor destination-ycor

    ; If we can reach destination this tick, just arrive there
    ifelse dist-to-dest <= move-speed [
      setxy destination-xcor destination-ycor
      if status = "en-route-to-scene" [
        set status "loading"
      ]
      if status = "en-route-to-hospital" [
        ; Unload casualties to hospital triage queue
        foreach loaded-casualties [ c ->
          ask c [
            ; Set the casualty's location to be the ambulance's location before moving to triage
            setxy [xcor] of myself [ycor] of myself

            record-event EVENT_HOSPITAL_ARRIVAL "Arrived at hospital"

            set location "triage-queue"
            set moving? true
            set destination-xcor triage-xcor + random 2 - 1
            set destination-ycor triage-ycor + random 2 - 1
          ]
          ask one-of hospitals [
            set triage-queue lput c triage-queue
          ]
        ]
        set loaded-casualties []
        set status "returning"
        set destination-xcor hospital-xcor + 8
        set destination-ycor hospital-ycor + (who - count hospitals) * 2
      ]
      if status = "returning" [
        set status "idle"
      ]
    ][
      ; Otherwise move toward destination
      forward move-speed

      ; Move loaded casualties with the ambulance
      foreach loaded-casualties [ c ->
        ask c [
          setxy [xcor] of myself [ycor] of myself
        ]
      ]
    ]
  ]

  if status = "idle" [
    ; Check if there are casualties at the scene
    let scene-casualties casualties with [location = "scene"]
    if any? scene-casualties [
      ; Go to scene
      set status "en-route-to-scene"
      set destination-xcor incident-xcor
      set destination-ycor incident-ycor
    ]
  ]

  if status = "loading" [
    ; Find casualties to load
    let loadable-casualties casualties with [location = "scene" and not moving?]
    if any? loadable-casualties and length loaded-casualties < capacity [
      ; Prioritize loading based on triage category (red first, then green)
      let priority-casualties loadable-casualties with [triage-category = "red"]
      if not any? priority-casualties [
        set priority-casualties loadable-casualties with [triage-category = "green"]
      ]

      if any? priority-casualties [
        let picked-casualty min-one-of priority-casualties [distance myself]
        ask picked-casualty [
          record-event EVENT_AMBULANCE_PICKUP (word "Picked up by ambulance " [who] of myself)
          set location "in-transit"
          set moving? true
          ; Move directly to the ambulance when loading
          setxy [xcor] of myself [ycor] of myself
        ]
        set loaded-casualties lput picked-casualty loaded-casualties
      ]
    ]

    ; If full or no more casualties to load, head to hospital
    if length loaded-casualties = capacity or not any? casualties with [location = "scene"] [
      set status "en-route-to-hospital"
      set destination-xcor triage-xcor
      set destination-ycor triage-ycor
    ]
  ]
end

; Create incident scene with visual representation
to create-incident-scene
  ; Draw scene boundary
  ask patches with [
    pxcor >= (incident-xcor - 4) and pxcor <= (incident-xcor + 4) and
    pycor >= (incident-ycor - 4) and pycor <= (incident-ycor + 4)
  ] [
    set pcolor orange + 3
  ]
end

to create-casualties-at-scene
  create-casualties number-of-casualties [
    setxy incident-xcor - 4 + random 8 incident-ycor - 4 + random 8
    set shape "person"
    set size 0.8

    ; Assign unique ID
    set unique-id who
    set scheduled-tasks []
    set resources-allocated []

    ; Use the red/green percentages to determine casualty severity
    let random-value random-float 100

    ifelse random-value < red-casualty-percentage [
      ; Red (critical) patients (RPM 1-3)
      set severity 1 + random 5
    ][
      ; Green (walking wounded) patients (RPM 9-12)
      set severity min list 12 (6 + random 6)
    ]

    ; Set initial triage category
    assign-initial-triage-category

    ; Initialize other properties
    set location "scene"
    set wait-time 0
    set treatment-time 30 + random 90  ; Base treatment time in minutes
    set treatment-started? false
    set treatment-progress 0
    set moving? false
    set destination-xcor xcor
    set destination-ycor ycor

    ; Calculate deterioration rate based on RPM score
    ; Lower RPM scores deteriorate faster
    set deterioration-rate 0.05 * (13 - severity)

    ; Calculate initial survival probability based on RPM
    calculate-survival-probability

    ; Initialize the event log
    set event-log []

    ; Record creation event
    record-event EVENT_CREATION (word "Initial RPM: " severity " Survival probability: " survival-probability)
  ]
end

; Add this procedure to the model
to self-transport-green-casualties
  ; Only run this once at the beginning of the simulation, but in some time after the incident
  if time-elapsed = 10 [
    ; Find green casualties at the scene who can walk
    let self-transport-candidates casualties with [
      location = "scene" and
      triage-category = "green" and
      severity > 6 and  ; Can walk (already in the model logic)
      not moving?
    ]

    ; Select some to self-transport
    let num-to-transport round (count self-transport-candidates * green-self-transport-percentage / 100)

    if num-to-transport > 0 and any? self-transport-candidates [
      ask n-of (min list num-to-transport count self-transport-candidates) self-transport-candidates [
        ; Log and record the self-transport decision
        record-event EVENT_MOVEMENT_START "Self-transporting to hospital"

        ; Use the existing zone transition logic to move to triage
        handle-zone-transition "triage-queue"
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRIAGE AND CASUALTY ASSESSMENT SECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Assign initial triage category based on RPM score - MODIFIED FOR SCENE TRIAGE
; At scene, can be red, green, or black (dead)
to assign-initial-triage-category
  ; If RPM is 0, casualty is dead (black)
  if severity = 0 [
    set triage-category "black"
    set location "deceased"
    set deceased-count deceased-count + 1
    stop
  ]

  ; At the scene, simplified initial triage:
  ; RPM 1-4 → red (critical)
  ; RPM 5-12 → green (walking wounded)
  ifelse severity <= 4 [
    set triage-category "red"
  ] [
    set triage-category "green"
  ]

  ; Update color based on new triage category
  update-casualty-color
end

; Step 2 triage for a casualty - assign final triage category
to step-2-triage
  ; Log beginning of Step 2 triage
  record-event EVENT_TRIAGE_START "Beginning triage assessment"

  ; Calculate/update survival probability based on current severity
  calculate-survival-probability

  ; Use RPM score to determine triage category
  if severity = 0 [
    set triage-category "black"
    set location "deceased"
    set deceased-count deceased-count + 1
    stop
  ]

  if severity <= 3 [
    ; Critically injured
    ifelse triage-strategy = "maximize-survivors" [
      ; In mass casualty situation, some critical patients might be triaged as expectant
      ifelse severity = 1 and random-float 1 > survival-probability [  ; Very low survival probability
        set triage-category "blue"  ; Expectant
      ] [
        set triage-category "red"
      ]
    ] [
      ; In standard triage, all critical get red
      set triage-category "red"
    ]
  ]

  if severity > 3 and severity <= 8 [
    set triage-category "yellow"
  ]

  if severity > 8 [
    set triage-category "green"
  ]

  ; Store the triage event in the transitions list for history tracking
  let transition-record (word "Time " time-elapsed ": Casualty " who " assigned triage category " triage-category)
  set recorded-transitions lput transition-record recorded-transitions

  ; Update color based on new triage category
  update-casualty-color

  ; Log completion of triage
  record-event EVENT_TRIAGE_COMPLETE (word "Assigned category: " triage-category)

end

; Calculate survival probability based on RPM score using Sacco's model
to calculate-survival-probability
  ; For RPM=0 (dead), probability should be 0
  if severity = 0 [
    set survival-probability 0
    stop
  ]

  ; For other scores, calculate using the logistic function
  ; These parameters are adjusted to match Sacco's model where:
  ; - Low RPM (1-3) should have low survival probability (~0.1-0.3)
  ; - Medium RPM (4-8) should have medium survival probability (~0.4-0.7)
  ; - High RPM (9-12) should have high survival probability (~0.8-0.95)
  let severity-mod (12 - severity)

  let w0 -1.4
  let w1 0.33
  let w (w0 + (w1 * severity-mod))
  set survival-probability 1 / (1 + exp w)
end

; Update casualty color based on triage category
to update-casualty-color
  ; First check if treatment is in progress
  if (treatment-started? = true) and (treatment-progress > 0) and (treatment-progress < 100) [
    set color cyan
    stop  ; Exit the procedure early once color is set
  ]

  ; Otherwise, set color based on triage category
  if triage-category = "red" [
    set color red
  ]
  if triage-category = "yellow" [
    set color yellow
  ]
  if triage-category = "green" [
    set color green
  ]
  if triage-category = "blue" [
    set color blue
  ]
  if triage-category = "black" or location = "deceased" [
    set color gray
  ]
  if location = "discharged" [
    set color brown
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VISUALIZATION AND SETUP SECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Set up coordinates for visualizing different zones
to setup-visualization-coordinates
  ; Incident scene (far left)
  set incident-xcor -20
  set incident-ycor 0

  ; Hospital main building (center-right)
  set hospital-xcor 5
  set hospital-ycor 0

  ; Triage area (entry point to hospital)
  set triage-xcor 0
  set triage-ycor 0

  ; Treatment zones (arranged in a clear pattern)
  ; Red zone (top)
  set red-zone-xcor 5
  set red-zone-ycor 10

  ; Yellow zone (right)
  set yellow-zone-xcor 12
  set yellow-zone-ycor 0

  ; Green zone (bottom)
  set green-zone-xcor 5
  set green-zone-ycor -10

  ; Blue zone (left)
  set blue-zone-xcor -2
  set blue-zone-ycor -10

  ; Advanced care areas
  set icu-xcor 12
  set icu-ycor 10

  set or-xcor 19
  set or-ycor 5

  set ward-xcor 19
  set ward-ycor -5

  ; Outcome areas
  set morgue-xcor 12
  set morgue-ycor -15

  set discharge-xcor 19
  set discharge-ycor -15
end

; Create hospital zones and labels
to create-hospital-zones
  ; Draw hospital outline
  ask patches with [
    pxcor >= -5 and pxcor <= 22 and
    pycor >= -17 and pycor <= 13
  ] [
    set pcolor gray - 3
  ]

  ; Draw triage zone
  ask patches with [
    pxcor >= (triage-xcor - 2) and pxcor <= (triage-xcor + 2) and
    pycor >= (triage-ycor - 2) and pycor <= (triage-ycor + 2)
  ] [
    set pcolor blue + 3  ; Light blue for triage
  ]

  ; Draw red zone
  ask patches with [
    pxcor >= (red-zone-xcor - 2) and pxcor <= (red-zone-xcor + 2) and
    pycor >= (red-zone-ycor - 2) and pycor <= (red-zone-ycor + 2)
  ] [
    set pcolor red - 2  ; Lighter red
  ]

  ; Draw yellow zone
  ask patches with [
    pxcor >= (yellow-zone-xcor - 2) and pxcor <= (yellow-zone-xcor + 2) and
    pycor >= (yellow-zone-ycor - 2) and pycor <= (yellow-zone-ycor + 2)
  ] [
    set pcolor yellow - 2  ; Lighter yellow
  ]

  ; Draw green zone
  ask patches with [
    pxcor >= (green-zone-xcor - 2) and pxcor <= (green-zone-xcor + 2) and
    pycor >= (green-zone-ycor - 2) and pycor <= (green-zone-ycor + 2)
  ] [
    set pcolor green - 3  ; Lighter green
  ]

  ; Draw blue zone (expectant)
  ask patches with [
    pxcor >= (blue-zone-xcor - 2) and pxcor <= (blue-zone-xcor + 2) and
    pycor >= (blue-zone-ycor - 2) and pycor <= (blue-zone-ycor + 2)
  ] [
    set pcolor blue - 3  ; Lighter blue
  ]

  ; Draw ICU
  ask patches with [
    pxcor >= (icu-xcor - 2) and pxcor <= (icu-xcor + 2) and
    pycor >= (icu-ycor - 2) and pycor <= (icu-ycor + 2)
  ] [
    set pcolor violet - 3  ; Lighter violet for ICU
  ]

  ; Draw OR
  ask patches with [
    pxcor >= (or-xcor - 2) and pxcor <= (or-xcor + 2) and
    pycor >= (or-ycor - 2) and pycor <= (or-ycor + 2)
  ] [
    set pcolor orange - 3  ; Lighter orange for OR
  ]

  ; Draw ward
  ask patches with [
    pxcor >= (ward-xcor - 2) and pxcor <= (ward-xcor + 2) and
    pycor >= (ward-ycor - 2) and pycor <= (ward-ycor + 2)
  ] [
    set pcolor brown - 3  ; Lighter brown for ward
  ]

  ; Draw morgue
  ask patches with [
    pxcor >= (morgue-xcor - 1) and pxcor <= (morgue-xcor + 1) and
    pycor >= (morgue-ycor - 1) and pycor <= (morgue-ycor + 1)
  ] [
    set pcolor black  ; Black for morgue
  ]

  ; Draw discharge area
  ask patches with [
    pxcor >= (discharge-xcor - 1) and pxcor <= (discharge-xcor + 1) and
    pycor >= (discharge-ycor - 1) and pycor <= (discharge-ycor + 1)
  ] [
    set pcolor white - 3  ; White for discharge
  ]

  ; Create zone labels
  create-zone-labels 1 [
    set color black
    set size 0
    setxy triage-xcor triage-ycor
    set label "TRIAGE"
    set zone-type "triage"
    set doctors-display 0
    set nurses-display 0
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy red-zone-xcor red-zone-ycor
    set label "RED"
    set zone-type "red-zone"
    set doctors-display 0
    set nurses-display 0
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy yellow-zone-xcor yellow-zone-ycor
    set label "YELLOW"
    set zone-type "yellow-zone"
    set count-display 0
    set doctors-display 0
    set nurses-display 0
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy green-zone-xcor green-zone-ycor
    set label "GREEN"
    set zone-type "green-zone"
    set doctors-display 0
    set nurses-display 0
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy blue-zone-xcor blue-zone-ycor
    set label "BLUE"
    set zone-type "blue-zone"
    set doctors-display 0
    set nurses-display 0
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy icu-xcor icu-ycor
    set label "ICU"
    set zone-type "icu"
    set doctors-display 0
    set nurses-display 0
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy or-xcor or-ycor
    set label "OR"
    set zone-type "or"
    set doctors-display 0
    set nurses-display 0
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy ward-xcor ward-ycor
    set label "WARD"
    set zone-type "ward"
    set doctors-display 0
    set nurses-display 0
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy morgue-xcor morgue-ycor
    set label "MORGUE"
    set zone-type "morgue"
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy discharge-xcor discharge-ycor
    set label "DISCHARGED"
    set zone-type "discharged"
  ]

  create-zone-labels 1 [
    set color black
    set size 0
    setxy incident-xcor incident-ycor
    set label "INCIDENT"
    set zone-type "scene"
  ]

  create-zone-labels 1 [
  set shape "circle"
  set color black
  set size 0
  setxy -15 17  ; Position above the other resource indicators
  set zone-type "resource-summary"
  set doctors-display 0
  set nurses-display 0
  set label ""
]
end

to update-resource-indicators
  ; Clear existing indicators
  ask resource-indicators [die]

  ; Create headers for both columns
  create-resource-indicators 1 [
    set shape "circle"
    set color black
    set size 0
    setxy -15 17
    set resource-type "doctor-header"
    set label "DOCTORS (B/All/Req)"
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color black
    set size 0
    setxy -8 17
    set resource-type "nurse-header"
    set label "NURSES (B/All/Req)"
  ]

  ; Get accurate resource usage by matching allocated resources with active treatment
  let red-busy-docs count casualties with [location = "red-zone" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "red-zone" and item 1 resources-allocated > 0]

  let yellow-busy-docs count casualties with [location = "yellow-zone" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "yellow-zone" and item 1 resources-allocated > 0]

  let green-busy-docs count casualties with [location = "green-zone" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "green-zone" and item 1 resources-allocated > 0]

  let blue-busy-docs count casualties with [location = "blue-zone" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "blue-zone" and item 1 resources-allocated > 0]

  let icu-busy-docs count casualties with [location = "icu" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "icu" and item 1 resources-allocated > 0]

  let or-busy-docs count casualties with [location = "or" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "or" and item 1 resources-allocated > 0]

  let ward-busy-docs count casualties with [location = "ward" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "ward" and item 1 resources-allocated > 0]

  let triage-busy-docs count casualties with [location = "triage-queue" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "triage-queue" and item 1 resources-allocated > 0]

  ; Similar tracking for nurses
  let red-busy-nurses count casualties with [location = "red-zone" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "red-zone" and item 2 resources-allocated > 0]

  let yellow-busy-nurses count casualties with [location = "yellow-zone" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "yellow-zone" and item 2 resources-allocated > 0]

  let green-busy-nurses count casualties with [location = "green-zone" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "green-zone" and item 2 resources-allocated > 0]

  let blue-busy-nurses count casualties with [location = "blue-zone" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "blue-zone" and item 2 resources-allocated > 0]

  let icu-busy-nurses count casualties with [location = "icu" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "icu" and item 2 resources-allocated > 0]

  let or-busy-nurses count casualties with [location = "or" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "or" and item 2 resources-allocated > 0]

  let ward-busy-nurses count casualties with [location = "ward" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "ward" and item 2 resources-allocated > 0]

  let triage-busy-nurses count casualties with [location = "triage-queue" and treatment-started? and
    not empty? resources-allocated and length resources-allocated > 2 and
    item 0 resources-allocated = "triage-queue" and item 2 resources-allocated > 0]

  ; Create Doctor resource indicators for each zone
  create-resource-indicators 1 [
    set shape "circle"
    set color red
    set size 0
    setxy -15 14
    set resource-type "red-zone-doctors"
    set label (word "RED: " red-busy-docs "/" doctors-in-red "/" red-zone-doctors-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color yellow
    set size 0
    setxy -15 13
    set resource-type "yellow-zone-doctors"
    set label (word "YELLOW: " yellow-busy-docs "/" doctors-in-yellow "/" yellow-zone-doctors-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color green
    set size 0
    setxy -15 12
    set resource-type "green-zone-doctors"
    set label (word "GREEN: " green-busy-docs "/" doctors-in-green "/" green-zone-doctors-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color blue
    set size 0
    setxy -15 11
    set resource-type "blue-zone-doctors"
    set label (word "BLUE: " blue-busy-docs "/" doctors-in-blue "/" blue-zone-doctors-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color violet
    set size 0
    setxy -15 10
    set resource-type "icu-doctors"
    set label (word "ICU: " icu-busy-docs "/" doctors-in-icu "/" icu-doctors-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color orange
    set size 0
    setxy -15 9
    set resource-type "or-doctors"
    set label (word "OR: " or-busy-docs "/" doctors-in-or "/" or-doctors-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color brown
    set size 0
    setxy -15 8
    set resource-type "ward-doctors"
    set label (word "WARD: " ward-busy-docs "/" doctors-in-ward "/" ward-doctors-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color cyan
    set size 0
    setxy -15 7
    set resource-type "triage-doctors"
    set label (word "TRIAGE: " triage-busy-docs "/" doctors-in-triage "/" triage-doctors-required)
  ]

  ; Create Nurse resource indicators for each zone
  create-resource-indicators 1 [
    set shape "circle"
    set color red
    set size 0
    setxy -8 14
    set resource-type "red-zone-nurses"
    set label (word "RED: " red-busy-nurses "/" nurses-in-red "/" red-zone-nurses-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color yellow
    set size 0
    setxy -8 13
    set resource-type "yellow-zone-nurses"
    set label (word "YELLOW: " yellow-busy-nurses "/" nurses-in-yellow "/" yellow-zone-nurses-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color green
    set size 0
    setxy -8 12
    set resource-type "green-zone-nurses"
    set label (word "GREEN: " green-busy-nurses "/" nurses-in-green "/" green-zone-nurses-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color blue
    set size 0
    setxy -8 11
    set resource-type "blue-zone-nurses"
    set label (word "BLUE: " blue-busy-nurses "/" nurses-in-blue "/" blue-zone-nurses-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color violet
    set size 0
    setxy -8 10
    set resource-type "icu-nurses"
    set label (word "ICU: " icu-busy-nurses "/" nurses-in-icu "/" icu-nurses-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color orange
    set size 0
    setxy -8 9
    set resource-type "or-nurses"
    set label (word "OR: " or-busy-nurses "/" nurses-in-or "/" or-nurses-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color brown
    set size 0
    setxy -8 8
    set resource-type "ward-nurses"
    set label (word "WARD: " ward-busy-nurses "/" nurses-in-ward "/" ward-nurses-required)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color cyan
    set size 0
    setxy -8 7
    set resource-type "triage-nurses"
    set label (word "TRIAGE: " triage-busy-nurses "/" nurses-in-triage "/" triage-nurses-required)
  ]

  ; Add summary indicators for each resource type
  create-resource-indicators 1 [
    set shape "circle"
    set color white
    set size 0
    setxy -15 16
    set resource-type "doctor-summary"
    let total-allocated-doctors (doctors-in-or + doctors-in-icu + doctors-in-red + doctors-in-yellow +
                               doctors-in-green + doctors-in-triage + doctors-in-blue + doctors-in-ward)
    let total-required-doctors (or-doctors-required + icu-doctors-required + red-zone-doctors-required +
                              yellow-zone-doctors-required + green-zone-doctors-required +
                              triage-doctors-required + blue-zone-doctors-required + ward-doctors-required)
    let doctors-actually-busy count casualties with [treatment-started? and not empty? resources-allocated
                                                 and length resources-allocated > 2 and item 1 resources-allocated > 0]
    set label (word "TOTAL: " doctors-actually-busy "/" total-allocated-doctors "/" total-required-doctors)
  ]

  create-resource-indicators 1 [
    set shape "circle"
    set color white
    set size 0
    setxy -8 16
    set resource-type "nurse-summary"
    let total-allocated-nurses (nurses-in-or + nurses-in-icu + nurses-in-red + nurses-in-yellow +
                              nurses-in-green + nurses-in-triage + nurses-in-blue + nurses-in-ward)
    let total-required-nurses (or-nurses-required + icu-nurses-required + red-zone-nurses-required +
                             yellow-zone-nurses-required + green-zone-nurses-required +
                             triage-nurses-required + blue-zone-nurses-required + ward-nurses-required)
    let nurses-actually-busy count casualties with [treatment-started? and not empty? resources-allocated
                                                and length resources-allocated > 2 and item 2 resources-allocated > 0]
    set label (word "TOTAL: " nurses-actually-busy "/" total-allocated-nurses "/" total-required-nurses)
  ]

  ; Add available resources indicator
  create-resource-indicators 1 [
    set shape "circle"
    set color white
    set size 0
    setxy -8 5
    set resource-type "available-summary"
    set label (word "AVAILABLE: " doctors-available " doctors, " nurses-available " nurses")
  ]
end

; Update zone labels with current counts
to update-zone-labels
  ask zone-labels [
    if zone-type = "triage" [
      set count-display length [triage-queue] of one-of hospitals
      set label (word "TRIAGE (" count-display ")")
    ]

    if zone-type = "red-zone" [
      let in-queue count casualties with [location = "red-zone" and not treatment-started?]
      let in-treatment count casualties with [location = "red-zone" and treatment-started?]
      set label (word "RED (" in-queue "|" in-treatment "|" red-zone-capacity ")")
    ]

    if zone-type = "yellow-zone" [
      let in-queue count casualties with [location = "yellow-zone" and not treatment-started?]
      let in-treatment count casualties with [location = "yellow-zone" and treatment-started?]
      set label (word "YELLOW (" in-queue "|" in-treatment "|" yellow-zone-capacity ")")
    ]

    if zone-type = "green-zone" [
      let in-queue count casualties with [location = "green-zone" and not treatment-started?]
      let in-treatment count casualties with [location = "green-zone" and treatment-started?]
      set label (word "GREEN (" in-queue "|" in-treatment "|" green-zone-capacity ")")
    ]

    if zone-type = "blue-zone" [
      let in-queue count casualties with [location = "blue-zone" and not treatment-started?]
      let in-treatment count casualties with [location = "blue-zone" and treatment-started?]
      set label (word "BLUE (" in-queue "|" in-treatment "|" blue-zone-capacity ")")
    ]

    if zone-type = "icu" [
      let in-queue count casualties with [location = "icu" and not treatment-started?]
      let in-treatment count casualties with [location = "icu" and treatment-started?]
      set label (word "ICU (" in-queue "|" in-treatment "|" icu-capacity ")")
    ]

    if zone-type = "or" [
      let in-queue count casualties with [location = "or" and not treatment-started?]
      let in-treatment count casualties with [location = "or" and treatment-started?]
      set label (word "OR (" in-queue "|" in-treatment "|" or-capacity ")")
    ]

    if zone-type = "ward" [
      let in-queue count casualties with [location = "ward" and not treatment-started?]
      let in-treatment count casualties with [location = "ward" and treatment-started?]
      set label (word "WARDS (" in-queue "|" in-treatment "|" ward-capacity ")")
    ]

    ; The other zones (morgue, discharged, scene) don't need doctor/nurse displays
    if zone-type = "morgue" [
      set count-display count casualties with [location = "deceased"]
      set label (word "MORGUE (" count-display ")")
    ]

    if zone-type = "discharged" [
      set count-display count casualties with [location = "discharged"]
      set label (word "DISCHARGED (" count-display ")")
    ]

    if zone-type = "scene" [
      set count-display count casualties with [location = "scene"]
      set label (word "INCIDENT (" count-display ")")
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STATISTICS AND REPORTING SECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Update global statistics
to update-statistics
  ; Calculate average wait time
  let waiting-casualties casualties with [(treatment-started? = false or treatment-started? = 0) and location != "discharged" and location != "deceased"]
  ifelse any? waiting-casualties [
    set avg-wait-time mean [wait-time] of waiting-casualties
    set max-wait-time max [wait-time] of waiting-casualties
  ] [
    set avg-wait-time 0
    set max-wait-time 0
  ]

  ; Update total deaths and treated
  set total-deaths deceased-count
  set total-treated discharged-count
end

; Reporter to calculate distribution of casualties by triage category
to-report triage-distribution
  let red-count count casualties with [triage-category = "red"]
  let yellow-count count casualties with [triage-category = "yellow"]
  let green-count count casualties with [triage-category = "green"]
  let blue-count count casualties with [triage-category = "blue"]

  report (word
    "Red:" red-count " (" precision (red-count / number-of-casualties * 100) 1 "%) "
    "Yellow:" yellow-count " (" precision (yellow-count / number-of-casualties * 100) 1 "%) "
    "Green:" green-count " (" precision (green-count / number-of-casualties * 100) 1 "%) "
    "Blue:" blue-count " (" precision (blue-count / number-of-casualties * 100) 1 "%) ")
end

; Reporter for survival statistics
to-report survival-statistics
  let total number-of-casualties
  let alive-count count casualties with [location != "deceased"]
  let total-deaths1 count casualties with [location = "deceased"]
  let mortality-rate (total-deaths1 / total) * 100

  report (word
    "Total:" total " "
    "Alive:" alive-count " (" precision ((alive-count / total) * 100) 1 "%) "
    "Deceased:" total-deaths1 " (" precision mortality-rate 1 "%)")
end

; Reporter for resource utilization statistics
to-report resource-utilization
  let doctors-utilized number-of-doctors - doctors-available
  let nurses-utilized number-of-nurses - nurses-available
  let ambulances-moving count ambulances with [status != "idle"]

  report (word
    "Docs:" doctors-utilized "/" number-of-doctors "(" precision (doctors-utilized / number-of-doctors * 100) 1 "%) "
    "Nurses:" nurses-utilized "/" number-of-nurses "(" precision (nurses-utilized / number-of-nurses * 100) 1 "%) "
    "Ambs:" ambulances-moving "/" number-of-ambulances "(" precision (ambulances-moving / number-of-ambulances * 100) 1 "%)")
end

; Reporter for bottleneck analysis
to-report bottleneck-analysis
  let bottlenecks ""

  ; Check OR bottleneck
  let or-utilization 100 * count casualties with [location = "or"] / or-capacity
  if or-utilization > 80 [
    set bottlenecks (word bottlenecks "OR at " precision or-utilization 1 "% capacity\n")
  ]

  ; Check ICU bottleneck
  let icu-utilization 100 * count casualties with [location = "icu"] / icu-capacity
  if icu-utilization > 80 [
    set bottlenecks (word bottlenecks "ICU at " precision icu-utilization 1 "% capacity\n")
  ]

  ; Check Red Zone bottleneck
  let red-utilization 100 * count casualties with [location = "red-zone"] / red-zone-capacity
  if red-utilization > 80 [
    set bottlenecks (word bottlenecks "Red Zone at " precision red-utilization 1 "% capacity\n")
  ]

  ; Check staff bottlenecks
  if doctors-available < doctors-minimum-guarantee [
    set bottlenecks (word bottlenecks "Doctor shortage - only " doctors-available " available\n")
  ]

  if nurses-available < nurses-minimum-guarantee [
    set bottlenecks (word bottlenecks "Nurse shortage - only " nurses-available " available\n")
  ]

  ; Return bottlenecks or "none"
  ifelse bottlenecks != "" [
    report bottlenecks
  ] [
    report "No critical bottlenecks detected"
  ]
end

; Reporter for triage accuracy
to-report triage-accuracy
  ; Compare initial triage with final disposition
  let correct-triage 0
  let total-triaged count casualties with [location = "discharged" or location = "deceased"]

  ; For red casualties - should have gone to OR or ICU at some point
  let red-correct count casualties with [
    triage-category = "red" and
    (location = "discharged" or location = "deceased") and
    (member? "or" recorded-transitions or member? "icu" recorded-transitions)
  ]

  ; For yellow casualties - should have gone to ward or discharged directly
  let yellow-correct count casualties with [
    triage-category = "yellow" and
    (location = "discharged" or location = "deceased") and
    (member? "ward" recorded-transitions or not member? "or" recorded-transitions)
  ]

  ; For green casualties - should have been discharged without OR/ICU care
  let green-correct count casualties with [
    triage-category = "green" and
    location = "discharged" and
    not member? "or" recorded-transitions and
    not member? "icu" recorded-transitions
  ]

  set correct-triage red-correct + yellow-correct + green-correct

  ifelse total-triaged > 0 [
    report (word "Triage accuracy: " precision (100 * correct-triage / total-triaged) 1 "%")
  ] [
    report "Triage accuracy: N/A - no completed cases"
  ]
end

; Reporter for zone throughput analysis
to-report zone-throughput
  ; Calculate patients processed per time unit for each zone
  let triage-throughput ifelse-value time-elapsed > 0 [precision (total-treated / time-elapsed) 2][0]
  let red-throughput count casualties with [member? "red-zone" recorded-transitions]
  let yellow-throughput count casualties with [member? "yellow-zone" recorded-transitions]
  let green-throughput count casualties with [member? "green-zone" recorded-transitions]
  let or-throughput count casualties with [member? "or" recorded-transitions]
  let icu-throughput count casualties with [member? "icu" recorded-transitions]

  report (word
    " Triage:" triage-throughput "/tick "
    " Red: " red-throughput ""
    " Yellow: " yellow-throughput "|"
    " Green: " green-throughput "|"
    " OR:" or-throughput "|"
    " ICU:" icu-throughput)
end

; Reporter for average wait times by zone
to-report wait-times-by-zone
  let red-waiting casualties with [location = "red-zone" and not treatment-started?]
  let red-avg ifelse-value any? red-waiting [precision (mean [wait-time] of red-waiting) 1][0]

  let yellow-waiting casualties with [location = "yellow-zone" and not treatment-started?]
  let yellow-avg ifelse-value any? yellow-waiting [precision (mean [wait-time] of yellow-waiting) 1][0]

  let green-waiting casualties with [location = "green-zone" and not treatment-started?]
  let green-avg ifelse-value any? green-waiting [precision (mean [wait-time] of green-waiting) 1][0]

  let or-waiting casualties with [location = "or" and not treatment-started?]
  let or-avg ifelse-value any? or-waiting [precision (mean [wait-time] of or-waiting) 1][0]

  let icu-waiting casualties with [location = "icu" and not treatment-started?]
  let icu-avg ifelse-value any? icu-waiting [precision (mean [wait-time] of icu-waiting) 1][0]

  report (word
    " Red: " red-avg "|"
    " Yellow: " yellow-avg "|"
    " Green:" green-avg "|"
    " OR:" or-avg "|"
    " ICU:" icu-avg)
end

; Add new procedure for recording events
to record-event [event-type details]
  ; Create complete state snapshot
  let state-snapshot (list
    location          ; Current zone/location
    severity          ; Current RPM score
    triage-category   ; Current triage category
    treatment-started? ; Whether treatment is in progress
    treatment-progress ; Percentage of treatment completed
    wait-time         ; Current accumulated wait time
    moving?           ; Whether casualty is moving
    survival-probability ; Current survival probability
  )

  ; Create the event record [time event-type details state-snapshot]
  let event-record (list time-elapsed event-type details state-snapshot)

  ; Add to event log
  set event-log lput event-record event-log
end

to export-tracking-data
  ; Create CSV header
  let csv "casualty_id,timestamp,event_type,event_details,location,severity,triage_category,treatment_started,treatment_progress,wait_time,moving,survival_probability\n"

  ; Add data for each casualty
  ask casualties [
    foreach event-log [ event ->
      ; Extract event details
      let time-stamp item 0 event
      let event-type item 1 event
      let event-details item 2 event

      ; Extract state snapshot
      let state-snapshot item 3 event
      let loc item 0 state-snapshot
      let rpm item 1 state-snapshot
      let triage item 2 state-snapshot
      let in-treatment? item 3 state-snapshot
      let progress item 4 state-snapshot
      let waiting item 5 state-snapshot
      let is-moving? item 6 state-snapshot
      let survival item 7 state-snapshot

      ; Format CSV row - using safer conversions for boolean values
      let row (word
        who ","
        time-stamp ","
        "\"" event-type "\"" ","
        "\"" event-details "\"" ","
        "\"" loc "\"" ","
        rpm ","
        "\"" triage "\"" ","
        ifelse-value (in-treatment? = true or in-treatment? = 1) ["true"] ["false"] ","
        progress ","
        waiting ","
        ifelse-value (is-moving? = true or is-moving? = 1) ["true"] ["false"] ","
        survival)

      set csv (word csv row "\n")
    ]
  ]

  ; Create a safe filename (replacing colons with underscores)
  let current-time date-and-time
  let safe-time ""
  let i 0
  while [i < length current-time] [
    let char item i current-time
    ifelse char = ":" or char = "/" or char = "\\" or char = "*" or char = "?" or char = "\"" or char = "<" or char = ">" or char = "|" [
      set safe-time (word safe-time "_")
    ] [
      set safe-time (word safe-time char)
    ]
    set i i + 1
  ]

  let filename (word "mci_tracking_" safe-time ".csv")

  ; Write to file using NetLogo's file commands
  output-print (word "Attempting to save to: " filename)
  carefully [
    file-open filename
    file-print csv
    file-close
    output-print (word "Data export complete. Saved to: " filename)
  ] [
    output-print (word "Error exporting data: " error-message)

    ; Try alternative location with simpler filename
    let simple-filename "mci_tracking_data.csv"
    output-print (word "Trying alternative filename: " simple-filename)
    carefully [
      file-open simple-filename
      file-print csv
      file-close
      output-print (word "Data export complete. Saved to: " simple-filename)
    ] [
      output-print (word "Alternative save also failed: " error-message)
    ]
  ]
end

@#$#@#$#@
GRAPHICS-WINDOW
211
6
894
557
-1
-1
14.57
1
10
1
1
1
0
1
1
1
-25
25
-20
20
0
0
1
ticks
30.0

BUTTON
5
9
70
43
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
71
9
135
43
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
137
9
198
44
Go Once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
11
54
171
88
number-of-casualties
number-of-casualties
1
500
500.0
1
1
NIL
HORIZONTAL

SLIDER
21
307
175
340
number-of-doctors
number-of-doctors
5
50
20.0
1
1
NIL
HORIZONTAL

SLIDER
21
272
176
305
number-of-ambulances
number-of-ambulances
1
20
12.0
1
1
NIL
HORIZONTAL

SLIDER
21
343
175
376
number-of-nurses
number-of-nurses
10
100
30.0
1
1
NIL
HORIZONTAL

CHOOSER
12
162
165
207
triage-strategy
triage-strategy
"standard" "maximize-survivors"
0

MONITOR
897
16
983
61
Time Elapsed
time-elapsed
17
1
11

MONITOR
990
18
1053
63
At Scene
count casualties with [location = \"scene\"]
17
1
11

MONITOR
1055
18
1119
63
In Hospital
count casualties with [location != \"scene\" and location != \"in-transit\" and location != \"discharged\" and location != \"deceased\"]
17
1
11

MONITOR
1110
70
1182
115
Went Home
count casualties with [location = \"discharged\"]
17
1
11

MONITOR
1184
70
1242
115
Died
count casualties with [location = \"deceased\"]
17
1
11

PLOT
903
263
1218
413
Casualty Distribution
Time
Count
0.0
1000.0
0.0
10.0
true
true
"" ""
PENS
"Red" 1.0 0 -2674135 true "" "plot count casualties with [triage-category = \"red\"]"
"Yellow" 1.0 0 -1184463 true "" "plot count casualties with [triage-category = \"yellow\"]"
"Green" 1.0 0 -10899396 true "" "plot count casualties with [triage-category = \"green\"]"
"Blue" 1.0 0 -13345367 true "" "plot count casualties with [triage-category = \"blue\"]"
"Deceased" 1.0 0 -16777216 true "" "plot count casualties with [location = \"deceased\"]"
"Discharged" 1.0 0 -10402772 true "" "plot count casualties with [location = \"discharged\"]"

PLOT
902
122
1217
262
Hospital Flow
Time
Count
0.0
1000.0
0.0
10.0
true
true
"" ""
PENS
"Red Zone" 1.0 0 -2674135 true "" "plot count casualties with [location = \"red-zone\"]"
"Green Zone" 1.0 0 -10899396 true "" "plot count casualties with [location = \"green-zone\"]"

PLOT
901
415
1217
565
Queues
Time
People
0.0
100.0
0.0
10.0
true
true
"" ""
PENS
"Triage" 1.0 0 -3026479 true "" "plot length [triage-queue] of one-of hospitals"
"Red" 1.0 0 -2674135 true "" "plot length [red-zone-queue] of one-of hospitals"
"Green" 1.0 0 -13840069 true "" "plot length [green-zone-queue] of one-of hospitals"
"Yellow" 1.0 0 -1184463 true "" "plot length [yellow-zone-queue] of one-of hospitals"
"ICU" 1.0 0 -5825686 true "" "plot length [icu-queue] of one-of hospitals"
"Op. Rooms" 1.0 0 -955883 true "" "plot length [or-queue] of one-of hospitals"
"Wards" 1.0 0 -6459832 true "" "plot length [ward-queue] of one-of hospitals"

BUTTON
17
216
157
250
Export Patient Logs
export-tracking-data
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
12
127
199
161
green-self-transport-percentage
green-self-transport-percentage
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
12
90
171
124
red-casualty-percentage
red-casualty-percentage
0
100
24.0
1
1
NIL
HORIZONTAL

MONITOR
1022
70
1109
115
Are Treated
count casualties with [treatment-started? and treatment-progress < 100]
17
1
11

MONITOR
900
70
954
115
Moving
count casualties with [moving? = true]
17
1
11

MONITOR
955
70
1020
115
Waiting
count casualties with [moving? = false and (treatment-started? = false or treatment-started? = 0) and location != \"discharged\" and location != \"deseased\" and location != \"scene\"]
17
1
11

SLIDER
194
563
384
597
red-zone-doctors-per-seat
red-zone-doctors-per-seat
0
3
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
194
598
384
632
red-zone-nurses-per-seat
red-zone-nurses-per-seat
0
3
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
390
562
580
596
yellow-zone-doctors-per-seat
yellow-zone-doctors-per-seat
0
2
1.1
0.1
1
NIL
HORIZONTAL

SLIDER
390
598
580
632
yellow-zone-nurses-per-seat
yellow-zone-nurses-per-seat
0
2
0.9
0.1
1
NIL
HORIZONTAL

SLIDER
193
637
384
671
green-zone-doctors-per-seat
green-zone-doctors-per-seat
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
192
671
384
705
green-zone-nurses-per-seat
green-zone-nurses-per-seat
0
2
0.9
0.1
1
NIL
HORIZONTAL

SLIDER
390
638
580
672
blue-zone-doctors-per-seat
blue-zone-doctors-per-seat
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
390
671
579
705
blue-zone-nurses-per-seat
blue-zone-nurses-per-seat
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
586
562
731
596
or-doctors-per-seat
or-doctors-per-seat
1
3
2.0
0.1
1
NIL
HORIZONTAL

SLIDER
586
596
731
630
or-nurses-per-seat
or-nurses-per-seat
1
3
2.0
0.1
1
NIL
HORIZONTAL

SLIDER
588
637
732
671
icu-doctors-per-seat
icu-doctors-per-seat
0
2
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
589
671
732
705
icu-nurses-per-seat
icu-nurses-per-seat
1
3
2.0
0.1
1
NIL
HORIZONTAL

SLIDER
737
638
895
672
ward-doctors-per-seat
ward-doctors-per-seat
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
737
672
895
706
ward-nurses-per-seat
ward-nurses-per-seat
0
2
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
735
561
895
595
triage-doctors-per-seat
triage-doctors-per-seat
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
735
595
895
629
triage-nurses-per-seat
triage-nurses-per-seat
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
23
415
174
450
red-zone-seats
red-zone-seats
1
20
10.0
1
1
NIL
HORIZONTAL

SLIDER
23
448
174
483
yellow-zone-seats
yellow-zone-seats
1
30
5.0
1
1
NIL
HORIZONTAL

SLIDER
24
485
174
520
green-zone-seats
green-zone-seats
1
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
25
520
172
555
blue-zone-seats
blue-zone-seats
0
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
24
558
174
593
or-seats
or-seats
1
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
23
594
173
629
icu-seats
icu-seats
1
20
4.0
1
1
NIL
HORIZONTAL

SLIDER
24
630
172
665
ward-seats
ward-seats
5
50
10.0
1
1
NIL
HORIZONTAL

SLIDER
24
665
171
700
triage-seats
triage-seats
1
100
20.0
1
1
NIL
HORIZONTAL

SWITCH
2
379
191
413
prioritize-minimum-staffing
prioritize-minimum-staffing
1
1
-1000

MONITOR
1220
300
1601
338
Triage Distribution
triage-distribution
17
1
9

MONITOR
1218
122
1533
160
Survival Stats
survival-statistics
17
1
9

MONITOR
1220
160
1545
198
Utilization
resource-utilization
17
1
9

MONITOR
1218
199
1420
237
Bottleneck
bottleneck-analysis
17
1
9

MONITOR
1220
336
1536
374
Triage Accuracy
triage-accuracy
17
1
9

MONITOR
1219
263
1534
301
Zone Throughput
zone-throughput
17
1
9

MONITOR
1220
416
1527
454
Wait Times
wait-times-by-zone
17
1
9

@#$#@#$#@
## WHAT IS IT?

This model simulates the response to a mass casualty incident (MCI) within a hospital emergency system. It focuses on the flow of casualties from an incident scene through triage, different treatment zones, and ultimately to discharge or the morgue. The model implements key principles from WHO's Mass Casualty Management Guide including resource allocation, triage protocols, and patient flow management during an overwhelmed system.

## HOW IT WORKS

The model operates on several interconnected components:

1. **Casualties** are created at an incident scene with varying severity levels (measured by RPM scores from 1-12, with lower values indicating more severe conditions).

2. **Triage protocols** sort casualties into four main categories:
   - Red (critical, immediate life-threatening conditions)
   - Yellow (urgent but stable)
   - Green (walking wounded)
   - Blue (expectant/palliative)

3. **Resource management** tracks and allocates doctors and nurses to different zones based on priority and availability.

4. **Treatment zones** process patients according to their needs:
   - Red Zone for critical patients
   - Yellow Zone for urgent but stable patients
   - Green Zone for minor injuries
   - Blue Zone for expectant patients
   - OR (Operating Room) for surgical cases
   - ICU for intensive care
   - Ward for continuing care

5. **Patient deterioration** occurs over time for untreated patients, with severity increasing at rates dependent on their initial condition.

6. **Patient flow** follows unidirectional pathways to minimize congestion, with detailed tracking of movements between zones.

7. **Ambulances** transport casualties from the scene to the hospital, with a priority for red casualties.

## HOW TO USE IT

### Setup Parameters
- **number-of-casualties**: Total number of casualties in the incident
- **red-casualty-percentage**: Percentage of casualties with critical injuries
- **number-of-ambulances**: Number of ambulances available for transport
- **number-of-doctors**: Total doctors available in the hospital
- **number-of-nurses**: Total nurses available in the hospital
- **triage-strategy**: Choice between standard and "maximize-survivors" strategies
- **prioritize-minimum-staffing**: Whether to ensure minimum staffing across all zones
- **green-self-transport-percentage**: Percentage of green casualties who transport themselves

### Zone Capacity Settings
- **red-zone-seats**: Capacity of red zone
- **yellow-zone-seats**: Capacity of yellow zone
- **green-zone-seats**: Capacity of green zone
- **blue-zone-seats**: Capacity of blue zone (expectant)
- **or-seats**: Capacity of operating rooms
- **icu-seats**: Capacity of intensive care unit
- **ward-seats**: Capacity of general ward
- **triage-seats**: Capacity of triage area

### Staffing Ratios
For each zone, you can set:
- **doctors-per-seat**: Number of doctors required per treatment position
- **nurses-per-seat**: Number of nurses required per treatment position

### Buttons
- **setup**: Initializes the model according to parameter settings
- **go**: Runs the simulation
- **export-tracking-data**: Saves detailed patient tracking data to a CSV file

### Monitors
The interface includes monitors for:
- Triage distribution
- Survival statistics
- Resource utilization
- Bottleneck analysis
- Zone throughput
- Wait times by zone

## THINGS TO NOTICE

1. **Resource bottlenecks**: Watch how limited resources create waiting queues in different zones.

2. **Triage accuracy**: The model tracks how initial triage decisions align with actual patient outcomes and needs.

3. **Patient deterioration**: Untreated patients will deteriorate over time, potentially changing their triage category.

4. **Survival rates**: Monitor how different resource allocations and triage strategies affect overall survival.

5. **Zone overload**: Observe how certain zones (particularly red zone and OR) can become bottlenecks.

6. **Staff allocation patterns**: Notice how the prioritization of staff resources impacts treatment capacity.

7. **Unidirectional flow**: See how patients move through the system in defined pathways.

## THINGS TO TRY

1. **Vary casualty numbers and severity**: Increase the total casualties and the red-casualty-percentage to see how the system handles increasing load.

2. **Modify resource availability**: Experiment with different numbers of doctors and nurses to find minimum effective staffing levels.

3. **Change triage strategies**: Toggle between standard triage and "maximize-survivors" strategies to see impacts on overall mortality.

4. **Adjust zone capacities**: Modify the capacity of different treatment zones to identify optimal space allocation.

5. **Change staff-per-seat ratios**: Experiment with different staffing models to see how specialist distribution affects outcomes.

6. **Introduce self-transport**: Increase the green-self-transport-percentage to see how bypassing ambulance transport affects system flow.

7. **Priority shifting**: Try different resource allocation schemes by adjusting the prioritize-minimum-staffing toggle.

## EXTENDING THE MODEL

1. **Geographic elements**: Add realistic travel distances and times between the incident site and hospital.

2. **Multiple hospitals**: Extend the model to include several hospitals with varying capacities and specialties.

3. **Staff fatigue**: Implement diminishing productivity for staff who work continuously without breaks.

4. **Equipment limitations**: Add constraints for specialized equipment beyond just space and staff.

5. **Specialized patient needs**: Include more diversity in patient requirements beyond just severity level.

6. **Pre-hospital interventions**: Model the impact of treatment provided at the scene before transport.

7. **Delayed arrivals of off-duty staff**: Include the gradual arrival of additional staff called in during the incident.

8. **Supply chain limitations**: Model consumable resources that deplete over time.

9. **Secondary transfers**: Implement transfers between hospitals for load balancing.

10. **CBRN considerations**: Add components for decontamination and special handling for Chemical, Biological, Radiological or Nuclear incidents.

## NETLOGO FEATURES

1. **Agent scheduling**: Uses a combination of the NetLogo scheduler and custom task scheduling to manage treatment timing.

2. **Event logging**: Maintains detailed event logs for each casualty throughout their journey.

3. **Resource allocation system**: Implements a complex resource management system with prioritization rules.

4. **Custom visualization**: Creates visual representations of different hospital zones and patient states.

5. **Data export**: Features a CSV export function for detailed analysis of patient flow data.

6. **Task lists**: Uses list operations to manage queues of patients waiting for treatment in different zones.

7. **Scheduled tasks**: Implements a custom scheduling system for delayed events using lists of [time-to-execute, command-to-run] pairs.

## RELATED MODELS

- Emergency Department models in NetLogo Models Library
- Kiva Warehouse model (for resource allocation concepts)
- Spread of Disease models (for understanding patient deterioration mechanisms)
- Hospital Staffing models
- Traffic Grid models (for understanding movement and congestion patterns)

## CREDITS AND REFERENCES

This model is based on several foundational works in mass casualty management:

1. WHO Academy. (2022). "Mass Casualty Preparedness and Response in Emergency Units Guide". World Health Organization.

2. Wang, Y., Luangkesorn, K.L., & Shuman, L. (2012). "Modeling emergency medical response to a mass casualty incident using agent based simulation". Socio-Economic Planning Sciences, 46(4), 281-290.

3. Staribacher, D., Rauner, M.S., & Niessner, H. (2023). "Hospital Resource Planning for Mass Casualty Incidents: Limitations for Coping with Multiple Injured Patients". Healthcare, 11(20), 2713.

4. Sacco, W.J., Navin, D.M., Fiedler, K.E., Waddell, R.K., Long, W.B., & Buckman, R.F. (2005). "Precise formulation and evidence-based application of resource-constrained triage". Academic Emergency Medicine, 12(8), 759-770.

Model developed as an educational tool for emergency management training and hospital disaster planning.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
