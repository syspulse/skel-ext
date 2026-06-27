# Project Context

When working with this codebase, prioritize readability over cleverness. Ask clarifying questions before making architectural changes.

## About This Project

Collection to Plugin modules which implement Extractor Detector under Sentinel Framework.

Detector modules are implemented as Scala modules under one projects.

Detector may look very similar, so use other detectors as examples.

There are two major types of detectors:
- Detectors which read on-chain data onTx and onBlock callbacks
- Detectors which read off-chain data onCron callback

Offchain data can be read from API or from file or from database or other sources.

All detectors generate Alerts (Events). Alert is a type of Event. You must use EventUtil.createEvent() or error() to 
generate Alerts.

onCron, onBlock, onTx return sequence of Alerts. Empty sequence means no Alerts.

There is a lifecycle of a detector:

- onInit - initialize detector (called once when Plugin is initialized)
Initialize here globla state

- onStart - start detector (called when Plugin is started)
Start here global state and start any background threads.
It is called when Plugin is started:
1. After first onInit call
2. After it was Enabled (after it was Disabled)

- onUpdate - update detector (called when Plugin is updated)

It is called when Plugin is updated - configuration changed

Configuraiton is read from SentryRun context

Use DetectorName object with val and default values for confuguration options

Check the DetectorBubblemaps.scala how to read and set configuration for SentryRun context

- onStop - stop detector (called when Plugin is DISABLED)

There is no onFinish or onDestroy


## Key Directories

- Detectors usually reside in their own modules (subdirectories)
- Mulitple Detectors can be in one module

## Standards

- NEVER put credintials (api keys or secrets) in Source files
- `conf/application.conf` is used for Production config 
- `conf/application-dev.conf` is used for Dev environment config 
  never put credentials or secretes into these files, use environment variable references
- Use Environment variables to access secrets and credentials
- Create detector-test.conf inf `conf/` directory
- `conf/` must have the following files:
  1. `logback.xml` (take from example)
  2. `apppliction.conf` (take from example)
  3. `application-dev.conf` (take form example)
  4. `application-{detector}.conf` (used for local testing)
  5. `application-local.conf` symbolic link to `application-{detector}.conf`
  6. `detector-bundle.conf` - file specifying detector plugins (take from example)
- use `requests` library for HTTP API calls
- use `os` library for file operations
- use `spray` library for JSON operations (deser)
- Differentiate a data feed ingest/access logic from Detector functions. Use a separate class which can be tested independetly from Detector (look at Bubblemap detctor implementation)
- Add Spec tests for Business Logic
- Add Spec tests for full Detector creation (from DetectorConfig) and simulating onCron call to check Alerts


## Common Commands

Run test detector:
```bash
../sentinel.sh --conf=conf/detector-test-{DetectorId}-1.conf
```

## Notes
