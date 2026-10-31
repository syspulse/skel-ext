# sentry-cloud

`DetectorCloud` monitors Cloud provider health and security events via Cloud API (cron only: no `onBlock`/`onTx`).

| System    | AWS service            | What is reported                                                                 |
|-----------|------------------------|----------------------------------------------------------------------------------|
| `Watch`   | CloudWatch             | Metric alarms (`ALARM`, optionally `INSUFFICIENT_DATA`): EC2, EKS, VPC, ... health |
| `Trail`   | CloudTrail             | Management events (write only, read-only with `track_event`): IAM changes, security group, bucket policy, ... |
| `IAM`     | IAM credential report  | Root access keys, root/console users without MFA, old access keys                |
| `Storage` | S3                     | Buckets availability (`ACCESS_DENIED`, `NOT_FOUND`, `UNAVAILABLE`, `UNREACHABLE`, `SLOW`) |

Clouds: `aws://` (implemented), `gcp://` and `azure://` (**mocked**).

## Alert metadata

Alerts have `provider` (`aws`), `system` (generic system: `Watch`, `Trail`, `IAM`, `Storage`) and `sys` (Cloud specific name of the system), 
`region` and `account` (if defined), `tx_hash`, `ref`, `resource`, `status`, `title`, `count` and system specific attributes. 
`sys` can be used in `desc` template: `{sys}: {title}`.

Every Alert also has:

- `ref` - http link to open the finding in the Cloud console (if the Cloud has one)
- `tx_hash` - unique id of the event/entity assigned by the Cloud provider. Uniqueness of the Alert is `eid` (Detector + finding): there is no other `id` attribute.

| `system` | `tx_hash` AWS                      | `ref` AWS                                  |
|----------|------------------------------------|--------------------------------------------|
| `Trail`  | CloudTrail `EventId`               | CloudTrail event page (`#/events/<EventId>`) |
| `Watch`  | alarm ARN                          | CloudWatch alarm page                      |
| `IAM`    | user ARN (root: `...:root`)        | IAM user security credentials              |
| `Storage`| bucket ARN `arn:aws:s3:::<bucket>` | S3 bucket page                             |

If the Cloud does not provide the id, `tx_hash` is the internal finding id. GCP and Azure (mocked) do not set `ref`.

| `system` | `sys` AWS  | `sys` GCP (mocked) | `sys` Azure (mocked) |
|----------|------------|--------------------|----------------------|
| `Watch`  | `CloudWatch` | `Cloud Monitoring` | `Azure Monitor`    |
| `Trail`  | `CloudTrail` | `Cloud Audit Logs` | `Activity Log`     |
| `IAM`    | `IAM`        | `Cloud IAM`        | `Entra ID`         |
| `Storage`| `S3`         | `Cloud Storage`    | `Blob Storage`     |

## Detectors

| Detector | Schema | URI | Systems (combobox) |
|----------|--------|-----|--------------------|
| `DetectorCloud` | `schema-DetectorCloud.json` | any (`aws://`, `gcp://`, `azure://`) | free text: `Watch`, `Trail`, `IAM`, `Storage` (aliases accepted) |
| `DetectorCloudAWS` | `schema-DetectorCloudAWS.json` | `aws://` | `CloudWatch`, `CloudTrail`, `IAM`, `S3` |
| `DetectorCloudGCP` | `schema-DetectorCloudGCP.json` | `gcp://` | `Cloud Monitoring`, `Cloud Audit Logs`, `Cloud IAM`, `Cloud Storage` (**mocked**) |
| `DetectorCloudAzure` | `schema-DetectorCloudAzure.json` | `azure://` | `Azure Monitor`, `Activity Log`, `Entra ID`, `Blob Storage` (**mocked**) |

Cloud specific Detectors are extensions of `DetectorCloud`. Schema field `description` is terse: details are in FAQ.

## Configuration

| Parameter   | Default                  | Description                                                              |
|-------------|--------------------------|--------------------------------------------------------------------------|
| `uri`       | `aws://`                 | Cloud URI (see below)                                                    |
| `auth`      | (empty)                  | Optional AuthURI: `secret://`, `env://`, `profile://`, `role://` (see [Auth](#auth)). Has a preference over credentials in `uri` |
| `cron`      | `10 min`                 | Check interval                                                           |
| `system`    | `Watch`                  | Comma-separated: `Watch`, `Trail`, `IAM`, `Storage` (aliases: `CloudWatch`, `CloudTrail`, `S3`) |
| `condition` | `> 0`                    | Condition on the number of findings (per system) to generate Alerts      |
| `track_new`  | `true`                   | Alert once on every new finding (a resolved finding alerts again if it returns) |
| `track_event` | `false`                | `Trail`: Alerts for ALL CloudTrail events including read-only (`Get*`, `List*`, ...) with `INFO` severity (default: write events only) |
| `timeout`   | `60000`                  | Cloud API timeout, ms                                                    |
| `desc`      | `{system}: {title}{err}` | Description template (any Alert metadata can be used: `{alarm}`, `{state}`, `{user}`, ...) |
| `track_err`, `err_always` | `true`     | Alert on Cloud errors                                                    |

Defaults are in `DetectorCloud` object (`DEF_*`). `schema-DetectorCloud.json` must be updated when parameters change.

## Cloud URI

```
<cloud>://ACCESS_KEY:ACCESS_SECRET[:SESSION_TOKEN]@<host>?param=value&param=value
```

- `<cloud>`: `aws`, `gcp`, `azure`
- `@` is required if credentials are specified in URI. `host` is optional (custom endpoint: LocalStack, moto)
- Credentials which are not in URI are read from environment. Prefix depends on the cloud (`AWS_`, `GCP_`, `AZURE_`):
  `<PREFIX>ACCESS_KEY`, `<PREFIX>ACCESS_SECRET`, `<PREFIX>SESSION_TOKEN`, `<PREFIX>REGION`.
  AWS standard variables are also supported: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`, `AWS_DEFAULT_REGION`.
  Key and secret are always taken as a **pair** of one scheme (`AWS_ACCESS_KEY` + `AWS_ACCESS_SECRET` or `AWS_ACCESS_KEY_ID` + `AWS_SECRET_ACCESS_KEY`), never mixed.
  If both pairs are defined (e.g. `AWS_ACCESS_KEY=test` left from the mock and real keys from SSO), the standard `AWS_ACCESS_KEY_ID` pair is used when 
  `AWS_SESSION_TOKEN` is defined (the token is issued only with it), otherwise `AWS_ACCESS_KEY` pair is used. Detector logs names (not values) of the variables in use.
  If nothing is found, AWS default credentials chain is used (`~/.aws`, instance profile).
- Region and account: `region=` and `account=` parameters, or `<PREFIX>REGION` and `<PREFIX>ACCOUNT` environment variables 
  (`AWS_REGION`, `AWS_ACCOUNT`, `GCP_REGION`, `GCP_ACCOUNT`, `AZURE_REGION`, `AZURE_ACCOUNT`). Parameters have priority. 
  They are added to every Alert (`region`, `account` metadata). `AWS_DEFAULT_REGION` is also supported.
- `{VAR}` is expanded in credentials: `aws://{MY_KEY}:{MY_SECRET}@?region=eu-west-1`
- **Never put real credentials into configuration files**: use environment variables.

### AWS

```
aws://                                                           credentials and region from environment
aws://?region=eu-west-1                                          region in URI
aws://AKIA...:SECRET:SESSION_TOKEN@?region=eu-west-1             temporary credentials
aws://localhost:5000?region=us-east-1&ssl=false                  custom endpoint (moto, LocalStack)
```

| System    | Parameters                                                                                       |
|-----------|--------------------------------------------------------------------------------------------------|
| `Watch`   | `states=ALARM,INSUFFICIENT_DATA`, `alarm_prefix=prod-`, `namespace=AWS/EC2,AWS/EKS,AWS/NATGateway` |
| `Trail`   | `source=iam.amazonaws.com,s3.amazonaws.com` (empty = all write events), `events=CreateUser,DeleteUser`, `window=60` (minutes), `trail_region` |
| `IAM`     | `max_key_age=90` (days)                                                                          |
| `Storage` | `buckets=a,b` (empty = all), `bucket_prefix=prod-`, `latency=3000` (ms, report slow buckets)     |

Notes:
- CloudTrail events are delayed by several minutes: use `window` of 30-60 minutes. 
  Global services (IAM, STS, ...) events are looked up in `us-east-1` (override with `trail_region`).
- By default only write events generate Alerts (severity `HIGH` for security sensitive ones, `MEDIUM` for others). Read-only events (`Get*`, `List*`, `Describe*`, ...) 
  are skipped (log: `Trail: events=73, alerts=0`). Detector config `"track_event": true` generates Alerts for them with `INFO` severity (`CloudAWS.DEF_TRAIL_EVENT_SEV`). Use `events=` and `source=` to limit the volume: max 200 events are looked up per source.
- Required permissions: `cloudwatch:DescribeAlarms`, `cloudtrail:LookupEvents`, `iam:GenerateCredentialReport`, `iam:GetCredentialReport`, `s3:ListAllMyBuckets`, `s3:ListBucket`

### AWS credentials and refresh

Temporary credentials (`SESSION_TOKEN`, SSO) **expire**. A running Detector cannot see environment changes 
and does not re-read the URI, so **a static `SESSION_TOKEN` (in URI or in environment) is never refreshed**
(Detector logs a warning if it is used). Use a source which is refreshed.

Credentials source (first match wins):

| # | Source | URI | Refreshed |
|---|--------|-----|-----------|
| 1 | Keys in URI | `aws://KEY:SECRET[:TOKEN]@` | only without `TOKEN` |
| 2 | Profile of shared credentials file | `aws://?profile=sentry[&profile_file=/path/credentials]` | yes |
| 3 | Keys from environment | `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`[, `AWS_SESSION_TOKEN`] | only without `AWS_SESSION_TOKEN` |
| 4 | AWS default chain (nothing specified) | `aws://?region=eu-west-1` | yes |

**`aws://` with environment variables**: 

- `AWS_ACCESS_KEY_ID` + `AWS_SECRET_ACCESS_KEY` + `AWS_SESSION_TOKEN`: all are used as is. Token expires and is NOT refreshed.
- `AWS_ACCESS_KEY_ID` + `AWS_SECRET_ACCESS_KEY` only (long-lived IAM user keys): the Cloud **requests the session token itself** 
  (STS `GetSessionToken`) and renews it before it expires. Disable with `sts=false` (keys are used as is).
  Keys of a URI follow the same rule. A token is never mixed from a different source (URI keys do not get `AWS_SESSION_TOKEN` from environment).
  `GetSessionToken` cannot be called with temporary credentials, so it is used only for long-lived keys.

`role_arn` can be added to any of them: the base credentials are used only to call STS `AssumeRole`, 
and the role credentials are refreshed automatically before they expire (`external_id`, `role_session`, `role_duration` in seconds, default 3600 are optional):

```
aws://?region=eu-west-1&role_arn=arn:aws:iam::123456789012:role/Sentry&external_id=abc
```

Recommended:

- **Production (ECS, EKS, EC2)**: no credentials in config. Use IAM role of the task/pod (IRSA)/instance and `aws://?region=eu-west-1`. 
  Default chain gets and refreshes credentials from the metadata service / web identity token.
  To access another account add `role_arn`.
- **Outside AWS**: long-lived key of a dedicated IAM user (or on-premise host) in environment variables + `role_arn` 
  (long-lived key is used only to assume the role).
- **Developer machine (SSO)**: `profile` with `credential_process`. AWS SDK v1 does not understand SSO profiles (and cannot parse 
  `Expiration` with `+00:00` offset returned by AWS CLI), but it runs `credential_process` and calls it again when credentials 
  are about to expire. `bin/aws-credential-process.sh` does it:
  ```ini
  # ~/.aws/credentials
  [sentry]
  credential_process = /path/to/sentry-cloud/bin/aws-credential-process.sh haas-dev
  ```
  ```
  aws://?region=eu-west-1&profile=sentry
  ```
  SSO session itself must be valid (`aws sso login --profile haas-dev`): when it expires, Detector reports an error (`track_err`) 
  and recovers after login, no restart is needed.

Note: `credential_process` must be in the shared *credentials* file (`~/.aws/credentials` or `profile_file`), not in `~/.aws/config`.

### Auth

`auth` is an optional AuthURI (package `cloud.auth`). If it is specified, it **overrides** credentials in `uri` 
(keys, `profile`, `profile_file`, `role_arn`, `external_id`, `sts`) and environment keys. `region`, `account` and the custom host are still taken from `uri`.

```
<scheme>://<body>?param=value&param=value
```

| AuthURI | Description | Refreshed |
|---------|-------------|-----------|
| `secret://<ACCESS_KEY_ID>:<SECRET_ACCESS_KEY>[:SESSION_TOKEN]` | Static keys. Without `SESSION_TOKEN` it is requested (STS `GetSessionToken`) | token is requested: yes, given token: no |
| `env://[$ACCESS_KEY_ID]:[$SECRET_ACCESS_KEY]:[$SESSION_TOKEN]` | Names of environment variables. Omitted name: default (`AWS_ACCESS_KEY`, `AWS_ACCESS_SECRET`, `AWS_SESSION_TOKEN`) | same as `secret://` |
| `profile://[file]` | Profile of the credentials file (default `~/.aws/credentials`). Name: `?profile=`, `AWS_PROFILE` or `default` | yes |
| `role://` | IAM role of the environment: ECS task, EKS pod (IRSA), EC2 instance profile (AWS default chain) | yes |

Parameters (any scheme): `arn` (assume the role with these credentials, refreshed automatically), `external_id`, `session`, `duration` (seconds, default 3600), `sts=false` (do not request `SESSION_TOKEN`).

```
secret://{AWS_ACCESS_KEY}:{AWS_ACCESS_SECRET}                    keys from environment (expanded)
env://                                                           default environment variables
env://$MY_KEY:$MY_SECRET                                         custom variables
env://::$MY_TOKEN                                                default key and secret, custom SESSION_TOKEN variable
profile://?profile=sentry                                        ~/.aws/credentials
profile:///etc/sentry/credentials?profile=sentry                 credentials file
role://                                                          IAM role of the pod/task/instance
role://?arn=arn:aws:iam::123456789012:role/Sentry&external_id=abc    assume the role
```

Notes:
- `SESSION_TOKEN` of the default variable belongs to the default keys: with custom `$KEY`/`$SECRET` names it is never taken from a default variable (only from the custom third name).
- Invalid `auth` (unknown scheme, `secret://` without secret, undefined environment variable) stops the Detector with an error Alert.
- Never put real keys in configuration files: use `env://` or `{VAR}` expansion in `secret://`. Detector configuration is a part of Alerts.
- GCP and Azure (mocked) ignore `auth`.

### GCP, Azure (mocked)

```
gcp://?project=my-project&mock=alarm
azure://?subscription=my-subscription&mock=alarm
```

`mock=alarm` (default): every system returns a sample finding, `mock=ok`: no findings, `mock=fail`: every check fails.

## Run Detector

Configurations are in `conf/`. Run from `sentry-cloud/` directory:

```bash
# AWS credentials (temporary)
source /tmp/env.dev

# CloudWatch alarms (basic AWS integration)
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-1.conf

# CloudWatch alarms of EC2, EKS, VPC
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-Watch-1.conf

# Refreshable credentials: assume role (AWS_ROLE_ARN) or profile (SSO, credential_process)
AWS_ROLE_ARN=arn:aws:iam::123456789012:role/Sentry ../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-Role-1.conf
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-Profile-1.conf

# CloudTrail: IAM changes
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-Trail-1.conf

# IAM credential report and S3 availability
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-2.conf

# Cloud specific Detectors (system is a combobox)
../sentinel.sh --conf=conf/detector-DetectorCloudAWS-1.conf
../sentinel.sh --conf=conf/detector-DetectorCloudGCP-1.conf
../sentinel.sh --conf=conf/detector-DetectorCloudAzure-1.conf

# GCP (mocked), no credentials
../sentinel.sh --conf=conf/detector-DetectorCloud-Mock-1.conf

# AWS mock (moto): CloudWatch, IAM, S3
./mock/moto.sh start
export AWS_ACCESS_KEY=test AWS_ACCESS_SECRET=test
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-Moto-1.conf
```

## Tests

```bash
# unit tests + tests against AWS mock (moto tests are canceled if mock is not running)
sbt sentry_cloud/test

# tests against mocked AWS (CloudWatch, IAM, S3)
./mock/moto.sh start
sbt "sentry_cloud/testOnly *CloudAWSMockSpec"

# tests against REAL AWS: creates and deletes temporary CloudWatch alarm and IAM user, 
# waits for CloudTrail events (up to CLOUD_IT_TRAIL_TIMEOUT minutes, default 20)
source /tmp/env.dev
CLOUD_AWS_IT=true sbt "sentry_cloud/testOnly *CloudAWSIntegrationSpec"
```

### Seeing Alerts from the mock in a running Sentinel

```bash
./mock/moto.sh start
export AWS_ACCESS_KEY=test AWS_ACCESS_SECRET=test                    # dummy credentials for AWS SDK
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-Moto-1.conf    # terminal 1 

CLOUD_MOCK_KEEP=true sbt "sentry_cloud/testOnly *CloudAWSMockSpec"   # terminal 2
```

By default tests delete their alarms/users/buckets at the end (they live for milliseconds), so a running Sentinel never sees them.
`CLOUD_MOCK_KEEP=true` keeps them. Sentinel checks on its cron (`1 min`, also the minimum), 
and prints every finding once (`track_new`): Alerts appear as `Event(DetectorCloud,...)` on stdout within a minute.

### AWS mock

`mock/moto.sh` starts [moto](https://github.com/getmoto/moto) server (docker image `motoserver/moto` if docker is available, otherwise python venv).
 
Verified with docker (`motoserver/moto:latest`, `localstack/localstack:3.8`):

| Service                     | moto                | LocalStack 3.8 (Community) |
|-----------------------------|---------------------|----------------------------|
| CloudWatch alarms           | yes                 | yes                        |
| IAM credential report       | yes                 | yes (report is generated)  |
| S3 `HeadBucket`             | yes                 | yes                        |
| CloudTrail `LookupEvents`   | **no** (HTTP 500)   | **no** ("pro feature")     |

`localstack/localstack:latest` refuses to start without a LocalStack account / auth token, so moto is used for tests.

`Trail` can be tested only against real AWS (`CloudAWSIntegrationSpec`). 
moto ignores `StateValue` filter of `DescribeAlarms`: the Detector also filters alarms by state itself.
