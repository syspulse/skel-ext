# sentry-cloud

`DetectorCloud` monitors Cloud provider health and security events via Cloud API (cron only: no `onBlock`/`onTx`).

| System    | AWS service            | What is reported                                                                 |
|-----------|------------------------|----------------------------------------------------------------------------------|
| `Watch`   | CloudWatch             | Metric alarms (`ALARM`, optionally `INSUFFICIENT_DATA`): EC2, EKS, VPC, ... health |
| `Trail`   | CloudTrail             | Management events (write only): IAM changes, security group, bucket policy, ... |
| `IAM`     | IAM credential report  | Root access keys, root/console users without MFA, old access keys                |
| `Storage` | S3                     | Buckets availability (`ACCESS_DENIED`, `NOT_FOUND`, `UNAVAILABLE`, `UNREACHABLE`, `SLOW`) |

Clouds: `aws://` (implemented), `gcp://` and `azure://` (**mocked**).

## Configuration

| Parameter   | Default                  | Description                                                              |
|-------------|--------------------------|--------------------------------------------------------------------------|
| `uri`       | `aws://`                 | Cloud URI (see below)                                                    |
| `cron`      | `10 min`                 | Check interval                                                           |
| `system`    | `Watch`                  | Comma-separated: `Watch`, `Trail`, `IAM`, `Storage` (aliases: `CloudWatch`, `CloudTrail`, `S3`) |
| `condition` | `> 0`                    | Condition on the number of findings (per system) to generate Alerts      |
| `new_only`  | `true`                   | Alert once on every new finding (a resolved finding alerts again if it returns) |
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
  If nothing is found, AWS default credentials chain is used (`~/.aws`, instance profile).
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
- Required permissions: `cloudwatch:DescribeAlarms`, `cloudtrail:LookupEvents`, `iam:GenerateCredentialReport`, `iam:GetCredentialReport`, `s3:ListAllMyBuckets`, `s3:ListBucket`

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

# CloudTrail: IAM changes
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-Trail-1.conf

# IAM credential report and S3 availability
../sentinel.sh --conf=conf/detector-DetectorCloud-AWS-2.conf

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
and prints every finding once (`new_only`): Alerts appear as `Event(DetectorCloud,...)` on stdout within a minute.

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
